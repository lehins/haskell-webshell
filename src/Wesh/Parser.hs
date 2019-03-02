module Wesh.Parser
  ( Ps
  , Pm
  , CSIFunc(..)
  , ESCFunc(..)
  , CtrlSeq(..)
  , KSeq(..)
  , tParse
  , feed
  ) where

import RIO
import Data.Attoparsec.ByteString hiding (inClass, satisfy)
import Data.Attoparsec.Combinator (many')
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8 hiding (takeWhile1)

-- Reference: http://invisible-island.net/xterm/ctlseqs/ctlseqs.html

type Ps = Int
type Pm = [Ps]

data CSIFunc =
  -- | @CSI Ps @@ Insert P s (Blank) Character(s) (default = 1) (ICH)
  ICH Ps |
  -- | @CSI Ps A@ Cursor Up P s Times (default = 1) (CUU).
  CUU Ps |
  -- | @CSI Ps B@ Cursor Down P s Times (default = 1) (CUD).
  CUD Ps |
  -- | @CSI Ps C@ Cursor Forward P s Times (default = 1) (CUF).
  CUF Ps |
  -- | @CSI Ps D@ Cursor Backward P s Times (default = 1) (CUB).
  CUB Ps |
  -- | @CSI Ps E@ Cursor Next Line P s Times (default = 1) (CNL).
  CNL Ps |
  -- | @CSI Ps F@ Cursor Preceding Line P s Times (default = 1) (CPL).
  CPL Ps |
  -- | @CSI Ps G@ Cursor Character Absolute [column] (default = [row,1]) (CHA).
  CHA Ps |
 -- | @CSI Ps ; Ps H@ Cursor Position [row;column] (default = [1,1]) (CUP).
  CUP Ps Ps |
  -- | @CSI P s I@ Cursor Forward Tabulation P s tab stops (default = 1) (CHT).
  CHT Ps |
  -- | @CSI Ps J@ Erase in Display (ED).
  -- * Ps = 0 → Erase Below (default).
  -- * Ps = 1 → Erase Above.
  -- * Ps = 2 → Erase All.
  -- * Ps = 3 → Erase Saved Lines (xterm).
  ED Ps |
  -- | @CSI ? Ps J@ Erase in Display (DECSED).
  -- * Ps = 0 → Selective Erase Below (default).
  -- * Ps = 1 → Selective Erase Above.
  -- * Ps = 2 → Selective Erase All.
  DECSED Ps |
  -- | @CSI Ps K@ Erase in Line (EL).
  -- * Ps = 0 → Erase to Right (default).
  -- * Ps = 1 → Erase to Left.
  -- * Ps = 2 → Erase All.
  EL Ps |
  -- | @CSI ? Ps K@ Erase in Line (DECSEL).
  -- * Ps = 0 → Selective Erase to Right (default).
  -- * Ps = 1 → Selective Erase to Left.
  -- * Ps = 2 → Selective Erase All.
  DECSEL Ps |
  -- | @CSI Ps L@ Insert Ps Line(s) (default = 1) (IL).
  IL Ps |
  -- | @CSI Ps M@ Delete Ps Line(s) (default = 1) (DL).
  DL Ps |
  -- | @CSI Ps P@ Delete Ps Character(s) (default = 1) (DCH).
  DCH Ps |
  -- | @CSI Ps S@ Scroll up Ps lines (default = 1) (SU).
  SU Ps |
  -- | @CSI Ps T@ Scroll down Ps lines (default = 1) (SD).
  SD Ps |
  -- | @CSI Ps ; Ps ; Ps ; Ps ; Ps T@ Initiate highlight mouse tracking.
  -- Parameters are @[func;startx;starty;firstrow;lastrow]@.
  MouseTrackInit Ps Ps Ps Ps Ps |
  -- | @CSI > Ps ; Ps T@ Reset one or more features of the title modes to the
  -- default value. Normally, "reset" disables the feature. It is possible to
  -- disable the ability to reset features by compiling a different default for
  -- the title modes into xterm.
  -- * Ps = 0 → Do not set window/icon labels using hexadecimal.
  -- * Ps = 1 → Do not query window/icon labels using hexadecimal.
  -- * Ps = 2 → Do not set window/icon labels using UTF-8.
  -- * Ps = 3 → Do not query window/icon labels using UTF-8.
  ResetTitleModes Ps Ps |
  -- | @CSI Ps X@ Erase Ps Character(s) (default = 1) (ECH).
  ECH Ps |
  -- | @CSI Ps Z@ Cursor Backward Tabulation P s tab stops (default = 1) (CBT).
  CBT Ps |
  -- | @CSI Pm `@ Character Position Absolute [column] (default = [row,1]) (HPA).
  HPA Pm |
  -- | @CSI Pm a@ Character Position Relative [columns]
  -- (default = [row,col+1]) (HPR).
  HPR Pm |
  -- | @CSI Ps b@ Repeat the preceding graphic character Ps times (REP).
  REP Ps |
  -- | @CSI Ps c@ Send Device Attributes (Primary DA).
  -- * Ps = 0 or omitted → request attributes from terminal.
  -- The response depends on the @decTerminalID@ resource setting.
  -- → CSI ? 1 ; 2 c (‘‘VT100 with Advanced Video Option’’)
  -- → CSI ? 1 ; 0 c (‘‘VT101 with No Options’’)
  -- → CSI ? 6 c (‘‘VT102’’)
  -- → CSI ? 6 0 ; 1 ; 2 ; 6 ; 8 ; 9 ; 1 5 ; c (‘‘VT220’’)
  -- The VT100-style response parameters do not mean anything by themselves. VT220 parameters do, telling the host what features the terminal supports:
  -- * Ps = 1 → 132-columns.
  -- * Ps = 2 → Printer.
  -- * Ps = 6 → Selective erase.
  -- * Ps = 8 → User-defined keys.
  -- * Ps = 9 → National replacement character sets.
  -- * Ps = 15 → Technical characters.
  -- * Ps = 18 → User windows.
  -- * Ps = 21 → Horizontal scrolling.
  -- * Ps = 22 → ANSI color, e.g., VT525.
  -- * Ps = 29 → ANSI text locator (i.e., DEC Locator mode).
  PrimDA Ps |
  -- | @CSI > Ps c@ Send Device Attributes (Secondary DA).
  -- * Ps = 0 or omitted → request the terminal’s identification code. The response depends on the @decTerminalID@ resource setting. It should apply only to VT220 and up, but xterm extends this to VT100.
  -- → CSI > Pp ; Pv ; Pc c
  -- where Pp denotes the terminal type:
  -- * Pp = 0 → ‘‘VT100’’.
  -- * Pp = 1 → ‘‘VT220’’.
  -- * Pp = 2 → ‘‘VT240’’.
  -- * Pp = 18 → ‘‘VT330’’.
  -- * Pp = 19 → ‘‘VT340’’.
  -- * Pp = 24 → ‘‘VT320’’.
  -- * Pp = 41 → ‘‘VT420’’.
  -- * Pp = 61 → ‘‘VT510’’.
  -- * Pp = 64 → ‘‘VT520’’.
  -- * Pp = 65 → ‘‘VT525’’.
  -- and Pv is the firmware version (for xterm, this was originally the XFree86 patch number, starting with 95). In a DEC terminal, Pc indicates the ROM cartridge registration number and is always zero.
  SecDA Ps |
  -- | @CSI Pm d@ Line Position Absolute [row] (default = [1,column]) (VPA).
  VPA Pm |
  -- | @CSI Pm e@ Line Position Relative [rows] (default = [row+1,column]) (VPR).
  VPR Pm |
  -- | @CSI Ps ; Ps f@ Horizontal and Vertical Position [row;column] (default = [1,1]) (HVP).
  HVP Ps Ps |
  -- | @CSI Ps g@ Tab Clear (TBC).
  -- * Ps = 0 → Clear Current Column (default).
  -- * Ps = 3 → Clear All.
  TBC Ps |
  -- | @CSI Pm h@ Set Mode (SM).
  -- * Ps = 2 → Keyboard Action Mode (AM).
  -- * Ps = 4 → Insert Mode (IRM).
  -- * Ps = 12 → Send/receive (SRM).
  -- * Ps = 20 → Automatic Newline (LNM).
  SM Pm |
  -- | @CSI ? Pm h@ DEC Private Mode Set (DECSET).
  -- * Ps = 1 → Application Cursor Keys (DECCKM).
  -- * Ps = 2 → Designate USASCII for character sets G0-G3 (DECANM), and set VT100 mode.
  -- * Ps = 3 → 132 Column Mode (DECCOLM).
  -- * Ps = 4 → Smooth (Slow) Scroll (DECSCLM).
  -- * Ps = 5 → Reverse Video (DECSCNM).
  -- * Ps = 6 → Origin Mode (DECOM).
  -- * Ps = 7 → Wraparound Mode (DECAWM).
  -- * Ps = 8 → Auto-repeat Keys (DECARM).
  -- * Ps = 9 → Send Mouse X & Y on button press. See the section Mouse Tracking.
  -- * Ps = 10 → Show toolbar (rxvt).
  -- * Ps = 12 → Start Blinking Cursor (att610).
  -- * Ps = 18 → Print form feed (DECPFF).
  -- * Ps = 19 → Set print extent to full screen (DECPEX).
  -- * Ps = 25 → Show Cursor (DECTCEM).
  -- * Ps = 30 → Show scrollbar (rxvt).
  -- * Ps = 35 → Enable font-shifting functions (rxvt).
  -- * Ps = 38 → Enter Tektronix Mode (DECTEK).
  -- * Ps = 40 → Allow 80 → 132 Mode.
  -- * Ps = 41 → more(1) fix (see curses resource).
  -- * Ps = 42 → Enable Nation Replacement Character sets (DECNRCM).
  -- * Ps = 44 → Turn On Margin Bell.
  -- * Ps = 45 → Reverse-wraparound Mode.
  -- * Ps = 46 → Start Logging. This is normally disabled by a compile-time option.
  -- * Ps = 47 → Use Alternate Screen Buffer. (This may be disabled by the titeInhibit resource).
  -- * Ps = 66 → Application keypad (DECNKM).
  -- * Ps = 67 → Backarrow key sends backspace (DECBKM).
  -- * Ps = 69 → Enable left and right margin mode (DECLRMM), VT420 and up.
  -- * Ps = 95 → Do not clear screen when DECCOLM is set/reset (DECNCSM), VT510 and up.
  -- * Ps = 1000 → Send Mouse X & Y on button press and release. See the section Mouse Tracking.
  -- * Ps = 1001 → Use Hilite Mouse Tracking.
  -- * Ps = 1002 → Use Cell Motion Mouse Tracking.
  -- * Ps = 1003 → Use All Motion Mouse Tracking.
  -- * Ps = 1004 → Send FocusIn/FocusOut events.
  -- * Ps = 1005 → Enable UTF-8 Mouse Mode.
  -- * Ps = 1006 → Enable SGR Mouse Mode.
  -- * Ps = 1007 → Enable Alternate Scroll Mode.
  -- * Ps = 1010 → Scroll to bottom on tty output (rxvt).
  -- * Ps = 1015 → Enable urxvt Mouse Mode.
  -- * Ps = 1011 → Scroll to bottom on key press (rxvt).
  -- * Ps = 1034 → Interpret "meta" key, sets eighth bit. (enables the eightBitInput resource).
  -- * Ps = 1035 → Enable special modifiers for Alt and NumLock keys. (This enables the numLock resource).
  -- * Ps = 1036 → Send ESC when Meta modifies a key. (This enables the metaSendsEscape resource).
  -- * Ps = 1037 → Send DEL from the editing-keypad Delete key.
  -- * Ps = 1039 → Send ESC when Alt modifies a key. (This enables the altSendsEscape resource).
  -- * Ps = 1040 → Keep selection even if not highlighted. (This enables the keepSelection resource).
  -- * Ps = 1041 → Use the CLIPBOARD selection. (This enables the selectToClipboard resource).
  -- * Ps = 1042 → Enable Urgency window manager hint when Control-G is received. (This enables the bellIsUrgent resource).
  -- * Ps = 1043 → Enable raising of the window when Control-G is received. (enables the popOnBell resource).
  -- * Ps = 1047 → Use Alternate Screen Buffer. (This may be disabled by the titeInhibit resource).
  -- * Ps = 1048 → Save cursor as in DECSC. (This may be disabled by the titeInhibit resource).
  -- * Ps = 1049 → Save cursor as in DECSC and use Alternate Screen Buffer, clearing it first. (This may be disabled by the titeInhibit resource). This combines the effects of the 1047 and 1048 modes. Use this with terminfo-based applications rather than the 4 7 mode.
  -- * Ps = 1050 → Set terminfo/termcap function-key mode.
  -- * Ps = 1051 → Set Sun function-key mode.
  -- * Ps = 1052 → Set HP function-key mode.
  -- * Ps = 1053 → Set SCO function-key mode.
  -- * Ps = 1060 → Set legacy keyboard emulation (X11R6).
  -- * Ps = 1061 → Set VT220 keyboard emulation.
  -- * Ps = 2004 → Set bracketed paste mode.
  DECSET Pm |
  -- | @CSI Pm i@ Media Copy (MC).
  -- * Ps = 0 → Print screen (default).
  -- * Ps = 4 → Turn off printer controller mode.
  -- * Ps = 5 → Turn on printer controller mode.
  MC Pm |
  -- | @CSI ? Pm i@ Media Copy (MC, DEC-specific).
  -- * Ps = 1 → Print line containing cursor.
  -- * Ps = 4 → Turn off autoprint mode.
  -- * Ps = 5 → Turn on autoprint mode.
  -- * Ps = 10 → Print composed display, ignores DECPEX.
  -- * Ps = 11 → Print all pages.
  DECMC Pm |
  -- | @CSI Pm l@ Reset Mode (RM).
  -- * Ps = 2 → Keyboard Action Mode (AM).
  -- * Ps = 4 → Replace Mode (IRM).
  -- * Ps = 12 → Send/receive (SRM).
  -- * Ps = 20 → Normal Linefeed (LNM).
  RM Pm |
  -- | @CSI ? Pm l@ DEC Private Mode Reset (DECRST).
  -- * Ps = 1 → Normal Cursor Keys (DECCKM).
  -- * Ps = 2 → Designate VT52 mode (DECANM).
  -- * Ps = 3 → 80 Column Mode (DECCOLM).
  -- * Ps = 4 → Jump (Fast) Scroll (DECSCLM).
  -- * Ps = 5 → Normal Video (DECSCNM).
  -- * Ps = 6 → Normal Cursor Mode (DECOM).
  -- * Ps = 7 → No Wraparound Mode (DECAWM).
  -- * Ps = 8 → No Auto-repeat Keys (DECARM).
  -- * Ps = 9 → Don’t send Mouse X & Y on button press.
  -- * Ps = 10 → Hide toolbar (rxvt).
  -- * Ps = 12 → Stop Blinking Cursor (att610).
  -- * Ps = 18 → Don’t print form feed (DECPFF).
  -- * Ps = 19 → Limit print to scrolling region (DECPEX).
  -- * Ps = 25 → Hide Cursor (DECTCEM).
  -- * Ps = 30 → Don’t show scrollbar (rxvt).
  -- * Ps = 35 → Disable font-shifting functions (rxvt).
  -- * Ps = 40 → Disallow 80 → 132 Mode.
  -- * Ps = 41 → No more(1) fix (see curses resource).
  -- * Ps = 42 → Disable Nation Replacement Character sets (DECNRCM).
  -- * Ps = 44 → Turn Off Margin Bell.
  -- * Ps = 45 → No Reverse-wraparound Mode.
  -- * Ps = 46 → Stop Logging. (This is normally disabled by a compile-time option).
  -- * Ps = 47 → Use Normal Screen Buffer.
  -- * Ps = 66 → Numeric keypad (DECNKM).
  -- * Ps = 67 → Backarrow key sends delete (DECBKM).
  -- * Ps = 69 → Disable left and right margin mode (DECLRMM), VT420 and up.
  -- * Ps = 95 → Clear screen when DECCOLM is set/reset (DECNCSM), VT510 and up.
  -- * Ps = 1000 → Don’t send Mouse X & Y on button press and release. See the section Mouse Tracking.
  -- * Ps = 1001 → Don’t use Hilite Mouse Tracking.
  -- * Ps = 1002 → Don’t use Cell Motion Mouse Tracking.
  -- * Ps = 1003 → Don’t use All Motion Mouse Tracking.
  -- * Ps = 1004 → Don’t send FocusIn/FocusOut events.
  -- * Ps = 1005 → Disable UTF-8 Mouse Mode.
  -- * Ps = 1006 → Disable SGR Mouse Mode.
  -- * Ps = 1007 → Disable Alternate Scroll Mode.
  -- * Ps = 1010 → Don’t scroll to bottom on tty output (rxvt).
  -- * Ps = 1015 → Disable urxvt Mouse Mode.
  -- * Ps = 1011 → Don’t scroll to bottom on key press (rxvt).
  -- * Ps = 1034 → Don’t interpret "meta" key. (This disables the eightBitInput resource).
  -- * Ps = 1035 → Disable special modifiers for Alt and NumLock keys. (This disables the numLock resource).
  -- * Ps = 1036 → Don’t send ESC when Meta modifies a key. (This disables the metaSendsEscape resource).
  -- * Ps = 1037 → Send VT220 Remove from the editing-keypad Delete key.
  -- * Ps = 1039 → Don’t send ESC when Alt modifies a key. (This disables the altSendsEscape resource).
  -- * Ps = 1040 → Do not keep selection when not highlighted. (This disables the keepSelection resource).
  -- * Ps = 1041 → Use the PRIMARY selection. (This disables the selectToClipboard resource).
  -- * Ps = 1042 → Disable Urgency window manager hint when Control-G is received. (This disables the bellIsUrgent resource).
  -- * Ps = 1043 → Disable raising of the window when Control-G is received. (This disables the popOnBell resource).
  -- * Ps = 1047 → Use Normal Screen Buffer, clearing screen first if in the Alternate Screen. (This may be disabled by the titeInhibit resource).
  -- * Ps = 1048 → Restore cursor as in DECRC. (This may be disabled by the titeInhibit resource).
  -- * Ps = 1049 → Use Normal Screen Buffer and restore cursor as in DECRC. (This may be disabled by the titeInhibit resource). This combines the effects of the 1047 and 1048 modes. Use this with terminfo-based applications rather than the 47 mode.
  -- * Ps = 1050 → Reset terminfo/termcap function-key mode.
  -- * Ps = 1051 → Reset Sun function-key mode.
  -- * Ps = 1052 → Reset HP function-key mode.
  -- * Ps = 1053 → Reset SCO function-key mode.
  -- * Ps = 1060 → Reset legacy keyboard emulation (X11R6).
  -- * Ps = 1061 → Reset keyboard emulation to Sun/PC style.
  -- * Ps = 2004 → Reset bracketed paste mode.
  DECRST Pm |
  -- | @CSI Pm m@ Character Attributes (SGR).
  -- * Ps = 0 → Normal (default).
  -- * Ps = 1 → Bold.
  -- * Ps = 4 → Underlined.
  -- * Ps = 5 → Blink (appears as Bold).
  -- * Ps = 7 → Inverse.
  -- * Ps = 8 → Invisible, i.e., hidden (VT300).
  -- * Ps = 22 → Normal (neither bold nor faint).
  -- * Ps = 24 → Not underlined.
  -- * Ps = 25 → Steady (not blinking).
  -- * Ps = 27 → Positive (not inverse).
  -- * Ps = 28 → Visible, i.e., not hidden (VT300).
  -- * Ps = 30 → Set foreground color to Black.
  -- * Ps = 31 → Set foreground color to Red.
  -- * Ps = 32 → Set foreground color to Green.
  -- * Ps = 33 → Set foreground color to Yellow.
  -- * Ps = 34 → Set foreground color to Blue.
  -- * Ps = 35 → Set foreground color to Magenta.
  -- * Ps = 36 → Set foreground color to Cyan.
  -- * Ps = 37 → Set foreground color to White.
  -- * Ps = 39 → Set foreground color to default (original).
  -- * Ps = 40 → Set background color to Black.
  -- * Ps = 41 → Set background color to Red.
  -- * Ps = 42 → Set background color to Green.
  -- * Ps = 43 → Set background color to Yellow.
  -- * Ps = 44 → Set background color to Blue.
  -- * Ps = 45 → Set background color to Magenta.
  -- * Ps = 46 → Set background color to Cyan.
  -- * Ps = 47 → Set background color to White.
  -- * Ps = 49 → Set background color to default (original).
  -- If 16-color support is compiled, the following apply. Assume that xterm’s resources are set so that the ISO color codes are the first 8 of a set of 16. Then the aixterm colors are the bright versions of the ISO colors:
  -- * Ps = 90 → Set foreground color to Black.
  -- * Ps = 91 → Set foreground color to Red.
  -- * Ps = 92 → Set foreground color to Green.
  -- * Ps = 93 → Set foreground color to Yellow.
  -- * Ps = 94 → Set foreground color to Blue.
  -- * Ps = 95 → Set foreground color to Magenta.
  -- * Ps = 96 → Set foreground color to Cyan.
  -- * Ps = 97 → Set foreground color to White.
  -- * Ps = 100 → Set background color to Black.
  -- * Ps = 101 → Set background color to Red.
  -- * Ps = 102 → Set background color to Green.
  -- * Ps = 103 → Set background color to Yellow.
  -- * Ps = 104 → Set background color to Blue.
  -- * Ps = 105 → Set background color to Magenta.
  -- * Ps = 106 → Set background color to Cyan.
  -- * Ps = 107 → Set background color to White.
  -- If xterm is compiled with the 16-color support disabled, it supports the following, from rxvt:
  -- * Ps = 100 → Set foreground and background color to default.
  -- Xterm maintains a color palette whose entries are identified by an index beginning with zero. If 88- or 256-color support is compiled, the following apply:
  -- * All parameters are decimal integers.
  -- * RGB values range from zero (0) to 255.
  -- * ISO-8613-3 can be interpreted in more than one way; xterm allows the semicolons in this control to be replaced by colons (but after the first colon, colons must be used).
  -- These ISO-8613-3 controls are supported:
  -- * Ps = 38 ; 2 ; Pr ; Pg ; Pb → Set foreground color to the closest match in xterm’s palette for the given RGB Pr /Pg /Pb .
  -- * Ps = 38 ; 5 ; Ps → Set foreground color to the second Ps .
  -- * Ps = 48 ; 2 ; Pr ; Pg ; Pb → Set background color to the closest match in xterm’s palette for the given RGB Pr /Pg /Pb .
  -- * Ps = 48 ; 5 ; Ps → Set background color to the second Ps .
  SGR Pm |
  -- | @CSI > Ps ; Ps m@ Set or reset resource-values used by xterm to decide whether to construct escape sequences holding information about the modifiers pressed with a given key. The first parameter identifies the resource to set/reset. The second parameter is the value to assign to the resource. If the second parameter is omitted, the resource is reset to its initial value.
  -- * Ps = 0 → modifyKeyboard.
  -- * Ps = 1 → modifyCursorKeys.
  -- * Ps = 2 → modifyFunctionKeys.
  -- * Ps = 4 → modifyOtherKeys.
  -- If no parameters are given, all resources are reset to their initial values.
  SetResVal Ps Ps |
  -- | @CSI Ps n@ Device Status Report (DSR).
  -- * Ps = 5 → Status Report. Result (‘‘OK’’) is
  -- CSI 0 n
  -- * Ps = 6 → Report Cursor Position (CPR) [row;column]. Result is
  -- CSI r ; c R
  DSR Ps |
  -- | @CSI > Ps n@ Disable modifiers which may be enabled via the @CSI > Ps ; Ps m@ sequence. This corresponds to a resource value of "-1", which cannot be set with the other sequence. The parameter identifies the resource to be disabled:
  -- * Ps = 0 → modifyKeyboard.
  -- * Ps = 1 → modifyCursorKeys.
  -- * Ps = 2 → modifyFunctionKeys.
  -- * Ps = 4 → modifyOtherKeys.
  -- If the parameter is omitted, modifyFunctionKeys is disabled. When modifyFunctionKeys is disabled, xterm uses the modifier keys to make an extended sequence of functions rather than adding a parameter to each function key to denote the modifiers.
  DisMod Ps |
  -- | @CSI ? Ps n@ Device Status Report (DSR, DEC-specific).
  -- * Ps = 6 → Report Cursor Position (DECXCPR) [row;column] as CSI ? r ; c R (assumes the default page, i.e., "1").
  -- * Ps = 15 → Report Printer status as CSI ? 1 0 n (ready). or CSI ? 1 1 n (not ready).
  -- * Ps = 25 → Report UDK status as CSI ? 2 0 n (unlocked) or CSI ? 2 1 n (locked).
  -- * Ps = 26 → Report Keyboard status as
  -- CSI ? 2 7 ; 1 ; 0 ; 0 n (North American).
  -- The last two parameters apply to VT400 & up, and denote keyboard ready and LK01 respectively.
  -- * Ps = 53 → Report Locator status as CSI ? 5 3 n Locator available, if compiled-in, or CSI ? 5 0 n No Locator, if not.
  -- * Ps = 62 → Report macro space (DECMSR) as CSI P n \* {
  -- * Ps = 63 → Report memory checksum (DECCKSR) as DCS P t ! x x x x ST
  -- P t is the request id (from an optional parameter to the request).
  -- The x’s are hexadecimal digits 0-9 and A-F.
  -- * Ps = 75 → Report data integrity as CSI ? 7 0 n (ready, no errors)
  -- * Ps = 85 → Report multi-session configuration as CSI ? 8 3 n (not configured for multiple-session operation).
  DECDSR Ps |
  -- | @CSI > Ps p@ Set resource value pointerMode. This is used by xterm to decide whether to hide the pointer cursor as the user types. Valid values for the parameter:
  -- * Ps = 0 → never hide the pointer.
  -- * Ps = 1 → hide if the mouse tracking mode is not enabled.
  -- * Ps = 2 → always hide the pointer, except when leaving the window.
  -- * Ps = 3 → always hide the pointer, even if leaving/entering the window. If no parameter is given, xterm uses the default, which is 1 .
  SetPointerMode Ps |
  -- | @CSI ! p@ Soft terminal reset (DECSTR).
  DECSTR |
  -- | @CSI Ps $ p@ Request ANSI mode (DECRQM). For VT300 and up, reply is @CSI Ps ; Pm $ y@
  -- where Ps is the mode number as in RM, and Pm is the mode value:
  -- * 0 - not recognized
  -- * 1 - set
  -- * 2 - reset
  -- * 3 - permanently set
  -- * 4 - permanently reset
  DECRQM Ps |
  -- | @CSI ? Ps $ p@ Request DEC private mode (DECRQM). For VT300 and up, reply is @CSI ? Ps ; Pm $ p@
  -- where P s is the mode number as in DECSET, Pm is the mode value as in the ANSI DECRQM.
  ReqDECRQM Ps |
  -- | @CSI Ps ; Ps “ p@ Set conformance level (DECSCL). Valid values for the first parameter:
  -- * Ps = 61 → VT100.
  -- * Ps = 62 → VT200.
  -- * Ps = 63 → VT300.
  -- Valid values for the second parameter:
  -- * Ps = 0 → 8-bit controls.
  -- * Ps = 1 → 7-bit controls (always set for VT100).
  -- * Ps = 2 → 8-bit controls.
  DECSCL Ps Ps |
  -- | @CSI Ps q@ Load LEDs (DECLL).
  -- * Ps = 0 → Clear all LEDS (default).
  -- * Ps = 1 → Light Num Lock.
  -- * Ps = 2 → Light Caps Lock.
  -- * Ps = 3 → Light Scroll Lock.
  -- * Ps = 21 → Extinguish Num Lock.
  -- * Ps = 22 → Extinguish Caps Lock.
  -- * Ps = 23 → Extinguish Scroll Lock.
  DECLL Ps |
  -- | @CSI Ps SP q@ Set cursor style (DECSCUSR, VT520).
  -- * Ps = 0 → blinking block.
  -- * Ps = 1 → blinking block (default).
  -- * Ps = 2 → steady block.
  -- * Ps = 3 → blinking underline.
  -- * Ps = 4 → steady underline.
  -- * Ps = 5 → blinking bar (xterm).
  -- * Ps = 6 → steady bar (xterm).
  DECSCUSR Ps |
  -- | @CSI Ps “ q@ Select character protection attribute (DECSCA). Valid values for the parameter:
  -- * Ps = 0 → DECSED and DECSEL can erase (default).
  -- * Ps = 1 → DECSED and DECSEL cannot erase.
  -- * Ps = 2 → DECSED and DECSEL can erase.
  DECSCA Ps |
  -- | @CSI Ps ; Ps r@ Set Scrolling Region [top;bottom] (default = full size of window) (DECSTBM).
  DECSTBM Ps Ps |
  -- | @CSI ? Pm r@ Restore DEC Private Mode Values. The value of Ps previously saved is restored. Ps values are the same as for DECSET.
  DECRestoreM Pm |
  -- | @CSI Pt ; Pl ; Pb ; Pr ; Ps $ r@ Change Attributes in Rectangular Area (DECCARA), VT400 and up.
  -- Pt ; Pl ; Pb ; Pr denotes the rectangle.
  -- Ps denotes the SGR attributes to change: 0, 1, 4, 5, 7.
  DECCARA Ps Ps Ps Ps Ps |
  -- | @CSI Pl ; Pr s@ Set left and right margins (DECSLRM), available only when DECLRMM is enabled (VT420 and up).
  DECSLRM Ps Ps |
  -- | @CSI s@ Save cursor (ANSI.SYS), available only when DECLRMM is disabled.
  CURSave |
  -- | @CSI ? Pm s@ Save DEC Private Mode Values. Ps values are the same as for DECSET.
  DECSaveM Pm |
  -- | @CSI Ps ; Ps ; Ps t@ Window manipulation (from dtterm, as well as extensions). These controls may be disabled using the allowWindowOps resource. Valid values for the first (and any additional parameters) are:
  -- * Ps = 1 → De-iconify window.
  -- * Ps = 2 → Iconify window.
  -- * Ps = 3 ; x ; y → Move window to [x, y].
  -- * Ps = 4 ; height ; width → Resize the xterm window to given height and width in pixels. Omitted parameters reuse the current height or width. Zero parameters use the display’s height or width.
  -- * Ps = 5 → Raise the xterm window to the front of the stacking order.
  -- * Ps = 6 → Lower the xterm window to the bottom of the stacking order.
  -- * Ps = 7 → Refresh the xterm window.
  -- * Ps = 8 ; height ; width → Resize the text area to given height and width in characters. Omitted parameters reuse the current height or width. Zero parameters use the display’s height or width.
  -- * Ps = 9 ; 0 → Restore maximized window.
  -- * Ps = 9 ; 1 → Maximize window (i.e., resize to screen size).
  -- * Ps = 9 ; 2 → Maximize window vertically.
  -- * Ps = 9 ; 3 → Maximize window horizontally.
  -- * Ps = 10 ; 0 → Undo full-screen mode.
  -- * Ps = 10 ; 1 → Change to full-screen.
  -- * Ps = 10 ; 2 → Toggle full-screen.
  -- * Ps = 11 → Report xterm window state. If the xterm window is open (non-iconified), it returns CSI 1 t . If the xterm window is iconified, it returns CSI 2 t .
  -- * Ps = 13 → Report xterm window position. Result is CSI 3 ; x ; y t
  -- * Ps = 14 → Report xterm window in pixels. Result is CSI 4 ; height ; width t
  -- * Ps = 18 → Report the size of the text area in characters. Result is CSI 8 ; height ; width t
  -- * Ps = 19 → Report the size of the screen in characters. Result is CSI 9 ; height ; width t
  -- * Ps = 20 → Report xterm window’s icon label. Result is OSC L label ST
  -- * Ps = 21 → Report xterm window’s title. Result is OSC l label ST
  -- * Ps = 22 ; 0 → Save xterm icon and window title on stack.
  -- * Ps = 22 ; 1 → Save xterm icon title on stack.
  -- * Ps = 22 ; 2 → Save xterm window title on stack.
  -- * Ps = 23 ; 0 → Restore xterm icon and window title from stack.
  -- * Ps = 23 ; 1 → Restore xterm icon title from stack.
  -- * Ps = 23 ; 2 → Restore xterm window title from stack.
  -- * Ps >= 24 → Resize to Ps lines (DECSLPP).
  WindowSet Ps Ps Ps |
  -- | @CSI Pt ; Pl ; Pb ; Pr ; Ps $ t@ Reverse Attributes in Rectangular Area (DECRARA), VT400 and up.
  -- Pt ; Pl ; Pb ; Pr denotes the rectangle.
  -- Ps denotes the attributes to reverse, i.e., 1, 4, 5, 7.
  DECRARA Ps Ps Ps Ps Ps |
  -- | @CSI > Ps ; Ps t@ Set one or more features of the title modes. Each parameter enables a single feature.
  -- * P s = 0 → Set window/icon labels using hexadecimal.
  -- * P s = 1 → Query window/icon labels using hexadecimal.
  -- * P s = 2 → Set window/icon labels using UTF-8.
  -- * P s = 3 → Query window/icon labels using UTF-8. (See discussion of "Title Modes")
  SetTitleMode Ps Ps |
  -- | @CSI Ps SP t@ Set warning-bell volume (DECSWBV, VT520).
  -- * Ps = 0 or 1 → off.
  -- * Ps = 2, 3 or 4 → low.
  -- * Ps = 5, 6, 7, or 8 → high.
  DECSWBV Ps |
  -- | @CSI u@ Restore cursor (ANSI.SYS).
  CURRest |
  -- | @CSI Ps SP u@ Set margin-bell volume (DECSMBV, VT520).
  -- * Ps = 1 → off.
  -- * Ps = 2, 3 or 4 → low.
  -- * Ps = 0, 5, 6, 7, or 8 → high.
  DECSMBV Ps |
  -- | @CSI Pt ; Pl ; Pb ; Pr ; Pp ; Pt ; Pl ; Pp $ v@ Copy Rectangular Area (DECCRA, VT400 and up).
  -- Pt ; Pl ; Pb ; Pr denotes the rectangle.
  -- Pp denotes the source page.
  -- Pt ; Pl denotes the target location.
  -- Pp denotes the target page.
  DECCRA Ps Ps Ps Ps Ps Ps Ps Ps |
  -- | @CSI Pt ; Pl ; Pb ; Pr ' w@ Enable Filter Rectangle (DECEFR), VT420 and up.
  -- Parameters are [top;left;bottom;right].
  -- Defines the coordinates of a filter rectangle and activates it. Anytime the locator is detected outside of the filter rectangle, an outside rectangle event is generated and the rectangle is disabled. Filter rectangles are always treated as "one-shot" events. Any parameters that are omitted default to the current locator position. If all parameters are omitted, any locator motion will be reported. DECELR always cancels any prevous rectangle definition.
  DECEFR Ps Ps Ps Ps |
  -- | @CSI Ps x@ Request Terminal Parameters (DECREQTPARM).
  -- if P s is a "0" (default) or "1", and xterm is emulating VT100, the control sequence elicits a response of the same form whose parameters describe the terminal:
  -- * Ps → the given Ps incremented by 2.
  -- * Pn = 1 ← no parity.
  -- * Pn = 1 ← eight bits.
  -- * Pn = 1 ← 2 8 transmit 38.4k baud.
  -- * Pn = 1 ← 2 8 receive 38.4k baud.
  -- * Pn = 1 ← clock multiplier.
  -- * Pn = 0 ← STP flags.
  DECREQTPARM Ps |
  -- | @CSI Ps * x@ Select Attribute Change Extent (DECSACE).
  -- * Ps = 0 → from start to end position, wrapped.
  -- * Ps = 1 → from start to end position, wrapped.
  -- * Ps = 2 → rectangle (exact).
  DECSACE Ps |
  -- | @CSI Pi ; Pg ; Pt ; Pl ; Pb ; Pr * y@ Request Checksum of Rectangular Area (DECRQCRA), VT420 and up. Response is
  -- DCS P t ! x x x x ST
  -- Pi is the request id.
  -- Pg is the page number.
  -- Pt ; Pl ; Pb ; Pr denotes the rectangle.
  -- The x’s are hexadecimal digits 0-9 and A-F.
  DECRQCRA Ps Ps Ps Ps Ps Ps |
  -- | @CSI Pc ; Pt ; Pl ; Pb ; Pr $ x@ Fill Rectangular Area (DECFRA), VT420 and up.
  -- Pc is the character to use.
  -- Pt ; Pl ; Pb ; Pr denotes the rectangle.
  DECFRA Ps Ps Ps Ps Ps |
  -- | @CSI Ps ; Pu ' z@ Enable Locator Reporting (DECELR).
  -- Valid values for the first parameter:
  -- * Ps = 0 → Locator disabled (default).
  -- * Ps = 1 → Locator enabled.
  -- * Ps = 2 → Locator enabled for one report, then disabled.
  -- The second parameter specifies the coordinate unit for locator reports.
  -- Valid values for the second parameter:
  -- * Pu = 0 ← or omitted → default to character cells.
  -- * Pu = 1 ← device physical pixels.
  -- * Pu = 2 ← character cells.
  DECELR Ps Ps |
  -- | @CSI Pt ; Pl ; Pb ; Pr $ z@ Erase Rectangular Area (DECERA), VT400 and up.
  -- Pt ; Pl ; Pb ; Pr denotes the rectangle.
  DECERA Ps Ps Ps Ps |
  -- | @CSI Pm ’ {@ Select Locator Events (DECSLE).
  -- Valid values for the first (and any additional parameters) are:
  -- * Ps = 0 → only respond to explicit host requests (DECRQLP).
  -- (This is default). It also cancels any filter rectangle.
  -- * Ps = 1 → report button down transitions.
  -- * Ps = 2 → do not report button down transitions.
  -- * Ps = 3 → report button up transitions.
  -- * Ps = 4 → do not report button up transitions.
  DECSLE Pm |
  -- | @CSI Pt ; Pl ; Pb ; Pr $ {@ Selective Erase Rectangular Area (DECSERA), VT400 and up.
  -- Pt ; Pl ; Pb ; Pr denotes the rectangle.
  DECSERA Ps Ps Ps Ps |
  -- | @CSI Ps ’ |@ Request Locator Position (DECRQLP).
  -- Valid values for the parameter are:
  -- Ps = 0, 1 or omitted → transmit a single DECLRP locator report.
  -- If Locator Reporting has been enabled by a DECELR, xterm will respond with a DECLRP Locator Report. This report is also generated on button up and down events if they have been enabled with a DECSLE, or when the locator is detected outside of a filter rectangle, if filter rectangles have been enabled with a DECEFR.
  -- → @CSI Pe ; Pb ; Pr ; Pc ; Pp & w@
  -- Parameters are [event;button;row;column;page].
  -- Valid values for the event:
  -- * Pe = 0 → locator unavailable - no other parameters sent.
  -- * Pe = 1 → request - xterm received a DECRQLP.
  -- * Pe = 2 → left button down.
  -- * Pe = 3 → left button up.
  -- * Pe = 4 → middle button down.
  -- * Pe = 5 → middle button up.
  -- * Pe = 6 → right button down.
  -- * Pe = 7 → right button up.
  -- * Pe = 8 → M4 button down.
  -- * Pe = 9 → M4 button up.
  -- * Pe = 1 0 → locator outside filter rectangle.
  -- ‘‘button’’ parameter is a bitmask indicating which buttons are pressed:
  -- * Pb = 0 ← no buttons down.
  -- * Pb & 1 ← right button down.
  -- * Pb & 2 ← middle button down.
  -- * Pb & 4 ← left button down.
  -- * Pb & 8 ← M4 button down.
  -- ‘‘row’’ and ‘‘column’’ parameters are the coordinates of the locator position in the xterm window, encoded as ASCII decimal.
  -- The ‘‘page’’ parameter is not used by xterm, and will be omitted.
  DECRQLP Ps |
  -- | @CSI Pm ’ }@ Insert P s Column(s) (default = 1) (DECIC), VT420 and up.
  DECIC Pm |
  -- | @CSI Pm ’ ~@ Delete P s Column(s) (default = 1) (DECDC), VT420 and up.
  DECDC Pm
  deriving (Eq, Show)


----------------------------------------------------------------------------------

data ESCFunc =
  -- | @ESC SP F@ 7-bit controls (S7C1T).
  Ctrl7Bit |
  -- | @ESC SP G@ 8-bit controls (S8C1T).
  Ctrl8Bit |
  -- | @ESC SP L@ Set ANSI conformance level 1 (dpANS X3.134.1).
  ANSIConf1 |
  -- | @ESC SP M@ Set ANSI conformance level 2 (dpANS X3.134.1).
  ANSIConf2 |
  -- | @ESC SP N@ Set ANSI conformance level 3 (dpANS X3.134.1).
  ANSIConf3 |
  -- | @ESC # 3@ DEC double-height line, top half (DECDHL).
  DECDHLt |
  -- | @ESC # 4@ DEC double-height line, bottom half (DECDHL).
  DECDHLb |
  -- | @ESC # 5@ DEC single-width line (DECSWL).
  DECSWL |
  -- | @ESC # 6@ DEC double-width line (DECDWL).
  DECDWL |
  -- | @ESC # 8@ DEC Screen Alignment Test (DECALN).
  DECALN |
  -- | @ESC % @@ Select default character set. That is ISO 8859-1 (ISO 2022).
  SelDefCharSet |
  -- | @ESC % G@ Select UTF-8 character set (ISO 2022).
  SelUTF8CharSet |
  -- | @ESC ( C@ Designate G0 Character Set (ISO 2022, VT100).
  -- Final character C for designating 94-character sets. In this list, 0 , A and B apply to VT100 and up, the remainder to VT220 and up:
  -- * C = 0 → DEC Special Character and Line Drawing Set.
  -- * C = A → United Kingdom (UK).
  -- * C = B → United States (USASCII).
  -- * C = 4 → Dutch.
  -- * C = C or 5 → Finnish.
  -- * C = R → French.
  -- * C = Q → French Canadian.
  -- * C = K → German.
  -- * C = Y → Italian.
  -- * C = E or 6 → Norwegian/Danish.
  -- * C = Z → Spanish.
  -- * C = H or 7 → Swedish.
  -- * C = = → Swiss.
  DesG0CharSetVT100 Char |
  -- | @ESC ) C@ Designate G1 Character Set (ISO 2022, VT100).
  -- The same character sets apply as for ESC ( C.
  DesG1CharSetVT220 Char |
  -- | @ESC * C@ Designate G2 Character Set (ISO 2022, VT220).
  -- The same character sets apply as for ESC ( C.
  DesG2CharSetVT220 Char |
  -- | @ESC + C@ Designate G3 Character Set (ISO 2022, VT220).
  -- The same character sets apply as for ESC ( C.
  DesG3CharSetVT220 Char |
  -- | @ESC - C@ Designate G1 Character Set (VT300).
  -- The same character sets apply as for ESC ( C.
  DesG1CharSetVT300 Char |
  -- | @ESC . C@ Designate G2 Character Set (VT300).
  -- The same character sets apply as for ESC ( C.
  DesG2CharSetVT300 Char |
  -- | @ESC / C@ Designate G3 Character Set (VT300).
  -- These work for 96-character sets only.
  -- * C = A → ISO Latin-1 Supplemental.
  DesG3CharSetVT300 Char |
  -- | @ESC 6@ Back Index (DECBI), VT420 and up.
  DECBI |
  -- | @ESC 7@ Save Cursor (DECSC).
  DECSC |
  -- | @ESC 8@ Restore Cursor (DECRC).
  DECRC |
  -- | @ESC 9@ Forward Index (DECFI), VT420 and up.
  DECFI |
  -- | @ESC =@ Application Keypad (DECKPAM).
  DECKPAM |
  -- | @ESC >@ Normal Keypad (DECKPNM).
  DECKPNM |
  -- | @ESC F@ Cursor to lower left corner of screen. This is enabled by the hpLowerleftBugCompat resource.
  CURLowLeft |
  -- | @ESC c@ Full Reset (RIS).
  RIS |
  -- | @ESC l@ Memory Lock (per HP terminals). Locks memory above the cursor.
  MemLock |
  -- | @ESC m@ Memory Unlock (per HP terminals).
  MemUnlock |
  -- | @ESC n@ Invoke the G2 Character Set as GL (LS2).
  LS2 |
  -- | @ESC o@ Invoke the G3 Character Set as GL (LS3).
  LS3 |
  -- | @ESC |@ Invoke the G3 Character Set as GR (LS3R).
  LS3R |
  -- | @ESC }@ Invoke the G2 Character Set as GR (LS2R).
  LS2R |
  -- | @ESC ~@ Invoke the G1 Character Set as GR (LS1R).
  LS1R
  deriving (Eq, Show)


data CtrlSeq = IND -- ESC D   Index (IND is 0x84).
              | ESC ESCFunc   -- ESC     Controls beginning with ESC except the rest.
--              | NEL NELFunc -- ESC E   Next Line ( NEL is 0x85).
--              | HTS HTSFunc -- ESC H   Tab Set ( HTS is 0x88).
--              | RI  RIFunc  -- ESC M   Reverse Index ( RI is 0x8d).
--              | SS2 SS2Func -- ESC N   Single Shift Select of G2 Character Set ( SS2 is 0x8e). This affects next character only.
--              | SS3 SS3Func -- ESC O   Single Shift Select of G3 Character Set ( SS3 is 0x8f). This affects next character only.
--              | DCS DCSFunc -- ESC P   Device Control String ( DCS is 0x90).
--              | SPA SPAFunc -- ESC V   Start of Guarded Area ( SPA is 0x96).
--              | EPA EPAFunc -- ESC W   End of Guarded Area ( EPA is 0x97).
--              | SOS SOSFunc -- ESC X   Start of String ( SOS is 0x98).
              | CSI CSIFunc -- ESC [   Control Sequence Introducer ( CSI is 0x9b).
--              | ST  STFunc  -- ESC \   String Terminator ( ST is 0x9c).
--              | OSC OSCFunc -- ESC ]   Operating System Command ( OSC is 0x9d).
--              | PM  PMFunc  -- ESC ^   Privacy Message ( PM is 0x9e).
--              | APC APCFunc -- ESC _   Application Program Command ( APC is 0x9f).
              -- depricated | DECID -- ESC Z Return Terminal ID (DECID is 0x9a). Obsolete form of CSI c (DA).
                 deriving (Eq, Show)

escWord :: Word8
escWord = 0x1b

pPs :: Maybe Int -> Char -> Parser Ps
pPs (Just d) s = option d decimal <* char s
pPs Nothing s = decimal <* char s


pPm :: Maybe Int -> Char -> Parser [Ps]
pPm def s = appendC <$> many (pPs Nothing ';') <*> pPs def s
  where
    appendC ls c = ls ++ [c]

csiFunc :: Parser CSIFunc
csiFunc =
  choice
    [ ICH <$> pPs (Just 1) '@'
    , CUU <$> pPs (Just 1) 'A'
    , CUD <$> pPs (Just 1) 'B'
    , CUF <$> pPs (Just 1) 'C'
    , CUB <$> pPs (Just 1) 'D'
    , CNL <$> pPs (Just 1) 'E'
    , CPL <$> pPs (Just 1) 'F'
    , CHA <$> pPs (Just 1) 'G'
    , CUP <$> pPs (Just 1) ';' <*> pPs (Just 1) 'H'
    , CUP <$> (pure :: Int -> Parser Ps) 1 <*>
      ((pure :: Int -> Parser Ps) 1 <* char 'H')
    , CHT <$> pPs (Just 1) 'I'
    , ED <$> pPs (Just 0) 'J'
    , DECSED <$> (char '?' *> pPs (Just 0) 'J')
    , EL <$> pPs (Just 0) 'K'
    , DECSEL <$> (char '?' *> pPs (Just 0) 'K')
    , IL <$> pPs (Just 1) 'L'
    , DL <$> pPs (Just 1) 'M'
    , DCH <$> pPs (Just 1) 'P'
    , SU <$> pPs (Just 1) 'S'
    , SD <$> pPs (Just 1) 'T'
    , MouseTrackInit <$> pPs Nothing ';' <*> pPs Nothing ';' <*> pPs Nothing ';' <*>
      pPs Nothing ';' <*>
      pPs Nothing 'T'
    , ResetTitleModes <$> (char '>' *> pPs Nothing ';') <*> pPs Nothing 'T'
    , ECH <$> pPs (Just 1) 'X'
    , CBT <$> pPs (Just 1) 'Z'
    , HPA <$> pPm Nothing '`'
    , HPR <$> pPm Nothing 'a'
    , REP <$> pPs Nothing 'b'
    , PrimDA <$> pPs (Just 0) 'c'
    , SecDA <$> (char '>' *> pPs (Just 0) 'c')
    , VPA <$> pPm Nothing 'd'
    , VPR <$> pPm Nothing 'e'
    , HVP <$> pPs (Just 1) ';' <*> pPs (Just 1) 'f'
    , TBC <$> pPs Nothing 'g'
    , SM <$> pPm Nothing 'h'
    , DECSET <$> (char '?' *> pPm Nothing 'h')
    , MC <$> pPm (Just 0) 'i'
    , DECMC <$> (char '?' *> pPm Nothing 'i')
    , RM <$> pPm Nothing 'l'
    , DECRST <$> (char '?' *> pPm Nothing 'l')
    , SGR <$> pPm (Just 0) 'm'
    , SetResVal <$> (char '>' *> pPs (Just $ -1) ';') <*> pPs (Just $ -1) 'm'
    , DSR <$> pPs Nothing 'n'
    , DisMod <$> (char '>' *> pPs (Just 4) 'n')
    , DECDSR <$> (char '?' *> pPs Nothing 'n')
    , SetPointerMode <$> (char '>' *> pPs (Just 1) 'p')
    , DECSTR <$ (char '!' <* char 'p')
    , DECRQM <$> (pPs Nothing '$' <* char 'p')
    , ReqDECRQM <$> (char '?' *> pPs Nothing '$' <* char 'p')
    , DECSCL <$> pPs Nothing ';' <*> (pPs Nothing '"' <* char 'p')
    , DECLL <$> pPs (Just 1) 'q'
    , DECSCUSR <$> (pPs (Just 1) ' ' <* char 'q')
    , DECSCA <$> (pPs (Just 1) '"' <* char 'q')
    , DECSTBM <$> pPs (Just $ -1) ';' <*> pPs (Just $ -1) 'r'
    , DECRestoreM <$> (char '?' *> pPm Nothing 'r')
    , DECCARA <$> pPs Nothing ';' <*> pPs Nothing ';' <*> pPs Nothing ';' <*>
      pPs Nothing ';' <*>
      (pPs Nothing '$' <* char 'r')
    , DECSLRM <$> pPs Nothing ';' <*> pPs Nothing 's'
    , CURSave <$ char 's'
    , DECSaveM <$> (char '?' *> pPm Nothing 's')
    , WindowSet <$> pPs Nothing ';' <*> pPs (Just $ -1) ';' <*>
      pPs (Just $ -1) 't'
    , DECRARA <$> pPs Nothing ';' <*> pPs Nothing ';' <*> pPs Nothing ';' <*>
      pPs Nothing ';' <*>
      (pPs Nothing '$' <* char 't')
    , SetTitleMode <$> (char '>' *> pPs Nothing ';') <*> pPs Nothing '$'
    , DECSWBV <$> (pPs Nothing ' ' <* char 't')
    , CURRest <$ char 'u'
    , DECSMBV <$> (pPs Nothing ' ' <* char 'u')
    , DECCRA <$> pPs Nothing ';' <*> pPs Nothing ';' <*> pPs Nothing ';' <*>
      pPs Nothing ';' <*>
      pPs Nothing ';' <*>
      pPs Nothing ';' <*>
      pPs Nothing ';' <*>
      (pPs Nothing '$' <* char 'v')
    , DECEFR <$> pPs Nothing ';' <*> pPs Nothing ';' <*> pPs Nothing ';' <*>
      (pPs Nothing '\'' <* char 'w')
    , DECREQTPARM <$> pPs (Just 0) 'x'
    , DECSACE <$> (pPs Nothing '*' <* char 'x')
    , DECRQCRA <$> pPs Nothing ';' <*> pPs Nothing ';' <*> pPs Nothing ';' <*>
      pPs Nothing ';' <*>
      pPs Nothing ';' <*>
      (pPs Nothing '*' <* char 'y')
    , DECFRA <$> pPs Nothing ';' <*> pPs Nothing ';' <*> pPs Nothing ';' <*>
      pPs Nothing ';' <*>
      (pPs Nothing '$' <* char 'x')
    , DECELR <$> pPs (Just 0) ';' <*> (pPs (Just 0) '\'' <* char 'z')
    , DECERA <$> pPs Nothing ';' <*> pPs Nothing ';' <*> pPs Nothing ';' <*>
      (pPs Nothing '$' <* char 'z')
    , DECSLE <$> (pPm (Just 0) '\'' <* char '{')
    , DECSERA <$> pPs Nothing ';' <*> pPs Nothing ';' <*> pPs Nothing ';' <*>
      (pPs Nothing '$' <* char '{')
    , DECRQLP <$> (pPs (Just 0) '\'' <* char '|')
    , DECIC <$> (pPm (Just 1) '\'' <* char '}')
    , DECDC <$> (pPm (Just 1) '\'' <* char '~')
    ]

escFunc :: Parser ESCFunc
escFunc = choice [
  Ctrl7Bit  <$ char ' ' <* char 'F',
  Ctrl8Bit  <$ char ' ' <* char 'G',
  ANSIConf1 <$ char ' ' <* char 'L',
  ANSIConf2 <$ char ' ' <* char 'M',
  ANSIConf3 <$ char ' ' <* char 'N',
  DECDHLt   <$ char '#' <* char '3',
  DECDHLb   <$ char '#' <* char '4',
  DECSWL    <$ char '#' <* char '5',
  DECDWL    <$ char '#' <* char '6',
  DECALN    <$ char '#' <* char '8',
  SelDefCharSet  <$ char '%' <* char '@',
  SelUTF8CharSet <$ char '%' <* char 'G',
  DesG0CharSetVT100 <$> (char '(' *> charset),
  DesG1CharSetVT220 <$> (char ')' *> charset),
  DesG2CharSetVT220 <$> (char '*' *> charset),
  DesG3CharSetVT220 <$> (char '+' *> charset),
  DesG1CharSetVT300 <$> (char '-' *> charset),
  DesG2CharSetVT300 <$> (char '.' *> charset),
  DesG3CharSetVT300 <$> (char '/' *> char 'A'),
  DECBI      <$ char '6',
  DECSC      <$ char '7',
  DECRC      <$ char '8',
  DECFI      <$ char '9',
  DECKPAM    <$ char '=',
  DECKPNM    <$ char '>',
  CURLowLeft <$ char 'F',
  RIS        <$ char 'c',
  MemLock    <$ char 'l',
  MemUnlock  <$ char 'm',
  LS2        <$ char 'n',
  LS3        <$ char 'o',
  LS3R       <$ char '|',
  LS2R       <$ char '}',
  LS1R       <$ char '~'
  ] where charset = satisfy $ inClass "0AB4C5RQKYE6ZH7="

ctrlSeq :: Parser CtrlSeq
ctrlSeq = word8 0x1b *> choice [
  CSI <$> (char '[' *> csiFunc),
  ESC <$> escFunc]

data KSeq
  = KString B.ByteString
  | KControl CtrlSeq
  | KUnknown Char
  deriving (Eq, Show)

kSeq :: Parser [KSeq]
kSeq = many' $ choice [
  KString <$> takeWhile1 (/=escWord),
  KControl <$> ctrlSeq,
  KUnknown <$> anyChar]


tParse :: ByteString -> Either String [KSeq]
tParse = parseOnly kSeq

