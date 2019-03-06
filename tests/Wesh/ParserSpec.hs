{-# LANGUAGE OverloadedStrings #-}

module Wesh.ParserSpec (spec) where

import Wesh.Parser
import Common

spec :: Spec
spec = do
  describe "Parser" $ do
    testContent <- runIO $ readFileBinary "files/vimtest.txt"
    it "Parse vimtests.txt" $
      case tParse testContent of
        Left err -> error $ "err: " ++ show err
        Right result -> result `shouldBe` expected
  where
    emptyLine =
      KString
        "~                                                                               "
    expected =
      [ KControl (CSI (DECSET [1049]))
      , KControl (CSI (DECSET [1]))
      , KControl (ESC DECKPAM)
      , KControl (CSI (DECSTBM 1 24))
      , KControl (CSI (DECSET [12, 25]))
      , KControl (CSI (DECRST [12]))
      , KControl (CSI (DECSET [25]))
      , KControl (CSI (SGR [27]))
      , KControl (CSI (SGR [0]))
      , KControl (CSI (CUP 1 1))
      , KControl (CSI (ED 2))
      , KControl (CSI (SecDA 0))
      , KControl (CSI (DECRST [25]))
      , KControl (CSI (CUP 2 1))
      , KControl (CSI (SGR [1]))
      , KControl (CSI (SGR [34]))
      , emptyLine
      , KControl (CSI (CUP 3 1))
      , emptyLine
      , KControl (CSI (CUP 4 1))
      , emptyLine
      , KControl (CSI (CUP 5 1))
      , emptyLine
      , KControl (CSI (CUP 6 1))
      , emptyLine
      , KControl (CSI (CUP 7 1))
      , emptyLine
      , KControl (CSI (CUP 8 1))
      , emptyLine
      , KControl (CSI (CUP 9 1))
      , emptyLine
      , KControl (CSI (CUP 10 1))
      , emptyLine
      , KControl (CSI (CUP 11 1))
      , emptyLine
      , KControl (CSI (CUP 12 1))
      , emptyLine
      , KControl (CSI (CUP 13 1))
      , emptyLine
      , KControl (CSI (CUP 14 1))
      , emptyLine
      , KControl (CSI (CUP 15 1))
      , emptyLine
      , KControl (CSI (CUP 16 1))
      , emptyLine
      , KControl (CSI (CUP 17 1))
      , emptyLine
      , KControl (CSI (CUP 18 1))
      , emptyLine
      , KControl (CSI (CUP 19 1))
      , emptyLine
      , KControl (CSI (CUP 20 1))
      , emptyLine
      , KControl (CSI (CUP 21 1))
      , emptyLine
      , KControl (CSI (CUP 22 1))
      , emptyLine
      , KControl (CSI (CUP 23 1))
      , emptyLine
      , KControl (CSI (SGR [0]))
      , KControl (CSI (CUP 24 63))
      , KString "0,0-1"
      , KControl (CSI (CUF 9))
      , KString "All"
      , KControl (CSI (CUP 6 32))
      , KString "VIM - Vi IMproved"
      , KControl (CSI (CUP 8 33))
      , KString "version 7.3.429"
      , KControl (CSI (CUP 9 29))
      , KString "by Bram Moolenaar et al."
      , KControl (CSI (CUP 10 13))
      , KString "Modified by pkg-vim-maintainers@lists.alioth.debian.org"
      , KControl (CSI (CUP 11 19))
      , KString "Vim is open source and freely distributable"
      , KControl (CSI (CUP 13 26))
      , KString "Help poor children in Uganda!"
      , KControl (CSI (CUP 14 18))
      , KString "type  :help iccf"
      , KControl (CSI (SGR [34]))
      , KString "<Enter>"
      , KControl (CSI (SGR [0]))
      , KString "       for information "
      , KControl (CSI (CUP 16 18))
      , KString "type  :q"
      , KControl (CSI (SGR [34]))
      , KString "<Enter>"
      , KControl (CSI (SGR [0]))
      , KString "               to exit         "
      , KControl (CSI (CUP 17 18))
      , KString "type  :help"
      , KControl (CSI (SGR [34]))
      , KString "<Enter>"
      , KControl (CSI (SGR [0]))
      , KString "  or  "
      , KControl (CSI (SGR [34]))
      , KString "<F1>"
      , KControl (CSI (SGR [0]))
      , KString "  for on-line help"
      , KControl (CSI (CUP 18 18))
      , KString "type  :help version7"
      , KControl (CSI (SGR [34]))
      , KString "<Enter>"
      , KControl (CSI (SGR [0]))
      , KString "   for version info"
      , KControl (CSI (CUP 1 1))
      , KControl (CSI (DECRST [12]))
      , KControl (CSI (DECSET [25]))
      , KString "\n"
      ]
