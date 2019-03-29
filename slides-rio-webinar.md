---
title: RIO, Standard Library for Haskell
author: Alexey Kuleshevich - FP Complete
date: March 6th, 2019
patat:
    slideLevel: 1
    wrap: true
    margins:
        left: 20
        right: 20
...

# Webinar outline

* [Video](https://www.youtube.com/watch?v=gu0ZCqQe3BY)
* [WebPage](https://www.fpcomplete.com/blog/rio-standard-library-for-haskell)

## Questions

* What does `rio` have?
* Which problems does it solve?
* How do we use it?

Documentation: https://www.stackage.org/package/rio

## Learn by example

How to write applications and libraries with `rio`?

Source code: https://github.com/lehins/haskell-webshell

# The name

## Municipality in Brazil

Nice picture of Rio de Janeiro on github can be misleading.

## Actual meaning

* `ReaderT` in `IO`.

* A small name ambiguity, that can usually be resolved from the context:

    * `rio`: the package

    * `RIO`: the module

        * `import RIO`

    * `RIO`: the type

        * `newtype RIO env a = RIO { unRIO :: ReaderT env IO a }`

# What is `rio`?

## Prelude replacement

* Re-exports most of the functions and types from `Prelude`.

* Partial functions are excluded: `(!!)`, `head`, `tail`, `foldl1`, `maximum`, etc.

* No functions that interact with the terminal are exported: `print`, `putStrLn`, `getLine`, etc.

* Commonly used functions and types from `base`, that normally have to be imported
  explicitly, are now available by default: `void`, `when`, `foldl'`, `Proxy` etc.

* Adds a whole collection of general purpose functions that are extremely useful:
    * `nubOrd :: Ord a => [a] -> [a]`
    * `whenM :: Monad m => m Bool -> m () -> m ()`
    * `mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b`
    * `mapMaybeA :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]`
    * `foldMapM :: (Monad m, Monoid w, Foldable t) => (a -> m w) -> t a -> m w`
    * ...

* Fixes broken functions:
    * `readFileUtf8 :: MonadIO m => FilePath -> m Text`
    * `writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()`
    * `withLazyFile :: MonadUnliftIO m => FilePath -> (ByteString -> m a) -> m a`
    * ...

* Special attention deserve functions from `RIO.File`:
    * `writeBinaryFileDurable`
    * `writeBinaryFileDurableAtomic`
    * `withBinaryFileDurable`
    * `withBinaryFileDurableAtomic`
    * `ensureFileDurable`

# What else is `rio`?

## Collection libraries

Re-exports a lot of functionality for libraries that are known to be fast, safe and work well with
each other.

Current dependency list for `rio`:

* `bytestring`
* `containers`
* `deepseq`
* `directory`
* `exceptions`
* `filepath`
* `hashable`
* `microlens`
* `mtl`
* `primitive`
* `text`
* `time`
* `typed-process`
* `unliftio`
* `unordered-containers`
* `vector`

# Which problems does it solve?

It doesn't solve all problems, but it does serve as a great guide to avoid many of them:

* Best practices:

    * Consistent imports
    * Safe GHC extensions
    * Suggested GHC flags

* Design patterns

    * ReaderT
    * Unified logging
    * Handling Exceptions
    * Dealing with resources
    * Spawning processes

# Consistent imports

Safe and commonly used functions for vetted libraries can be imported directly from `rio`:

Instead of:

```haskell
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.Merge as Map

import qualified Data.List as L
```
now we do:

```haskell
import RIO
import qualified RIO.ByteString as B
import qualified RIO.Map as Map
import qualified RIO.List as L
```

Data types (`ByteString`, `Map`, `Set`, etc.), are available through `import RIO`.

## Uncategorized

Not everything is re-exported, a lot of functionality that seldom used, is not recommended or simply
omitted for no particular reason will require a manual import and addition of the library to
`dependencies`.


# The dangerous

## Partial

Partial functions have to be imported explicitly, but try to avoid them if you can:

```haskell
import RIO.Map.Partial (findMin)
import qualified RIO.ByteString.Partial as B (foldl1')
import qualified RIO.List.Partial as L (foldl1')
import RIO.Vector.Unboxed.Partial (!)
```

_## Unchecked_

Functions in these modules will break, if their preconditions are not satisfied, as such they can be
dangerous and yield unexpected results.

```haskell
import RIO.Map.Unchecked as Map (fromAscList)
import RIO.Set.Unchecked as Set (fromAscList)
```

__## Unsafe__

Same as with "Unchecked", you better be confident that all implied preconditions are satisfied,
otherwise these functions can do nasty stuff.

```haskell
import RIO.Vector.Unboxed.Unsafe (unsafeNew, unsafeFreeze, unsafeWrite)
```

# Safe Extensions


Full vetted list of GHC extensions

|                               |                         |
|:------------------------------|-------------------------|
| `AutoDeriveTypeable`          | `InstanceSigs`          |
| `BangPatterns`                | `KindSignatures`        |
| `BinaryLiterals`              | `LambdaCase`            |
| `ConstraintKinds`             | `MonadFailDesugaring`   |
| `DataKinds`                   | `MultiParamTypeClasses` |
| `DefaultSignatures`           | `MultiWayIf`            |
| `DeriveDataTypeable`          | `NamedFieldPuns`        |
| `NoImplicitPrelude`           | `OverloadedStrings`     |
| `DeriveFoldable`              | `PartialTypeSignatures` |
| `DeriveFunctor`               | `PatternGuards`         |
| `DeriveGeneric`               | `PolyKinds`             |
| `DeriveTraversable`           | `RankNTypes`            |
| `DoAndIfThenElse`             | `RecordWildCards`*      |
| `EmptyDataDecls`              | `ScopedTypeVariables`   |
| `ExistentialQuantification`   | `StandaloneDeriving`    |
| `FlexibleContexts`            | `TupleSections`         |
| `FlexibleInstances`           | `TypeFamilies`          |
| `FunctionalDependencies`      | `TypeSynonymInstances`  |
| `GADTs`                       | `ViewPatterns`          |
| `GeneralizedNewtypeDeriving`  |                         |

*`RecordWildCards` - can be a bit surprising at times, but extremely useful at other times, so use at
you own discretion

# GHC flags

It is suggested to turn on the GHC flags

* `-Wall`
* `-Wcompat`
* `-Widentities`
* `-Wincomplete-record-updates`
* `-Wincomplete-uni-patterns`
* `-Wpartial-fields`
* `-Wredundant-constraints`

For production and CI it's a good idea to also enable:

* `-Werror`



# How to start?

Use `stack` and a specially crafted `rio` template:

```shell
$ stack new my-app rio
```
which will create a project skeleton:

```
my-app/
├── app
│   └── Main.hs
├── ChangeLog.md
├── LICENSE
├── my-app.cabal
├── package.yaml
├── README.md
├── Setup.hs
├── src
│   ├── Import.hs
│   ├── Run.hs
│   ├── Types.hs
│   └── Util.hs
├── stack.yaml
└── test
    ├── Spec.hs
    └── UtilSpec.hs
```

Or follow some other means and just add `rio` as a dependency.


# Global extensions:

Add some or all extensions from before to the `package.yaml` for `hpack`:

```yaml
dependencies:
- base >= 4.9 && < 5
- rio
default-extensions:
- NoImplicitPrelude
- ...
```

or to your `my-app.cabal` file:

```
  build-depends: base >= 4.9 && < 5
               , rio
  extensions: NoImplicitPrelude
            , BangPatterns
            , OverloadedStrings
            , ...
```

# Design patterns - Exceptions

Many ways to fail, and so many confusing ways to recover, or not.

```haskell
λ> minBound * (-1 :: Int8)
-128
λ> minBound `div` (-1 :: Int8)
*** Exception: arithmetic overflow
```

`rio` doesn't help with the above problem, but it can certainly guide us towards the correct ways on how
to deal with exceptions in order to avoid common disasters:

* Be explicit with types that a pure function can fail:
    * `Maybe a`
    * `Either MyException a`
    * more general case: `throwM :: (MonadThrow m, Exception e) => e -> m a`

* Never hide behind:
    * `error` (useful alternative `throwString`)
    * `throw` (renamed to `impureThrow`)
    * `undefined`
    * `ExceptT m a`, unless `m` is something pure like `Identity`

* Use `throwIO :: (MonadIO m, Exception e) => e -> m a`

* When dealing with side-effects, it is impossible to describe at the type level every way that a
  function can fail:
    * Almost all `IO` actions throw exceptions
    * Async exceptions can popup at any time.
    * There is also such thing as masking state.

# Catching exceptions

Just like when dealing with mutable state:

* Bite the bullet, stay in `IO`/`RIO`
* Or simple transformers like `ReaderT env IO` or `IdentityT IO`.
* Do not recover from async exceptions.

## MonadUnliftIO

* `bracket` and `finally` for dealing with resource cleanups.
* `catch`, `handle`, etc. which will only catch synchronous exceptions
* `with*` style, eg. `withFile`, `withProcess`, `withMVar` etc. all of which will have
  `MonadUnliftIO m` restriction

Further reading: https://www.fpcomplete.com/blog/2017/07/announcing-new-unliftio-library

# `unliftio`

Imported by default with `import RIO`:

* `module Control.Monad.IO.Unlift`
* `module UnliftIO.Async`
* `module UnliftIO.Chan`
* `module UnliftIO.Exception`
* `module UnliftIO.IO`
* `module UnliftIO.IORef`
* `module UnliftIO.Memoize`
* `module UnliftIO.MVar`
* `module UnliftIO.STM`
* `module UnliftIO.Temporary`
* `module UnliftIO.Timeout`

Separately accessible, but also comes from `unliftio`:

* `RIO.Directory` ( Goes good together with `RIO.FilePath` )


# ReaderT design pattern

## Global immutable environment

* The good:
    * `env` type variable within the `MonadReader env m`

* The bad:
    * Compile time flags

* And the ugly:
    * Global variable with `unsafePerformIO`

## Ways to pass environment

Not so good approaches:

* `ImplicitParams`?
    * Possible, but error prone.

* As an extra argument to every function. Again, possible, but:
    * Complicates refactoring dramatically
    * Not very composable.

Something that works really well:

* `ReaderT env IO` or a newtype wrapper around it, like `RIO env`.
* Above can be generalized at any time with `(MonadIO m, MonadReader env m)`.

# Global mutable state

## Also ReaderT pattern

* Use explicit `IORef`, `TVar`, `MVar`, etc.

## Why not other transformers

* `StateT`, `WriterT`, `ExceptT`:
    * problems with exceptions
    * look pure but not really
    * issues with concurrency

* Deeply nested stack of transformers:
    * confusing
    * hurts performance

# Finally examples, almost

## WebShell

* Bring a terminal to the browser

```
                                                  +-------------------------------------------+
       Browser                                    |                                           |
+---------------------+                           |                                           |
|                     |        Websockets         |     +-----------+                         |
|                     +-------------------------------->+           |                         |
|    Terminal         |                           |     |   bash    |                         |
|                     +<--------------------------------+           |                         |
|                     |                           |     |           |                         |
|                     |                           |     +-----------+                         |
|                     |                           |                                           |
+---------------------+                           |                                           |
                                                  |                                           |
                                                  |                                           |
                                                  |             Webserver                     |
                                                  |                                           |
                                                  |                                           |
     Browser                                      |                                           |
  +----------------------+                        |                                           |
  |                      |                        |     +-----------+                         |
  |                      |       Websockets       |     |           |                         |
  |                      +----------------------------->+   bash    |                         |
  |    Terminal          |                        |     |           |                         |
  |                      <------------------------------+           |                         |
  |                      |                        |     +-----------+                         |
  |                      |                        |                                           |
  |                      |                        |                                           |
  +----------------------+                        |                                           |
                                                  |                                           |
                                                  +-------------------------------------------+
```

# Explicit State

```haskell
data Terminal = Terminal
  { tMasterHandle :: !Handle
  , tSlaveHandle  :: !Handle
  , ...
  }

type WeshState = IORef (Map Token Terminal)

withPseudoTerminal :: Token -> (Terminal -> RIO WeshState b) -> RIO WeshState b
withPseudoTerminal token onPseudoTerminal = do
  stateRef <- ask
  let openPseudoTerminalHandles = do
        terminal <- liftIO makePseudoTerminal
        atomicModifyIORef' stateRef $ \state -> (Map.insert token terminal state, ())
        pure terminal
      closeHandles Terminal {tMasterHandle, tSlaveHandle} = do
        atomicModifyIORef' stateRef $ \state -> (Map.delete token state, ())
        liftIO $ hClose tMasterHandle >> hClose tSlaveHandle
  bracket openPseudoTerminalHandles closeHandles onPseudoTerminal
```

* Proper resource cleanup
* Exception safe global state update

A bit of a problem:

* Having environment restricted to `WeshState` makes it hard to compose. Same as:

```haskell
withPseudoTerminal :: Token -> (Terminal -> ReaderT WeshState IO b) -> ReaderT WeshState IO b
```


# `Has*` type class approach or "on a need-to-know basis".

Instead of:

```haskell
withPseudoTerminal :: Token -> (Terminal -> RIO WeshState b) -> RIO WeshState b
withPseudoTerminal token onPseudoTerminal = do
  stateRef <- ask
  ...
```

We create a `HasWeshState` class and abstract away the `RIO` monad.

```haskell
class HasWeshState env where
  weshStateG :: SimpleGetter env WeshState

withPseudoTerminal ::
     (MonadReader env m, MonadUnliftIO m, HasWeshState env)
  => Token
  -> (Terminal -> m b)
  -> m b
withPseudoTerminal token onPseudoTerminal = do
  stateRef <- view weshStateG
  ...
```

A little bit of boiler plate, but we got a lot in return:

* Made our function most general and composable
* Type signature tells us that it needs access to the state, and nothing but the state from the
  `env`
* With `MonadUnliftIO` we know that it runs some `IO` action that is passed as an argument, although
  we knew that already: `(Terminal -> m b)`

# Why is `Has*` simpler?

```haskell
data WeshEnv = WeshEnv
  { weshEnvState   :: !WeshState
  , weshEnvLogFunc :: !LogFunc
  }

instance HasWeshState WeshEnv where
  weshStateG = to weshEnvState


data WeshSession = WeshSession
  { weshSessionEnv            :: !WeshEnv
  , weshSessionProcessContext :: !ProcessContext
  , weshSessionConnection     :: !Connection
  }

instance HasWeshState WeshSession where
  weshStateG = to (weshEnvState . weshSessionEnv)
```

* We can now use either one of `WeshSession` or `WeshEnv` as `env` for `withPseudoTerminal`
* In fact `WeshSession` is being used, but we know from the type signature that nothing else, except
  `WeshState` is being used.

# Running the shell

```haskell
withShell ::
     (MonadReader env m, MonadUnliftIO m, HasLogFunc env, HasProcessContext env, HasWeshState env)
  => Token -- ^ Opaque identifier for the terminal session
  -> FilePath -- ^ Name of shell
  -> [String] -- ^ Shell arguments
  -> (Terminal -> m a) -- ^ Action to execute on the terminal that runs the shell
  -> m (Either a ExitCode)
withShell token cmd args onTerminal =
  withPseudoTerminal token $ \terminal@Terminal {tSlaveHandle} ->
    proc cmd args $ \processConfig ->
      let customProcessConfig =
            setCreateGroup True $
            setNewSession True $
            setStdin (useHandleClose tSlaveHandle) $
            setStdout (useHandleClose tSlaveHandle) $
            setStderr (useHandleClose tSlaveHandle) processConfig
       in withProcess customProcessConfig $ \process ->
            race (onTerminal terminal) (waitExitCode process)
```

* `withPseudoTerminal` creates handles and updates global state, which are cleaned up no matter
  what.
* `proc` sets up the environment for the process, uses logging facility to report running time
* `processConfig` is a customizable configuration that will be used for running the process.
* `withProcess` invokes the process and calls the supplied action.
* `ProcessContext` is a customizable and cacheable process context:
    * environment variables
    * current directory

# Terminal resize

Main reason why we need global state is to be able to identify the terminal for resizing:

```haskell
resizeTerminal ::
     (MonadReader env m, MonadIO m, HasWeshState env, HasLogFunc env)
  => Token -- ^ Token that identifies the terminal session
  -> TerminalSize -- ^ New size for the terminal window
  -> m Bool
resizeTerminal token tSize@TerminalSize {tsWidth, tsHeight} = do
  stateRef <- view weshStateG
  state <- readIORef stateRef
  case Map.lookup token state of
    Just Terminal{tFd} -> do
        ioctlResize (fromIntegral tFd) (fromIntegral tsWidth) (fromIntegral tsHeight)
        logDebug $ "Changed the size of the terminal to: " <> display tSize
        pure True
    Nothing -> pure False

ioctlResize :: MonadIO m => CInt -> CUShort -> CUShort -> m ()
ioctlResize fd width height =
  void $
  liftIO $
  throwErrnoIfMinus1 "Could not resize the terminal" $ c_resize fd width height
```

# A bit about logging

```haskell
logDebug :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack) => Utf8Builder -> m ()
```
* Others are `logInfo`, `logWarn`, `logError`, `logOther`
* Also a special one `logSticky`
* Supports colors and is terminal aware
* Optionally adds timestamp and source location
* Thread safe
* Extremely efficient with `Utf8Builder`

```haskell
class Display a where
  display :: a -> Utf8Builder
  textDisplay :: a -> Text
```

```haskell
instance Display TerminalSize where
  display TerminalSize {tsWidth, tsHeight} =
    "width=" <> displayShow tsWidth <> ",height=" <> displayShow tsHeight
```

# What is `HasLogFunc`?

Example instance is

```haskell
instance HasLogFunc WeshEnv where
  logFuncL = lens weshEnvLogFunc (\c f -> c {weshEnvLogFunc = f})
```

Here are functions that create `LogOptions` and `LogFunc`

```haskell
logOptionsHandle :: MonadIO m => Handle -> Bool -> m LogOptions

withLogFunc :: MonadUnliftIO m => LogOptions -> (LogFunc -> m a) -> m a
```

Usage example (would be executed in `Main.hs`):

```haskell
  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelInfo defLogOptions
  withLogFunc logOptions $ \weshEnvLogFunc -> do
    weshEnvState <- newIORef Map.empty
    let appWeshEnv = WeshEnv {weshEnvState, weshEnvLogFunc}
    runRIO appWeshEnv someMonadReaderAction
    ...
```

* `LogFunc` is a `Monoid`, so loggers are composable together.
* Another consequence is that `mempty` means no logging.

# Pseudo terminal emulation

## Conduit way of communicating with the terminal:

```haskell
terminalInputSink :: MonadIO m => Terminal -> ConduitT ByteString o m ()
terminalInputSink = sinkHandle . tMasterHandle

terminalOutputSource :: MonadIO m => Terminal -> ConduitT i ByteString m ()
terminalOutputSource = sourceHandle . tMasterHandle
```

## Missing function

```haskell
makePseudoTerminal :: IO Terminal
makePseudoTerminal = do
  (masterFd, slaveFd) <- openPseudoTerminal
  masterHdl <- fdToHandle masterFd
  slaveHdl <- fdToHandle slaveFd
  pure $
    Terminal {tMasterHandle = masterHdl, tSlaveHandle = slaveHdl, tFd = slaveFd}
```

## Summary

Very concise, but a fully functional and safe terminal emulator.

The whole module is self-contained and could be released as a separate package.

We did not have to use `RIO` monad at all, because we could treat it as a library.

# Web part of the WebShell

```haskell
data App = App
  { appSqlBackendPool :: !(Pool SqlBackend)
  , appStatic         :: !Static
  , appWeshEnv        :: !WeshEnv
  }

instance Yesod App

staticFiles "files/static/"

mkYesod "App" [parseRoutes|
/ HomeR GET
/static StaticR Static appStatic
/terminal/#Token TerminalR GET
/resize/#Token ResizeTerminalR POST
|]

getHomeR :: HandlerFor App Html
getHomeR = do
  randomBS <- liftIO $ getRandomBytes 32
  let token = textDisplay $ Utf8Builder $ byteStringHex randomBS
  defaultLayout $ do
    setTitle "Haskell Web Shell - hwesh"
    addStylesheet $ StaticR wesh_css
    addScriptRemote "https://code.jquery.com/jquery-3.3.1.min.js"
    -- `hterm` created by google, built locally and served as a static file. Source:
    -- https://chromium.googlesource.com/apps/libapps/+/master/hterm
    addScript $ StaticR hterm_all_js
    addScript $ StaticR wesh_js
    [whamlet|<div id="terminal" data-token="#{token}">|]
```

# Communication

Yesod route that serves as an endpoint for websockets communication:

```haskell
getTerminalR :: Token -> HandlerFor App ()
getTerminalR token = do
  App {appWeshEnv} <- getYesod
  attemptCommunication token appWeshEnv
```

A RESTful endpoint that allows resizing the terminal window:
```haskell
postResizeTerminalR :: Token -> HandlerFor App Value
postResizeTerminalR token = do
  App {appWeshEnv} <- getYesod
  parseJsonBody >>= \case
    A.Error err ->
      sendStatusJSON badRequest400 $
      makeError ("JSON Parse Error: " <> T.pack err)
    A.Success termSize -> do
      hasResized <-
        runRIO appWeshEnv $ do
          RIO.logInfo $ "Attempt to resize: " <> display termSize
          resizeTerminal token termSize
      if hasResized
        then pure $ object ["success" .= True]
        else pure $ makeError "Failed to resize the terminal"

makeError :: Text -> Value
makeError txt = object ["error" .= txt]
```

# Websockets

Below we initialize the session and start the communication:

```haskell
attemptCommunication :: (MonadHandler m, MonadUnliftIO m) => Token -> WeshEnv -> m ()
attemptCommunication token weshEnv = do
  weshEnvProcessContext <- mkDefaultProcessContext
  let session = WeshSession weshEnv weshEnvProcessContext
  WS.webSockets $ withReaderT session (liftRIO (communicate token))
```

Here is the problem:

```haskell
WS.webSockets :: (MonadUnliftIO m, MonadHandler m) => ReaderT Connection m () -> m ()
```

where

```haskell
communicate :: Token -> RIO WeshSession ()
```

that we solve with `withReaderT` and:

```haskell
liftRIO :: (MonadIO m, MonadReader env m) => RIO env a -> m a
```

# ConduitT, ReaderT and RIO

```haskell
-- From yesod-websockets:

type WS.WebSocketsT = ReaderT Connection

WS.sourceWS :: (MonadIO m, WebSocketsData a, MonadReader Connection m) => ConduitT i a m ()

WS.sinkWSText :: (MonadIO m, WebSocketsData a, MonadReader Connection m) => ConduitT a o m ()
```

```haskell
class HasConnection env where
  connectionG :: SimpleGetter env Connection

fromWebSocketsT :: HasConnection env => WS.WebSocketsT IO a -> RIO env a
fromWebSocketsT ws = RIO (withReaderT (^. connectionG) ws)

sourceWSText :: HasConnection env => ConduitT i ByteString (RIO env) ()
sourceWSText = transPipe fromWebSocketsT WS.sourceWS

sinkWSText :: ConduitT ByteString o (RIO WeshSession) ()
sinkWSText = transPipe fromWebSocketsT WS.sinkWSText
```

* `withReaderT` and `transPipe` to the rescue.

# The connector

```haskell
communicate :: Token -> RIO WeshSession ()
communicate token = do
  eExitCode <-
    tryAny $ withShell token "/bin/bash" [] $ \ t ->
      let terminalOutput = terminalOutputSource t .| debugConduit "Output" .| sinkWSText
          terminalInput = sourceWSText .| debugConduit "Input" .| terminalInputSink t
          runCommunication = do
            runConduit (yield "\ESC[?25h" .| sinkWSText) -- turn on cursor
            race_ (runConduit terminalOutput) (runConduit terminalInput)
       in catch runCommunication $ \case
            exc@CloseRequest {} ->
              logInfo $ "Session was closed by the client: " <> displayShow exc
            exc -> logError $ "Session was terminated: " <> displayShow exc
  case eExitCode of
    Left exc ->
      logError $ "Error running a terminal connection: " <> displayShow exc
    Right (Right exitCode) -> logDebug $ "Exited with: " <> displayShow exitCode
    Right _ -> pure ()
```

```haskell
debugConduit :: (MonadIO m, MonadReader env m, HasLogFunc env)
             => Utf8Builder -> ConduitT ByteString ByteString m ()
debugConduit channel = iterMC logChars
  where logChars bs =
          case tParse bs of
            Left err ->
              logWarn $ "Couldn't parse the control sequence: " <> fromString err
            Right ctlSeq -> logDebug $ channel <> ": " <> displayShow ctlSeq
```

# rio-orphans

Instances for `RIO`

* `MonadLogger` from `monad-logger`
* `MonadCatch` and `MonadMask` from `exceptions`
* `MonadBase` from `transformers-base`
* `MonadBaseControl` from `monad-control`
* `MonadResource` from `resourcet`

# Demo

<!-- Gotta go deeper -->

# Summary

## Why choose rio

* Safety first approach and well established design patterns of `rio` allows us to write robust,
  secure and efficient software with minimal efforts.

## You have to follow all the rules!

* No you don't. You can still take advantage of many benefits that `rio` brings even if you stick to
  a subset.
* Helps a lot to have clear specification of guidelines when a team works on a large size project

## When `rio` is not a good choice?

* If writing an application, almost always `rio` is gonna be good idea.
* Writing a library that doesn't deal with exceptions, concurrency and little or no IO? One of those
  pure category theory packages? Maybe `rio` is an overkill.
* Selecting some parts of `rio` is also always an option.

<!-- pseudoTerminal example: typed-process + unliftIO -->
