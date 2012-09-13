module Shell.UI.Vty
    ( start
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty
import Graphics.Vty.Widgets.All

import Manipulator
import Mash.Core
import Shell.Core


display :: Widget FormattedText -> Sink Text IO ()
display w = do
    ls <- CL.consume
    liftIO $ schedule $ setText w $ T.unpack $ T.unlines ls

start :: Manipulator a -> IO ()
start initState = do
    -- UI construction
    v <- plainText ""
    setNormalAttribute v $ with_back_color current_attr black
    f <- vFill ' '
    setNormalAttribute f $ with_back_color current_attr bright_black
    e <- editWidget
    setNormalAttribute e $ with_back_color current_attr black
    ui <- (return v <--> return f) <--> return e
    setBoxChildSizePolicy ui $ PerChild BoxAuto (BoxFixed 1)

    fg <- newFocusGroup
    setFocusGroupNextKey fg (KASCII '\0') []
    setFocusGroupPrevKey fg (KASCII '\0') []
    _ <- addToFocusGroup fg v
    _ <- addToFocusGroup fg e

    c <- newCollection
    switchToMainUI <- addToCollection c ui fg

    -- Run a manipulator and a shell.
    toShell <- newTChanIO

    _ <- forkIO $ runServer "/tmp/mash_test" $ manipulator initState
    _ <- forkIO $ runClient "/tmp/mash_test" $ shell toShell $ display v

    -- Vty event handlers
    v `onKeyPressed` \_ key _ ->
        if key == KASCII ':'
            then do
                setEditText e ":"
                setEditCursorPosition e (0, 1)
                focus e
                return True
            else return False
    e `onChange` \l ->
        when (l == "\n") $ do
            setEditText e ""
            focus v
    e `onActivate` \this -> do
        l <- init <$> tail <$> getEditText this
        case l of
            "quit" -> shutdownUi
            _ -> do
                atomically $ writeTChan toShell $ CommandInput l
                setEditText e ""
                focus v

    runUi c defaultContext
