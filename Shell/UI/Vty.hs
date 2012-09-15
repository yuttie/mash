module Shell.UI.Vty
    ( start
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Text as T
import Graphics.Vty
import Graphics.Vty.Widgets.All

import Manipulator
import Mash.Core
import Shell.Core


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
    fromShell <- newTChanIO
    toShell <- newTChanIO

    _ <- forkIO $ runServer "/tmp/mash_test" $ manipulator initState
    _ <- forkIO $ runClient "/tmp/mash_test" $ shell toShell fromShell

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
        atomically $ writeTChan toShell $ CommandInput l
        u <- atomically $ readTChan fromShell
        case u of
            ShowOutput t -> do
                setText v $ T.unpack t
                setEditText e ""
                focus v
            ShowError err -> do
                setEditText e $ "Error: " ++ err
                focus v
            Shutdown -> shutdownUi

    runUi c defaultContext
