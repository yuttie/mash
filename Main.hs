module Main where

import Control.Monad
import Graphics.Vty
import Graphics.Vty.Widgets.All


main :: IO ()
main = do
    v <- plainText "Test"
    e <- editWidget
    ui <- vBox v e
    setBoxChildSizePolicy ui $ PerChild BoxAuto (BoxFixed 1)

    fg <- newFocusGroup
    setFocusGroupNextKey fg (KASCII '\0') []
    setFocusGroupPrevKey fg (KASCII '\0') []
    _ <- addToFocusGroup fg v
    _ <- addToFocusGroup fg e

    c <- newCollection
    switchToMainUI <- addToCollection c ui fg

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
    e `onActivate` \this ->
        getEditText this >>= (error . ("You entered: " ++))

    runUi c defaultContext
