import XMonad
import XMonad.Config.Xfce
import XMonad.Layout.NoBorders

main = xmonad xfceConfig
    { modMask = mod4Mask
    , workspaces = ["1","2","3","4","5","6","7","8","9"]
    , normalBorderColor = "#fdfdfd"
    , focusedBorderColor = "#ffdd76"
    , borderWidth = 1 }
