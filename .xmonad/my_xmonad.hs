{-# OPTIONS_GHC -fglasgow-exts #-}

import XMonad
import XMonad.Actions.NoBorders
import XMonad.Actions.WithAll
import XMonad.Config.Xfce
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers

import qualified Data.Map as M

myKeys (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm,                   xK_w), spawn "uzbl")
    , ((modm,                   xK_m), spawn "wine \"C:/Program files/spotify/spotify.exe\"")
    , ((modm,                   xK_p), spawn "pidgin")
    , ((modm,                   xK_f), spawn "firefox")
    , ((modm,                   xK_s), spawn "skype")
    , ((modm,                   xK_i), spawn "xterm -e irssi")
    , ((modm .|. shiftMask,     xK_b), withAll toggleBorder)
    ]

main = xmonad xfceConfig {
      modMask = mod4Mask
    , workspaces = ["1","2","3","4","5","6","7","8","9"]
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#d00f0f"
    , borderWidth = 1
    , terminal = "xterm"
    , keys = \k -> myKeys k `M.union` keys xfceConfig k
}

