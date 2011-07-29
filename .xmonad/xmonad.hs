{-# OPTIONS_GHC -fglasgow-exts #-}

import XMonad
import XMonad.Actions.NoBorders
import XMonad.Actions.WithAll
import XMonad.Config.Xfce
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers

import qualified Data.Map as M

myKeys (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. controlMask,   xK_w), spawn "uzbl")
    , ((modm .|. controlMask,   xK_f), spawn "firefox")
    , ((modm .|. controlMask,   xK_o), spawn "opera")
    , ((modm .|. controlMask,   xK_c), spawn "chrome")

    , ((modm .|. controlMask,   xK_m), spawn "spotify")
    , ((modm .|. controlMask,   xK_s), spawn "skype")

    , ((modm .|. controlMask,   xK_p), spawn "pidgin")
    , ((modm .|. controlMask,   xK_i), spawn "xterm -e irssi")

    , ((modm .|. controlMask,   xK_t), spawn "mtpaint")
    , ((modm .|. controlMask,   xK_h), spawn "Thunar")
    , ((modm .|. controlMask,   xK_e), spawn "emacs")

    , ((modm .|. shiftMask,     xK_p), spawn "scrot screenshots/screen_%Y-%m-%d_%T.jpg -d")
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

