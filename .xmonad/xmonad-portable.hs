import XMonad
import XMonad.Core
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import MyConf

portableKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. controlMask,   xK_u), spawn "setxkbmap us")
    , ((modm .|. controlMask,   xK_space), spawn "setxkbmap se")
    , ((modm,                   xK_b     ), sendMessage ToggleStruts)
    ]

myDzen = " dzen2 -xs 1 -dock -h 18 -ta 'l' -fn '" ++ myFont ++ "' -fg '" ++
    normalStatusFG ++ "' -bg '" ++ normalStatusBG ++ "' "

myStatusBar = myDzen ++ " -x '0' -y '0' -ta 'l' -w 700"
myTopRight = "conky -c ~/.conky/conky_bar_laptop | " ++ myDzen ++ " -x '600' -y '0' -ta 'r' -p"

main = do
    topLeft <- spawnPipe myStatusBar
    topRight <- spawnPipe myTopRight

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
      modMask = mod4Mask
    , workspaces = myWorkspaces
    , normalBorderColor = gb_background
    , focusedBorderColor = gb_background_soft
    , borderWidth = 1
    , terminal = term
    , keys = \k -> myKeys k `M.union` portableKeys k `M.union` keys defaultConfig k
    , logHook = dynamicLogWithPP $ myDzenPP topLeft
    , layoutHook = avoidStrutsOn[U] $ layoutHook defaultConfig
    , manageHook = manageDocks <+> manageHook defaultConfig
    , startupHook = setWMName "LG3D"
}
