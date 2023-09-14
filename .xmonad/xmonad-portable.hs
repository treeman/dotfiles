import XMonad
import XMonad.Core
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Gaps

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import MyConf

portableKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. controlMask,   xK_u), spawn "setxkbmap us")
    , ((modm .|. controlMask,   xK_space), spawn "setxkbmap se")
    , ((modm,                   xK_b     ), sendMessage ToggleStruts)
    ]

    ++

    -- mod-[1..9, 0], Switch to workspace N
    -- mod-shift-[1..9, 0], Move client to workspace N
    -- non greedy view, changed from default!
    [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

myDzen = " dzen2 -xs 1 -dock -h 18 -ta 'l' -fn '" ++ myFont ++ "' -fg '" ++
    fg_title ++ "' -bg '" ++ bg ++ "' "

myStatusBar = myDzen ++ " -x '0' -y '0' -ta 'l' -w 700"
myTopRight = "conky -c ~/.conky/conky_bar_laptop | " ++ myDzen ++ " -x '600' -y '0' -ta 'r' -p"
-- myTime = "conky -c ~/.conky/conky_time_laptop"
-- myTodo = "conky -c ~/.conky/conky_todo_laptop"
stdWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

main = do
    topLeft <- spawnPipe myStatusBar
    topRight <- spawnPipe myTopRight
    -- myTime <- spawnPipe myTime
    -- myTodo <- spawnPipe myTodo

    xmonad $ withUrgencyHook NoUrgencyHook $ def {
      modMask = mod4Mask
    , workspaces = stdWorkspaces
    , normalBorderColor = bg
    , focusedBorderColor = fg_sel
    , borderWidth = 1
    , terminal = term
    , keys = \k -> myKeys k `M.union` portableKeys k `M.union` keys def k
    , logHook = dynamicLogWithPP $ myDzenPP topLeft
    -- Avoid struts doesn't work properly with dzen here, workaround by hardcoding a gap
    -- , layoutHook = avoidStrutsOn[U] $ layoutHook def
    , layoutHook = gaps [(U,18)] $ layoutHook def
    , manageHook = manageDocks <+> manageHook def
    , startupHook = setWMName "LG3D"
}

