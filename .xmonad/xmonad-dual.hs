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

dualKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask,     xK_t), spawn "urxvt")
    ]

    ++

    -- mod-e, mod-w switch workspaces (I've got flipped monitors, flip e and w from standard
    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

myDzen = " dzen2 -xs 1 -dock -h 20 -ta 'l' -fn '" ++ myFont ++ "' -fg '" ++
    normalStatusFG ++ "' -bg '" ++ normalStatusBG ++ "' "

myStatusBar = myDzen ++ " -x '0' -y '0' -ta 'l' -w 800"
myTopRight = "conky -c ~/.conky/conky_bar | " ++ myDzen ++ " -x '800' -y '0' -ta 'r' -p"

main = do
    topLeft <- spawnPipe myStatusBar
    topRight <- spawnPipe myTopRight

    -- conkyKernel <- spawnPipe "conky -c ~/.conky/conky_kernel"
    -- conkyTime <- spawnPipe "conky -c ~/.conky/conky_time"
    -- TODO make ticker work first!
    -- conkyTicker <- spawnPipe "conky -c ~/.conky/conky_ticker"

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        modMask = mod4Mask
        , workspaces = myWorkspaces
        , normalBorderColor = gb_background
        , focusedBorderColor = gb_background_soft
        , borderWidth = 1
        , terminal = "kitty"
        , keys = \k -> dualKeys k `M.union` myKeys k `M.union` keys defaultConfig k
        , logHook = dynamicLogWithPP $ myDzenPP topLeft
        , layoutHook = avoidStrutsOn[U] $ layoutHook defaultConfig
        , manageHook = manageDocks <+> myManageHook
        -- Necessary to recognize struts for dzen
        , handleEventHook = docksEventHook <+> handleEventHook defaultConfig

        -- Trick java apps like minecraft to correctly recognize windowed screen
        -- resolution in dual screen mode
        , startupHook = setWMName "LG3D"
    }

