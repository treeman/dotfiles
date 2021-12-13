module MyConf where

import XMonad
import XMonad.Actions.WithAll
import XMonad.Actions.NoBorders
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

--import XMonad.Layout.NoBorders
--import XMonad.Actions.NoBorders
--import XMonad.Actions.WithAll
--import XMonad.Hooks.UrgencyHook
--import XMonad.Hooks.SetWMName

import System.IO
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- 16 bit Colors inspired from gruvbox
-- names may differ from appearence ;)
gb_black = "#282828"
gb_darkgrey = "#928374"
gb_darkred = "#cc241d"
gb_red = "#fb4934"
gb_darkgreen = "#98971a"
gb_green = "#b8bb26"
gb_darkyellow = "#d79921"
gb_yellow = "#fabd2f"
gb_darkblue = "#458588"
gb_blue = "#83a598"
gb_darkmagenta = "#b16286"
gb_magenta = "#d3869b"
gb_darkcyan = "#689d6a"
gb_cyan = "#8ec07c"
gb_lightgrey = "#a89984"
gb_white = "#ebdbb2"
-- hard contrast background
gb_background = "#1d2021"
gb_background_soft = "#32302f"
-- random colors
gb_purple = "#8f3f71"
gb_darkorange = "#d65d0e"
gb_orange = "#fe8019"
gb_light0 = "#fdf4c1"
gb_light1 = "#ebdbb2"
gb_light2 = "#d5c4a1"
gb_light3 = "#bdae93"
gb_light4 = "#a89984"

myWorkspaces = ["6", "4", "0", "2", "8", "9", "3", "1", "5", "7"]
myFont = "Consolasi:size=9"
myIconDir = "/home/tree/dotfiles/icons/"

normalStatusFG = gb_darkgrey
normalStatusBG = gb_background

term = "alacritty"

-- See default keybindings here: 
-- https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      ((modm,                   xK_Return), spawn $ term)
    , ((modm .|. controlMask,   xK_t), spawn "urxvt")

    , ((modm,                   xK_f), spawn "firefox")
    , ((modm,                   xK_m), spawn "spotify")
    , ((modm,                   xK_v), spawn "neovide")

    , ((modm,                   xK_p), spawn "scrot screenshots/screen_%Y-%m-%d_%T.png")

    , ((modm .|. shiftMask,     xK_c), kill)

    , ((modm .|. controlMask,   xK_u), spawn "setxkbmap us; xmodmap .xmodmap")
    , ((modm .|. controlMask,   xK_space), spawn "setxkbmap se; xmodmap .xmodmap")

    , ((modm .|. controlMask,   xK_b), withAll toggleBorder)
    , ((modm .|. shiftMask,     xK_b), sendMessage $ ToggleStrut U)
    , ((modm,                   xK_y), focusUrgent)

    -- Do not leave useless conky, dzen and after restart
    , ((modm,                   xK_q), spawn "killall conky dzen2; xmonad --recompile; xmonad --restart")

    -- Move focus with arrows, for split keyboard
    , ((modm,                   xK_Down), windows W.focusDown)
    , ((modm,                   xK_Up), windows W.focusUp)
    , ((modm,                   xK_Right), windows W.focusMaster)
    , ((modm .|. shiftMask,     xK_Down), windows W.swapDown)
    , ((modm .|. shiftMask,     xK_Up), windows W.swapUp)
    , ((modm .|. shiftMask,     xK_Right), windows W.swapMaster)
    ]

resizeAndMove w =  withDisplay $ \d -> do
               focus w
               io $ resizeWindow d w 800 600
               float w
               mouseMoveWindow w
               windows W.shiftMaster

myManageHook = composeAll
    [ className =? "Steam"  --> doFloat
    , className =? "steam"  --> doFullFloat
    , className =? "MainThrd"  --> doFloat
    , title =? "SmallCity"  --> doFloat
    {-, title =? "ld33"  --> doFloat-}
    -- Hack workaround trololol
    , title =? "ld33"  --> doRectFloat (W.RationalRect 0.2 0.1 (800/1914) (600/1076))
    , title =? "plasma-desktop"  --> doIgnore
    , manageDocks]

myDzenPP h = defaultPP
    { ppOutput = hPutStrLn h
    , ppCurrent = wrapFg gb_orange . dropId                 -- Current workspace
    , ppVisible = wrapFg gb_yellow . dropId                 -- Workspace for other screen
    , ppHidden = wrapFg gb_white . dropId                   -- Hidden workspace with windows
    , ppHiddenNoWindows = wrapFg gb_darkgrey . dropId       -- Hidden workspace without windows
    , ppUrgent = wrapFg gb_darkred . dropId                 -- Signaling workspace
    , ppTitle = wrap "< " " > " . wrapFg gb_white           -- Window title
    , ppSep = " "
    , ppLayout = wrapFg normalStatusFG .
        (\x -> case x of
            "Tall" -> wrapBitmap "Tall.xbm"
            "Mirror Tall" -> wrapBitmap "MirrorTall.xbm"
            "Full" -> wrapBitmap "Full.xbm"
            _ -> x
        )
    }
    where dropId x = if (':' `elem` x) then drop 2 x else x
          wrapFgBg fg bg content = wrap ("^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")") "^fg()^bg()" content
          wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
          wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
          wrapBitmap bitmap = "^p(2)^i(" ++ myIconDir ++ bitmap ++ ")^p(2)"

