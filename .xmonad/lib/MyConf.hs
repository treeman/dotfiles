module MyConf where

import XMonad
import XMonad.Actions.WithAll
import XMonad.Actions.NoBorders
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import System.IO
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Colors from melange
bg = "#292522"

fg_title = "#867462" 
fg_urgent ="#8f3f71"  -- gruvbox color
fg_current_workspace = "#D47766"
fg_other_workspace = "#78997A"
fg_windowed_workspace ="#C1A78E"
fg_empty_workspace = "#403A36"
fg_ui = "#867462"
fg_sel = "#403A36"

myWorkspaces = ["6", "4", "0", "2", "8", "9", "3", "1", "5", "7"]
myFont = "Consolasi:size=11"
myIconDir = "/home/tree/dotfiles/icons/"

term = "ghostty"

-- See default keybindings here: 
-- https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      ((modm,                   xK_Return), spawn $ term)
    -- , ((modm,                   xK_t), spawn "alacritty")

    , ((modm,                   xK_f), spawn "firefox")
    , ((modm,                   xK_m), spawn "spotify")
    , ((modm,                   xK_v), spawn "neovide")
    , ((modm,                   xK_c), spawn "chromium")

    , ((modm,                   xK_p), spawn "scrot screenshots/screen_%Y-%m-%d_%T.png")

    , ((modm .|. shiftMask,     xK_c), kill)

    , ((modm .|. controlMask,   xK_u), spawn "setxkbmap us; xmodmap .xmodmap")
    , ((modm .|. controlMask,   xK_space), spawn "setxkbmap se; xmodmap .xmodmap")

    , ((modm .|. controlMask,   xK_b), withAll toggleBorder)
    , ((modm .|. shiftMask,     xK_b), sendMessage $ ToggleStrut U)
    , ((modm,                   xK_y), focusUrgent)

    -- Do not leave useless conky, dzen and after restart
    , ((modm,                   xK_q), spawn "killall conky dzen2; xmonad --recompile; xmonad --restart")

    , ((modm .|. shiftMask,     xK_l), spawn "xsecurelock")

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
    [
    -- className =? "Steam"  --> doFloat
    -- , className =? "steam"  --> doFullFloat
    className =? "MainThrd"  --> doFloat
    -- , title =? "SmallCity"  --> doFloat
    {-, title =? "ld33"  --> doFloat-}
    -- Hack workaround trololol
    -- , title =? "ld33"  --> doRectFloat (W.RationalRect 0.2 0.1 (800/1914) (600/1076))
    -- , title =? "plasma-desktop"  --> doIgnore
    , manageDocks]

myDzenPP h = def
    { ppOutput = hPutStrLn h
    , ppCurrent = wrapFg fg_current_workspace . dropId          -- Current workspace
    , ppVisible = wrapFg fg_other_workspace . dropId            -- Workspace for other screen
    , ppHidden = wrapFg fg_windowed_workspace . dropId          -- Hidden workspace with windows
    , ppHiddenNoWindows = wrapFg fg_empty_workspace . dropId    -- Hidden workspace without windows
    , ppUrgent = wrapFg fg_urgent . dropId                      -- Signaling workspace
    , ppTitle = wrap "< " " > " . wrapFg fg_title               -- Window title
    , ppSep = " "
    , ppLayout = wrapFg fg_title .
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

