
import XMonad
import XMonad.Core
import XMonad.Util.Run(spawnPipe)

import XMonad.Layout.NoBorders

import XMonad.Actions.NoBorders
import XMonad.Actions.WithAll

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import System.IO

import qualified Data.Map as M
import qualified XMonad.StackSet as W

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. controlMask,   xK_f), spawn "firefox")
    , ((modm .|. controlMask,   xK_u), spawn "uzbl")

    , ((modm .|. controlMask,   xK_s), spawn "skype")
    , ((modm .|. controlMask,   xK_i), spawn "xterm -e irssi")
    , ((modm .|. controlMask,   xK_p), spawn "pidgin")

    , ((modm .|. controlMask,   xK_m), spawn "spotify")
    , ((modm .|. controlMask,   xK_t), spawn "mtpaint")

    , ((modm .|. shiftMask,     xK_p), spawn "scrot screenshots/screen_%Y-%m-%d_%T.png -d")

    , ((modm .|. controlMask,   xK_b), withAll toggleBorder)
    , ((modm .|. shiftMask,     xK_b), withFocused toggleBorder)
    , ((modm,                   xK_y), focusUrgent)
    -- Do not leave useless conky, dzen and xmobar after restart
    , ((modm,                   xK_q), spawn "killall xmobar conky dzen2; xmonad --recompile; xmonad --restart")

    --, ((modm,                   xK_b     ), sendMessage ToggleStruts)
    ]

    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- non greedy view, changed from default!
    [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

    ++

    -- mod-e, mod-w switch workspaces (I've got flipped monitors, flip e and w from standard
    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

-- Delete later, not used
myLogHook h = dynamicLogWithPP $ xmobarPP
  { ppOutput          = hPutStrLn h
  , ppTitle           = color "grey" . shorten 50
  , ppHiddenNoWindows = color "grey" . dropId
  , ppCurrent         = color "yellow" . dropId
  , ppVisible         = color "#F711DD" . dropId
  , ppHidden          = color "lightskyblue" . dropId
  --, ppUrgent          = color "red" . dropId
  }
  where color x = xmobarColor x ""
        dropId x = if (':' `elem` x) then drop 2 x else x

--["α","β","γ","δ","ε","ζ"]
myWorkspaces = ["α","β","γ", "4:prog", "5:www", "6:chat", "7:irc", "8:music", "9:misc"]

--myIconDir = "/home/and1/.dzen"
myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
mySeperatorColor = "#555555"

myFont = "Consolasi:size=9"
myIconDir = "/home/tree/.workspace/icons/"
myNormalStatusFG = "#616161"
myNormalStatusBG = "#0f0f0f"

-- Triades
-- yellow: #F2FF00
-- lightblue: #00F2FF
-- pink: #FF00F2
--
-- Tetrades
-- red: #BD5E5E
-- green: #8EBD5E
-- blue: #5EBDBD
-- purple: #8E5EBD

myDzen = " dzen2 -xs 1 -dock -h 18 -ta 'l' -fn '" ++ myFont ++ "' -fg '" ++ myNormalStatusFG ++ "' -bg '" ++ myNormalStatusBG ++ "' "

myStatusBar = myDzen ++ " -x '0' -y '0' -ta 'l' -w 800"
myTopRight = "conky -c ~/.workspace/conky_bar | " ++ myDzen ++ " -x '800' -y '0' -ta 'r' -p"

myHackerTop = "conky -c ~/.workspace/conkyrc-hackertop"

myDzenPP h = defaultPP
    --{ ppCurrent = wrapFg "#FFFF00" . dropId
    --, ppVisible = wrapFg "#F711DD" . dropId
    --, ppHidden = wrapFg "#1FA0E6" . dropId
    --, ppHiddenNoWindows = wrapFg "#616161" . dropId

    --, ppCurrent = wrapFg "#8EBD5E" . dropId
    --, ppVisible = wrapFg "#8E5EBD" . dropId
    --, ppHidden = wrapFg "#5EBDBD" . dropId
    --, ppHiddenNoWindows = wrapFg "#616161" . dropId
    --, ppUrgent = wrapFg "#BD5E5E" . dropId
    { ppOutput = hPutStrLn h
    , ppCurrent = wrapFg "#FFB600" . dropId
    , ppVisible = wrapFg "#FFD86E" . dropId
    , ppHidden = wrapFg "#E8E8E8" . dropId
    , ppHiddenNoWindows = wrapFg "#616161" . dropId
    , ppUrgent = wrapFg "#FF0049" . dropId
    , ppTitle = wrap "< " " > " . wrapFg "#ffffff"
    , ppSep = " "
    , ppLayout = wrapFg myNormalStatusFG .
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

main = do
    topLeft <- spawnPipe myStatusBar
    topRight <- spawnPipe myTopRight

    --conkyTodo <- spawnPipe "conky -c ~/.workspace/conky_todo"
    --conkyKernel <- spawnPipe "conky -c ~/.workspace/conky_kernel"
    --conkyTime <- spawnPipe "conky -c ~/.workspace/conky_time"
    --conkySchool <- spawnPipe "conky -c ~/.workspace/conky_school"
    --conkyTimezone <- spawnPipe "conky -c ~/.workspace/conky_timezone"
    --conkyTicker <- spawnPipe "conky -c ~/.workspace/conky_ticker"

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
      modMask = mod4Mask
    , workspaces = myWorkspaces
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#363636"
    , borderWidth = 0
    , terminal = "xterm"
    , keys = \k -> myKeys k `M.union` keys defaultConfig k
    , logHook = dynamicLogWithPP $ myDzenPP topLeft
    , layoutHook = avoidStrutsOn[U] $ layoutHook defaultConfig
    , manageHook = manageDocks <+> manageHook defaultConfig
}
