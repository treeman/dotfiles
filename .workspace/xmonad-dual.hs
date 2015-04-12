
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
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus

import System.IO

import qualified Data.Map as M
import qualified XMonad.StackSet as W

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. controlMask,   xK_f), spawn "firefox")
    , ((modm .|. controlMask,   xK_c), spawn "chrome")

    , ((modm .|. controlMask,   xK_e), spawn "emacs")

    , ((modm .|. controlMask,   xK_s), spawn "skype")
    , ((modm .|. controlMask,   xK_i), spawn "start_irc")

    , ((modm .|. controlMask,   xK_m), spawn "spotify")
    , ((modm .|. controlMask,   xK_t), spawn "mtpaint")
    , ((modm .|. controlMask,   xK_a), spawn "anki")

    , ((modm .|. shiftMask,     xK_t), spawn "xterm") -- Just a backup for now, with the borked enter
    , ((modm .|. shiftMask,     xK_p), spawn "scrot screenshots/screen_%Y-%m-%d_%T.png -d")

    , ((modm .|. controlMask,   xK_u), spawn "setxkbmap us; xmodmap .xmodmap")
    , ((modm .|. controlMask,   xK_space), spawn "setxkbmap se; xmodmap .xmodmap")

    , ((modm .|. controlMask,     xK_p), spawn "pom --continue")
    , ((modm .|. controlMask,     xK_s), spawn "pom --stop")

    , ((modm .|. controlMask,   xK_b), withAll toggleBorder)
    , ((modm .|. shiftMask,     xK_b), withFocused toggleBorder)
    , ((modm,                   xK_y), focusUrgent)

    -- Do not leave useless conky, dzen and after restart
    , ((modm,                   xK_q), spawn "killall conky dzen2; xmonad --recompile; xmonad --restart")
    ]

    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- non greedy view, changed from default!
    [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

    ++

    -- mod-e, mod-w switch workspaces (I've got flipped monitors, flip e and w from standard
    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
--myWorkspaces = ["α","β","γ","δ","ε","ζ","η","θ","ι","κ"]
--myWorkspaces = ["α","β","γ", "4:prog", "5:www", "6:chat", "7:irc", "8:music", "9:misc"]

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

myDzenPP h = defaultPP
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

myManageHook = composeAll
    [ className =? "Steam"  --> doFloat
    , className =? "steam"  --> doFullFloat
    , className =? "MainThrd"  --> doFloat
    , title =? "SmallCity"  --> doFloat
    , title =? "plasma-desktop"  --> doIgnore
    --[ className =? "Steam"  --> doIgnore
    , manageDocks]

main = do
    topLeft <- spawnPipe myStatusBar
    topRight <- spawnPipe myTopRight

    conkyHabit <- spawnPipe "conky -c ~/.workspace/conky_habit"
    conkyKernel <- spawnPipe "conky -c ~/.workspace/conky_kernel"
    conkyTime <- spawnPipe "conky -c ~/.workspace/conky_time"
    conkySchool <- spawnPipe "conky -c ~/.workspace/conky_school"
    conkyTimezone <- spawnPipe "conky -c ~/.workspace/conky_timezone"
    conkyTicker <- spawnPipe "conky -c ~/.workspace/conky_ticker"

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        modMask = mod4Mask
        , workspaces = myWorkspaces
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#363636"
        , borderWidth = 1
        , terminal = "urxvt"
        , keys = \k -> myKeys k `M.union` keys defaultConfig k
        , logHook = dynamicLogWithPP $ myDzenPP topLeft
        , layoutHook = avoidStrutsOn[U] $ layoutHook defaultConfig
        , manageHook = manageDocks <+> myManageHook

        -- Trick java apps like minecraft to correctly recognize windowed screen resolution in dual screen mode
        , startupHook = setWMName "LG3D"
    }

