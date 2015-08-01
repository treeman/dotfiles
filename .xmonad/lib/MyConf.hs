module MyConf where

import XMonad
import XMonad.Actions.WithAll
import XMonad.Actions.NoBorders
import XMonad.Hooks.UrgencyHook

import qualified Data.Map as M
import qualified XMonad.StackSet as W

keys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. controlMask,   xK_f), spawn "firefox")
    , ((modm .|. controlMask,   xK_c), spawn "chromium")

    , ((modm .|. controlMask,   xK_e), spawn "emacs")

    , ((modm .|. controlMask,   xK_s), spawn "skype")
    , ((modm .|. controlMask,   xK_i), spawn "start_irc")

    , ((modm .|. controlMask,   xK_m), spawn "spotify")
    , ((modm .|. controlMask,   xK_t), spawn "mtpaint")
    , ((modm .|. controlMask,   xK_a), spawn "anki")

    , ((modm .|. shiftMask,     xK_t), spawn "xterm") -- Just a backup for now, with the borked enter
    , ((modm .|. controlMask,   xK_p), spawn "scrot screenshots/screen_%Y-%m-%d_%T.png -d")

    , ((modm .|. controlMask,   xK_u), spawn "setxkbmap us; xmodmap .xmodmap")
    , ((modm .|. controlMask,   xK_space), spawn "setxkbmap se; xmodmap .xmodmap")

    , ((modm .|. shiftMask,     xK_p), spawn "pom --continue")
    , ((modm .|. shiftMask,     xK_o), spawn "pom --stop")

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

