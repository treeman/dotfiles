-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------
 
import XMonad hiding (Tall)
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ComboP
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run
import Control.Monad (liftM2)
import Data.Monoid
import Graphics.X11
import System.Exit
import System.IO
 
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M
 
-- XMonad:
main = do
  --xmobar <- spawnPipe myStatusBar
  dzen <- spawnPipe myStatusBar
  conkytop <- spawnPipe myTopBar
  conkympd <- spawnPipe myMPDBar
  conkyhdd <- spawnPipe myHDDBar
  xmonad $ myUrgencyHook $ defaultConfig
 
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
 
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
 
    , layoutHook         = myLayout
    , manageHook         = myManageHook <+> manageDocks <+> dynamicMasterHook
    , handleEventHook    = myEventHook
    , logHook            = dynamicLogWithPP $ myDzenPP dzen
    --, logHook          = dynamicLogWithPP $ myXmobarPP xmobar
    , startupHook        = myStartupHook
    }
 
myTerminal = "urxvt"
myFocusFollowsMouse = True
myBorderWidth = 1
myModMask = mod1Mask
myWorkspaces = ["1:irc", "2:www", "3:music", "4:misc", "5:xbmc", "6:gimp", "7:mplayer", "8:foo()", "9:vbox"]
myNormalBorderColor = "#0f0f0f"
myFocusedBorderColor = "#1f1f1f"
 
myEventHook = mempty
--myLogHook = dynamicLogWithPP $ myDzenPP dzen
myStartupHook = return ()
 
-- Color, font and iconpath definitions:
--myFont = "-xos4-terminus-medium-r-normal-*-14-*-*-*-c-*-iso10646-1"
myFont = "-*-montecarlo-medium-r-normal-*-11-*-*-*-c-*-*-*"
myIconDir = "/home/and1/.dzen"
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
 
-- GSConfig options:
myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    --, gs_colorizer = ""
    , gs_font = "" ++ myFont ++ ""
    --, gs_navigate = ""
    --, gs_originFractX = ""
    --, gs_originFractY = ""
    }
 
-- XPConfig options:
myXPConfig = defaultXPConfig
    { font = "" ++ myFont ++ ""
    , bgColor = "" ++ myNormalBGColor ++ ""
    , fgColor = "" ++ myNormalFGColor ++ ""
    , fgHLight = "" ++ myNormalFGColor ++ ""
    , bgHLight = "" ++ myUrgentBGColor ++ ""
    , borderColor = "" ++ myFocusedBorderColor ++ ""
    , promptBorderWidth = 1
    , position = Bottom
    , height = 16
    , historySize = 100
    --, historyFilter = ""
    --, promptKeymap = ""
    --, completionKey = ""
    --, defaultText = ""
    --, autoComplete = "KeySym"
    --, showCompletionOnTab = ""
    }
 
-- Theme options:
myTheme = defaultTheme
    { activeColor = "" ++ myFocusedBGColor ++ ""
    , inactiveColor = "" ++ myDzenBGColor ++ ""
    , urgentColor = "" ++ myUrgentBGColor ++ ""
    , activeBorderColor = "" ++ myFocusedBorderColor ++ ""
    , inactiveBorderColor = "" ++ myNormalBorderColor ++ ""
    , urgentBorderColor = "" ++ myNormalBorderColor ++ ""
    , activeTextColor = "" ++ myFocusedFGColor ++ ""
    , inactiveTextColor = "" ++ myDzenFGColor ++ ""
    , urgentTextColor = "" ++ myUrgentFGColor ++ ""
    , fontName = "" ++ myFont ++ ""
    --, decoWidth = ""
    --, decoHeight = ""
    }
 
-- Statusbar options:
--myStatusBar = "xmobar"
myStatusBar = "dzen2 -x '0' -y '0' -h '16' -w '1300' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myTopBar = "conky -c .conkytop | dzen2 -x '1300' -y '0' -h '16' -w '620' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myMPDBar = "conky -c .conkympd | dzen2 -x '0' -y '1184' -h '16' -w '1600' -ta 'l' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myHDDBar = "conky -c .conkyhdd | dzen2 -x '1600' -y '1184' -h '16' -w '320' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
 
-- Urgency hint options:
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", "1184", "-h", "16", "-w", "1920", "-ta", "r", "-expand", "l", "-fg", "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""] }
 
-- Layouts:
myLayout = avoidStruts $ layoutHints $ onWorkspace "1:irc" (resizableTile ||| Mirror resizableTile) $ onWorkspace "6:gimp" gimpLayout $ smartBorders (Full ||| resizableTile ||| Mirror resizableTile)
    where
    resizableTile = ResizableTall nmaster delta ratio []
    tabbedLayout = tabbedBottomAlways shrinkText myTheme
    --gimpLayout = tabbedLayout ****||* Full
    --gimpLayout = Full *||**** tabbedLayout
    gimpLayout = combineTwoP (TwoPane 0.04 0.82) (tabbedLayout) (Full) (Not (Role "gimp-toolbox"))
    nmaster = 1
    ratio = toRational (2/(1+sqrt(5)::Double))
    delta = 3/100
 
-- Key bindings:
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((mod4Mask, xK_q), spawn "urxvt -name irssi -e ssh and1@donnergurgler.net")
    , ((mod4Mask, xK_w), spawn "nitrogen --no-recurse --sort=alpha /home/and1/wallpapers/")
    , ((mod4Mask .|. shiftMask, xK_w), spawn "nitrogen --no-recurse --sort=alpha /home/and1/wallpapers/_old/")
    , ((mod4Mask, xK_e), spawn "iceweasel")
    , ((mod4Mask, xK_r), oneShotHook (className =? "URxvt") (doF $ (W.swapDown . W.shiftMaster)) >> spawn "urxvt")
    , ((mod4Mask, xK_r), spawn "urxvt")
    , ((mod4Mask .|. shiftMask, xK_r), oneShotHook (className =? "URxvt") (doF $ (W.swapUp . W.shiftMaster)) >> spawn "urxvt")
    , ((mod4Mask, xK_i), spawn "env GTK2_RC_FILES=$HOME/.themes/Clearlooks-DarkBlue/gtk-2.0/gtkrc inkscape")
    , ((mod4Mask, xK_o), spawn "env GTK2_RC_FILES=$HOME/.themes/Clearlooks-DarkBlue/gtk-2.0/gtkrc openoffice.org")
    , ((mod4Mask, xK_p), shellPrompt myXPConfig)
    , ((mod4Mask, xK_d), spawn "date +'%Y-%m-%d %H:%M' | osd_cat -p bottom -A center -s 2 -c '#a10a30' -f '-xos4-terminus-medium-r-normal-*-140-*-*-*-c-*-iso10646-1'")
    , ((mod4Mask .|. shiftMask, xK_d), spawn "date +'%H:%M' | osd_cat -p middle -A center -s 2 -c '#a10a30' -f '-xos4-terminus-medium-r-normal-*-140-*-*-*-c-*-iso10646-1'")
    , ((mod4Mask, xK_f), oneShotHook (className =? "URxvt") doFloat >> spawn "urxvt -g 80x25+25+765")
    , ((mod4Mask .|. shiftMask, xK_f), oneShotHook (className =? "URxvt") doFloat >> spawn "urxvt -g 140x10+25+975")
    , ((mod4Mask, xK_g), spawn "gimp")
    , ((mod4Mask, xK_k), spawn "k3b")
    , ((mod4Mask, xK_l), spawn "slock")
    , ((mod4Mask, xK_x), spawn "xbmc")
    , ((mod4Mask, xK_v), spawn "virtualbox")
    , ((mod4Mask, xK_n), spawn "nicotine")
    , ((mod4Mask, xK_m), spawn "gmpc")
    , ((mod4Mask, xK_Print), spawn "scrot screen_%Y-%m-%d.png -d 1") -- take screenshot
    , ((modMask .|. controlMask, xK_Home), spawn "mpc toggle") -- play/pause song
    , ((modMask .|. controlMask, xK_End), spawn "mpc stop") -- stop playback
    , ((modMask .|. controlMask, xK_Prior), spawn "mpc prev") -- previous song
    , ((modMask .|. controlMask, xK_Next), spawn "mpc next") -- next song
    , ((modMask, xK_Tab), windows W.focusDown) -- move focus to the next window
    , ((modMask, xK_j), windows W.focusDown) -- move focus to the next window
    , ((modMask, xK_k), windows W.focusUp) -- move focus to the previous window
    --, ((modMask, xK_n), refresh) -- resize viewed windows to the correct size
    , ((modMask, xK_m), windows W.swapMaster) -- swap the focused window and the master window
    , ((modMask, xK_comma), sendMessage (IncMasterN 1)) -- increment the number of windows in the master area
    , ((modMask, xK_period), sendMessage (IncMasterN (-1))) -- deincrement the number of windows in the master area
    , ((modMask, xK_Return), windows W.focusMaster) -- move focus to the master window
    , ((modMask, xK_f), sendMessage NextLayout) -- rotate through the available layout algorithms
    , ((modMask, xK_g), goToSelected myGSConfig) -- display grid select and go to selected window
    , ((modMask .|. shiftMask, xK_Tab), windows W.focusUp) -- move focus to the previous window
    , ((modMask .|. shiftMask, xK_g), gridselectWorkspace myGSConfig W.view) -- display grid select and go to selected workspace
    , ((modMask .|. shiftMask, xK_h), sendMessage Shrink) -- shrink the master area
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown) -- swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)  -- swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_l), sendMessage Expand) -- expand the master area
    , ((modMask .|. shiftMask, xK_Return), focusUrgent) -- move focus to urgent window
    --, ((modMask .|. shiftMask, xK_Left), withFocused (keysResizeWindow (-30,0) (0,0))) -- shrink floated window horizontally by 50 pixels
    --, ((modMask .|. shiftMask, xK_Right), withFocused (keysResizeWindow (30,0) (0,0))) -- expand floated window horizontally by 50 pixels
    --, ((modMask .|. shiftMask, xK_Up), withFocused (keysResizeWindow (-30,-30) (0,0))) -- shrink floated window by 50 pixels
    --, ((modMask .|. shiftMask, xK_Down), withFocused (keysResizeWindow (30,30) (0,0))) -- expand floated window by 50 pixels
    , ((modMask .|. controlMask, xK_q), io (exitWith ExitSuccess)) -- quit xmonad
    , ((modMask .|. controlMask, xK_r), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart") -- restart xmonad
    , ((modMask .|. controlMask, xK_d), withFocused $ windows . W.sink) -- push window back into tiling
    , ((modMask .|. controlMask, xK_f), setLayout $ XMonad.layoutHook conf) -- reset the layouts on the current workspace to default
    , ((modMask .|. controlMask, xK_h), sendMessage MirrorExpand) -- expand the height/width
    , ((modMask .|. controlMask, xK_j), windows W.swapDown) -- swap the focused window with the next window
    , ((modMask .|. controlMask, xK_k), windows W.swapUp)  -- swap the focused window with the previous window
    , ((modMask .|. controlMask, xK_l), sendMessage MirrorShrink) -- shrink the height/width
    , ((modMask .|. controlMask, xK_x), kill) -- close focused window
    , ((modMask .|. controlMask, xK_Left), withFocused (keysMoveWindow (-30,0))) -- move floated window 10 pixels left
    , ((modMask .|. controlMask, xK_Right), withFocused (keysMoveWindow (30,0))) -- move floated window 10 pixels right
    , ((modMask .|. controlMask, xK_Up), withFocused (keysMoveWindow (0,-30))) -- move floated window 10 pixels up
    , ((modMask .|. controlMask, xK_Down), withFocused (keysMoveWindow (0,30))) -- move floated window 10 pixels down
    ]
    ++
    [ ((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F9] -- mod-[F1..F9], switch to workspace n
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- mod-shift-[F1..F9], move window to workspace n
    ]
    ++
    [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_F10, xK_F11, xK_F12] [0..] -- mod-{F10,F11,F12}, switch to physical/Xinerama screens 1, 2, or 3
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] -- mod-shift-{F10,F11,F12}, move window to screen 1, 2, or 3
    ]
 
-- Mouse bindings:
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- set the window to floating mode and move by dragging
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster)) -- raise the window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) -- set the window to floating mode and resize by dragging
    , ((modMask, button4), (\_ -> prevWS)) -- switch to previous workspace
    , ((modMask, button5), (\_ -> nextWS)) -- switch to next workspace
    ]
 
-- Window rules:
myManageHook = composeAll . concat $
    [ [isDialog --> doFloat]
    , [className =? c --> doFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "1:irc" | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "2:www" | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "3:music" | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "4:misc" | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "5:xbmc" | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "6:gimp" | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "7:mplayer" | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "8:foo()" | x <- my8Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "9:vbox" | x <- my9Shifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Ekiga", "MPlayer", "Nitrogen", "Nvidia-settings", "XCalc", "XFontSel", "Xmessage"]
    myTFloats = ["Downloads", "Iceweasel Preferences", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window", "kdesktop"]
    my1Shifts = []
    my2Shifts = ["Iceweasel"]
    my3Shifts = ["Gmpc"]
    my4Shifts = ["Geeqie", "Gthumb"]
    my5Shifts = ["xbmc.bin"]
    my6Shifts = ["Gimp"]
    my7Shifts = ["MPlayer"]
    my8Shifts = ["Easytag", "Gconf-editor", "K3b", "MusicBrainz Picard", "OpenOffice.org 3.2", "VCLSalFrame.DocumentWindow"]
    my9Shifts = ["VirtualBox", "Wine"]
 
-- dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
    { ppCurrent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHidden = wrap ("^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    --, ppHiddenNoWindows = wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . dropIx $ wsId
    , ppUrgent = wrap (("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myUrgentFGColor ++ ")")) "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-full.xbm)"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-tall-right.xbm)"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-mirror-bottom.xbm)"
        --"Hinted combining Tabbed Bottom Simplest and Full with DragPane  Vertical 0.1 0.8" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        "Hinted combining Tabbed Bottom Simplest and Full with TwoPane using Not (Role \"gimp-toolbox\")" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["1:irc", "2:www", "3:music", "4:misc", "5:xbmc"]
 
-- dynamicLog pretty printer for xmobar:
myXmobarPP h = defaultPP
    { ppCurrent = wrap ("[<fc=" ++ myUrgentFGColor ++ ">") "</fc>]" . \wsId -> dropIx wsId
    , ppVisible = wrap ("[<fc=" ++ myNormalFGColor ++ ">") "</fc>]" . \wsId -> dropIx wsId
    , ppHidden = wrap "" "" . \wsId -> dropIx wsId -- don't use <fc> here!!
    --, ppHiddenNoWindows = wrap ("<fc=" ++ myDzenFGColor ++ ">") "</fc>" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("<fc=" ++ myDzenFGColor ++ ">") "</fc>" . dropIx $ wsId
    , ppUrgent = wrap ("<fc=" ++ myUrgentFGColor ++ ">") "</fc>" . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = xmobarColor (""++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppLayout = xmobarColor (""++ myDzenFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "[ ]"
        "Hinted ResizableTall" -> "[|]"
        "Hinted Mirror ResizableTall" -> "[-]"
        --"Hinted combining Tabbed Bottom Simplest and Full with DragPane  Vertical 0.1 0.8" -> "[/]"
        "Hinted combining Tabbed Bottom Simplest and Full with TwoPane using Not (Role \"gimp-toolbox\")" -> "[/]"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["1:irc", "2:www", "3:music", "4:misc", "5:xbmc"]
