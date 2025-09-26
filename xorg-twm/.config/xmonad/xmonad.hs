-- -*- compile-command: "xmonad --recompile" -*-
import Colors.Current
import Control.Monad
import qualified Data.Map as M
import Data.Ratio ((%))

import System.Exit
import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow
  ( copiesPP
  , copy
  , copyToAll
  , kill1
  , killAllOtherCopies
  )
import XMonad.Actions.CycleWS
  ( Direction1D(..)
  , WSType(..)
  , anyWS
  , emptyWS
  , hiddenWS
  , ignoringWSs
  , moveTo
  , nextScreen
  , nextWS
  , prevScreen
  , prevWS
  , shiftNextScreen
  , shiftPrevScreen
  , shiftTo
  , shiftToNext
  , shiftToPrev
  , toggleOrDoSkip
  , toggleWS'
  )
import XMonad.Actions.CycleWindows
import XMonad.Actions.EasyMotion (EasyMotionConfig(..), selectWindow, textSize)
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.ToggleFullFloat
  ( toggleFullFloat
  , toggleFullFloatEwmhFullscreen
  )
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WithAll (killAll, killOthers, sinkAll, withAll, withAll')

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, checkDock, docks)
import XMonad.Hooks.ManageHelpers
  ( doCenterFloat
  , doFullFloat
  , doLower
  , doRectFloat
  , isDialog
  , isFullscreen
  )
import XMonad.Hooks.RefocusLast (refocusLastLogHook, toggleFocus)
import XMonad.Hooks.ShowWName (SWNConfig(..), showWNameLogHook)
import XMonad.Hooks.StatusBar (defToggleStrutsKey, statusBarProp, withEasySB)
import XMonad.Hooks.StatusBar.PP
  ( PP(..)
  , filterOutWsPP
  , ppSep
  , shorten
  , wrap
  , xmobarBorder
  , xmobarColor
  , xmobarRaw
  , xmobarStrip
  )
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHookExclude)

import XMonad.Actions.RotSlaves
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.Magnifier
import XMonad.Layout.Master
import XMonad.Layout.Maximize (maximizeRestore, maximizeWithPadding)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspaces)

-- import XMonad.Layout.Reflect (reflectHoriz) -- TODO:
import XMonad.Layout.Renamed (Rename(Replace), renamed)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed -- (Theme(..), shrinkText, tabbed)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.FuzzyMatch

-- import XMonad.Prompt.Layout
import XMonad.Prompt.Man
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad

import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Hacks
  ( javaHack
  , trayAbovePanelEventHook
  , trayPaddingEventHook
  , trayPaddingXmobarEventHook
  , trayerAboveXmobarEventHook
  , trayerPaddingXmobarEventHook
  , windowedFullscreenFixEventHook
  )
import XMonad.Util.Loggers (logTitles)
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce (spawnOnce)

main :: IO ()
main =
  xmonad
    . toggleFullFloatEwmhFullscreen
    . ewmhFullscreen
    . ewmh
    . withEasySB
        (statusBarProp
           ("xmobar " ++ myXmobarConfig)
           (copiesPP (xmobarColor colorBlue "" . wrap (":") ("")) myXmobarPP))
        defToggleStrutsKey
    . docks
    $ myConfig

myXmobarConfig :: String
myXmobarConfig = "~/.config/xmobar/xmobarrc"

myConfig =
  def
    { modMask = mod4Mask
    , layoutHook = myLayout
    , manageHook = myManageHook <> namedScratchpadManageHook myScratchpads
    , handleEventHook =
        windowedFullscreenFixEventHook
          <> trayerPaddingXmobarEventHook
    , startupHook = myStartupHook
    , workspaces = myWorkspaces
    , terminal = myTerminal
    , logHook =
        showWNameLogHook mySWNConfig
          >> refocusLastLogHook
          >> updatePointer (0.5, 0.7) (0, 0)
    , borderWidth = 4
    , normalBorderColor = colorNormal
    , focusedBorderColor = colorActive
    }
    `additionalKeysP` myKeys
    `removeKeysP` ["M-S-q"]

myTerminal :: String
myTerminal = "urxvtc"

-- Define browser variables
-- "~/.Zen-browser/zen-bin"
browser :: String
browser = "~/.Zen-browser/zen-bin"

privateBrowser :: String
privateBrowser = "firefox --private-window"

myWorkspaces = map show [1 .. 9 :: Int]

prompt :: XPConfig
prompt =
  def
    { fgColor = colorText
    , fgHLight = colorHText
    , bgColor = colorNormal
    , bgHLight = colorCyan
    , font = "xft:Iosevka:weight=bold:size=11.25:antialias=true"
    , alwaysHighlight = True
    , height = 25
    , position = Top
    , promptBorderWidth = 0
    , historySize = 50
    , historyFilter = deleteAllDuplicates
    , maxComplRows = Just 5 -- Max rows to show in completion window
        -- , maxComplColumns = Just 5
    , promptKeymap = emacsLikeXPKeymap
    , complCaseSensitivity = CaseInSensitive
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    , completionKey = (controlMask, xK_n)
    , prevCompletionKey = (controlMask, xK_p)
    }

myTabConfig =
  def
    { activeColor = colorActive
    , activeBorderColor = colorActive
    , activeTextColor = colorWhite
    , inactiveColor = colorNormal
    , inactiveBorderColor = colorNormal
    , inactiveTextColor = colorLowWhite
    , urgentColor = colorRed
    , urgentBorderColor = colorRed
    , urgentTextColor = colorWhite
    , decoHeight = 20
    , fontName = "xft:Iosevka:weight=bold:size=11.25:antialias=true"
    }

mySWNConfig =
  def
    { swn_font = "xft:Iosevka:weight=bold:size=31.25:antialias=true"
    , swn_bgcolor = colorActive
    , swn_color = "#ffffff"
    , swn_fade = 1
    }

myLayout =
  avoidStruts
    $ onWorkspaces ["8", "9"] (tabs ||| htiled)
    $ htiled ||| masterTabs ||| tabs ||| mtile
  where
    threeCol =
      setName "Threecol"
        $ smartBorders
        $ maximizeWithPadding 1
        $ magnifiercz' 1.3
        $ ThreeColMid nmaster delta ratio
    mtile = setName "Mtall" $ Mirror htiled
    tabs = setName "Tabs" $ smartBorders (tabbed shrinkText myTabConfig)
    masterTabs =
      setName "Tile Tabs"
        $ mastered (1 / 100) (1 / 2)
        $ smartBorders (tabbed shrinkText myTabConfig)
    htiled =
      setName "HTall"
        $ smartBorders
        $ maximizeWithPadding 1
        $ hiddenWindows
        $ mySpacing' 2
        $ ResizableTall nmaster delta ratio []
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

mySpacing ::
     Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' ::
     Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

setName :: String -> l a -> ModifiedLayout Rename l a
setName n = renamed [Replace n]

rTall :: Int -> Rational -> Rational -> ResizableTall l
rTall m r c = ResizableTall m r c []

viewShift :: WorkspaceId -> WindowSet -> WindowSet
viewShift i = W.greedyView i . W.shift i

toggleShiftDynamic :: [WorkspaceId] -> X ()
toggleShiftDynamic skips = do
  ws <- gets (W.workspaces . windowset)
  cur <- gets (W.currentTag . windowset)
  let hidden = filter (\w -> W.tag w `notElem` skips && W.tag w /= cur) ws
  case hidden of
    [] -> return ()
    (x:_) -> toggleOrDoSkip skips viewShift (W.tag x)

myScratchpads =
  [ NS
      "ncmpcpp"
      "urxvtc -name ncmpcpp -e ncmpcpp"
      (title =? "ncmpcpp")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS
      "Main"
      "urxvtc -name Main -e tmux new-session -A -s 'Main'"
      (title =? "Main" <&&> appName =? "Main" <&&> resource =? "Main")
      (doRectFloat $ W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4))
  ]

myKeys :: [(String, X ())]
myKeys =
  [ ( "M-S-r"
    , spawn
        "xmonad --recompile && xmonad --restart && \
       \dunstify -a ignore -i '/home/ukiran/.config/xmonad/icons/new_xmonad.png' 'Reloaded Xmonad'")
  , ("M-C-r", spawn "xmonad-restart.sh")
  , ( "M-C-S-<Escape>"
    , confirmPrompt prompt "exit Xmonad" $ io (exitWith ExitSuccess))
  , ("M-<F9>", spawn browser)
  , ("M-S-<F9>", spawn privateBrowser)
  , ("M-p m", manPrompt prompt)
  , ("M-p p", spawn "rofi-pass")
  , ("M-a", spawn "rofi -show drun -show-icons")
  , ("M-w", spawn "rofi -show window -show-icons")
  , ("M-s", spawn "rofi -show run")
  , ("M-e", spawn "emacsclient -c -a 'emacs'")
  , ("M-<Return>", spawn "urxvtc -e tmux new-session -A -s 'Main'")
  , ("M-<Home>", namedScratchpadAction myScratchpads "ncmpcpp")
  , ("M-<End>", namedScratchpadAction myScratchpads "Main")
  , ("M-<Up>", rotSlavesUp)
  , ("M-<Down>", rotSlavesDown)
  ]
    ++ [ ("M-<Right>", nextScreen)
       , ("M-<Left>", prevScreen)
       , ("M-S-<Right>", shiftNextScreen)
       , ("M-S-<Left>", shiftPrevScreen)
       , ("M-.", moveTo Next nonNSP)
       , ("M-,", moveTo Prev nonNSP)
       , ("M-S-.", shiftTo Next nonNSP >> moveTo Next nonNSP)
       , ("M-S-,", shiftTo Prev nonNSP >> moveTo Prev nonNSP)
       , ("M-M1-.", shiftTo Next nonNSP)
       , ("M-M1-,", shiftTo Prev nonNSP)
       , ( "M-C-'"
         , moveTo Next
             $ hiddenWS :&: emptyWS :&: ignoringWSs [scratchpadWorkspaceTag])
       , ( "M-C-;"
         , moveTo Prev
             $ hiddenWS :&: emptyWS :&: ignoringWSs [scratchpadWorkspaceTag])
       , ("M-<Backspace>", toggleWS' ["NSP"])
       , ("M-S-<Backspace>", toggleShiftDynamic ["NSP"])
       , ( "M-'"
         , moveTo Next
             $ hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag])
       , ( "M-;"
         , moveTo Prev
             $ hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag])
       , ( "M-S-'"
         , shiftTo Next
             $ hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag])
       , ( "M-S-;"
         , shiftTo Prev
             $ hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag])
       -- , ("M-n", refresh) -- What it does?
       , ("M-l", sendMessage Expand)
       , ("M-h", sendMessage Shrink)
       , ("M-S-l", sendMessage MirrorShrink)
       , ("M-S-h", sendMessage MirrorExpand)
       , ("M-t", withFocused toggleFloat)
       , ("M-S-f", withFocused toggleFullFloat)
       , ("M-C-d", killOthers)
       , ("M-S-d", killAll)
       , ( "M-o o"
         , selectWindow
             def
               { txtCol = "Green"
               , cancelKey = xK_Escape
               , emFont = "xft:Iosevka:weight=bold:pixelsize=40:antialias=true"
               , overlayF = textSize
               }
             >>= (`whenJust` windows . W.focusWindow))
       , ( "M-o M-o"
         , selectWindow
             def
               { txtCol = "Green"
               , cancelKey = xK_Escape
               , emFont = "xft:Iosevka:weight=bold:pixelsize=40:antialias=true"
               , overlayF = textSize
               }
             >>= (`whenJust` windows . W.focusWindow))
       , ( "M-o x"
         , selectWindow
             def
               { txtCol = "Red"
               , cancelKey = xK_Escape
               , emFont = "xft:Iosevka:weight=bold:pixelsize=40:antialias=true"
               , overlayF = textSize
               }
             >>= (`whenJust` killWindow))
       , ( "M-o M-x"
         , selectWindow
             def
               { txtCol = "Red"
               , cancelKey = xK_Escape
               , emFont = "xft:Iosevka:weight=bold:pixelsize=40:antialias=true"
               , overlayF = textSize
               }
             >>= (`whenJust` killWindow))
       , ("M-q", kill1)
       , ("M-C-q", kill)
       , ("M-i", withFocused hideWindow)
       , ("M-S-i", popOldestHiddenWindow)
       , ("M-f", withFocused (sendMessage . maximizeRestore))
       , ("M-<Tab>", windows W.focusDown)
       , ("M-S-<Tab>", windows W.focusUp)
       , ("M-m", windows W.focusMaster)
       , ("M-S-m", windows W.swapMaster)
       , ("M-C-m", windows W.shiftMaster)
       , ("M-S-j", windows W.swapDown)
       , ("M-S-k", windows W.swapUp)
       ]
    ++ [ ("M-" ++ m ++ k, windows $ f i)
       | (i, k) <- zip (myWorkspaces) (map show [1 :: Int ..])
       , (f, m) <-
           [ (W.view, "")
           , (W.shift, "S-")
           , (viewShift, "C-")
           , (copy, "c ")
           , (swapWithCurrent, "M1-")
           ]
       ]
    ++ [("M-S-w", windowPrompt prompt BringCopy allWindows)]
    ++ [ ("M-c a", windows copyToAll)
       , ("M-c x", killAllOtherCopies)
       , ("M-c q", kill1)
       ]
    ++ [ ("M-S-[", decWindowSpacing 2)
       , ("M-S-]", incWindowSpacing 2)
       , ("M-C-<Space>", toggleWindowSpacingEnabled)
       ]
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))
    toggleFloat w =
      windows
        (\s ->
           if M.member w (W.floating s)
             then W.sink w s
             else (W.float w (W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)) s))

myXmobarPP :: PP
myXmobarPP =
  filterOutWsPP [scratchpadWorkspaceTag]
    $ def
        { ppSep = slategray " | "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent = tagActive . wrap " " " "
        , ppVisible = yellow . wrap ("(") (")")
        , ppHidden = wrap "+" ""
        , ppHiddenNoWindows = gray . const ""
        , ppUrgent = tagUrgent . wrap " " " "
        , ppLayout = wrap " " " " . lowWhite
        , ppOrder = \(ws:l:_:ex) -> [ws, l] ++ ex
        , ppExtras = [logTitles formatFocused formatUnfocused]
        }
  where
    formatFocused = tagActive . wrap " " " " . ppWindow
    formatUnfocused =
      xmobarColor colorBlue colorSlateGray . wrap " " " " . ppWindow
    ppWindow :: String -> String
    ppWindow =
      xmobarRaw
        . (\w ->
             if null w
               then "untitled"
               else w)
        . shorten 30
    blue, yellow, red, lowWhite, cyan, slategray, gray :: String -> String
    tagActive = xmobarColor colorWhite colorBlue
    tagUrgent = xmobarColor colorWhite colorWhite
    tagHidden = xmobarColor colorBlack colorGray
    blue = xmobarColor colorBlue ""
    yellow = xmobarColor colorYellow ""
    red = xmobarColor colorRed ""
    lowWhite = xmobarColor colorLowWhite ""
    cyan = xmobarColor colorCyan ""
    slategray = xmobarColor colorSlateGray ""
    gray = xmobarColor colorGray ""

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat
    , title
        =? "World"
        --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
    , title
        =? "tinee"
        --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
    , title =? "EEverywhere" --> viewShiftHook (myWorkspaces !! 8)
    , className
        =? "mpv"
        --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
    , className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat
    , resource =? "desktop_window" --> doIgnore
    , className =? "TelegramDesktop" --> viewShiftHook (myWorkspaces !! 8)
    , className =? "Tor Browser" --> viewShiftHook (myWorkspaces !! 8)
    , className =? "calibre" --> viewShiftHook (myWorkspaces !! 8)
    , className =? "qBittorrent" --> viewShiftHook (myWorkspaces !! 8)
    , className =? "Liferea" --> viewShiftHook (myWorkspaces !! 7)
    , className =? "Thunderbird" --> viewShiftHook (myWorkspaces !! 7)
    , className =? "Blender" --> viewShiftHook (myWorkspaces !! 7)
    , className =? "Inkscape" --> viewShiftHook (myWorkspaces !! 7)
    , className
        =? "qBittorrent"
        <&&> title
        =? "Open Torrent Files"
        --> doRectFloat (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
    , isDialog --> doCenterFloat
    , isFullscreen --> doFullFloat
    , checkDock --> doLower
    ]
  where
    viewShiftHook = doF . liftM2 (.) W.greedyView W.shift

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "urxvtd -q &"
  spawnOnce "bash $HOME/.config/startup/xmonad_startup"
  spawnOnce "~/.fehbg &"
  spawnOnce "dunst &"
  spawnOnce "greenclip daemon &"
  spawnOnce "thunar --daemon &"
  spawnOnce "nmcli networking off &"
  spawnOnce "numlockx on &"
  spawnOnce "xmodmap $XDG_CONFIG_HOME/X11/xmodmap"
  spawnOnce "xfce4-power-manager &"
  spawnOnce "/usr/bin/pipewire-pulse &"
  spawnOnce "/usr/bin/pipewire &"
  spawnOnce "/usr/libexec/polkit-gnome-authentication-agent-1 &"
  spawnOnce "sxhkd -c $HOME/.config/sxhkd/xmonad_sxhkdrc"
  spawnOnce
    "trayer --edge top --align right --widthtype request --SetDockType true \
   \--SetPartialStrut true --expand true --monitor 1 --transparent true \
   \--alpha 0 --tint 0x171d23 --height 21 &"
  spawnOnce
    "{ xrandr | grep 'HDMI-1-0 connected' >/dev/null \
   \&& xrandr --output HDMI-1-0 --auto --primary \
   \&& xrandr --output eDP-1 --off ;} \
   \|| { xrandr --output eDP-1 --auto --primary \
   \&& xrandr --output HDMI-1-0 --off ;}"
