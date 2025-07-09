-- -*- compile-command: "xmonad --recompile" -*-
import Colors.Current
import Control.Monad -- (liftM2, unless)
import qualified Data.Map as M
import Data.Ratio ((%))
import Doc.Help
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
  , toggleWS'
  )
import XMonad.Actions.CycleWindows
import XMonad.Actions.EasyMotion (EasyMotionConfig(..), selectWindow, textSize)
import XMonad.Actions.Minimize
  ( maximizeWindowAndFocus
  , minimizeWindow
  , withLastMinimized
  , withMinimized
  )
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

import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.Magnifier
import XMonad.Layout.Master
import XMonad.Layout.Maximize (maximizeRestore, maximizeWithPadding)
import XMonad.Layout.Minimize (minimize)
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

-- ;;
--  focusChangeHook
-- ;;
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
        windowedFullscreenFixEventHook <> trayerPaddingXmobarEventHook
    , startupHook = myStartupHook
    , workspaces = myWorkspaces
    , terminal = myTerminal
    , logHook =
        showWNameLogHook mySWNConfig
                -- >> focusChangeHook
          >> updatePointer (0.5, 0.7) (0, 0)
    , borderWidth = 4
    , normalBorderColor = colorNormal
    , focusedBorderColor = colorActive
    }
    `additionalKeysP` myKeys
    `removeKeysP` ["M-S-q"]

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces = map show [1 .. 9 :: Int]

prompt :: XPConfig
prompt =
  def
    { fgColor = colorText
    , fgHLight = colorHText
    , bgColor = colorNormal
    , bgHLight = colorCyan
    , font = "xft:Iosevka:weight=bold:pixelsize=13.25:antialias=true"
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
    , decoHeight = 17
    , fontName = "xft:Iosevka:weight=bold:pixelsize=13.25:antialias=true"
    }

mySWNConfig =
  def
    { swn_font = "xft:Iosevka:weight=bold:pixelsize=37.25:antialias=true"
    , swn_bgcolor = colorActive
    , swn_color = "#ffffff"
    , swn_fade = 1
    }

myLayout =
  avoidStruts
    $ BW.boringWindows
    $ onWorkspaces ["8", "9"] (tabs ||| tiled)
    $ tiled ||| masterTabs ||| tabs ||| mtile
  where
    threeCol =
      setName "Threecol"
        $ smartBorders
        $ maximizeWithPadding 1
        $ minimize
        $ magnifiercz' 1.3
        $ ThreeColMid nmaster delta ratio
    mtile = setName "Mtall" $ Mirror tiled
    tabs = setName "Tabs" $ smartBorders (tabbed shrinkText myTabConfig)
    masterTabs =
      setName "Tile Tabs"
        $ mastered (1 / 100) (1 / 2)
        $ smartBorders (tabbed shrinkText myTabConfig)
    tiled =
      setName "Tall"
        $ smartBorders
        $ maximizeWithPadding 1
        $ minimize
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

switchToLayout :: String -> X ()
switchToLayout = sendMessage . JumpToLayout

-- TODO:
-- data MyLayoutPrompt = MyLayoutPrompt String
-- instance XPrompt MyLayoutPrompt where
--     showXPrompt (MyLayoutPrompt s) = s ++ "> "
-- layoutPrompt :: XPConfig -> X ()
-- layoutPrompt c = do
--     let ls = ["ThreeCol", "mtile", "tab", "tiled"]
--     mkXPrompt (MyLayoutPrompt "Layout") c
--              (mkComplFunFromList' c ls)
--              (sendMessage . JumpToLayout)
-- | Define your viewShift function (fixed)
viewShift :: WorkspaceId -> WindowSet -> WindowSet
viewShift i = W.greedyView i . W.shift i

maximizeFocusedOrLastMinimized :: X ()
maximizeFocusedOrLastMinimized =
  withFocused $ \w -> do
    withMinimized $ \minimized ->
      if w `elem` minimized
        then maximizeWindowAndFocus w
        else withLastMinimized maximizeWindowAndFocus

windowCount :: X (Maybe String)
windowCount = do
  ws <- gets windowset
  let currentWindows = W.integrate' (W.stack (W.workspace (W.current ws)))
  minimizedWindows <- withMinimized return
  let totalCount = length currentWindows
      minimizedCount = length (filter (`elem` currentWindows) minimizedWindows)
  return
    $ Just
    $ if minimizedCount > 0
        then show (totalCount - minimizedCount) ++ "+" ++ show minimizedCount
        else show totalCount

myScratchpads =
  [ NS
      "htop"
      "xterm -e htop"
      (title =? "htop")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS
      "Main"
      "urxvtc -name Main -e tmux new-session -A -s 'Main'"
      (title =? "Main" <&&> appName =? "Main" <&&> resource =? "Main")
      (doRectFloat $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
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
  , ("M-S-/", xmessage myHelp)
  -- , ("M-p l", layoutPrompt prompt)
  -- , ("M-C-p", switchToLayout "threeCol")
  , ("M-<F9>", spawn "~/.Zen-browser/zen-bin")
  , ("M-S-<F9>", spawn "~/.Zen-browser/zen-bin --private-window")
  -- , ("M-S-<F9>", spawnOn "6" "firefox")
  , ("M-p m", manPrompt prompt)
  , ("M-p p", spawn "rofi-pass")
  , ("M-a", spawn "rofi -show drun -show-icons")
  , ("M-w", spawn "rofi -show window -show-icons")
  , ("M-s", spawn "rofi -show run")
  , ("M-e", spawn "emacsclient -c -a 'emacs'")
  , ("M-S-e", spawn "emacsclient --eval '(emacs-everywhere)'")
  , ("M-<Return>", spawn "urxvtc -e tmux new-session -A -s 'Main'")
  , ("M-<Home>", namedScratchpadAction myScratchpads "htop")
  , ("M-<End>", namedScratchpadAction myScratchpads "Main")
  ]
    ++ [ ("M-<Right>", nextScreen)
       -- , ("M-<Left>", spawn "notify-send 'Window Focus' '" ++ show name ++ "'")
       -- , ("M-<Left>", prevScreen)
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
       , ("M-n", refresh) -- What it does?
       , ("M-l", sendMessage Expand)
       , ("M-h", sendMessage Shrink)
       , ("M-S-l", sendMessage MirrorShrink)
       , ("M-S-h", sendMessage MirrorExpand)
       , ("M-t", withFocused toggleFloat)
       , ("M-S-f", withFocused toggleFullFloat)
       , ("M-C-d", killOthers)
       , ("M-S-d", killAll)
         -- ("M-S-y", workspacePrompt def (windows . W.shift)),
         -- ("M-S-y" , windowPrompt def { autoComplete = Just 500000 }
         --                               Goto allApplications),
         -- EasyMotion
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
         -- clients
       , ("M-q", kill')
       , ("M-C-q", kill)
       , ("M-i", withFocused minimizeWindow)
       , ("M-S-i", maximizeFocusedOrLastMinimized)
       , ("M-f", withFocused (sendMessage . maximizeRestore))
       -- , ("M-j", sendMessage MirrorExpand)
       -- , ("M-k", sendMessage MirrorShrink)
       , ("M-<Tab>", BW.focusDown)
       , ("M-S-<Tab>", BW.focusUp)
       -- , ("M-g", W.focusDown')
       -- , ("M-S-g", W.focusUp')
       , ("M-m", BW.focusMaster)
       , ("M-S-m", windows W.swapMaster)
       , ("M-C-m", windows W.shiftMaster)
       , ("M-S-j", BW.swapDown)
       , ("M-S-k", BW.swapUp)
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
             else (W.float w (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)) s))

-- | Kills focused window unless minimized. For Copied windows, kills
-- only the Copy. Preserves minimized windows.
kill' :: X ()
kill' =
  withFocused $ \window -> do
    minimized <- withMinimized return
    unless (window `elem` minimized) kill1

myXmobarPP :: PP
myXmobarPP =
  filterOutWsPP [scratchpadWorkspaceTag]
    $ def
        { ppSep = slategray " | "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent = tagActive . wrap " " " "
        , ppVisible = yellow . wrap ("(") (")")
        , ppHidden = wrap "+" ""
        , ppHiddenNoWindows = gray . const " -"
        -- , ppHiddenNoWindows = gray . wrap " " ""
        , ppUrgent = tagUrgent . wrap " " " "
        , ppLayout = wrap " " " " . lowWhite
        , ppOrder = \(ws:l:_:ex) -> [ws, l] ++ ex
        , ppExtras =
            [formattedWindowCount, logTitles formatFocused formatUnfocused]
        }
  where
    formatFocused = tagActive . wrap " " " " . ppWindow
    formatUnfocused =
      xmobarColor colorBlue colorSlateGray . wrap " " " " . ppWindow
    formattedWindowCount :: X (Maybe String)
    formattedWindowCount = fmap (fmap $ lowWhite) windowCount
    ppWindow :: String -> String
    ppWindow =
      xmobarRaw
        . (\w ->
             if null w
               then "untitled"
               else w)
        . shorten 25
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
    , className
        =? "mpv"
        --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
    -- TODO: , className =? "Emacs" <&&> title =? "Emacs Everywhere" --> doShift (myWorkspaces !! 8)
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
