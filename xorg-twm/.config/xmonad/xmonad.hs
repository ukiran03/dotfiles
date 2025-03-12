-- import XMonad.Layout.WindowNavigation
-- import XMonad.Actions.WindowMenu
-- import XMonad.Actions.ShowText
-- import XMonad.Actions.SimpleDate
-- import XMonad.Hooks.DynamicLog NOTE:
-- NOTE:
-- import XMonad.Layout.Magnifier
-- import XMonad.Layout.ShowWName (showWName)
-- import qualified XMonad.Layout.BoringWindows (boringWindows, swapUp, swapDown, focusMaster, focusUp, focusDown, boringAuto)
-- import XMonad.Layout.LayoutModifier
-- import XMonad.Layout.Simplest
-- TODO:
-- import XMonad.Layout.ThreeColumns
-- import XMonad.Layout.DecorationMadness
-- import XMonad.Layout.SimpleDecoration
-- import XMonad.Layout.NoFrillsDecoration
import Colors.CityLights
import Control.Monad (liftM2)
import Data.Ratio ((%))
import XMonad
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
  , nextWS
  , prevWS
  , shiftTo
  , shiftToNext
  , shiftToPrev
  , toggleWS'
  )
import XMonad.Actions.EasyMotion (EasyMotionConfig(..), selectWindow, textSize)
import XMonad.Actions.Minimize
  ( maximizeWindowAndFocus
  , minimizeWindow
  , withLastMinimized
  )
import XMonad.Actions.ToggleFullFloat
  ( toggleFullFloat
  , toggleFullFloatEwmhFullscreen
  )
import XMonad.Actions.WithAll (killAll, killOthers, sinkAll)
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
  , xmobarColor
  , xmobarRaw
  , xmobarStrip
  )
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Maximize (maximizeRestore, maximizeWithPadding)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed (Rename(Replace), renamed)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Layout.Tabbed (Theme(..), shrinkText, tabbed)
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
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

-- import XMonad.Util.WorkspaceCompare
main :: IO ()
main =
  xmonad
    . toggleFullFloatEwmhFullscreen
    . ewmhFullscreen
    . ewmh
    . withEasySB
        (statusBarProp
           "xmobar ~/.config/xmobar/xmobarrc"
           (copiesPP (xmobarColor colorBlue "" . wrap (":") ("")) myXmobarPP)
          -- (pure myXmobarPP)
         )
        defToggleStrutsKey
    . docks
    $ myConfig

myConfig =
  def
    { modMask = mod4Mask
    , layoutHook = myLayout
    , manageHook = myManageHook
    , handleEventHook =
        windowedFullscreenFixEventHook <> trayerPaddingXmobarEventHook
    , startupHook = myStartupHook
    , workspaces = myWorkspaces
    , terminal = myTerminal
    , logHook = showWNameLogHook mySWNConfig
    , borderWidth = 3
    , normalBorderColor = colorNormal
    , focusedBorderColor = colorActive
    }
    `additionalKeysP` myKeys
    `removeKeysP` ["M-S-q", "M-p"]

-- | Switch to a certain layout.
switchToLayout :: String -> X ()
switchToLayout = sendMessage . JumpToLayout

-- | Define your viewShift function (fixed)
viewShift :: WorkspaceId -> WindowSet -> WindowSet
viewShift i = W.greedyView i . W.shift i

myKeys :: [(String, X ())]
myKeys =
  [ ( "M-S-r"
    , spawn
        "xmonad --recompile && xmonad --restart && dunstify -a ignore -i '/home/ukiran/.config/xmonad/icons/new_xmonad.png' 'Reloaded Xmonad'")
  , ("M-C-r", spawn "xmonad --restart")
  , ("M-a", spawn "rofi -show drun -show-icons")
  , ("M-w", spawn "rofi -show window -show-icons")
  , ("M-s", spawn "rofi -show run")
  , ("M-e", spawn "emacsclient -c -a 'emacs'")
  , ("M-<Return>", spawn "urxvtc -e tmux new-session -A -s 'Main'")
  , ("M-<Home>", namedScratchpadAction myScratchpads "htop")
  , ("M-<End>", namedScratchpadAction myScratchpads "scratch")
  ]
    ++ [ ("M-.", moveTo Next nonNSP) -- nextWS
       , ("M-,", moveTo Prev nonNSP) -- prevWS
       , ("M-S-.", shiftTo Next nonNSP >> moveTo Next nonNSP)
       , ("M-S-,", shiftTo Prev nonNSP >> moveTo Prev nonNSP)
       , ("M-C-.", shiftTo Next nonNSP)
       , ("M-C-,", shiftTo Prev nonNSP)
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
       , ("M-l", sendMessage Shrink) -- %! Shrink the master area
       , ("M-h", sendMessage Expand) -- %! Expand the master area
       , ("M-t", withFocused $ windows . W.sink)
       , ("M-S-t", sinkAll)
       , ("M-S-f", withFocused toggleFullFloat) -- fullScreen the win
       , ("M-S-x", killAll) -- kill all wins
         -- checkout withAll
       , ("M-C-d", killOthers) -- kill all wins but focused
         -- ("M-S-y", workspacePrompt def (windows . W.shift)),
         -- ("M-S-y" , windowPrompt def { autoComplete = Just 500000 }
         --                               Goto allApplications),
       , ("M-S-w", windowPrompt prompt BringCopy allWindows) -- brings a copy of window
         -- EasyMotion
       , ( "M-o"
         , selectWindow
             def
               { txtCol = "Green"
               , cancelKey = xK_Escape
               , emFont = "xft:Iosevka:weight=bold:pixelsize=40:antialias=true"
               , overlayF = textSize
               }
             >>= (`whenJust` windows . W.focusWindow))
       , ( "M-x"
         , selectWindow
             def
               { txtCol = "Red"
               , cancelKey = xK_Escape
               , emFont = "xft:Iosevka:weight=bold:pixelsize=40:antialias=true"
               , overlayF = textSize
               }
             >>= (`whenJust` killWindow))
         -- clients
       , ("M-q", kill1)
       , ("M-C-q", kill)
       , ("M-i", withFocused minimizeWindow)
       , ("M-S-i", withLastMinimized maximizeWindowAndFocus)
       , ("M-f", withFocused (sendMessage . maximizeRestore))
       , ("M-j", sendMessage MirrorExpand)
       , ("M-k", sendMessage MirrorShrink)
       , ("M-<Tab>", BW.focusDown)
       , ("M-S-<Tab>", BW.focusUp)
       , ("M-g", windows W.focusDown)
       , ("M-S-g", windows W.focusUp)
       , ("M-m", BW.focusMaster)
       , ("M-S-m", windows W.swapMaster)
       , ("M-S-j", BW.swapDown)
       , ("M-S-k", BW.swapUp)
       ]
    ++ [ ("M-" ++ m ++ k, windows $ f i)
       | (i, k) <- zip (myWorkspaces) (map show [1 :: Int ..])
       , (f, m) <-
           [(W.view, ""), (W.shift, "S-"), (viewShift, "C-"), (copy, "c ")]
       ]
    ++ [ ("M-c a", windows copyToAll)
       , ("M-c x", killAllOtherCopies)
       , ("M-c q", kill1)
       ]
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

-- where
prompt :: XPConfig
prompt =
  def
    { fgColor = colorWhite
    , fgHLight = colorBlack
    , bgColor = colorNormal
    , bgHLight = colorCyan
    , font = "xft:Iosevka:weight=bold:pixelsize=13.25:antialias=true"
    , alwaysHighlight = True -- Current best match
    , height = 25
    , position = Top
    , promptBorderWidth = 0 -- Fit in with rest of config
    , historySize = 50
    , historyFilter = deleteAllDuplicates
    , maxComplRows = Just 5 -- Max rows to show in completion window
    , promptKeymap = emacsLikeXPKeymap
      -- , searchPredicate   = fuzzyMatch
      -- , sorter            = fuzzySort
      -- , completionKey     = (0, xK_Right, xK_Tab)
      -- , prevCompletionKey = (0, xK_Left)
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
    { swn_font = "xft:Iosevka:weight=bold:pixelsize=30:antialias=true"
    , swn_bgcolor = colorActive
    , swn_color = colorWhite
    , swn_fade = 1
    }

myScratchpads
    -- run htop in xterm, find it by title, use default floating window placement
 =
  [ NS
      "htop"
      "xterm -e htop"
      (title =? "htop")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS
      "scratch"
      "urxvtc -name scratch"
      (title =? "scratch")
      (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  ]

myLayout =
  avoidStruts
    $ BW.boringWindows
    $
      -- BW.boringAuto $ -- check what it does NOTE:
     onWorkspaces ["8", "9"] (tab ||| tiled)
    $ tiled ||| tab ||| mtile
  where
    -- threeCol =
    --   smartBorders $
    --     renamed [Replace "threecol"] $
    --       maximizeWithPadding 1 $
    --         minimize $
    --           magnifiercz' 1.3 $
    --             ThreeColMid nmaster delta ratio
    -- full = smartBorders $ renamed [Replace "Full"] $ minimize $ Full -- Not Compatible with BW
    -- full = smartBorders $ renamed [Replace "full"] $ minimize $ Simplest -- Better Option
    mtile = renamed [Replace "mtall"] $ Mirror tiled
    tab =
      smartBorders
        $ renamed [Replace "tabs"]
        $ noBorders (tabbed shrinkText myTabConfig)
    tiled =
      smartBorders
        $ renamed [Replace "tall"]
        $ reflectHoriz
        $ maximizeWithPadding 1
        $ minimize
        $ smartSpacing 2
        $ ResizableTall nmaster delta ratio []
    twoPane = TwoPane (3 / 100) (1 / 2)
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces = map show [1 .. 9 :: Int]

windowCount :: X (Maybe String)
windowCount =
  gets
    $ Just
        . show
        . length
        . W.integrate'
        . W.stack
        . W.workspace
        . W.current
        . windowset

myXmobarPP :: PP
myXmobarPP =
  filterOutWsPP [scratchpadWorkspaceTag]
    $ def
        { ppSep = slategray " | "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent = cyan . wrap ("[") ("]")
        , ppHidden = lowWhite . wrap "+" ""
        , ppHiddenNoWindows = slategray . wrap " " ""
        , ppUrgent = red . wrap (yellow "!") (yellow "!")
        , ppLayout = wrap " " " " . lowWhite
        , ppOrder = \(ws:l:_:ex) -> [ws, l] ++ ex
        , ppExtras =
            [formattedWindowCount, logTitles formatFocused formatUnfocused]
        }
  where
    formatFocused = wrap (blue "(") (blue ")") . blue . ppWindow
    formatUnfocused = wrap (lowWhite "") (lowWhite "") . lowWhite . ppWindow
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
    blue, yellow, red, lowWhite, cyan, slategray :: String -> String
    blue = xmobarColor colorBlue ""
    yellow = xmobarColor colorYellow ""
    red = xmobarColor colorRed ""
    lowWhite = xmobarColor colorLowWhite ""
    cyan = xmobarColor colorCyan ""
    slategray = xmobarColor colorGray ""

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
    , className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat -- Firefox pip
    , resource =? "desktop_window" --> doIgnore
    , className =? "TelegramDesktop" --> viewShift (myWorkspaces !! 8)
    , className =? "Tor Browser" --> viewShift (myWorkspaces !! 8)
    , className =? "qBittorrent" --> viewShift (myWorkspaces !! 8)
    , className =? "calibre" --> viewShift (myWorkspaces !! 8)
    , className =? "Liferea" --> viewShift (myWorkspaces !! 7)
    , className =? "Thunderbird" --> viewShift (myWorkspaces !! 7)
    , isDialog --> doCenterFloat
    , isFullscreen --> doFullFloat
    , checkDock --> doLower
    , namedScratchpadManageHook myScratchpads
    ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "urxvtd -q &"
  spawnOnce "bash $HOME/.config/startup/xmonad_startup"
  spawnOnce "~/.fehbg &"
  spawnOnce "dunst &"
  spawnOnce "greenclip daemon &"
  spawnOnce "thunar --daemon &"
  spawnOnce "nm-applet &"
  spawnOnce "nmcli networking off &"
  spawnOnce "numlockx on &"
  spawnOnce "xmodmap $XDG_CONFIG_HOME/X11/xmodmap"
  spawnOnce "xfce4-power-manager &"
  spawnOnce "/usr/bin/pipewire-pulse &"
  spawnOnce "/usr/bin/pipewire &"
  spawnOnce "/usr/libexec/polkit-gnome-authentication-agent-1 &"
  spawnOnce
    "trayer --edge top --align right --widthtype request --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x171d23 --height 21 &"
  spawnOnce
    "{ xrandr | grep 'HDMI-1-0 connected' >/dev/null && xrandr --output HDMI-1-0 --auto --primary && xrandr --output eDP-1 --off ;} || { xrandr --output eDP-1 --auto --primary && xrandr --output HDMI-1-0 --off ;}"
  spawnOnce "sxhkd -c $HOME/.config/sxhkd/xmonad_sxhkdrc"
