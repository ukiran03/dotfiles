import Colors.Current
import Control.Monad (liftM2)
import qualified Data.Map as M
import Data.Ratio ((%))
import Doc.Help
import System.Exit
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
import XMonad.Actions.EasyMotion (EasyMotionConfig(..), selectWindow, textSize)
import XMonad.Actions.Minimize
  ( maximizeWindowAndFocus
  , minimizeWindow
  , withLastMinimized
  , withMinimized
  )
import XMonad.Actions.ToggleFullFloat
  ( toggleFullFloat
  , toggleFullFloatEwmhFullscreen
  )
import XMonad.Actions.UpdatePointer
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
  , xmobarBorder
  , xmobarColor
  , xmobarRaw
  , xmobarStrip
  )
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize (maximizeRestore, maximizeWithPadding)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed (Rename(Replace), renamed)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed (Theme(..), shrinkText, tabbed)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Layout
import XMonad.Prompt.Man
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
    , manageHook = myManageHook
    , handleEventHook =
        windowedFullscreenFixEventHook <> trayerPaddingXmobarEventHook
    , startupHook = myStartupHook
    , workspaces = myWorkspaces
    , terminal = myTerminal
    , logHook = showWNameLogHook mySWNConfig >> updatePointer (0.5, 0.7) (0, 0)
    , borderWidth = 3
    , normalBorderColor = colorNormal
    , focusedBorderColor = colorActive
    }
    `additionalKeysP` myKeys
    `removeKeysP` ["M-S-q"]

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
  , ("M-C-r", spawn "xmonad-restart.sh")
  , ( "M-C-S-<Escape>"
    , confirmPrompt prompt "exit Xmonad" $ io (exitWith ExitSuccess))
  , ("M-S-/", xmessage myHelp)
  , ("M-p l", layoutPrompt prompt)
  , ("M-p m", manPrompt prompt)
  , ("M-a", spawn "rofi -show drun -show-icons")
  , ("M-w", spawn "rofi -show window -show-icons")
  , ("M-s", spawn "rofi -show run")
  , ("M-e", spawn "emacsclient -c -a 'emacs'")
  , ("M-<Return>", spawn "urxvtc -e tmux new-session -A -s 'Main'")
  , ("M-<Home>", namedScratchpadAction myScratchpads "htop")
  , ("M-<End>", namedScratchpadAction myScratchpads "Main")
  ]
    ++ [ ("M-<Right>", nextScreen)
       , ("M-<Left>", prevScreen)
       , ("M-S-<Right>", shiftNextScreen)
       , ("M-S-<Left>", shiftPrevScreen)
       , ("M-.", moveTo Next nonNSP) -- nextWS
       , ("M-,", moveTo Prev nonNSP) -- prevWS
         -- Shift & Move to n/p WS
       , ("M-S-.", shiftTo Next nonNSP >> moveTo Next nonNSP)
       , ("M-S-,", shiftTo Prev nonNSP >> moveTo Prev nonNSP)
         -- Shift to n/p WS
       , ("M-M1-.", shiftTo Next nonNSP)
       , ("M-M1-,", shiftTo Prev nonNSP)
         -- Move to n/p empty WS
       , ( "M-C-."
         , moveTo Next
             $ hiddenWS :&: emptyWS :&: ignoringWSs [scratchpadWorkspaceTag])
       , ( "M-C-,"
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
       , ("M-l", sendMessage Shrink) -- %! Shrink the master area
       , ("M-h", sendMessage Expand) -- %! Expand the master area
       , ("M-t", withFocused $ windows . W.sink)
       , ("M-C-t", withFocused toggleFloat)
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
       , ("M-q", kill1)
       , ("M-C-q", kill)
       , ("M-i", withFocused minimizeWindow)
       , ("M-S-i", maximizeFocusedOrLastMinimized)
       , ("M-f", withFocused (sendMessage . maximizeRestore))
       -- , ("M-j", sendMessage MirrorExpand)
       -- , ("M-k", sendMessage MirrorShrink)
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
    ++ [("M-p w", windowPrompt prompt BringCopy allWindows)]
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

maximizeFocusedOrLastMinimized :: X ()
maximizeFocusedOrLastMinimized =
  withFocused $ \w -> do
    withMinimized $ \minimized ->
      if w `elem` minimized
        then maximizeWindowAndFocus w
        else withLastMinimized maximizeWindowAndFocus

prompt :: XPConfig
prompt =
  def
    { fgColor = colorText
    , fgHLight = colorHText
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
    , swn_color = "#ffffff"
    , swn_fade = 1
    }

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

myLayout =
  avoidStruts
    $ BW.boringWindows
    -- BW.boringAuto $ -- check what it does NOTE:
    $ onWorkspaces ["8", "9"] (tab ||| tiled)
    $ tiled ||| tab ||| mtile
  where
    threeCol =
      setName "Threecol"
        $ smartBorders
        $ maximizeWithPadding 1
        $ minimize
        $ magnifiercz' 1.3
        $ ThreeColMid nmaster delta ratio
    -- full = smartBorders $ renamed [Replace "Full"] $ minimize $ Full -- Not Compatible with BW
    -- full = smartBorders $ renamed [Replace "full"] $ minimize $ Simplest -- Better Option
    mtile = setName "Mtall" $ Mirror tiled
    tab = setName "Tabs" $ smartBorders (tabbed shrinkText myTabConfig)
    hacking =
      setName "Hacking"
        $ reflectHoriz . limitWindows 3 . magnify 1.3 (NoMaster 3) True
        $ rTall 1 (3 % 100) (13 % 25)
    tiled =
      setName "Tall"
        $ smartBorders
        $ reflectHoriz
        $ maximizeWithPadding 1
        $ minimize
              -- \$ smartSpacing 2
        $ mySpacing' 2
                -- \$ magnifiercz' 1.2
        $ ResizableTall nmaster delta ratio []
    -- ntile = simpleDeco shrinkText mySDConfig (ResizableTall nmaster delta ratio [])
    -- ntile = noFrillsDeco shrinkText mySDConfig (ResizableTall nmaster delta ratio [])
    twoPane = setName "TwoPane" $ reflectHoriz $ TwoPane (3 / 100) (1 / 2)
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

-- Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
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

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces = map show [1 .. 9 :: Int]

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

myXmobarPP :: PP
myXmobarPP =
  filterOutWsPP [scratchpadWorkspaceTag]
    $ def
        { ppSep = slategray " | "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent = tagActive . wrap " " " "
        , ppVisible = yellow . wrap ("(") (")")
        , ppHidden = wrap "+" ""
        , ppHiddenNoWindows = gray . wrap " " ""
        , ppUrgent = tagUrgent . wrap " " " "
        , ppLayout = wrap " " " " . lowWhite
        , ppOrder = \(ws:l:_:ex) -> [ws, l] ++ ex
        , ppExtras =
            [formattedWindowCount, logTitles formatFocused formatUnfocused]
        }
  where
    formatFocused =
      xmobarColor colorBlue colorSlateGray . wrap " " " " . ppWindow
    formatUnfocused = wrap (lowWhite "(") (lowWhite ")") . lowWhite . ppWindow
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
    , className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat
    , resource =? "desktop_window" --> doIgnore
    , className =? "TelegramDesktop" --> viewShift (myWorkspaces !! 8)
    , className =? "Tor Browser" --> viewShift (myWorkspaces !! 8)
    , className =? "calibre" --> viewShift (myWorkspaces !! 8)
    , className =? "Liferea" --> viewShift (myWorkspaces !! 7)
    , className =? "Thunderbird" --> viewShift (myWorkspaces !! 7)
    , className =? "qBittorrent" --> viewShift (myWorkspaces !! 8)
    , className
        =? "qBittorrent"
        <&&> title
        =? "Open Torrent Files"
        --> doRectFloat (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
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
  spawnOnce "nmcli networking off &"
  spawnOnce "numlockx on &"
  spawnOnce "xmodmap $XDG_CONFIG_HOME/X11/xmodmap"
  spawnOnce "xfce4-power-manager &"
  spawnOnce "/usr/bin/pipewire-pulse &"
  spawnOnce "/usr/bin/pipewire &"
  spawnOnce "/usr/libexec/polkit-gnome-authentication-agent-1 &"
  spawnOnce "sxhkd -c $HOME/.config/sxhkd/xmonad_sxhkdrc"
  -- https://github.com/TheMC47/dotfiles/blob/974d07559b2df474797d7f7ac96341900f6ad6b4/xmonad/.xmonad/xmonad.hs#L544 --
  spawnOnce
    "trayer --edge top --align right --widthtype request --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x171d23 --height 21 &"
  spawnOnce
    "{ xrandr | grep 'HDMI-1-0 connected' >/dev/null && xrandr --output HDMI-1-0 --auto --primary && xrandr --output eDP-1 --off ;} || { xrandr --output eDP-1 --auto --primary && xrandr --output HDMI-1-0 --off ;}"
