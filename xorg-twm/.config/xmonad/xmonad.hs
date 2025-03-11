import Control.Monad (liftM2)
import Data.Ratio ((%))
import XMonad
-- import XMonad.Layout.WindowNavigation
-- import XMonad.Actions.WindowMenu
-- import XMonad.Actions.ShowText
-- import XMonad.Actions.SimpleDate
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.EasyMotion (EasyMotionConfig (..), selectWindow, textSize)
import XMonad.Actions.Minimize
import XMonad.Actions.ToggleFullFloat
import XMonad.Actions.WithAll
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
-- import XMonad.Layout.Magnifier
-- import XMonad.Layout.ShowWName (showWName)
import XMonad.Hooks.ShowWName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
-- import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
-- import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Hacks (javaHack, trayAbovePanelEventHook, trayPaddingEventHook, trayPaddingXmobarEventHook, trayerAboveXmobarEventHook, trayerPaddingXmobarEventHook, windowedFullscreenFixEventHook)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce (spawnOnce)
-- import XMonad.Layout.DecorationMadness
-- import XMonad.Layout.SimpleDecoration
-- import XMonad.Layout.NoFrillsDecoration

main :: IO ()
main =
  xmonad
    . toggleFullFloatEwmhFullscreen
    . ewmhFullscreen
    . ewmh
    . withEasySB
      ( statusBarProp
          "xmobar ~/.config/xmobar/xmobarrc"
          (copiesPP (xmobarColor "#2fafff" "" . wrap (":") ("")) myXmobarPP)
          -- (pure myXmobarPP)
      )
      defToggleStrutsKey
    . docks
    $ myConfig

-- myL = noFrillsDeco shrinkText def (layoutHook def)

myConfig =
  def
    { modMask = mod4Mask, -- Rebind Mod to the Super key
      layoutHook = myLayout, -- Use custom layouts
      -- layoutHook = myL, -- Use custom layouts
      manageHook = myManageHook, -- Match on certain windows
      handleEventHook = windowedFullscreenFixEventHook <> trayerPaddingXmobarEventHook,
      startupHook = myStartupHook,
      workspaces = myWorkspaces,
      terminal = myTerminal,
      logHook = showWNameLogHook mySWNConfig,
      borderWidth = 3,
      normalBorderColor = "#1b2b34",
      focusedBorderColor = "#0080FF"
    }
    `additionalKeysP` myKeys
    `removeKeysP` ["M-S-q"]

-- Define your viewShift function (fixed)
viewShift :: WorkspaceId -> WindowSet -> WindowSet
viewShift i = W.greedyView i . W.shift i

myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart && dunstify -a ignore -i '/home/ukiran/.config/xmonad/icons/new_xmonad.png' 'Reloaded Xmonad'"),
    ("M-C-r", spawn "xmonad --restart"),
    ("M-a", spawn "rofi -show drun -show-icons"),
    ("M-w", spawn "rofi -show window -show-icons"),
    ("M-s", spawn "rofi -show run"),
    ("M-e", spawn "emacsclient -c -a 'emacs'"),
    ("M-<Return>", spawn "urxvtc -e tmux new-session -A -s 'Main'")
  ]
    ++ [ ("M-.", nextWS), -- %! Switch to Next WS
         ("M-,", prevWS), -- %! Switch to Prev WS
         ("M-S-.", shiftToNext >> nextWS), -- %! Switch Window to Next WS
         ("M-S-,", shiftToPrev >> prevWS), -- %! Switch Window to Next WS
         ("M-C-.", shiftToNext), -- %! Move Window to Next WS
         ("M-C-,", shiftToPrev), -- %! Move Window to Prev WS
         ("M-<Backspace>", toggleWS), -- %! Switch to Last Used WS
         ("M-'", moveTo Next $ hiddenWS :&: Not emptyWS), -- %! Switch to Next Non-Empty WS
         ("M-;", moveTo Prev $ hiddenWS :&: Not emptyWS), -- %! Switch to Prev Non-Empty WS
         ("M-S-'", shiftTo Next $ hiddenWS :&: Not emptyWS),
         ("M-S-;", shiftTo Prev $ hiddenWS :&: Not emptyWS),
         ("M-n", refresh), -- What it does?

         -- resizing the master/slave ratio
         ("M-l", sendMessage Shrink), -- %! Shrink the master area
         ("M-h", sendMessage Expand), -- %! Expand the master area
         -- floating layer support
         ("M-t", withFocused $ windows . W.sink),
         ("M-S-t", sinkAll), -- push back to tiling all wins-- %! Push window back into tiling
         ("M-f", withFocused toggleFullFloat), -- fullScreen the win
         ("M-S-x", killAll), -- kill all wins

         -- EasyMotion
         ( "M-o",
           selectWindow
             def
               { txtCol = "Green",
                 cancelKey = xK_Escape,
                 emFont = "xft:Iosevka:weight=bold:pixelsize=40:antialias=true",
                 overlayF = textSize
               }
             >>= (`whenJust` windows . W.focusWindow)
         ),
         ( "M-x",
           selectWindow
             def
               { txtCol = "Red",
                 cancelKey = xK_Escape,
                 emFont = "xft:Iosevka:weight=bold:pixelsize=40:antialias=true",
                 overlayF = textSize
               }
             >>= (`whenJust` killWindow)
         ),
         -- clients
         ("M-q", kill),
         ("M-i", withFocused minimizeWindow),
         ("M-S-i", withLastMinimized maximizeWindowAndFocus),
         ("M-u", withFocused (sendMessage . maximizeRestore)),
         ("M-j", sendMessage MirrorExpand),
         ("M-k", sendMessage MirrorShrink),
         ("M-<Tab>", BW.focusDown), ---- With Boring (BW)
         ("M-S-<Tab>", BW.focusUp),
         ("M-g", windows W.focusDown), -- Without BW
         ("M-S-g", windows W.focusUp),
         ("M-m", BW.focusMaster), -- %! Move focus to the master window
         -- modifying the window order
         ("M-S-m", windows W.swapMaster),
         -- Checkout: BW.siftDown, BW.siftUp
         ("M-S-j", BW.swapDown), -- %! Swap the focused window with the next window
         ("M-S-k", BW.swapUp) -- %! Swap the focused window with the previous window
       ]
    ++ [ ("M-" ++ m ++ k, windows $ f i)
       | (i, k) <- zip (myWorkspaces) (map show [1 :: Int ..]),
         (f, m) <- [(W.view, ""), (W.shift, "S-"), (viewShift, "C-"), (copy, "c ")]
       ]
    ++ [ ("M-c c", windows copyToAll),
         ("M-c x", killAllOtherCopies),
         ("M-c q", kill1)
       ]

-- viewShift :: WorkspaceId -> X ()
-- viewShift i = do
-- windows (W.greedyView i . W.shift i)

myTabConfig =
  def
    { activeColor = "#3548cf",
      activeBorderColor = "#3548cf",
      -- activeBorderColor = "#0080FF",
      activeTextColor = "#FFFFFF",
      inactiveColor = "#171D23",
      inactiveBorderColor = "#171D23",
      inactiveTextColor = "#9E9E9E",
      urgentColor = "#EC7875",
      urgentBorderColor = "#EC7875",
      urgentTextColor = "#FFFFFF",
      decoHeight = 17,
      fontName = "xft:Iosevka:weight=bold:pixelsize=13.25:antialias=true"
    }

mySWNConfig =
  def
    { swn_font = "xft:Iosevka:weight=bold:pixelsize=30:antialias=true",
      swn_bgcolor = "#3548cf",
      swn_color = "#FFFFFF",
      swn_fade = 1
    }

myLayout =
  -- showWName $
  avoidStruts $
    BW.boringWindows $ -- Diff b/w BW.boringWindows and BW.boringAuto ??
      onWorkspaces ["8", "9"] (tab ||| tiled) $
        tiled ||| tab ||| mtile
  where
    -- threeCol =
    --   smartBorders $
    --     renamed [Replace "threecol"] $
    --       maximizeWithPadding 1 $
    --         minimize $
    --           magnifiercz' 1.3 $
    --             ThreeColMid nmaster delta ratio
    -- full = smartBorders $ renamed [Replace "Full"] $ minimize $ Full -- Not Compatible with BW
    -- full = smartBorders $ renamed [Replace "full"] $ minimize $ Simplest
    mtile = renamed [Replace "mtall"] $ Mirror tiled
    tab = smartBorders $ renamed [Replace "tabs"] $ noBorders (tabbed shrinkText myTabConfig)
    tiled =
      smartBorders $
        renamed [Replace "tall"] $
          reflectHoriz $
            maximizeWithPadding 1 $
              minimize $
                smartSpacing 2 $
                  ResizableTall nmaster delta ratio []
    twoPane = TwoPane (3 / 100) (1 / 2)
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces = map show [1 .. 9 :: Int]

windowCount :: X (Maybe String)
windowCount =
  gets $
    Just
      . show
      . length
      . W.integrate'
      . W.stack
      . W.workspace
      . W.current
      . windowset

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = fadeGray " | ",
      -- copiesPP = yellow . wrap ("&") (""),
      ppTitleSanitize = xmobarStrip,
      ppCurrent = cyan . wrap ("[") ("]"),
      ppHidden = silver . wrap "+" "",
      ppHiddenNoWindows = slategray . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppLayout = wrap " " " " . lowWhite,
      ppOrder = \(ws : l : _ : ex) -> [ws, l] ++ ex,
      ppExtras = [formattedWindowCount, logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (blue "(") (blue ")") . blue . ppWindow
    formatUnfocused = wrap (lowWhite "") (lowWhite "") . lowWhite . ppWindow

    formattedWindowCount :: X (Maybe String)
    formattedWindowCount = fmap (fmap $ lowWhite) windowCount

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 25

    blue, lowWhite, red, yellow, fadeGray, blueGray :: String -> String
    blue = xmobarColor "#2fafff" ""
    yellow = xmobarColor "#d0bc00" ""
    red = xmobarColor "#ff5f59" ""
    lowWhite = xmobarColor "#d0d0d0" ""
    cyan = xmobarColor "#00fcfc" ""
    slategray = xmobarColor "#2F4F4F" ""
    silver = xmobarColor "#C0C0C0" ""
    fadeGray = xmobarColor "#3C4449" ""
    blueGray = xmobarColor "#6D8895" ""

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      title =? "World" --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)),
      className =? "mpv" --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)),
      className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat, -- Firefox pip
      resource =? "desktop_window" --> doIgnore,
      className =? "TelegramDesktop" --> viewShift (myWorkspaces !! 8),
      className =? "Tor Browser" --> viewShift (myWorkspaces !! 8),
      className =? "qBittorrent" --> viewShift (myWorkspaces !! 8),
      className =? "calibre" --> viewShift (myWorkspaces !! 8),
      className =? "Liferea" --> viewShift (myWorkspaces !! 7),
      className =? "Thunderbird" --> viewShift (myWorkspaces !! 7),
      isDialog --> doCenterFloat,
      isFullscreen --> doFullFloat,
      checkDock --> doLower
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
  spawnOnce "trayer --edge top --align right --widthtype request --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x171d23 --height 21 &"
  spawnOnce "{ xrandr | grep 'HDMI-1-0 connected' >/dev/null && xrandr --output HDMI-1-0 --auto --primary && xrandr --output eDP-1 --off ;} || { xrandr --output eDP-1 --auto --primary && xrandr --output HDMI-1-0 --off ;}"
  spawnOnce "sxhkd -c $HOME/.config/sxhkd/xmonad_sxhkdrc"
