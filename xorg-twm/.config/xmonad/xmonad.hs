<<<<<<< HEAD
import Control.Monad (liftM2)
import Data.Ratio ((%))
import XMonad
import XMonad.Actions.CycleWS
-- import XMonad.Hooks.ShowWName -- TEST IT

import XMonad.Actions.EasyMotion (EasyMotionConfig (..), selectWindow, textSize)
import XMonad.Actions.Minimize
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName (showWName)
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
-- import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Hacks (javaHack, trayAbovePanelEventHook, trayPaddingEventHook, trayPaddingXmobarEventHook, trayerAboveXmobarEventHook, trayerPaddingXmobarEventHook, windowedFullscreenFixEventHook)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce (spawnOnce)

-- import XMonad.Actions.ShowText
main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB
      ( statusBarProp
          "xmobar ~/.config/xmobar/xmobarrc"
          (pure myXmobarPP)
      )
      defToggleStrutsKey
    . docks
    $ myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig {modMask = m} = (m, xK_b)

myConfig =
  def
    { modMask = mod4Mask, -- Rebind Mod to the Super key
      layoutHook = myLayout, -- Use custom layouts
      manageHook = myManageHook, -- Match on certain windows
      handleEventHook = windowedFullscreenFixEventHook <> trayerPaddingXmobarEventHook,
      startupHook = myStartupHook,
      workspaces = myWorkspaces,
      terminal = myTerminal,
      borderWidth = 3,
      normalBorderColor = "#1b2b34",
      focusedBorderColor = "#6495ed"
    }
    `additionalKeysP` myKeys
    `removeKeysP` ["M-S-q"]

numPadKeys =
  [ xK_KP_End,
    xK_KP_Down,
    xK_KP_Page_Down, -- 1, 2, 3
    xK_KP_Left,
    xK_KP_Begin,
    xK_KP_Right, -- 4, 5, 6
    xK_KP_Home,
    xK_KP_Up,
    xK_KP_Page_Up -- 7, 8, 9
  ]

myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart"),
    ("M-C-r", spawn "xmonad --restart"),
    ("M-a", spawn "rofi -show drun -show-icons"),
    ("M-w", spawn "rofi -show window -show-icons"),
    ("M-s", spawn "rofi -show run"),
    ("M-e", spawn "emacsclient -c -a 'emacs'"),
    ("M-<Return>", spawn "alacritty -e tmux new-session -A -s 'Main'")
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
         ("M-t", withFocused $ windows . W.sink), -- %! Push window back into tiling

         -- EasyMotion
         ( "M-o",
           selectWindow
             def
               { txtCol = "Green",
                 cancelKey = xK_Escape,
                 emFont = "xft: Sans-40",
                 overlayF = textSize
               }
             >>= (`whenJust` windows . W.focusWindow)
         ),
         ( "M-x",
           selectWindow
             def
               { txtCol = "Red",
                 cancelKey = xK_Escape,
                 emFont = "xft: Sans-40",
                 overlayF = textSize
               }
             >>= (`whenJust` killWindow)
         ),
         -- clients
         ("M-q", kill),
         ("M-i", withFocused minimizeWindow),
         ("M-S-i", withLastMinimized maximizeWindowAndFocus),
         ("M-u", withFocused (sendMessage . maximizeRestore)),
         ("M-<Tab>", BW.focusDown), ---- With Boring (BW)
         ("M-S-<Tab>", BW.focusUp),
         ("M-g", windows W.focusDown),
         ("M-j", BW.focusDown), -- %! Move focus to the next window
         ("M-k", BW.focusUp), -- %! Move focus to the previous window
         ("M-m", BW.focusMaster), -- %! Move focus to the master window
         -- modifying the window order
         ("M-S-m", windows W.swapMaster),
         -- Checkout: BW.siftDown, BW.siftUp
         ("M-S-j", BW.swapDown), -- %! Swap the focused window with the next window
         ("M-S-k", BW.swapUp) -- %! Swap the focused window with the previous window
       ]

-- Makes setting the spacingRaw simpler to write.
-- The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myLayout =
  showWName $
    avoidStruts $
      BW.boringAuto $
        onWorkspaces ["8", "9"] (full ||| tiled) $
          tiled ||| full ||| threeCol
  where
    threeCol =
      smartBorders $
        renamed [Replace "ThreeCol"] $
          maximizeWithPadding 1 $
            minimize $
              magnifiercz' 1.3 $
                ThreeColMid nmaster delta ratio
    -- full = smartBorders $ renamed [Replace "Full"] $ minimize $ Full -- Not Compatible with BW
    full = smartBorders $ renamed [Replace "Full"] $ minimize $ Simplest
    mtile = renamed [Replace "MTall"] $ Mirror tiled
    tiled =
      smartBorders $
        renamed [Replace "Tall"] $
          reflectHoriz $
            maximizeWithPadding 1 $
              minimize $
                smartSpacing 2 $
                  Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myTerminal :: String
myTerminal = "alacritty"

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
      ppTitleSanitize = xmobarStrip,
      ppCurrent = cyan . wrap ("[") ("]"),
      ppHidden = silver . wrap "+" "",
      -- ppHiddenNoWindows = slategray  . \s -> "<fn=1>\xf111</fn>",
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

    blue, lowWhite, magenta, red, white, yellow, fadeGray, blueGray :: String -> String
    magenta = xmobarColor "#feacd0" ""
    blue = xmobarColor "#2fafff" ""
    white = xmobarColor "#a6a6a6" ""
    yellow = xmobarColor "#d0bc00" ""
    red = xmobarColor "#ff5f59" ""
    lowWhite = xmobarColor "#d0d0d0" ""
    cyan = xmobarColor "#00d3d0" ""
    slategray = xmobarColor "#2F4F4F" ""
    silver = xmobarColor "#C0C0C0" ""
    fadeGray = xmobarColor "#3C4449" ""
    blueGray = xmobarColor "#6D8895" ""

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      className =? "mpv" --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)),
      -- className =? "mpv" --> doCenterFloat,
      className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat, -- Firefox pip
      resource =? "desktop_window" --> doIgnore,
      className =? "TelegramDesktop" --> viewShift (myWorkspaces !! 8),
      className =? "Tor Browser" --> viewShift (myWorkspaces !! 8),
      className =? "qBittorrent" --> viewShift (myWorkspaces !! 8),
      className =? "calibre" --> viewShift (myWorkspaces !! 8),
      className =? "Liferea" --> viewShift (myWorkspaces !! 7),
      className =? "Thunderbird" --> viewShift (myWorkspaces !! 7),
      isDialog --> doFloat,
      isFullscreen --> doFullFloat,
      checkDock --> doLower
    ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "bash $HOME/.config/startup/xmonad_startup"
  spawnOnce "sxhkd -c $HOME/.config/sxhkd/xmonad_sxhkdrc"
||||||| 9d8c701
=======
import Control.Monad (liftM2)
import Data.Ratio ((%))
import XMonad
import XMonad.Actions.CycleWS
-- import XMonad.Hooks.ShowWName -- TEST IT

import XMonad.Actions.EasyMotion (EasyMotionConfig (..), selectWindow, textSize)
import XMonad.Actions.Minimize
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName (showWName)
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
-- import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Hacks (javaHack, trayAbovePanelEventHook, trayPaddingEventHook, trayPaddingXmobarEventHook, trayerAboveXmobarEventHook, trayerPaddingXmobarEventHook, windowedFullscreenFixEventHook)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce (spawnOnce)

-- import XMonad.Actions.ShowText
main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB
      ( statusBarProp
          "xmobar ~/.config/xmobar/xmobarrc"
          (pure myXmobarPP)
      )
      defToggleStrutsKey
    . docks
    $ myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig {modMask = m} = (m, xK_b)

myConfig =
  def
    { modMask = mod4Mask, -- Rebind Mod to the Super key
      layoutHook = myLayout, -- Use custom layouts
      manageHook = myManageHook, -- Match on certain windows
      handleEventHook = windowedFullscreenFixEventHook <> trayerPaddingXmobarEventHook,
      startupHook = myStartupHook,
      workspaces = myWorkspaces,
      terminal = myTerminal,
      borderWidth = 3,
      normalBorderColor = "#1b2b34",
      focusedBorderColor = "#6495ed"
    }
    `additionalKeysP` myKeys
    `removeKeysP` ["M-S-q"]

numPadKeys =
  [ xK_KP_End,
    xK_KP_Down,
    xK_KP_Page_Down, -- 1, 2, 3
    xK_KP_Left,
    xK_KP_Begin,
    xK_KP_Right, -- 4, 5, 6
    xK_KP_Home,
    xK_KP_Up,
    xK_KP_Page_Up -- 7, 8, 9
  ]

myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart"),
    ("M-C-r", spawn "xmonad --restart"),
    ("M-a", spawn "rofi -show drun -show-icons"),
    ("M-w", spawn "rofi -show window -show-icons"),
    ("M-s", spawn "rofi -show run"),
    ("M-e", spawn "emacsclient -c -a 'emacs'"),
    ("M-<Return>", spawn "alacritty -e tmux new-session -A -s 'Main'")
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
         ("M-t", withFocused $ windows . W.sink), -- %! Push window back into tiling

         -- EasyMotion
         ( "M-o",
           selectWindow
             def
               { txtCol = "Green",
                 cancelKey = xK_Escape,
                 emFont = "xft: Sans-40",
                 overlayF = textSize
               }
             >>= (`whenJust` windows . W.focusWindow)
         ),
         ( "M-x",
           selectWindow
             def
               { txtCol = "Red",
                 cancelKey = xK_Escape,
                 emFont = "xft: Sans-40",
                 overlayF = textSize
               }
             >>= (`whenJust` killWindow)
         ),
         -- clients
         ("M-q", kill),
         ("M-i", withFocused minimizeWindow),
         ("M-S-i", withLastMinimized maximizeWindowAndFocus),
         ("M-u", withFocused (sendMessage . maximizeRestore)),
         ("M-<Tab>", BW.focusDown), ---- With Boring (BW)
         ("M-S-<Tab>", BW.focusUp),
         ("M-g", windows W.focusDown),
         ("M-j", BW.focusDown), -- %! Move focus to the next window
         ("M-k", BW.focusUp), -- %! Move focus to the previous window
         ("M-m", BW.focusMaster), -- %! Move focus to the master window
         -- modifying the window order
         ("M-S-m", windows W.swapMaster),
         -- Checkout: BW.siftDown, BW.siftUp
         ("M-S-j", BW.swapDown), -- %! Swap the focused window with the next window
         ("M-S-k", BW.swapUp) -- %! Swap the focused window with the previous window
       ]

-- Makes setting the spacingRaw simpler to write.
-- The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

myLayout =
  showWName $
    avoidStruts $
      BW.boringAuto $
        onWorkspaces ["8","9"] (full ||| tiled) $
          tiled ||| full ||| threeCol
  where
    threeCol =
      smartBorders $
        renamed [Replace "ThreeCol"] $
          maximizeWithPadding 1 $
            minimize $
              magnifiercz' 1.3 $
                ThreeColMid nmaster delta ratio
    -- full = smartBorders $ renamed [Replace "Full"] $ minimize $ Full -- Not Compatible with BW
    full = smartBorders $ renamed [Replace "Full"] $ minimize $ Simplest
    mtile = renamed [Replace "MTall"] $ Mirror tiled
    tiled =
      smartBorders $
        renamed [Replace "Tall"] $
          reflectHoriz $
            maximizeWithPadding 1 $
              minimize $
                smartSpacing 2 $
                  Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myTerminal :: String
myTerminal = "alacritty"

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
      ppTitleSanitize = xmobarStrip,
      ppCurrent = cyan . wrap ("[") ("]"),
      ppHidden = silver . wrap "+" "",
      -- ppHiddenNoWindows = slategray  . \s -> "<fn=1>\xf111</fn>",
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

    blue, lowWhite, magenta, red, white, yellow, fadeGray, blueGray :: String -> String
    magenta = xmobarColor "#feacd0" ""
    blue = xmobarColor "#2fafff" ""
    white = xmobarColor "#a6a6a6" ""
    yellow = xmobarColor "#d0bc00" ""
    red = xmobarColor "#ff5f59" ""
    lowWhite = xmobarColor "#d0d0d0" ""
    cyan = xmobarColor "#00d3d0" ""
    slategray = xmobarColor "#2F4F4F" ""
    silver = xmobarColor "#C0C0C0" ""
    fadeGray = xmobarColor "#3C4449" ""
    blueGray = xmobarColor "#6D8895" ""

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      className =? "mpv" --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)),
      -- className =? "mpv" --> doCenterFloat,
      className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat, -- Firefox pip
      resource =? "desktop_window" --> doIgnore,
      className =? "TelegramDesktop" --> viewShift (myWorkspaces !! 8),
      className =? "Tor Browser" --> viewShift (myWorkspaces !! 8),
      className =? "qBittorrent" --> viewShift (myWorkspaces !! 8),
      className =? "calibre" --> viewShift (myWorkspaces !! 8),
      className =? "Liferea" --> viewShift (myWorkspaces !! 7),
      className =? "Thunderbird" --> viewShift (myWorkspaces !! 7),
      isDialog --> doFloat,
      isFullscreen --> doFullFloat,
      checkDock --> doLower
    ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "bash $HOME/.config/startup/xmonad_startup"
  spawnOnce "sxhkd -c $HOME/.config/sxhkd/xmonad_sxhkdrc"
>>>>>>> origin/main
