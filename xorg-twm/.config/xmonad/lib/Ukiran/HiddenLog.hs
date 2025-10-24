module Ukiran.HiddenLog where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Typeable (Typeable)
import XMonad
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
import XMonad.Layout.Hidden
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

--------A--------
-- Workspace → [(Window, Title)] (in hide order)
newtype HiddenLog =
  HiddenLog (Map WorkspaceId [(Window, String)])
  deriving (Typeable, Show, Read) -- 👈 Add Read here

instance ExtensionClass HiddenLog where
  initialValue = HiddenLog M.empty
  extensionType = PersistentExtension

logAndHideWindow :: Window -> X ()
logAndHideWindow w =
  withWindowSet $ \ws -> do
    let wid = W.currentTag ws
    title <- runQuery (stringProperty "WM_NAME") w
    XS.modify (add wid (w, title))
    hideWindow w
  where
    add wid pair (HiddenLog m) = HiddenLog (M.insertWith (++) wid [pair] m)

-- truncate to max n characters
truncateTitle :: Int -> String -> String
truncateTitle n s
  | length s > n = take n s ++ "…" -- add ellipsis if truncated
  | otherwise = s

hiddenLogPP :: X (Maybe String)
hiddenLogPP =
  withWindowSet $ \ws -> do
    let wid = W.currentTag ws
    HiddenLog m <- XS.get
    case M.lookup wid m of
      Nothing -> pure Nothing
      Just [] -> pure Nothing
      Just xs -> pure . Just $ fmt xs
  where
    fmt xs =
      "["
        ++ unwords
             (map (\(_, title) -> "(" ++ truncateTitle 30 title ++ ")") xs)
        ++ "]"

-- lowWhite = xmobarColor colorLowWhite ""
-- hiddenLogPP :: X (Maybe String)
-- hiddenLogPP =
--   withWindowSet $ \ws -> do
--     let wid = W.currentTag ws
--     HiddenLog m <- XS.get
--     case M.lookup wid m of
--       Nothing -> pure Nothing
--       Just [] -> pure Nothing
--       Just xs -> pure . Just $ fmt xs
--         -- wrap each hidden window title with parentheses, shorten to 30 chars,
--         -- and color using your lowWhite helper (or any color you like)
--         where fmt xs =
--                 unwords
--                   (map (\(_, t) -> lowWhite $ "(" ++ shorten 30 t ++ ")") xs)


-- Restore the oldest hidden window AND update the log
popOldestHiddenWindowLogged :: X ()
popOldestHiddenWindowLogged =
  withWindowSet $ \ws -> do
  -- Look at your extension state
    HiddenLog mp <- XS.get
    case M.lookup (W.currentTag ws) mp of
      Nothing -> popOldestHiddenWindow -- fallback: no log, just pop
      Just [] -> popOldestHiddenWindow -- nothing recorded
      Just ((w, _):rest) -> do
      -- Remove from HiddenLog
        XS.put (HiddenLog (M.insert (W.currentTag ws) rest mp))
      -- Restore via HiddenWindows
        popOldestHiddenWindow

-- | Pop all hidden windows in the current workspace and clear the log
popAllInWS :: X ()
popAllInWS =
  withWindowSet $ \ws -> do
    let wid = W.currentTag ws
    HiddenLog mp <- XS.get
    case M.lookup wid mp of
      Nothing -> return () -- nothing logged, nothing to do
      Just [] -> return () -- log is empty
      Just xs -> do
      -- Restore all windows in order
        mapM_ (popHiddenWindow . fst) xs
      -- Clear the log for this workspace
        XS.put (HiddenLog (M.insert wid [] mp))
