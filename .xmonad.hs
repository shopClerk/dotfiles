{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}
 
-- XMonad configuration file by Thomas ten Cate <ttencate@gmail.com>
-- 
-- Works on xmonad-0.8, NOT on 0.7 or below; and of course
-- xmonad-contrib needs to be installed as well
--
-- This is designed to play nice with a standard Ubuntu Hardy installation.
-- It gives you 10 workspaces, available through Alt+F1..F10. You can move
-- windows to a workspace with Win+F1..F10 (and you will automatically switch
-- to that workspace too).
--
-- All workspaces except F9 respect panels and docks.
-- F9 is the fullscreen workspace (for mplayer, etc.).
-- F10 is the instant messaging workspace.
--
-- Pidgin and Skype windows are automatically placed onto the IM workspace.
-- Their contact lists each get a column on the right side of the screen,
-- and all their other windows (chat windows, etc.) go into a grid layout
-- in the remaining space.
-- (This uses a copied and modified version of XMonad.Layout.IM.)
--
-- Keybindings mostly use the Windows key, but some use Alt to mimic other
-- window managers. In general: Alt is used for navigation, Win for modification.
-- Some of the bindings resemble the XMonad defaults, but most don't.
-- The bindings are set up to be comfortable to use on a dvorak keyboard layout.
--
-- Navigation:
-- Alt+F1..F10          switch to workspace
-- Ctrl+Alt+Left/Right  switch to previous/next workspace
-- Alt+Tab              focus next window
-- Alt+Shift+Tab        focus previous window
--
-- Window management:
-- Win+F1..F10          move window to workspace
-- Win+Up/Down          move window up/down
-- Win+C                close window
-- Alt+ScrollUp/Down    move focused window up/down
-- Win+M                move window to master area
-- Win+N                refresh the current window
-- Alt+LMB              move floating window
-- Alt+MMB              resize floating window
-- Alt+RMB              unfloat floating window
-- Win+T                unfloat floating window
--
-- Layout management:
-- Win+Left/Right       shrink/expand master area
-- Win+W/V              move more/less windows into master area
-- Win+Space            cycle layouts
--
-- Other:
-- Win+Enter            start a terminal
-- Win+R                open the Gnome run dialog
-- Win+Q                restart XMonad
-- Win+Shift+Q          display Gnome shutdown dialog
 
import XMonad
-- import XMonad.Layout.SimpleFloat
import XMonad.Layout.DecorationMadness
-- import XMonad.Layout.MouseResizableTile
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as S
import XMonad.Actions.CycleWS
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Util.WindowProperties
import XMonad.Actions.PhysicalScreens
import Control.Monad
import Data.Ratio
import qualified Data.Map as M
 
-- defaults on which we build
-- use e.g. defaultConfig or gnomeConfig
myBaseConfig = xfceConfig

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
 
-- display
-- replace the bright red border with a more stylish colour
myBorderWidth = 2
myNormalBorderColor = "#dddddd"
myFocusedBorderColor = "#6699cc"
-- myFocusedBorderColor = "#859900"
-- myFocusedBorderColor = "#DC143C"

-- Themes for certain floating modes
--myTheme = defaultTheme { inactiveBorderColor = "#FF0000"
--                       , activeTextColor     = "#00FF00" }
 
-- workspaces
myWorkspaces = ["web", "editor", "terms"] ++ (miscs 5) ++ ["fullscreen", "im"]
    where miscs = map (("misc" ++) . show) . (flip take) [1..]
isFullscreen = (== "fullscreen")
 
-- layouts
basicLayout = Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2
tallLayout = named "tall" $ avoidStruts $ basicLayout
wideLayout = named "wide" $ avoidStruts $ Mirror basicLayout
singleLayout = named "single" $ avoidStruts $ noBorders Full
fullscreenLayout = named "fullscreen" $ noBorders Full
imLayout = avoidStruts $ reflectHoriz $ withIMs ratio rosters chatLayout where
    chatLayout      = Grid
    ratio           = 1%6
    rosters         = [pidginRoster]
    pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
 
myLayoutHook = fullscreen $ im $ normal where
-- Interesting thing here V (simple float)
    normal     = tallLayout ||| wideLayout ||| singleLayout ||| floatSimpleSimple ||| circleSimpleDecoResizable
    fullscreen = onWorkspace "fullscreen" fullscreenLayout
    im         = onWorkspace "im" imLayout

-- xprop gives the names of thingies
mathHook = composeAll . concat $
    [ [appName =? a --> doCenterFloat | a <- myFloatAS ]
    ] where
        myFloatAS = ["MATLAB  R2012a"]
 
imManageHooks = composeAll [isIM --> moveToIM] where
    isIM     = foldr1 (<||>) [isPidgin, isSkype]
    isPidgin = className =? "Pidgin"
    isSkype  = className =? "Skype"
    moveToIM = doF $ S.shift "im"

otrManageHooks = composeAll 
   [ className =? "Rhythmbox" --> doShift "="
   -- , className =? "Xfce4-terminal"      --> doShift "misc2"
   , className =? "Xmessage"  --> doFloat
   , className =? "Xfrun4"  --> doFloat
   , className =? "Wrapper"  --> doFloat
   , className =? "xfce4-popup-whiskermenu"  --> doFloat
   , className =? "xfce4-appfinder"  --> doFloat
   , className =? "Xfce4-appfinder"  --> doFloat
   -- , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
   , className =? "xfce4-notes"  --> doFloat
   -- , className =? "keepass2"  --> doFloat
   -- , className =? "Keepass2"  --> doFloat
   , className =? "Xfce4-notes"  --> doFloat
   , manageDocks
   ]
 
-- special treatment for specific windows:
-- put the Pidgin and Skype windows in the im workspace
myManageHook = mathHook <+> otrManageHooks <+> manageHook myBaseConfig

-- Mod4 is the Super / Windows key
myModMask = mod4Mask
altMask = mod1Mask
 
-- better keybindings for dvorak
myKeys conf = M.fromList $
    [ ((myModMask               , xK_Return), spawn $ XMonad.terminal conf)
    -- , ((myModMask              , xK_r     ), xfrun4)
    , ((myModMask               , xK_c     ), kill)
    , ((myModMask               , xK_space ) , sendMessage NextLayout)
    , ((myModMask .|. shiftMask , xK_space ) , setLayout $ XMonad.layoutHook conf)
    , ((myModMask               , xK_n     ), refresh)
    , ((myModMask               , xK_m     ), windows S.swapMaster)
    -- , ((myModMask               , xK_Tab   ), windows S.focusDown)
    -- , ((myModMask .|. shiftMask , xK_Tab   ), windows S.focusUp)
    , ((myModMask               , xK_a     ), windows S.focusDown)
    , ((myModMask               , xK_o     ), windows S.focusUp)
    , ((myModMask               , xK_m     ), windows S.focusMaster  )
    , ((myModMask               , xK_Return), windows S.swapMaster)
    , ((myModMask               , xK_j 	   ), windows S.swapDown)
    , ((myModMask               , xK_k     ), windows S.swapUp)
    , ((myModMask               , xK_h     ), sendMessage Shrink)
    , ((myModMask               , xK_s     ), sendMessage Expand)
    , ((myModMask               , xK_t     ), withFocused $ windows . S.sink)
    , ((myModMask               , xK_w     ), sendMessage (IncMasterN 1))
    , ((myModMask               , xK_v     ), sendMessage (IncMasterN (-1)))
    , ((myModMask               , xK_q     ), broadcastMessage ReleaseResources >> restart "xmonad" True)
    , ((myModMask .|. shiftMask , xK_q     ), spawn "xfce4-session-logout")
    , ((altMask .|. controlMask , xK_Left  ), prevWS)
    , ((altMask .|. controlMask , xK_Right ), nextWS)
    -- , ((myModMask,               xK_u), sendMessage ShrinkSlave) -- %! Shrink a slave area
    -- , ((myModMask,               xK_i), sendMessage ExpandSlave) -- %! Expand a slave area
    ] ++
    -- Alt+F1..F10 switches to workspace
    -- (Alt is in a nicer location for the thumb than the Windows key,
    -- and 1..9 keys are already in use by Firefox, irssi, ...)
    [ ((altMask, k), windows $ S.greedyView i)
        | (i, k) <- zip myWorkspaces workspaceKeys
    ] ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((myModMask .|. mask, key), f sc)
	| (key, sc) <- zip [xK_semicolon, xK_comma, xK_period] [0..]
	, (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]] ++
    -- mod+F1..F10 moves window to workspace and switches to that workspace
    [ ((myModMask, k), (windows $ S.shift i) >> (windows $ S.greedyView i))
        | (i, k) <- zip myWorkspaces workspaceKeys
    ]
    where workspaceKeys = [xK_F1 .. xK_F10]
 
-- mouse bindings that mimic Gnome's
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((altMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((altMask, button2), (\w -> focus w >> mouseResizeWindow w))
    , ((altMask, button3), (\w -> focus w >> (withFocused $ windows . S.sink)))
    , ((altMask, button4), (const $ windows S.swapUp))
    , ((altMask, button5), (const $ windows S.swapDown))
    ]
 
-- put it all together
main = xmonad $ myBaseConfig
    { modMask = myModMask
    -- no idea of why i put that V
    , startupHook = setWMName "LG3D"
    -- , startupHook = setWMName "Xmonad"
    , workspaces = myWorkspaces
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , focusFollowsMouse = myFocusFollowsMouse
    , keys = myKeys
    -- , mouseBindings = myMouseBindings
    }
 -- modified version of XMonad.Layout.IM --
 
-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)
 
instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"
 
-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props
 
-- | IM layout modifier applied to the Grid layout
gridIMs :: Rational -> [Property] -> ModifiedLayout AddRosters Grid a
gridIMs ratio props = withIMs ratio props Grid
 
hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w
 
-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> S.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack = S.stack wksp
    let ws = S.integrate' $ stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= S.filter (`notElem` rosters)
    (a,b) <- runLayout (wksp {S.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)
