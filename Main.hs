module Main where

import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive.Gtk2
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Events

import Data.WAVE

import Graphics.UI.SDL.Mixer

import Data.IORef
import Data.List
import qualified Data.Map as Map

{- TODOs
  - load file support
  - select sample rate
  - fix issue when samples have different lengths
-}

data State = State { bits :: Int, samples :: Int , sampleRate :: Int } deriving Show;

defaultState :: State
defaultState = State 8 16 2400

main :: IO ()
main = do
  boxes1 <- newIORef []
  boxes2 <- newIORef []
  state1 <- newIORef defaultState
  state2 <- newIORef defaultState

  _ <- initGUI
  window <- windowNew
  set window [windowTitle := "Text Entry", containerBorderWidth := 10]
  mainBox <- vBoxNew False 0

  channelBox1 <- channelBoxNew "channel1" 0 boxes1 state1
  channelBox2 <- channelBoxNew "channel2" 1 boxes2 state2

  buttonBox <- hBoxNew False 0

  stopButton <- buttonNewWithLabel "Stop"
  quitButton <- buttonNewWithLabel "Quit"
  
  boxPackStart buttonBox stopButton PackGrow 0
  boxPackStart buttonBox quitButton PackGrow 0

  boxPackStart mainBox channelBox1 PackGrow 0
  boxPackStart mainBox channelBox2 PackGrow 0
  boxPackStart mainBox buttonBox PackNatural 0

  containerAdd window mainBox
  widgetShowAll window
  
  objectDestroyReactive window              =:> mainQuit
  _ <- onButtonPress stopButton (\_ -> soundStop)
  _ <- onButtonPress quitButton (\_ -> do {_ <- widgetDestroy window; mainQuit ; return True })
  _ <- onKeyPress window (handleKey window)

  openAudio defaultFrequency AudioS16Sys 1 16
  -- start main loop
  mainGUI

soundStop :: IO Bool
soundStop =
  do
    haltChannel (-1)
    return True

channelBoxNew :: String -> Int -> IORef [(Int,Int)] -> IORef State -> IO VBox
channelBoxNew channelName channelNo boxes state =
  do
      channel <- drawingAreaNew
      channelBox <- vBoxNew False 0
      channelHBox <- hBoxNew False 0
      channelButtons <- vBoxNew False 0
      channelSave <- buttonNewWithLabel "Save"
      channelLoad <- buttonNewWithLabel "Load"
      channelClear <- buttonNewWithLabel "Clear"
      channelPlay <- buttonNewWithLabel "Play"
      channelStop <- buttonNewWithLabel "Stop"
      configBox <- configBoxNew boxes state channel channelName
      
      boxPackStart channelBox channelHBox PackGrow 0
      boxPackStart channelBox configBox PackNatural 0
      boxPackStart channelHBox channel PackGrow 0
      boxPackStart channelHBox channelButtons PackNatural 0
      boxPackStart channelButtons channelSave PackGrow 0
      boxPackStart channelButtons channelLoad PackGrow 0
      boxPackStart channelButtons channelClear PackGrow 0
      boxPackStart channelButtons channelPlay PackGrow 0
      boxPackStart channelButtons channelStop PackGrow 0

      _ <- onButtonPress channelSave (doSave)
      _ <- onButtonPress channelLoad (doLoad channel)
      _ <- onButtonPress channelClear (doClear channel)
      _ <- onButtonPress channelPlay (\_ -> doPlay)
      _ <- onButtonPress channelStop (\_ -> doStop)
      
      
      _ <- onRealize channel (updateDraw channel channelName state boxes >>= \_ -> return ())
      _ <- afterExpose channel (\_ -> do { _ <- updateDraw channel channelName state boxes ; return True })
      _ <- afterConfigure channel (\_ -> do { _ <- updateDraw channel channelName state boxes ; return True })
      _ <- onButtonPress channel (\e -> do {_ <- handleClick e channel state boxes ; _ <- updateDraw channel channelName state boxes ; return True })
      return channelBox
  where
    doSave _ =
      do
        chooser <- fileChooserDialogNew Nothing Nothing FileChooserActionSave [("Save",ResponseAccept),("Cancel",ResponseCancel)]
        _ <- dialogRun chooser
        fp <- fileChooserGetFilename chooser
        widgetDestroy chooser
        case fp of {
          Just f -> saveWav state boxes f ;
          _ -> return False
        }

    doLoad channel _ =
      do
        chooser <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen [("Save",ResponseAccept),("Cancel",ResponseCancel)]
        _ <- dialogRun chooser
        fp <- fileChooserGetFilename chooser
        widgetDestroy chooser
        case fp of {
          Just f -> (do
                        _ <- loadWav state boxes f
                        updateDraw channel channelName state boxes
                    );
          _ -> return False
        }
    doClear channel _ =
      do
        writeIORef boxes []
        updateDraw channel channelName state boxes
    doPlay =
      do
        chunk <- loadWAV (channelName ++ ".wav")
        _ <- volumeChunk chunk (maxVolume)
        _ <- playChannel channelNo chunk (-1)
        return True
    doStop =
      do
        haltChannel channelNo
        return True

configBoxNew :: IORef [(Int,Int)] -> IORef State -> DrawingArea -> String -> IO HBox
configBoxNew boxesRef stateRef channel channelName =
  do
    (State sBits sSteps sSamples) <- readIORef stateRef
    configBox <- hBoxNew False 0
    bitsBox <- vBoxNew False 0
    bitsLabel <- labelNew (Just "Bits")
    bitsScale <- hScaleNewWithRange 8 32 8
    rangeSetValue bitsScale (fromIntegral sBits)
    boxPackStart bitsBox bitsLabel PackNatural 0
    boxPackStart bitsBox bitsScale PackNatural 0
    stepsBox <- vBoxNew False 0
    stepsLabel <- labelNew (Just "Samples")
    stepsScale <- hScaleNewWithRange 4 32 1
    rangeSetValue stepsScale (fromIntegral sSteps)
    boxPackStart stepsBox stepsLabel PackNatural 0
    boxPackStart stepsBox stepsScale PackNatural 0
  
    samplesBox <- vBoxNew False 0
    samplesLabel <- labelNew (Just "Sample rate")
    samplesScale <- hScaleNewWithRange 8000 44000 4000
    rangeSetValue samplesScale (fromIntegral sSamples)
    boxPackStart samplesBox samplesLabel PackNatural 0
    boxPackStart samplesBox samplesScale PackNatural 0
    
    boxPackStart configBox bitsBox PackGrow 0
    boxPackStart configBox stepsBox PackGrow 0
    boxPackStart configBox samplesBox PackGrow 0

    bitsAdjustment <- rangeGetAdjustment bitsScale
    stepsAdjustment <- rangeGetAdjustment stepsScale
    samplesAdjustment <- rangeGetAdjustment samplesScale
    _ <- onValueChanged bitsAdjustment (updateState bitsScale 0)
    _ <- onValueChanged stepsAdjustment (updateState stepsScale 1)
    _ <- onValueChanged samplesAdjustment (updateState samplesScale 2)
    return configBox
  where
    updateState :: HScale -> Int -> IO ()
    updateState scale field =
      do
        val <- rangeGetValue scale
        (State sBits sSteps sSamples) <- readIORef stateRef        
        case field of { 
          0 -> writeIORef stateRef (State (round val) sSteps sSamples) ;
          1 -> writeIORef stateRef (State sBits (round val) sSamples) ;
          2 -> writeIORef stateRef (State sBits sSteps (round val)) ;
          _ -> return ()
          }
        _ <- updateDraw channel channelName stateRef boxesRef
        return ()

handleKey :: Window -> Event -> IO Bool
handleKey window (Key _ _ _ _ _ _ _ _ _ (Just 'q')) =
  do
    widgetDestroy window
    mainQuit
    return True
handleKey _ _ = return False

handleClick :: Event -> DrawingArea -> IORef State -> IORef [(Int, Int)] -> IO Bool
handleClick (Button _ _ _ x y _ LeftButton _ _) draw stateRef boxes =
  do
    (State sBits sSteps _) <- readIORef stateRef
    dWindow <- widgetGetDrawWindow draw
    (maxX,maxY) <- drawableGetSize dWindow
    let deltaX = maxX `div` sSteps
    let deltaY = maxY `div` sBits
    oBoxes <- readIORef boxes
    let b = (round x `div` deltaX,round y `div` deltaY)
    putStrLn $ show b
    let nBoxes = if b `elem` oBoxes then oBoxes \\ [b] else b:oBoxes
    writeIORef boxes nBoxes
    return True
handleClick _ _ _ _ = return False

updateDraw :: WidgetClass widget => widget -> String -> IORef State ->IORef [(Int,Int)] -> IO Bool
updateDraw draw name stateRef boxesRef =
  do
    (State sBits sSteps _) <- readIORef stateRef
    boxes <- readIORef boxesRef
    dWindow <- widgetGetDrawWindow draw
    (maxX,maxY) <- drawableGetSize dWindow
    let deltaX = maxX `div` sSteps
    let deltaY = maxY `div` sBits
    context <- gcNew dWindow
    gcSetValues context (newGCValues { foreground = Color 65535 65535 65535, background = Color 0 0 0})
    drawWindowClear dWindow
    drawWindowBeginPaintRect dWindow (Rectangle 0 0 maxX maxY)
    drawRectangle dWindow context True 0 0 maxX maxY
    gcSetValues context (newGCValues { foreground = Color 0 0 0, background= Color 65535 65535 65535})
    if deltaY > 0 && deltaX > 0 then
      do
        drawLine dWindow context (0,0) (maxX `div` deltaX * deltaX,0)
        drawLine dWindow context (0,0) (0,maxY `div` deltaY * deltaY)
        sequence_ [drawLine dWindow context (c*deltaX,0) (c*deltaX,maxY `div` deltaY * deltaY) | c <- [1..maxX `div` deltaX]]
        sequence_ [drawLine dWindow context (0,c*deltaY) (maxX `div` deltaX * deltaX ,c*deltaY) | c <- [1..maxY `div` deltaY]]
        drawLine dWindow context (maxX `div` deltaX * deltaX,0) (maxX `div` deltaX * deltaX,maxY `div` deltaY * deltaY)
      else return ()
    sequence_ [drawRectangle dWindow context True (x*deltaX+2) (y*deltaY+2) (deltaX-4) (deltaY-4) | (x,y) <- boxes, x < sSteps, y < sBits]
    drawWindowEndPaint dWindow
    saveWav stateRef boxesRef (name ++ ".wav")


saveWav :: IORef State -> IORef [(Int,Int)] -> FilePath -> IO Bool
saveWav stateRef boxesRef fileName =
  do
    (State sBits sSteps sSampleRate) <- readIORef stateRef
    boxes <- readIORef boxesRef
    let boxMap = Map.fromList $ [(x,fromIntegral y) | (x,y) <- sort boxes] :: Map.Map Int Double
    let wSamples = [doubleToSample $ (fromIntegral (sBits-1)-y)/(fromIntegral (sBits-1) / 2)-1.0 | x <- [0..(sSteps-1)], let y = Map.findWithDefault ((fromIntegral $ sBits-1)/2) x boxMap :: Double, x < sSteps, y < fromIntegral sBits ]
    let header = WAVEHeader 1 sSampleRate sBits (Just $ length wSamples)
    putWAVEFile fileName (WAVE header [wSamples])
    return True

loadWav :: IORef State -> IORef [(Int,Int)] -> FilePath -> IO Bool
loadWav stateRef boxesRef fileName=
  do
    (WAVE wHeader wSamples) <- getWAVEFile fileName
    let (WAVEHeader channels sSampleRate sBits (Just sSteps)) = wHeader
    if channels == 1 && sSampleRate <= 44000 && sBits <= 32 && sSteps <= 32 then
      do
        writeIORef stateRef (State sBits sSteps sSampleRate)
        let boxes = [(x, (sBits-1)-round ((y'+1.0)*(fromIntegral (sBits-1))/2)) | (x,y) <- zip [0..] (concat wSamples), let y' = sampleToDouble y] -- TODO
        writeIORef boxesRef boxes
        return True
      else
      return False
