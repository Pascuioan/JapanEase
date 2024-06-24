{-# LANGUAGE TemplateHaskell #-}
module Main where
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Data.Char
import Data.Text.Zipper
import GHC.IO.Handle (Handle, hIsEOF, hClose, hGetLine)
import System.IO (openFile, utf8)
import System.Random
import Control.Monad.IO.Class
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Encoding (setLocaleEncoding)
import Control.Monad (void)
import Graphics.Vty.Config
import Graphics.Vty.CrossPlatform
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick as T
import Brick.AttrMap ()
import Brick (Widget, updateAttrMap, applyAttrMappings, hLimit, fg, on)
import Brick.Widgets.Core (vBox, str)
import Brick.Widgets.Center (center)

-- data types needed for Brick

data Name = QUESTION | INPUT
  deriving (Ord, Show, Eq)

data QuestionType = JP | EN

data State =
  State {
    _editor :: E.Editor String Name,
    _wordsQ :: [(String, String, String)],
    _question :: (String, String, String),
    _questionType :: QuestionType,
    _help :: String,
    _bColor :: V.Attr
  }
makeLenses ''State

-- functions needed for Brick

handleEvent :: T.BrickEvent Name e -> T.EventM Name State ()
-- if space is pressed reveal the answer
handleEvent (T.VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  currentQuestion <- use question
  currentQuestionType <- use questionType
  help .= getAnswer currentQuestion currentQuestionType
-- if enter is pressed validate the answer
handleEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
  -- get editor contents
  e <- use editor
  let currentInput = head $ E.getEditContents e
  -- clear the editor
  editor %= E.applyEdit clearZipper
  -- get the current question
  currentQuestion <- use question
  currentQuestionType <- use questionType
  let correctAnswer = getValidatingAnswer currentQuestion currentQuestionType
  -- check if the answer is correct
  let isCorrect = correctAnswer == strip currentInput
  if isCorrect then
    bColor .= fg V.green
  else
    bColor .= fg V.red
  -- set a new question
  newQuestion <- getRandomQuestion
  newQuestionType <- getRandomQuestionType
  question .= newQuestion
  questionType .= newQuestionType
  help .= " " 
handleEvent (T.VtyEvent (V.EvKey V.KEsc [])) = do M.halt
handleEvent ev = do
  zoom editor $ E.handleEditorEvent ev

drawUI :: State -> [Widget Name]
drawUI s = [ui]
  where
    ui = updateAttrMap (applyAttrMappings [(B.borderAttr, s^.bColor)]) $ B.border $ center $ vBox [center questionObj, center helpObj, center inputObj]
    questionObj = str $ getQuestion (s^.question) (s^.questionType)
    helpObj = str (s^.help)
    inputObj = hLimit 30 $ E.renderEditor (str . unlines) True (s^.editor)

app :: M.App State e Name
app = M.App {
  M.appDraw = drawUI,
  M.appStartEvent = return (),
  M.appHandleEvent = handleEvent,
  M.appAttrMap = const $ T.attrMap V.defAttr [(E.editAttr, V.black `on` V.white)],
  M.appChooseCursor = const $ M.showCursorNamed INPUT
}

main :: IO ()
main = do
  setLocaleEncoding utf8
  fileHandle <- openFile "words.txt" ReadMode
  wordsQ <- readData fileHandle
  hClose fileHandle
  let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  initialVty <- buildVty
  void $ M.customMain initialVty buildVty Nothing app $ buildInitialState wordsQ


-- helper function to create the initial state
buildInitialState :: [(String, String, String)] -> State
buildInitialState words = State{
  _editor = E.editor INPUT (Just 1) "",
  _wordsQ = words,
  _question = ("Press Enter to submit or Space to get the solution when stuck!","Hello:)",""),
  _questionType = EN,
  _help = " ",
  _bColor = fg V.white
}

-- get a new question
getRandomQuestion :: T.EventM Name State (String, String, String)
getRandomQuestion = do
    w <- use wordsQ
    ind <- liftIO randomIO
    let index = ind `mod` length w
    return $ w !! index

-- get a new question type
getRandomQuestionType :: T.EventM Name State QuestionType
getRandomQuestionType = do
  ind <- liftIO randomIO
  let index = ind `mod` 2
  return $ [JP, EN] !! index

-- helper functions to get the question and answers
getAnswer :: (String, String, String) -> QuestionType -> String
getAnswer (en, _, _) JP = en
getAnswer (_, jp, _) EN = jp

getValidatingAnswer :: (String, String, String) -> QuestionType -> String
getValidatingAnswer (en, _, _) JP = en
getValidatingAnswer (_, _, ro) EN = ro

getQuestion :: (String, String, String) -> QuestionType -> String
getQuestion (_, jp, _) JP = jp
getQuestion (en, _, _) EN = en

-- reads data from the file
readData :: Handle -> IO [(String, String, String)]
readData h = do
  hEOF <- hIsEOF h
  if hEOF
    then return []
    else do
      line <- hGetLine h
      rest <- readData h
      return $ parse line : rest

-- helpes parse a line into words
parse :: String -> (String, String, String)
parse s = (w1, w2, w3)
  where (w1, r1) = stringUntil ("", s)
        (w2, r2) = stringUntil ("", r1)
        (w3, _) = stringUntil ("", r2)
        stringUntil (s, h:t) 
          |h == '#' = (s, t)
          |otherwise = (h : s', t')
            where (s', t') = stringUntil (s, t)
        stringUntil (s, []) = (s, []) 

-- helper functions for strings
stripLeadingSpaces :: String -> String
stripLeadingSpaces [] = []
stripLeadingSpaces (h : t)
  |isSpace h = stripLeadingSpaces t
  |otherwise = h:t

stripEndingSpaces :: String -> String
stripEndingSpaces = reverse . stripLeadingSpaces . reverse

strip :: String -> String
strip = stripEndingSpaces . stripLeadingSpaces