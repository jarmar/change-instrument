module Main where

import qualified Data.Map.Strict as M
import Data.Monoid
import Data.List (stripPrefix)
import Data.Char (isSpace)

import Options.Applicative

import qualified Sound.MIDI.File.Load as L
import qualified Sound.MIDI.File.Save as S
import qualified Sound.MIDI.Message.Channel as CM
import qualified Sound.MIDI.Message.Channel.Voice as VM
import qualified Sound.MIDI.General as GM

import Filter

instance Read CM.Channel where
  readsPrec = channelReadsPrec

instance Read VM.Program where
  readsPrec = programReadsPrec

-- It would probably be nicer to rewrite these in a monadic style.
channelReadsPrec :: Int -> ReadS CM.Channel
channelReadsPrec _ s =
  case reads s :: [(Int, String)] of
       [(n, s')] -> if n >= 0 && n < 16
                       then [(CM.toChannel n, s')]
                       else []
       _         -> []

programReadsPrec :: Int -> ReadS VM.Program
programReadsPrec _ s =
  case reads s :: [(Int, String)] of
       [(n, s')] -> if n >= 0 && n < 128
                       then [(VM.toProgram n, s')]
                       else []
       _         -> matchInstrumentNames s

matchInstrumentNames :: ReadS VM.Program
matchInstrumentNames s =
  foldl go [] GM.instrumentNames
  where
    trimmed = dropWhile isSpace s
    go :: [(VM.Program, String)] -> String -> [(VM.Program, String)]
    go res instrName =
      case stripPrefix instrName trimmed of
           Nothing -> res
           Just s' -> case GM.instrumentNameToProgram instrName of
                           Nothing -> res -- this REALLY shouldn't happen,
                           -- and if it does, it should probably not just be
                           -- ignored
                           Just p  -> (p, s'):res

data Args = Args
  { outfile :: FilePath
  , useSameInstr :: Bool
  , defaultInstr :: VM.Program
  , instrumentSpecs :: [(CM.Channel, VM.Program)]
  , infile :: FilePath
  }
  deriving Show

instrumentSpec :: Parser (CM.Channel, VM.Program)
instrumentSpec = option auto
  ( short 'c'
  <> metavar "(CH, INSTR)"
  <> help "Lorem ipsum dolor sit amet." )

useSame :: Parser Bool
useSame = switch
  ( long "same"
  <> short 's'
  <> help "Foo bar baz quux" )

defaultInstrument :: Parser VM.Program
defaultInstrument = option auto
  ( long "default"
  <> short 'd'
  <> value (VM.toProgram 0) )

outfileName :: Parser FilePath
outfileName = strOption
  ( long "output"
  <> short 'o'
  <> help "Outfile file name. This option is mandatory." )

parseArgs :: Parser Args
parseArgs = Args
  <$> outfileName
  <*> useSame
  <*> defaultInstrument
  <*> many instrumentSpec
  <*> argument str (metavar "FILE")

testMap :: ChannelInstrumentMap
testMap = M.fromList [(CM.toChannel 0, VM.toProgram 0)
                     ,(CM.toChannel 1, VM.toProgram 0)]

defaultMap :: VM.Program -> ChannelInstrumentMap
defaultMap p =
  M.fromList $ map (\n -> (CM.toChannel n, p)) [0..15]

mapFromArgs :: Args -> ChannelInstrumentMap
mapFromArgs args =
  let base      = if useSameInstr args
                     then defaultMap (defaultInstr args)
                     else M.empty
      specified = M.fromList $ instrumentSpecs args
   in M.union specified base

main = do
  args <- execParser (info parseArgs mempty)
  before <- L.fromFile (infile args)
  let instrumentMap = mapFromArgs args
  after <- return $ modifyFileWithMap instrumentMap before
  S.toSeekableFile (outfile args) after

