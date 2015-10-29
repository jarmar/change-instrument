module Arguments (
  Args (..)
  , parseArgs
  ) where

import Options.Applicative

import qualified Sound.MIDI.Message.Channel as CM
import qualified Sound.MIDI.Message.Channel.Voice as VM

import Instances

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
  <> metavar "\"(CH, INSTR)\""
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
