module Instances() where

import Data.List (stripPrefix)
import Data.Char (isSpace)

import qualified Sound.MIDI.Message.Channel as CM
import qualified Sound.MIDI.Message.Channel.Voice as VM
import qualified Sound.MIDI.General as GM

-- Orphan instances are kinda bad because we'd get issues if midi decided
-- to define these instances. In the meantime, let's hope they don't.
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
