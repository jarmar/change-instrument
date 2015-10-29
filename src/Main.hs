module Main where

import qualified Data.Map.Strict as M
import Data.Monoid
import Data.List (delete)

import Options.Applicative

import qualified Sound.MIDI.File.Load as L
import qualified Sound.MIDI.File.Save as S
import qualified Sound.MIDI.Message.Channel as CM
import qualified Sound.MIDI.Message.Channel.Voice as VM

import Filter
import Instances
import Arguments


testMap :: ChannelInstrumentMap
testMap = M.fromList [(CM.toChannel 0, VM.toProgram 0)
                     ,(CM.toChannel 1, VM.toProgram 0)]

defaultMap :: VM.Program -> ChannelInstrumentMap
defaultMap p =
  M.fromList $ map (\n -> (CM.toChannel n, p)) (delete 9 [0..15])

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

