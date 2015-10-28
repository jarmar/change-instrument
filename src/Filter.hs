module Filter where

import qualified Data.Map.Strict as M

import qualified Data.EventList.Relative.TimeBody as EL
import qualified Sound.MIDI.File as File
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as CM
import qualified Sound.MIDI.Message.Channel.Voice as VM

type ChannelInstrumentMap = M.Map CM.Channel CM.Program

modifyFileWithMap :: ChannelInstrumentMap -> File.T -> File.T
modifyFileWithMap m (File.Cons t d tracks) =
  File.Cons t d $
    map (EL.mapBody (modifyEventWithMap m)) tracks

modifyEventWithMap :: ChannelInstrumentMap -> E.T -> E.T
modifyEventWithMap m evt =
  case evt of
       E.MIDIEvent msg -> E.MIDIEvent $ modifyChannelMessageWithMap m msg
       _               -> evt

modifyChannelMessageWithMap :: ChannelInstrumentMap -> CM.T -> CM.T
modifyChannelMessageWithMap m msg@(CM.Cons channel body) =
  case body of
       CM.Mode _     -> msg
       CM.Voice vmsg ->
         CM.Cons channel (CM.Voice $ modifyVoiceMessageWithMap m channel vmsg)

modifyVoiceMessageWithMap :: ChannelInstrumentMap -> CM.Channel -> 
                             VM.T -> VM.T
modifyVoiceMessageWithMap m ch msg =
  case msg of
       VM.ProgramChange p ->
         VM.ProgramChange $ M.findWithDefault p ch m
       _                  ->
         msg
