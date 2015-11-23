
# WebMidi for Elm
**Web MIDI API for Elm language**

 Web MIDI API on [W3C](http://webaudio.github.io/web-midi-api/)

## Start MIDI Synch on Linux

- Start JACK

> qjackcl &

- Add Virtual Keyboard

> vkeybd &

- Chose Synthetizer

> zynaddsubfx &

> qsynth &

.. or both

Link audio inputs an MIDI instruments in `qjackl` UI.

## Examples

1. [List MIDI Ports](examples/ListMIDIPorts.elm)

Demonstrate how to request access to MIDI system.

```elm
  WebMidi.requestMIDIAccess defaultSettings
```

2. [Play a Note](examples/PlayNote.elm)

- Access MIDI sysbsystem
- Open Synch input port and associate it to output port via mailbox
- Send Event with encoded note to the mailbox

```
  synch = "Synth input port (16600:0)"

  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi -> WebMidi.open (withDefault "none"
                                            (selectInstrument synch midi.outputs)) midiOut.signal
           `andThen` \p -> Signal.send midiOut.address (encodeChannelEvent c4on 0)

```
3. [Listent to input events from keyboard](examples/PlayNote.elm)
- Open input port by name. Second argument is `WebMidi.channel` which
  is multiplexed input port for all instruments.
- Listen on all input events: channel and system. Those from system comming on predefined `WebMidi.system` port.

```
  keyboard = "Virtual Keyboard"

  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi ->
             WebMidi.open (withDefault "none" (selectInstrument keyboard midi.inputs)) WebMidi.channel

```
4. Performe music
