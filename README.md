
# WebMidi for Elm
**Web MIDI API for Elm language**

 Web MIDI API on [W3C](http://webaudio.github.io/web-midi-api/)

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

```elm
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

```elm
  keyboard = "Virtual Keyboard"

  WebMidi.requestMIDIAccess defaultSettings
           `andThen` \midi ->
             WebMidi.open (withDefault "none" (selectInstrument keyboard midi.inputs)) WebMidi.channel


  main =
    Signal.map show (Signal.map2 (,) WebMidi.channel WebMidi.system)

```
4. [Perform music](examples/PerformMusic.elm)

- Eccess MIDI out port as in Ex. 2
- Chaine `WebMidi.jiffy` task which fetch current `performance.now()` time from browser.
- Use `jiffy` as time reference to serialize `track` of MIDI events usimg task sequencer
- Events `track` extracted from MIDI File structure which resemble
  MIDI Type 1 file with stream per instrument track list.



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

![Configure JACK connections](demo/MIDI-on-Linux.png)
