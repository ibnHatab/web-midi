
# WebMidi for Elm
**Web MIDI API for Elm language**

 Web MIDI API on [W3C](http://webaudio.github.io/web-midi-api/)

## Implementation details
Obtaining Access to MIDI Devices implemented as Task to bind it nicely to promises
returned by `requestMIDIAccess()`, `open()` and `close()` methods.
The `onstatechange` of MIDIAccess object fire task provided to `WebMidi.requestMIDIAccess` in
parameters hash.
`WebMidi.open` on output device bind it with `Signal.mailbox` of `MidiMessage`.
All events from input deviced multiplexed on two input `Signal`: `WebMidi.channel`
and `WebMidi.system`.
Channel messages coreponds to Note events and have arout 20ms jitter time until become
sensible to processing delay. System messages can be handled in 1s message loop.
When opening input device please give it `WebMidi.channel` as `port` parameter.





## Examples

### [List MIDI Ports](examples/ListMIDIPorts.elm)
   Demonstrate how to request access to MIDI system.

```elm
  WebMidi.requestMIDIAccess defaultSettings
  ```

[DEMO](https://raw.githack.com/ibnHatab/WebMidi/master/demo/ListMIDIPorts.html)

### [Play a Note](examples/PlayNote.elm)

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

[DEMO](https://raw.githack.com/ibnHatab/WebMidi/master/demo/PlayNote.html)

### [Listent to input events from keyboard](examples/InputEventsFromKbd.elm)
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

[DEMO](https://raw.githack.com/ibnHatab/WebMidi/master/demo/InputEventsFromKbd.html)

### [Perform music](examples/PerformMusic.elm)

- Eccess MIDI out port as in Ex. 2
- Chaine `WebMidi.jiffy` task which fetch current `performance.now()` time from browser.
- Use `jiffy` as time reference to serialize `track` of MIDI events via task sequencer
- Events `track` extracted from MIDI File structure which resemble
  MIDI Type 1 file with stream per instrument track list.

- Compose simple tune


```elm
cMaj = [c,e',g] |> List.map (\n -> n 4 hn)

cMajArp = Music.line  cMaj
cMajChd = Music.chord cMaj

tune : Music
tune = (Music.repeatM 3 cMajArp) :+: cMajChd

```

- Convert it to performance

```elm
ctx : Context
ctx = Context 0 AcousticGrandPiano 3 0

performance : Performance
performance = performM ctx tune

```

[DEMO](https://raw.githack.com/ibnHatab/WebMidi/master/demo/PerformMusic.html)


## Configuring MIDI Synch on Linux

- Start JACK

> qjackcl &

- Add Virtual Keyboard

> vkeybd &

- Chose Synthetizer

> zynaddsubfx &

> qsynth &


Link audio inputs and MIDI instruments in `qjackl` UI.

![Configure JACK connections](demo/MIDI-on-Linux.png)
