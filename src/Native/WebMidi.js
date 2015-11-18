Elm.Native.WebMidi = {};
Elm.Native.WebMidi.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.WebMidi = localRuntime.Native.WebMidi || {};

    if (localRuntime.Native.WebMidi.values)
    {
	return localRuntime.Native.WebMidi.values;
    }

    var Dict = Elm.Dict.make(localRuntime);
    var Task = Elm.Native.Task.make(localRuntime);

    var midi = null;  // global MIDIAccess object

    function requestMIDIAccess (settings) {
        return Task.asyncFunction(function(callback) {

            function onMIDISuccess (midiAccess) {
                midi = midiAccess;

                var elmMidiAccess =  {
                    _: {},
                    inputs: Dict.empty,
                    outputs: Dict.empty,
                    sysexEnabled: midiAccess.sysexEnabled
                }

                midiAccess.inputs.forEach(function(port){
                    var value = { ctor: 'MIDIPort', _0: port.name,
                                  _1:  port.manufacturer, _2: port.version }
                    elmMidiAccess.inputs =
                        A3(Dict.insert, port.id, value, elmMidiAccess.inputs);
                });

                midiAccess.outputs.forEach(function(port){
                    var value = { ctor: 'MIDIPort', _0: port.name,
                                  _1:  port.manufacturer, _2: port.version }
                    elmMidiAccess.outputs =
                        A3(Dict.insert, port.id, value, elmMidiAccess.outputs);
                });

		// MIDI device connect/disconnect
		if (settings.onChange.ctor === 'Just')
		{
                    midiAccess.onstatechange = function(event) {
			var task = settings.onChange._0(event.port.id);
			Task.spawn(task);
		    };
	        }

                return callback(Task.succeed(elmMidiAccess))
            }

            navigator.requestMIDIAccess({
                sysex: settings.sysex
            }).then(onMIDISuccess, function(error) {
		return callback(Task.fail(new Error('No Web MIDI support')));
	    });
        })};

    var Signal = Elm.Native.Signal.make(localRuntime);
    var Port = Elm.Native.Port.make(localRuntime);

    function open (id, signal) {
        return Task.asyncFunction(function(callback) {
            var dev = midi.inputs.get(id) || midi.outputs.get(id);
            dev.open().then(
                function(port) {
                    if(port.type === "output") {
                        var midiOut = Port.outboundSignal("midiOut-" + signal.name,
                                                          function (v) {
                                                              return {noteOn: v.noteOn
                                                                      ,pitch: v.pitch};
                                                          },
                                                          signal);

                        var midiOutSignal = localRuntime.ports["midiOut-" + signal.name];

                        midiOutSignal.subscribe(function(note) {
                            console.log(note);
                            port.send([ 0x90, 0x45, 0x7f ] );
                        });
                    } else if (port.type === "input") {
                        port.onmidimessage = function(data) {
                            console.log(data);
                            var note = { ctor: "MidiNote", _0: true, _1: 43 };
                            localRuntime.notify(signal.id, note)
                        }
                    }

                    return callback(Task.succeed(port))
                },
                function(error) {
                    return callback(Task.fail(error))
                } );
        });
    }

    function close (id) {
        return Task.asyncFunction(function(callback) {

        });
    }
    return localRuntime.Native.WebMidi.values = {
        requestMIDIAccess: requestMIDIAccess,
        open: F2(open),
        close: close
    };
};
