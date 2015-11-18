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


    function requestMIDIAccess (settings) {
        return Task.asyncFunction(function(callback) {

            function onMIDISuccess (midiAccess) {
                console.log("onMIDISuccess")
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

		// start
		if (settings.onStart.ctor === 'Just')
		{
		    req.addEventListener('loadStart', function() {
			var task = settings.onStart._0;
			Task.spawn(task);
		    });
		}


                return callback(Task.succeed(elmMidiAccess))
            }


            navigator.requestMIDIAccess({
                sysex: settings.sysex
            }).then(onMIDISuccess, function(error) {
                console.log("onMIDIError: " + error)
		return callback(Task.fail(new Error('No Web MIDI support')));
	    });
        })};

    return localRuntime.Native.WebMidi.values = {
        requestMIDIAccess: requestMIDIAccess
    };
};
