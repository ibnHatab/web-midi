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
    var Signal = Elm.Native.Signal.make(localRuntime);
    var Port = Elm.Native.Port.make(localRuntime);
    var Tuple2 = Elm.Native.Utils.make(localRuntime).Tuple2;

    // global MIDIAccess object
    var midi = null;

    function requestMIDIAccess (settings) {
        return Task.asyncFunction(function(callback) {

            if (! ("requestMIDIAccess" in navigator)) {
                console.error('No Web MIDI support')
                return callback(Task.fail(new Error('No Web MIDI support')));
            }

            function onMIDISuccess (midiAccess) {
                midi = midiAccess;

                var elmMidiAccess =  {
                    _ : {},
                    inputs: Dict.empty,
                    outputs: Dict.empty,
                    sysexEnabled: midiAccess.sysexEnabled
                }

                midiAccess.inputs.forEach(function(port){
                    var value = { _ : {},
                                  name: port.name,
                                  manufacturer:  port.manufacturer,
                                  version: port.version };
                    elmMidiAccess.inputs =
                        A3(Dict.insert, port.id, value, elmMidiAccess.inputs);
                });

                midiAccess.outputs.forEach(function(port){
                    var value = { _ : {},
                                  name: port.name,
                                  manufacturer:  port.manufacturer,
                                  version: port.version };
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


    function open (id, signal) {
        return Task.asyncFunction(function(callback) {
            var dev = midi.inputs.get(id) || midi.outputs.get(id);

            if (! dev) {
                return callback(Task.fail(new Error("No such device found")));
            }

            dev.open().then(
                function(port) {
                    if(port.type === "output") {
                        // FIXME: cascading ports on raise with runtime?
                        var midiOut = Port.outboundSignal("midiOut-" + signal.name,
                                                          function (v) { return v; },
                                                          signal);

                        var midiOutSignal = localRuntime.ports["midiOut-" + signal.name];

                        midiOutSignal.subscribe(function(e) {
                            console.log(port)
                            console.log(e)
                            if ("command" in e) {
                                var status = (e.command << 4) + (e.channel - 1)
                                var message = [status, e.data1];

                                if (e.data2 > 0) { message.push(e.data2); }

                                console.log(message)
                                //port.send(message, e.timestamp);
                                port.send([ 0x90, 0x45, 0x7f ] );
                            } if ("event" in e) {
                                var message = [e.event];
                                if (e.data > 0) { message.push(e.data); }

                                var dev = midi.outputs.get(e.device);
                                dev.send(message);
                            }
                        });
                    } else if (port.type === "input") {

                        if (! signal.id == channelIn.id) {
                            throw new Error(
                                "Signal Error:\n" +
                                    "Can not associate " + signal.name + " with MIDI input.\n" +
                                    "Use WebMidi.channel to open input device."
                            );
                        }

                        port.onmidimessage = function(event) {
                            if (event.data[0] < 240) {      // device and channel-specific message
                                var elmEvent = handleChannelEvent(event);
                                console.log(event.data)
                                console.log(elmEvent)
                                localRuntime.notify(channelIn.id, elmEvent);
                            } else if (e.data[0] <= 255) {  // system message
                                var elmEvent = handleSystemEvent(event);
                                localRuntime.notify(systemIn.id, elmEvent);
                            }
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
            var dev = midi.inputs.get(id) || midi.outputs.get(id);

            if (! dev) {
                return callback(Task.fail(new Error("No such device found")));
            }

            dev.close().then(
                function(port) {
                    if(port.type === "input") {
                        port.onmidimessage = null;
                    }
                    return callback(Task.succeed(port))
                },
                function(error) {
                    return callback(Task.fail(error))
                });
        });
    }

    // EVENT HANDLERS

    function makeChannelMessage(a, b, c, d, e) {
        return {_: {}
                 ,command: a
                 ,data1: b
                 ,data2: c
                 ,channel: d
                 ,timestamp: d
               };
    }

    function makeSystemMessage(a, b, c) {
        return {_: {}
                , data: c
                ,device: b
                ,event: a};
    }

    function handleChannelEvent(e) {
        var command = e.data[0] >> 4;
        var channel = (e.data[0] & 0xf) + 1;
        var data1 = 0, data2 = 0;

        if (e.data.length > 1) {
            data1 = e.data[1];
            data2 = e.data.length > 2 ? e.data[2] : -1;
        }

        return makeChannelMessage(command, data1, data2, channel, e.receivedTime);
    }

    function handleSystemEvent(e) {
        var eventId = e.data[0];
        var data = e.data[1]

        return makeSystemMessage(eventId, data, e.target.id);
    }

    // SIGNALS

    var performance = Signal.input('WebMidi.performance', function () {
        console.error("FIXME")
        performance.now()});

    var channelIn = Signal.input('WebMidi.channel', makeChannelMessage(0,0,0,0,0));

    var systemIn = Signal.input('WebMidi.system', makeSystemMessage(0,0,0));

    return localRuntime.Native.WebMidi.values = {
        requestMIDIAccess: requestMIDIAccess,
        open: F2(open),
        close: close,
        performance: performance,
        channel: channelIn,
        system: systemIn
    };
};
