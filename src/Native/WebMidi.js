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
    var List = Elm.List.make(localRuntime);

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


    function enableOutput (id, channelSignal, sytemSignal) {
        return Task.asyncFunction(function(callback) {
            var dev = midi.outputs.get(id);

            if (! dev) {
                return callback(Task.fail(new Error("No such device found")));
            }

            dev.open().then(
                function(port) {
                    var midiOut = Port.outboundSignal("midiOut-" + channelSignal._0.name,
                                                      function (v) { return v; },
                                                      channelSignal._0);

                    var midiOutSignal = localRuntime.ports["midiOut-" + channelSignal._0.name];

                    if(channelSignal.ctor === "Single"){
                        midiOutSignal.subscribe(function(e) {
                            var status = (e.command << 4) + (e.channel - 1)
                            var message = [status, e.data1];

                            if (e.data2 >= 0) { message.push(e.data2); }
                            port.send(message, e.timestamp);
                        });
                    } else if(channelSignal.ctor === "Batch") {
                        midiOutSignal.subscribe(function(es) {
                            while (es.ctor !== '[]') {
                                var e = es._0;
                                var status = (e.command << 4) + (e.channel - 1)
                                var message = [status, e.data1];
                                if (e.data2 >= 0) { message.push(e.data2); }
                                port.send(message, e.timestamp);
			        es = es._1;
                            }
                        });
                    } else {
                        console.error("Impossible signal type");
                    }

                    var sysOut = Port.outboundSignal("sysOut-" + sytemSignal.name,
                                                     function (v) { return v; },
                                                     sytemSignal);

                    var sysOutSignal = localRuntime.ports["sysOut-" + sytemSignal.name];

                    sysOutSignal.subscribe(function(e) {
                        var message = [e.event];
                        if (e.data >= 0) { message.push(e.data); }
                        var dev = midi.outputs.get(e.device);
                        dev.send(message);
                    });

                return callback(Task.succeed(port))
            },
            function(error) {
                return callback(Task.fail(error))
            } );
        });
    }

    function enableInput (id) {
        return Task.asyncFunction(function(callback) {
            var dev = midi.inputs.get(id);

            if (! dev) {
                return callback(Task.fail(new Error("No such device found")));
            }

            dev.open().then(
                function(port) {
                    port.onmidimessage = function(event) {
                        if (event.data[0] < 240) {      // device and channel-specific message
                            var elmEvent = handleChannelEvent(event);
                            localRuntime.notify(channelIn.id, elmEvent);
                        } else if (e.data[0] <= 255) {  // system message
                            var elmEvent = handleSystemEvent(event);
                            localRuntime.notify(systemIn.id, elmEvent);
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

    var getCurrentTime = Task.asyncFunction(function(callback) {
	return callback(Task.succeed(performance.now()));
    });

    var channelIn = Signal.input('WebMidi.channel', makeChannelMessage(0,0,0,0,0));

    var systemIn = Signal.input('WebMidi.system', makeSystemMessage(0,0,0));

    return localRuntime.Native.WebMidi.values = {
        requestMIDIAccess: requestMIDIAccess,
        enableOutput: F3(enableOutput),
        enableInput: enableInput,
        close: close,
        channel: channelIn,
        system: systemIn,
        jiffy: getCurrentTime
    };
};
