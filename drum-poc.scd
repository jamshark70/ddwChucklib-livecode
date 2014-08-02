// poc with a few drum parts

(
Proto {
	~event = (eventKey: \singleSynthPlayer);

	~defaultParm = \go;
	~parmMap = (
		go: ($x: 0)
	);
	~beatsPerBar = { ~clock.beatsPerBar };
	~division = 0.25;

	~prep = {
		if(~phrases.isNil) {
			~phrases = (
				main: PbindProxy(\dur, Pfunc { Rest(~clock.beatsPerBar) })
			);
		};
		~userprep.();
		Environment.current
	};
	~freeCleanup = {
		~userfree.();
	};

	~valueForParm = { |event, parm|
		var dict = ~parmMap[parm];
		if(dict.notNil) { dict[event] };  // else nil
	};

	~setPattern = { |phrase, parm, pattern|
		var pat = ~phrases[phrase];
		if(pat.isNil) {
			pat = PbindProxy.new;
			~phrases[phrase] = pat;
		};
		~clock.schedAbs(~clock.nextBar - 0.001, e {
			pat.set(parm, pattern);
			nil
		});
	};

	~asPattern = {
		PnNilSafe(Plazy {
			~phrases[\main]
		}, inf, 20)
	};
} => PR(\abstractLiveCode);
)

(
BP(\kik).free;
PR(\abstractLiveCode).chuck(BP(\kik), nil, (
	userprep: {
		SynthDef(\kik, { |out, freq = 44.444, top = 300, drop = 0.06, time = 1, preamp = 1.5, amp = 1|
			var oscfreq = XLine.kr(top, freq, drop),
			eg = EnvGen.kr(Env([0, 1, 0.8, 0.8, 0], [0.01, 0.06, 0.88, 0.05], -4), timeScale: time, doneAction: 2),
			sig = SinOsc.ar(oscfreq, mul: preamp).tanh * amp * eg;
			Out.ar(out, sig);
		}).add;

		~chan = MixerChannel(~collIndex, s, 1, 2);
		~event.put(\chan, ~chan);
	},
	userfree: { ~chan.free },

	time: 0.25,

	phrases: (
		main: PbindProxy(
			\instrument, \kik,
			\time, BPStream(\time),
			\dur, Pn(Rest(4), inf),  // this will be overridden
		)
	)
));
)

p = BP(\kik);
p.phrases[\main].postcs;

p.play;

TempoClock.tempo = 2;

/kik = "xxxx"

/kik = "x|x  x|x|x"

/kik = "x|x  x|xx| x  "

p.stop;

p.free;