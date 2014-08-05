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
	~phraseSeq = \main;

	~prep = {
		if(~phrases.isNil) {
			~phrases = (
				main: Pchain(PbindProxy(\dur, Pfuncn { Rest(~clock.beatsPerBar) }), ~defaults)
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

	~defaults = ();  // or Pbind

	~setPattern = { |phrase, parm, pattern|
		var pat = ~phrases[phrase];
		if(pat.isNil) {
			pat = PbindProxy.new;
			~phrases[phrase] = Pchain(pat, ~defaults);
		} {
			pat = pat.patterns.first;
		};
		~clock.schedAbs(~clock.nextBar - 0.001, e {
			if(parm.notNil) {
				pat.set(parm, pattern);
			} {
				// composite pattern
				~phraseSeq = pattern;
				if(~isPlaying) { ~reschedule.() };
			};
			nil
		});
	};

	~asPattern = {
		Psym(BPStream(\phraseSeq), ~phrases);
		// PnNilSafe(Plazy {
		// 	~phrases[\main]
		// }, inf, 20)
	};

	~reschedule = { |quant(-1)|
		var oldStreamPlayer = ~eventStreamPlayer,
		newStreamPlayer = BP(~collIndex).asEventStreamPlayer;
		~clock.schedAbs(~clock.nextTimeOnGrid(quant) - 0.001, { oldStreamPlayer.stop });
		~clock.play(newStreamPlayer.refresh, quant);
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

	time: 0.12,
	defaults: Pbind(
		\instrument, \kik,
		\time, Plazy { BPStream(\time) }
	),

	phrases: (
		main: Pchain(PbindProxy(
			\dur, Pn(Rest(4), 1)  // this will be overridden
		), Plazy { PnNilSafe(~defaults, inf) })
	)
));
)

p = BP(\kik);
TempoClock.default => p;
p.phrases[\main].postcs;

p.listMethods;

p.play;

TempoClock.tempo = 2;

/kik+

/kik = "xxxx"

/kik = "x|x  x|x|x"

/kik.f1 = "x|x  x|xx| x  "

/kik = .main*3.f1

/kik-

p.stop;
p.isPlaying;

p.free;

p.phrases.inspect