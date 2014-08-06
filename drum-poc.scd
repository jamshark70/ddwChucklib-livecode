// poc with a few drum parts

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