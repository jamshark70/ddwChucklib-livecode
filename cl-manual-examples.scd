(
PR(\abstractLiveCode).chuck(BP(\beep), nil, (
	userprep: {
		~buf = Buffer.read(
			s,
			Platform.resourceDir +/+ "sounds/a11wlk01.wav",
			4982, 10320
		);
		SynthDef(\buf1, { |out, bufnum, pan, time = 0.1, amp|
			var sig = PlayBuf.ar(1, bufnum),
			eg = EnvGen.kr(
				Env.linen(0.02, time - 0.02, 0.02),
				doneAction: 2
			);
			Out.ar(out, Pan2.ar(sig, pan, amp * eg));
		}).add;
		~defaults[\bufnum] = ~buf;
	},
	userfree: {
		~buf.free;
	},
	defaultParm: \amp,
	parmMap: (
		amp: ($.: 0.1, $-: 0.4, $^: 0.8),
		pan: (
			$<: -0.9, $>: 0.9,
			$(: -0.4, $): 0.4,
			$-: 0
		)
	),
	defaults: (instrument: \buf1),
	postDefaults: Pbind(
		\time, (Pkey(\dur) * 0.6 / Pfunc { ~clock.tempo }).clip(0.04, 0.2)
	)
));
)

TempoClock.tempo = 2;

/beep = "^|.. .| .- | .  ";
/beep+;

/beep..pan = "<><><><>";

/beep-;

/beep(free);