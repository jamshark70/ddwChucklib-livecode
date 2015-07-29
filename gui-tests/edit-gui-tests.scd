
BP(\edit).free; PR(\clEditGui).chuck(BP(\edit), nil, (bounds: Rect(800, 50, 500, 600)));
BP(\edit).pages

BP(\edit).newPage(\z);
BP(\edit).newPage(\a);

/edit(updateDefer)
/edit(updateDefer = 0.1)

// test

/k(free)

(
PR(\abstractLiveCode).chuck(BP(\k), nil, (
	defaultParm: \amp,
	parmMap: (amp: ($-: 0.5, $.: 0.1))
));

PR(\abstractLiveCode).chuck(BP(\j), nil, (
	defaultParm: \amp,
	parmMap: (amp: ($-: 0.5, $.: 0.1))
));
)

e = BP(\edit);
z = e.pages[0].sections[0][0];
z.strings.do(_.listVars);

e.pages[0].sectionForPhraseIndex(nil)
e.phraseAndIndex("main").postcs


/k = "----";  // k, j: OK; j, k: inserts one char early, messes up bookmarks
/j = " - -";

/k = "|-  -|--| -  ";
/j = "|-  -||-";
1+1

/k.main0 = "-| -|-|";

/k.main3 = "-| -|-|";
/j.main3 = " - -";

/k.main2 = "-| -|-|";

/j.main1 = "-";
/k.main1 = "--";


/k.a0 = "-|   -| -- |";

/k.d1 = "-| -|-  -| -  ";


// highlighting
NotificationCenter.notify(\k, \main, true);
NotificationCenter.notify(\k, \main, false);

NotificationCenter.notify(\k, \main0, true);
NotificationCenter.notify(\k, \main0, false);

NotificationCenter.notify(\k, \main3, true);
NotificationCenter.notify(\k, \main3, false);

NotificationCenter.registrationsFor(\k, \main);



// something that will really play
s.boot;

BP(\x).free;

(
PR(\abstractLiveCode).chuck(BP(\x), nil, (
	defaultParm: \amp,
	parmMap: (amp: ($-: 0.5, $.: 0.1)),
	userprep: {
		~chan = MixerChannel(~collIndex, s, 2, 2, 0.5);
		~buf = Buffer.readAndQuery(s, "/home/dlm/share/SC/sounds/drums/Tabla Kit Small/tin.wav",
			completionFunc: e { ~defaults = (bufnum: ~buf, time: ~buf.duration) });
		~event.put(\chan, ~chan).put(\instrument, \bufGrainPan);
	},
	userfree: {
		[~chan, ~buf].free;
	}
));
)


/x = "-| -|-|";

/x+
/x-

/x.a0 = "-  -| . .- - |-  .  - | .  -. -";
/x.a1 = "- ..| .- -. -| .- -  .| . .";

/x = (a**2);

BP(\x).phrases.keys