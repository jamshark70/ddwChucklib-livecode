
BP(\edit).free; PR(\clEditGui).chuck(BP(\edit), nil, (bounds: Rect(800, 50, 500, 600)));
BP(\edit).pages

BP(\edit).win.bounds  // Rect(296, 40, 500, 672)

// performance:
(thisProcess.nowExecutingPath.dirname.dirname +/+ "edit-gui.scd").load;
BP(\edit).free; PR(\clEditGui).chuck(BP(\edit), nil, (bounds: Rect(296, 40, 500, 672)));

BP(\edit).newPage(\z);
BP(\edit).newPage(\a);

/edit(updateDefer)
/edit(updateDefer = 0.1)

Object.dependantsDictionary.keys  // check for stray Protos

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

e.listVars

e.pages[0].sectionForPhraseIndex(nil)
e.phraseAndIndex("main").postcs


/k = "----";  // k, j: OK; j, k: inserts one char early, messes up bookmarks
/j = " - -";

// free, recreate
BP(\k).free;
BP(\j).free;

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


BP(\edit).currentSection.name


// find section view bounds
e = BP(\edit).pages[0].sections[1];
e[1].view.view.bounds;

e = BP(\edit).pages[0];
e.view.children[3].bounds;
e.view.visibleOrigin = e.view.children[3].bounds.leftTop;


(
w = Window("test", Rect(800, 200, 500, 400));
v = View(w, w.view.bounds.insetBy(5, 5))
.layout_(VLayout(
	t = TextField() // TextView()
));
w.front;
)

t.keyDownAction = { |view, char, modifiers, unicode|
	if(char.notNil) { char.ascii.postln };
};
t.keyDownAction = nil;

$/.ascii
(64 + 31).asAscii

v.keyDownAction = nil;

(
w = Window("test", Rect(800, 200, 500, 70));
w.layout = HLayout(
	PopUpMenu()
);
w.front;
)

w.layout.add(u = UserView().fixedSize_(Size(120, 20)));
u.remove;



// tried but failed to reproduce a strange bug in ScrollView
// where ESC navigating to the last section on a page
// caused a light background to appear
// but then it stopped happening

(
{
	v = ScrollView(nil, Rect(800, 200, 500, 400)).front;
	l = VLayout(
		*Array.fill(2, {
			[
				StaticText().string_("x"),
				TextView()
				.background_(Color(0, 0, 0.2))
				.minHeight_(250)
			]
		}).flat
	);
	v.canvas = View().layout_(l).background_(Color.gray(0.15));
	v.background_(Color.gray(0.15));
	1.0.wait;
	v.visibleOrigin_(Point(0, 250));
}.fork(AppClock);
)


// split line problem

(
t = TextView(nil, Rect(800, 200, 500, 400))
.font_(Font.monospace(24))
.string_("if(10.rand.even) { \"it was an even number\".postln } { \"it was an odd number\".postln };")
.select(2, 0)
.front;
)

t.currentLine;



// adding parser-selector
PR(\clEditGui).chuck(BP(\gui), nil, (bounds: Rect(800, 0, 500, Window.screenBounds.height)));
p = BP(\gui);

524288.asHexString(8)

p.listVars;
p.pages[p.currentPage].listVars
p.currentSection.listVars
p.currentSection.strings[0].listVars

p.pages[p.currentPage].sections[1][1].listVars
q = p.pages[p.currentPage].sections[1][1].madSelector;
q.listVars

q.statement.listVars
q.statement.children[1].listVars
q.statement.children[1].children[1].listVars

