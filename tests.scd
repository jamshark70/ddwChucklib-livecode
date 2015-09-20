BP(\kik).free;
Proto {
	~defaultParm = \go;
	~map = ($x: 1, $.: 0.1);
	~valueForParm = { |event| ~map[event] };
	~beatsPerBar = 4;
	~division = 0.25;
	~phrases = (main: PbindProxy(\dur, Rest(4)));
	~setPattern = { |... args| args.asCompileString.debug("setPattern") };
} => BP(\kik);

/kik.k1 = 'xxxx';
/kik.k1 .xyz = 'xxxx';
/PR.kik.k1.buf = 'xxxx';  // should throw error

/kik = 'xxxx';

/kik = "x|x|x|x";
PR(\chucklibLiveCode).instance.listVars;

/kik = " x|x|x|x";  // || segments occupy equal divisions of the bar
/kik = 3"x|x|x|x";  // 3 beats per bar
/kik = /"x|x|x";    // redundant: same as no marker
/kik = +1/4"x x|x |x x";  // additive: each slot is 1/4 beat

/kik = +1/4"x xx x x";  // additive: each slot is 1/4 beat (| = ignored, maybe visually useful)

/kik = "xxxx";

/kik = /"xxxx";
/kik = /"xxx";

/kik = +0.5"x x xx x";

/kik = "x x xx x";

/kik.main.parm2 = "x x xx x";

/kik..parm2 = "x x xx x";

/kik = "x||x xx|";

/kik = "|x|x xx|";


\clParseIntoStatements.eval("x = x + 1; y = y + 1");

\clParseIntoStatements.eval("x = x + 1; // xyz
y = y + 1");

\clParseIntoStatements.eval("x = (x + 1; x + 2); y = y + 1");

\clParseIntoStatements.eval("/kik = \"xxxx\"; /hh = \".-.-.-.-\"");

\chucklibLiveCode.eval("x = x + 1; y = y + 1");

\chucklibLiveCode.eval("/kik.main.go = \"xxxx\";");

\chucklibLiveCode.eval("/kik.main.go = \"xxxx\"; /kik.main.amp = \"x|x|x  .|x\"");


c = CollStream(" abc */ def");
\clParseDelimComment.eval(c);
c.next

c = CollStream(" abc /* def */ ghi */ jkl");
\clParseDelimComment.eval(c);
c.nextN(5);

c = CollStream(" abc");
\clParseDelimComment.eval(c);
c.next;


\clParseIntoStatements.eval("/kik = /* yoo hoo */ \"xxxx\"; /hh = \".-.-.-.-\"");


// regexp for start/stop processes
x = "/a/b/c d e +-1 z y --1".findRegexp(PR(\chucklibLiveCode).replaceRegexpMacros(
	"[+-][`spc]*[\\-0-9\\.]*|`id"
));

x.separate { |a, b| a[1].first.isAlpha.not };


// composite processes
x = PR(\clCompGrouping).copy.process(CollStream("abc.def*3.ghi)"));
x.listVars;
x.clumpOperators;
x.listVars;
x.asPatString;

y = x.asPatString.interpret.asStream;
y.nextN(6);

x = PR(\clCompGrouping).copy.process(CollStream("abc.def|ghi)"));
x.listVars;
x.clumpOperators;
x.listVars;
x.items.listVars;
x.items.items.do(_.listVars);

x = PR(\clCompGrouping).copy.process(CollStream("abc*3.(def|ghi)*5)"));
x.listVars;
x.clumpOperators;
x.listVars;
x.items.listVars;
x.items.items[1].listVars;
x.items.items[1].items.listVars;

x.items.items[2].clumpOperators;
x.items.items[2].listVars;
x.items.items[2].items.listVars;

x = PR(\clCompGrouping).copy.process(CollStream("abc.(def.ghi))")).items;
x[2].items

x = PR(\clCompGrouping).copy.process(CollStream("abc.(def.ghi)*3)*10"));
x.listVars
x.items[2].listVars

x = PR(\clCompGrouping).copy.process(CollStream("abc.(def.ghi*5)*3)*10"));
x.listVars;

x = PR(\clCompGrouping).copy.process(CollStream("abc.def|ghi)"));
x.listVars;
x.splitArray(x.items, $|);


/kik = ".b1*3.f1";



// pitches?
PR(\clPatternSet).copy.process("kik = \"0 |23\"");
PR(\chucklibLiveCode).instance.listVars;

PR(\clPatternSet).copy.process("kik = \"0 23\"");

PR(\clPatternSet).copy.process("kik = \"1 3+4.\"");

PR(\clPatternSet).copy.process("kik = \"0| |2.2. 2.|3\"");

PR(\clPatternSet).copy.process("kik = \" 0.| |2.2. 2.|3\"");


x = PR(\chucklibLiveCode).replaceRegexpMacros("(`id)(\\.`id)? -> (`id)");
"fm -> m1".findRegexp(x);
"fm.m0 -> m1".findRegexp(x);

"fm.m0 -> m1.m2.m3".findRegexp(x);

/kik.main -> m1
/kik.main ->> m1

"kik.main ->> m1".findRegexp(PR(\clXferPattern).copy.idRegexp)

BP(\kik).listVars;

x = PR(\clPatternSet).copy.process("kik = (x)");
x = PR(\clPatternSet).copy.process("kik = (x)*1");

// ok
x = PR(\clPatternSet).copy;
x.process("kik = (x.(a|b))");

x = PR(\clPatternSet).copy;
x.process("kik = (x.a|b)");

x.listVars

// ok?
x = PR(\clPatternSet).copy;
x.process("kik = (x|a.b)*1");


x = PR(\clCompGrouping).copy;
x.process(CollStream("x.(a|b))"));
x.clumpOperators;
x.listVars
x.asPatString;

x = PR(\clCompGrouping).copy;
x.process(CollStream("x|(a.b))"));
x.clumpOperators;
x.listVars;
x.asPatString;

x = PR(\clCompGrouping).copy;
x.process(CollStream("x|(a*3.b))"));
x.clumpOperators;
x.asPatString;

x.items[2].listVars
x.items.items[1].clumpOperators
x.items.items[1].listVars;

x.items.listVars
x.items.items[1].clumpOperators
x.items.items[1].listVars;
x.items.items[1].items.listVars;


x = PR(\clPatternSet).copy;
x.process("bs = (main.rest)");


// weighted rand
x = PR(\clCompGrouping).copy;
x.process(CollStream("x.(a%1|b%2))"));
x.clumpOperators;
x.asPatString;

// the next 2 should be identical: %1*2 or *2%1 doesn't matter
x = PR(\clCompGrouping).copy;
x.process(CollStream("x*3.(a*2%1|b%2))"));
x.clumpOperators;
x.asPatString;

x = PR(\clCompGrouping).copy;
x.process(CollStream("x*3.(a%1*2|b%2))"));
x.clumpOperators;
x.asPatString;

x = PR(\clCompGrouping).copy;
x.process(CollStream("x.(a*2|b))"));
x.clumpOperators;
x.asPatString;


x = PR(\clCompGrouping).copy;
x.process(CollStream("'x.*')"));
x.clumpOperators;
x.asPatString;

x = PR(\clCompGrouping).copy;
x.process(CollStream("'x.*'*5.'y.*'*2)"));
x.clumpOperators;
x.asPatString;

// seems working!
a = (phrases: (a0: 0, a1: 1, b0: 10, b1: 11, b2: 12));

a.use {
	var x = PR(\clCompGrouping).copy;
	x.process(CollStream("'a.*'|'b.*')*1000"));
	x.clumpOperators;
	~pat = x.asPatString.postln.interpret;
	~str = ~pat.asStream;
};

a.str.next;

a.phrases[\a2] = 2;

a.str.next;



x = PR(\clGenerator).copy.process("sn.a *10 /1/2 +3 \"abc\"");
x.listVars
x.idString.join(".")

x = PR(\clGenerator).copy.process("sn.a *10 /1/2 +3'.' \"abc\"");

x = PR(\clGenerator).copy.process("sn.a *10 /1/2 +3. \"abc\"");

x = PR(\clGenerator).copy.process("sn.a *10 /1/2 +\\stepwise \"abc\"");
x = PR(\clGenerator).copy.process("sn.a *10 /1/2 +\\stepwise(1, 2) \"abc\"");
x.listVars

"abc".split($|)

[[1, 2, 3], [4, 5, 6]].reduce('++')


x = PR(\clGenerator).copy.process("sn.a *0 +3 \"|-||-\"");
x.listVars;
x.template[0].size

x = PR(\clGenerator).copy.process("sn.a *0 +3 \"|-|| -\"");

x = PR(\clGenerator).copy.process("sn.a *0 +3 \"|-| ..|\"");

x = PR(\clGenerator).copy.process("sn.a *0 +3 +\"|-| ..|\"");
x = PR(\clGenerator).copy.process("sn.a *0 +3 5/4\"|-| ..|\"");
x.listVars

x = PR(\clGenerator).copy.process("sn.a *2 +3. \"|-||-\"");
x.listVars

x.template[0][0].asCompileString

{ |... args| args.postln; $. } => Func(\testfunc);

x = PR(\clGenerator).copy.process("sn.a *2 +3. \"|-||-\"");

x = PR(\clGenerator).copy.process("sn.a *2 +3\\testfunc(1, 3) \"|-||-\"");
x.scanBackward(x.template, 1, 3).asCompileString;

x.segmentsAsString(x.variants[1]).asCompileString;


x = PR(\clGenerator).copy.process("sn.a *2 +3'1.' \"1_| 1.||\"");

x = PR(\clGenerator).copy.process("bs.a *2 +3'1.' \"1_| 1.||\"");


x = PR(\clPatternSet).copy;

x.decodePitch("4+.");
x.encodePitch(3.1);
x.encodePitch(2.9);
x.encodePitch(10.1);


PR(\chucklibLiveCode).replaceRegexpMacros("^ *`id(\\.|`id|`id\\*[0-9]+)* = .*")


// *n for copy/xfer

PR(\chucklibLiveCode).v.listVars

r = PR(\chucklibLiveCode).replaceRegexpMacros("(`id)(%.`id)? % (`id)".format($\\, "->"));
r = PR(\chucklibLiveCode).replaceRegexpMacros("(`id)(%.`id|%.`id%*`int)? % (`id)".format($\\, $\\, $\\, "->"));

"/bs.m -> n".findRegexp(r);
"/bs -> n".findRegexp(r);
"/bs.m*4 -> n".findRegexp(r);

/make(tablaBP)
/tb.m = "!|11|1#| 1";

x = PR(\clCopyPattern).copy;
x.parseIDs("/tb.m -> n");
x.listVars;

x = PR(\clCopyPattern).copy;
x.parseIDs("/tb.m*2 -> n");
x.listVars;

PR(\clCopyPattern).copy.process("/tb.m -> n");
PR(\clCopyPattern).copy.process("/tb.m*2 -> n");

/bars.(\tb, 2);
/tb.m0 = "!|11|1#| 1";
/tb.m1 = "  33|3 23| 23 |3#  ";
/tb+8

/tb.m*2 -> n

/tb(phrases)

/tb.n0 = "!|3333|13 3| #";
/tb.n1 = "  33|3 23| 23 |3#  ";

/setm.(\tb, 2, \n);

/setm.(\tb, 2, \m);

/tb-8

/tb(phraseSeq.asCompileString)

/tb = (m0.m1);

PR(\clXferPattern).copy.process("tb.m*2 ->> n");

/tb.m*2 ->> n


// pass quant with composite pattern
e = PR(\clPatternSet).copy;
e.process("tb = (m0.m1)(-2)");
e.listVars

e[\stream].dump
e[\stream].collection[6..]

PR(\clPatternSet).listMethods


e = PR(\clMake).copy;
e.process("make(cricketBP:cl/cricketBP)")



// this should work.
/drone = ((a0.a1)%3|(b0.b1)%1);

x = PR(\clCompGrouping).copy;
x.process(CollStream("x.(a%1|b%2))"));

x = PR(\clCompGrouping).copy;
x.process(CollStream("x.(y|z)*2)"));
x.clumpOperators;
x.asPatString;

x = PR(\clCompGrouping).copy;
x.process(CollStream("(a0.a1)%3|(b0.b1)%1)"));
x.clumpOperators;
x.asPatString;

x = PR(\clCompGrouping).copy;
x.process(CollStream("a0%3|b0%1)"));
x.clumpOperators;
x.asPatString;

x = PR(\clCompGrouping).copy;
x.process(CollStream("(a0|a1)%3|b0%1)"));
x.listVars

x.items[0].listVars

x.clumpOperators;
x.asPatString;

x = PR(\clCompGrouping).copy;
x.process(CollStream("(a0.a1)%3|(b0.b1)%1)"));
x.listVars
x.items[0].listVars

x.clumpOperators;
x.asPatString;

p = PR(\clPatternSet).copy;
p.process("drone = ((a0.a1)%3|(b0.b1)%1)");
p.listVars
p.group.listVars
p.group.items[0][0].listVars



// double-star in compGroups
(
~checkDoubleStar = { |stream|
	var str = stream.collection, regex, id, num, index;
	regex = str.findRegexp("([A-Za-z0-9_]+)\\*\\*([0-9]+)");
	if(regex.notNil) {
		regex.clump(3).reverseDo { |triplet|
			id = triplet[1][1];
			num = triplet[2][1].asInteger;
			index = triplet[0][0];
			str = "%(%)%".format(
				if(index > 0) { str[ .. index - 1] } { "" },
				Array.fill(num, { |i| "'^%%'".format(id, i) }).join("."),
				if(index + triplet[0][1].size < (str.size)) {
					str[index + triplet[0][1].size .. ]
				} { "" }
			);
		};
	};
	stream.collection = str;
};
)

x = CollStream("a.(b|c*2))");
~checkDoubleStar.(x);
x.collection;

x = CollStream("a**3.(b|c**2))");
~checkDoubleStar.(x);
x.collection;

x = CollStream("a**2.b**2)");
~checkDoubleStar.(x);
x.collection;

x = CollStream("a**3x");
~checkDoubleStar.(x);
x.collection;

y = PR(\clCompGrouping).copy;
y.process(x);
y.clumpOperators;
y.asPatString;

y = PR(\clCompGrouping).copy;
y.process(CollStream("a**3.(b|c**2))"));
y.clumpOperators;
y.asPatString;

y = PR(\clCompGrouping).copy;
y.process(CollStream("a**2.b**2)"));
y.clumpOperators;
y.asPatString;

y = PR(\clCompGrouping).copy;
y.process(CollStream("(a%4|b)*4.(a|b%4)*2)"));
y.clumpOperators;
y.asPatString;

y = PR(\clCompGrouping).copy;
y.process(CollStream("x.a|b)"));
y.clumpOperators;
y.asPatString;


// multi-bar transfer
/k(free);

(
PR(\abstractLiveCode).chuck(BP(\k), nil, (
	defaultParm: \amp,
	parmMap: (amp: ($-: 0.5, $.: 0.1))
));
)

/bars.(\k, 4, \a);
/k.a0 = "-|| -|";
/k.a1 = "-  -| -| -|";
/k.a2 = "-|| - -|";
/k.a3 = "-  -| - -| -|";

/k(phraseSeq.asCompileString)

/k.a*4 ->> b;

/k(phraseSeq.asCompileString)

/bars.(\k, 4, \p);
/k.p0 = "-|| -|";
/k.p1 = "-  -| -| -|";
/k.p2 = "-|| - -|";
/k.p3 = "-  -| - -| -|";

/k.p*4 ->> q;

/k.q0 = "-|| -|";


"a?b".matchRegexp("ab");
"a?b".matchRegexp("b");

/BP.k(listVars);

x = PR(\chucklibLiveCode);
y = x.replaceRegexpMacros(x.statements.detect { |item| item.key == \clPassThru }.value);

y.matchRegexp("BP.k(listVars)");

Voicer(1, \default) => VC(\d);

/VC.d(inspect);
v.releaseAll



// Setting "|||" to an isPitch parm produces an invalid pattern :bug:
(
BP(\x).free;
PR(\abstractLiveCode).chuck(BP(\x), nil, (
	defaultParm: \a,
	parmMap: (a: (isPitch: true))
));
)

/x = "|||";
/x = " ";



// moving value conversion to process
// when is it an array?
ChuckableBrowser.newWindow;

(
BP(\k).free;
PR(\abstractLiveCode).chuck(BP(\k), nil, (
	defaultParm: \test,
	parmMap: (
		test: (
			alias: #[bufnum, rate],
			$a: [[0, 1]],
			$b: [[1, 2]]
		)
	)
));
)

/k = "ab";


// test move
(
BP(\k).free;
PR(\abstractLiveCode).chuck(BP(\k), nil, (
	event: (
		parent: (
			play: { Event.current.postln }
		)
	),
	defaultParm: \test,
	parmMap: (
		test: (
			$a: 1,
			$b: 2
		),
		alias: (
			alias: \xyz,
			$a: 1,
			$b: [2, 3]
		),
		array: (
			alias: #[bufnum, rate],
			$a: [0, 1],
			$b: [1, 2]
		),
		pitch: (isPitch: true)
	)
));
)

/k = "";

/k = "a  b";
/k..pitch = "1  3+";
/k..alias = "ab  ";
/k..array = "a| b";

BP(\k).prepareForPlay;
e = BP(\k).eventStream.next(BP(\k).event.copy);

/k+
/k-


/make(baseDrone);
/drone+;

/make(bpfBP);
/bpf => t;
/bars.(\bpf, 2, \a);
/bpf.a0 = "ooo |h  h| u| u";
/bpf.a1 = "";

/bpf+;
/bpf-;

// a0 above -- this is wrong:
"Pstep(Pseq(List[ $o, $o, $o, $ , $h, $ , $ , $h, $ , $u, $ , $u ], 1), Pseq([ 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.5 ], 1), 1)"

/bpf(free);


// generator tests
(
"preprocessor-generators.scd".resolveRelative.loadPath;

{ |items| Prand(items, inf) } => Func(\rand);
{ |items| Pxrand(items, inf) } => Func(\xrand);
{ (Pwhite(1, 4, inf) * 0.25).trace(prefix: "rhythm: ") } => Func(\rhythm);
)

a = PR(\clGen).copy.putAll((args: (items: "abcde")));

b = a.asStream;
b.next;

// oh ok, this works as expected!
(
fork {
	var stream = Pstep(
		Pseries(0, 1, inf),
		Pn(1, 4)
	).asStream,
	next, start = thisThread.clock.beats;
	while { (next = stream.next).notNil } {
		next.postln;
		1.0.wait;
	};
	(thisThread.clock.beats - start).debug("Stopped after beats");
};
)

(
a = PR(\clStepGen).copy.putAll((
	valueGen: PR(\clGen).copy.putAll((dur: 2, args: (items: "abcde"))),
	rhythmGen: PR(\clGen).copy.putAll((dur: 2, func: \rhythm))
));

fork {
	var stream = a.asStream;
	var next, start = thisThread.clock.beats;
	while { (next = stream.next((test: true))).notNil } {
		[next, thisThread.clock.beats - start].postln;
		0.25.wait;
	};
	(thisThread.clock.beats - start).debug("Stopped after beats");
};
)

(
a = PR(\clRhythmGen).copy.putAll((
	valueGen: PR(\clGen).copy.putAll((func: \xrand, args: (items: "abcde"))),
	rhythmGen: PR(\clGen).copy.putAll((func: \rhythm))
));

fork {
	var stream = a.asStream;
	var next, start = thisThread.clock.beats;
	while { (next = stream.next((test: true))).notNil } {
		[next, thisThread.clock.beats - start].postln;
		next[1].wait;
	};
	(thisThread.clock.beats - start).debug("Stopped after beats");
};
)

