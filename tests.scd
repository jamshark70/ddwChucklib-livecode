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