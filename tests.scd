BP(\kik).free;
Proto {
	~defaultParm = \go;
	~map = ($x: 1, $.: 0.1);
	~valueForParm = { |event| ~map[event] };
	~beatsPerBar = 4;
	~division = 0.25;
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