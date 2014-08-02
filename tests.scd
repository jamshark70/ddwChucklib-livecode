BP(\kik).free;
Proto {
	~defaultParm = \go;
	~valueForParm = { |event| if(event == $x) { 0 } };
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

/kik = " x|x|x|x";
/kik = 3"x|x|x|x";
/kik = /"x|x|x";
/kik = +1/4"x x|x |x x";

/kik = "xxxx";

/kik = /"xxxx";
/kik = +0.5"x x xx x";

/kik = "x x xx x";

/kik.main.parm2 = "x x xx x";
