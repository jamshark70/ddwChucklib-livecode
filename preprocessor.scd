this.preProcessor = { |code|
	if(code.first == $/) {
		\chucklibLiveCode.eval(code.drop(1))
	} {
		code
	}
};

// main preprocessor
{ |code|
	PR(\chucklibLiveCode)/*.copy?*/.process(code);
} => Func(\chucklibLiveCode);

(
Proto {
	~process = { |code|
		var result;
		block { |break|
			~statements.keysValuesDo { |key, value|
				if(~replaceRegexpMacros.(value).matchRegexp(code)) {
					if((result = key.envirGet).notNil) {
						result = result.value(code);
					} {
						// result = PR(key).copy.process(code);
						// for testing:
						~instance = PR(key).copy;
						result = ~instance.process(code);
					};
					break.(result);
				};
			};
			"Code does not match any known cl-livecode statement template. Ignored.".warn;
			nil
		};
	};

	~tokens = (
		id: "[A-Za-z0-9_]+",
		int: "(-[0-9]+|[0-9]+)"
	);

	~statements = (
		clPatternSet: "`id(.`id)+ = .*"
	);

	// support functions

	// ~replaceRegexpMacros.("`id(.`id)+ = .*");
	// ~replaceRegexpMacros.("blah`id");

	// ~replaceRegexpMacros.("`id(.`id)+ = .*").matchRegexp("kik.k1 = 'xxxx'");

	~replaceRegexpMacros = { |regexp|
		var key;
		// should replace from right to left -- don't break indices
		~removeDupIndices.(regexp.findRegexp("`[a-z0-9]+")).reverseDo { |found|
			// allow escaping "\`"
			if(found[0] == 0 or: { regexp[found[0]-1] != $\\ }) {
				key = found[1].drop(1).asSymbol;
				if(~tokens[key].notNil) {
					// replace only one instance: before match ++ replacement ++ after match
					regexp = "%%%".format(
						if(found[0] > 0) { regexp[.. found[0] - 1] } { "" },
						~tokens[key],
						if(found[0] + found[1].size < regexp.size) {
							regexp[found[0] + found[1].size ..]
						} { "" }
					);
				};
			};
		};
		regexp
	};
	// this assumes duplicates will be adjacent.
	// results of findRegexp appear to be sorted from left to right in the source string
	// so this is *probably* ok.
	~removeRegexpDups = { |regexpResults|
		var out = Array(regexpResults.size).add(regexpResults.first);
		regexpResults.doAdjacentPairs { |a, b|
			if(b != a) { out.add(b) };
		};
		out
	};
	~removeDupIndices = { |regexpResults|
		var out = Array(regexpResults.size).add(regexpResults.first);
		regexpResults.doAdjacentPairs { |a, b|
			if(b[0] != a[0]) { out.add(b) };
		};
		out
	};
} => PR(\chucklibLiveCode);

// statement handlers will use instances, so I can set state variables
Proto {
	~clClass = BP;

	~process = { |code|
		~eqIndex = code.indexOf($=);
		if(~eqIndex.isNil) {
			Error("patternSet statement has no '=': This should never happen").throw;
		};
		~parseIDs.(code);
		~parsePattern.(code);
		// ~buildStatement.();
		code.quote
	};

	~parseIDs = { |code|
		var i, ids, test;
		// everything before ~eqIndex should be the ID string
		ids = code[.. ~eqIndex - 1].split($.).collect(_.trim);  // wslib dependency
		ids = Pseq(ids).asStream;

		// class (I expect this won't be used often)
		test = ids.next;
		if(test.first.isUpper) {
			~clClass = test.asSymbol.asClass;
			test = ids.next;
		};

		// chucklib object key
		~objKey = test.asSymbol;  // really? what about array types?
		if(~clClass.exists(~objKey).not) {
			Error("clPatternSet: %(%) does not exist.".format(~clClass.name, ~objKey.asCompileString)).throw;
		};
		test = ids.next;

		// phrase name
		if(test.isNil) {
			~phrase = \main;
		} {
			~phrase = test.asSymbol;
		};
		test = ids.next;

		// parameter name
		if(test.isNil) {
			~parm = ~clClass.new(~objKey)[\defaultParm] ?? { \main };
		} {
			~parm = test.asSymbol;
		};
	};
}.import((chucklibLiveCode: #[tokens, replaceRegexpMacros, removeRegexpDups, removeDupIndices]), #[tokens]) => PR(\clPatternSet);
)

Proto { ~defaultParm = \dur } => BP(\kik);

/kik.k1 = 'xxxx';
/kik.k1 .xyz = 'xxxx';
/PR.kik.k1.buf = 'xxxx';  // should throw error

/kik = 'xxxx';

PR(\chucklibLiveCode).instance.listVars;

