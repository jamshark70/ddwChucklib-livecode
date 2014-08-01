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
					result = key.envirGet.value(code);
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
		patternSet: "`id(.`id) = .*"
	);

	~patternSet = { |code|
		"did patternSet".debug;
	};

	// support functions

	// ~replaceRegexpMacros.("`id(.`id)+ = .*");
	// ~replaceRegexpMacros.("blah`id");

	// ~replaceRegexpMacros.("`id(.`id)+ = .*").matchRegexp("kik.k1 = 'xxxx'");

	~replaceRegexpMacros = { |regexp|
		var key;
		// should replace from right to left -- don't break indices
		~removeRegexpDups.(regexp.findRegexp("`[a-z0-9]+")).reverseDo { |found|
			// allow escaping "\`"
			if(found[0] == 0 or: { found[1][found[0]-1] != $\ }) {
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
} => PR(\chucklibLiveCode);
)

PR(\chucklibLiveCode).process("kik.k1 = 'xxxx'");
PR(\chucklibLiveCode).process("flippityblargh");
