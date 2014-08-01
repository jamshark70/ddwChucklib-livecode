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
		clPatternSet: "^ *`id(.`id)* = .*"
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
	~isMain = false;

	~process = { |code|
		~eqIndex = code.indexOf($=);
		if(~eqIndex.isNil) {
			Error("patternSet statement has no '=': This should never happen").throw;
		};
		~parseIDs.(code);
		~parsePattern.(code);
		// ~buildStatement.();
		// code.quote
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
			~isMain = true;
		} {
			~parm = test.asSymbol;
		};
	};

	// cases:
	//   - composite (with \)
	//   - no |
	//   - has |
	~cases = [
		{ |code| code.includes($\\) } -> \compositePattern,
		{ |code| code.includes($|) } -> \patternWithDividers,
		true -> \patternWithoutDividers
	];

	~parsePattern = { |code|
		var case;

		code = code[~eqIndex + 1 ..].trim;

		// cases
		case = ~cases.detect { |case| case.key.(code) };
		if(case.isNil) {
			Error("clPatternSet: Pattern does not match any known cases. This should never happen").throw;
		};
		~patString = case.value.envirGet.(code);
	};

	~compositePattern = { |code|
		Error("Composite patterns not supported yet").throw;
	};

	~patternWithDividers = { |code|
		var obj = ~clClass.new(~objKey),
		time = 0, division, value,
		levels = Array.new, times = Array.new, deltas,
		pat = code.findRegexp("\".*\"");
		if(pat.size == 0 or: { pat.first.size <= 0 }) {
			Error("clPatternSet: Couldn't find pattern string").throw
		};
		pat = pat[0][1];
		pat = pat[1 .. pat.size-2].split($|).collectAs(~divideEvents, Array);
		~parseBeatsPerBar.(code, pat);
		pat.do { |seg, i|
			division = ~beatsPerSeg.wrapAt(i) / seg.size;
			seg.do { |event, i|
				value = obj.valueForParm(event, ~parm);
				if(value.notNil) {
					levels = levels.add(value);
					times = times.add(time + (division * i));
				};
			};
			time = time + (division * seg.size);
		};
		#deltas, levels = ~timesToDeltas.(times, levels);
		~deltas = deltas;
		~levels = levels;
		~times = times;
		"Pstep(Pseq(%, 1), Pseq(%, 1), 1)".format(levels.asCompileString, deltas.asCompileString).postcs;
	};

	~patternWithoutDividers = { |code|
		var obj = ~clClass.new(~objKey),
		time = 0, division, value,
		levels = Array.new, times = Array.new, deltas,
		pat = code.findRegexp("\".*\"");
		if(pat.size == 0 or: { pat.first.size <= 0 }) {
			Error("clPatternSet: Couldn't find pattern string").throw
		};
		pat = pat[0][1];
		pat = [pat[1 .. pat.size-2].collect(~divideEvents)];
		~parseBeatsPerBar.(code, pat, $/);
		division = ~beatsPerSeg[0] / pat[0].size;
		pat[0].do { |event, i|
			value = obj.valueForParm(event, ~parm);
			if(value.notNil) {
				levels = levels.add(value);
				times = times.add(time + (division * i));
			};
		};
		#deltas, levels = ~timesToDeltas.(times, levels);
		~deltas = deltas;
		~levels = levels;
		~times = times;
		"Pstep(Pseq(%, 1), Pseq(%, 1), 1)".format(levels.asCompileString, deltas.asCompileString).postcs;
	};

	~divideEvents = { |seg|
		// for now: one char per event
		// later, this will support pitches and ties/slurs
		seg
	};

	// matchOverride needs refactoring
	~parseBeatsPerBar = { |code, pat, matchOverride|
		var obj = ~clClass.new(~objKey),
		quoteI = code.indexOf($"), div, total,
		matchArray = [code.first, matchOverride];
		case { matchArray.any(_.isDecDigit) } {
			~beatsPerBar = code[.. quoteI - 1].interpret; // allows math for bpb
			~beatsPerSeg = [~beatsPerBar / pat.size];
		}
		// additive rhythm: all events are equal duration; assume BP's subdivision if not given
		{ matchArray.any(_ == $+) } {
			div = code[1 .. quoteI - 1].trim;
			if(div.notEmpty) {
				div = div.interpret;
			} {
				div = obj.division ?? { 0.25 };
			};
			~beatsPerSeg = pat.collect({ |events| events.size * div });
			~beatsPerBar = ~beatsPerSeg.sum;
		}
		// divide BP's bar length equally by segments (may create overlapping subdivisions)
		{ matchArray.any(_ == $/) } {
			~beatsPerBar = ~clClass.new(~objKey).beatsPerBar;
			~beatsPerSeg = [~beatsPerBar / pat.size];
		}
		{
			~beatsPerBar = pat.size;
			~beatsPerSeg = [~beatsPerBar];
		};  // default case
		~beatsPerBar
	};

	~timesToDeltas = { |times, levels|
		var deltas = (times ++ ~beatsPerBar).differentiate;
		if(times.first == 0) {
			deltas = deltas.drop(1);
		} {
			deltas[0] = Rest(times[0]);
			levels = levels.insert(0, levels[0]);
		};
		[deltas, levels]
	};
}.import((chucklibLiveCode: #[tokens, replaceRegexpMacros, removeRegexpDups, removeDupIndices]), #[tokens]) => PR(\clPatternSet);
)

BP(\kik).free;
Proto {
	~defaultParm = \dur;
	~valueForParm = { |event| if(event == $x) { 0 } };
	~beatsPerBar = 4;
	~division = 0.25;
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
