this.preProcessor = { |code|
	if(code.first == $/) {
		\chucklibLiveCode.eval(code)
	} {
		code
	}
};

// main preprocessor
{ |code|
	code = \clParseIntoStatements.eval(code);
	code.do { |stmt, i|
		if(stmt.first == $/ and: { stmt[1] != $/ }) {
			code[i] = PR(\chucklibLiveCode)/*.copy?*/.process(stmt.drop(1));
		};
	};
	code.join(";\n");
} => Func(\chucklibLiveCode);

// remove slashes and semicolons
// separate strings
(
{ |code|
	var escape = false, betweenStmts = true, // quote = false, squote = false,
	paren = 0, brackets = 0, braces = 0,
	ch, ch2, statements = Array(), str = String.new, continue;
	code = CollStream(code);
	while { (ch = code.next).notNil } {
		case
		{ ch == $; and: { [paren, brackets, braces].every(_ == 0) } } {
			statements = statements.add(str);
			str = String.new;
			betweenStmts = true;
		}
		{ ch == $/ } {
			ch2 = code.next;
			switch(ch2)
			{ $/ } {
				while { (ch = code.next).notNil and: { ch != $\n } };
				code.pos = code.pos - 1;
			}
			// a separate function allows recursion for nesting
			{ $* } { \clParseDelimComment.eval(code) }
			{ if(ch2.notNil) { str = str.add(ch).add(ch2); betweenStmts = false; } };
		}
		{ betweenStmts and: { ch.isSpace.not } } {
			betweenStmts = false;
			str = String.with(ch);  // remove dead matter between statements
		}
		{ betweenStmts /*and: { ch.isSpace }*/ } {
			nil
		}
		{ ch == $\" } {
			continue = true;
			str = str ++ ch;
			while { continue and: { (ch = code.next).notNil } } {
				switch(ch)
				{ $\\ } {
					escape = escape.not;
				}
				{ $\" } {
					str = str ++ ch;
					if(escape) {
						escape = false;
					} {
						continue = false;
					}
				}
				{ str = str ++ ch }
			}
		}
		{ ch == $\" } {
			continue = true;
			str = str ++ ch;
			while { continue and: { (ch = code.next).notNil } } {
				switch(ch)
				{ $\\ } {
					escape = escape.not;
				}
				{ $\" } {
					str = str ++ ch;
					if(escape) {
						escape = false;
					} {
						continue = false;
					}
				}
				{ str = str ++ ch }
			}
		}
		// because a ; inside grouping delimiters should not end a statement
		{ ch == $( } { str = str ++ ch; paren = paren + 1 }
		{ ch == $) } { str = str ++ ch; paren = paren - 1 }
		{ ch == $[ } { str = str ++ ch; brackets = brackets + 1 }
		{ ch == $] } { str = str ++ ch; brackets = brackets - 1 }
		{ ch == ${ } { str = str ++ ch; braces = braces + 1 }
		{ ch == $} } { str = str ++ ch; braces = braces - 1 }
		{ str = str ++ ch };  // default case
	};
	statements.add(str)
} => Func(\clParseIntoStatements);

// 'code' is a CollStream
// assumes we've already read the slash-star
{ |code|
	var ch, ch2, continue = true;
	while { (ch = code.next).notNil and: { continue } } {
		switch(ch)
		{ $/ } {
			ch2 = code.next;
			if(ch2 == $*) { \clParseDelimComment.eval(code) };  // recursion
		}
		{ $* } {
			ch2 = code.next;
			if(ch2 == $/) { continue = false };
		}
	};
	if(ch.notNil) { code.pos = code.pos - 1 };
	code
} => Func(\clParseDelimComment);
);

(
Proto {
	~process = { |code|
		var result;
		block { |break|
			~statements.do { |assn|
				if(~replaceRegexpMacros.(assn.value).matchRegexp(code)) {
					if((result = assn.key.envirGet).notNil) {
						result = result.value(code);
					} {
						// result = PR(key).copy.process(code);
						// for testing:
						~instance = PR(assn.key).copy;
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
		al: "A-Za-z",
		dig: "0-9",
		id: "[A-Za-z][A-Za-z0-9_]*",
		int: "(-[0-9]+|[0-9]+)",
		// http://www.regular-expressions.info/floatingpoint.html
		float: "[\\-+]?[0-9]*\\.?[0-9]+([eE][\\-+]?[0-9]+)?",
		spc: " 	"  // space, tab, return
	);

	~statements = [
		\clPatternSet -> "^ *`id(\\.|`id)* = .*",
		\clGenerator -> "^ *`id(\\.|`id)* \\*.*",
		\clXferPattern -> "^ *`id(\\.`id)? ->>",  // harder match should come first
		\clCopyPattern -> "^ *`id(\\.`id)? ->",
		\clStartStop -> "^([/`spc]*`id)+[`spc]*[+-]"
	];

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
	~isPitch = false;
	// note: ~parm will be set to nil for composite patterns

	~process = { |code|
		~eqIndex = code.indexOf($=);
		if(~eqIndex.isNil) {
			Error("patternSet statement has no '=': This should never happen").throw;
		};
		~parseIDs.(code);
		~parsePattern.(code);
		~buildStatement.();
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
		if(test.size == 0) {
			~phrase = \main;
		} {
			~phrase = test.asSymbol;
		};
		test = ids.next;

		// parameter name
		if(test.size == 0) {
			~parm = ~clClass.new(~objKey)[\defaultParm] ?? { \main };
			~isMain = true;
		} {
			~parm = test.asSymbol;
			~isMain = (~parm == ~clClass.new(~objKey)[\defaultParm]);
		};
		// if the target object doesn't exist, we would already have thrown an error
		// so no need to check again
		~isPitch = ~clClass.new(~objKey).v.tryPerform(\parmIsPitch, ~parm) ?? { false };
		currentEnvironment
	};

	// cases:
	//   - composite
	//   - no |
	//   - has |
	~cases = [
		{ |code|
			var i;
			if(code[0] == $") { i = 1 } { i = 0 };
			// ".(".includes(code[i])  // want to use '.' as a segment char too
			code[i] == $(
		} -> \compositePattern,
		{ |code| code.includes($|) } -> \patternWithDividers,
		true -> \patternWithoutDividers
	];

	~parsePattern = { |code|
		var case;

		code = code[~eqIndex + 1 ..].trim;
		// code.asCompileString.debug("code");

		// cases
		case = ~cases.detect { |case| case.key.(code) };
		if(case.isNil) {
			Error("clPatternSet: Pattern does not match any known cases. This should never happen").throw;
		};
		~patString = case.value.envirGet.(code);
	};

	~compositePattern = { |code|
		var stream, group;
		~isMain = false;
		~isPitch = false;
		~parm = nil;
		code = ~unquoteString.(code);
		// code = code.drop(1).drop(-1);
		if(code.first != $() {
			code = code.drop(1) ++ $);
		} {
			code = code.drop(1);
		};
		stream = CollStream(code);
		group = PR(\clCompGrouping).copy.process(stream);
		group.clumpOperators;
		if(group.repeats.isNil) { group.repeats = inf };
		group.asPatString;
	};
	~unquoteString = { |str, pos = 0, delimiter = $"|
		var i = str.indexOf(delimiter), j, escaped = false;
		if(i.isNil) {
			str
		} {
			j = i;
			while {
				j = j + 1;
				j < str.size and: {
					escaped or: { str[j] != delimiter }
				}
			} {
				if(str[j] == $\\) { escaped = escaped.not } { escaped = false };
			};
			str[i + 1 .. j - 1];
		};
	};

	~patternWithDividers = { |code|
		var obj = ~clClass.new(~objKey),
		time = 0, division, value,
		levels = List.new, times = List.new, legatos = List.new, deltas, i,
		pat = code.findRegexp("\".*\"");
		if(pat.size == 0 or: { pat.first.size <= 0 }) {
			Error("clPatternSet: Couldn't find pattern string").throw
		};
		pat = pat[0][1];
		pat = pat[1 .. pat.size-2].split($|).collectAs(~divideEvents, Array);
		~segments = pat;
		~parseBeatsPerBar.(code, pat);
		i = 0;
		pat.do { |seg, segI|
			if(seg.size >= 1) {
				division = ~beatsPerSeg.wrapAt(segI) / seg.size;
				seg.do { |event, i|
					value = ~valueForParm.(event, obj, ~parm);
					if(value.size > 0) {
						division = division / value.size;
						value.do { |item|
							~prAddEntries.(item, levels, times, legatos, time, division, i);
							i = i + 1;
						};
					} {
						~prAddEntries.(value, levels, times, legatos, time, division, i);
						i = i + 1;
					};
				};
			};
			time = time + ~beatsPerSeg.wrapAt(segI);
		};
		#deltas, levels, legatos = ~timesToDeltas.(times, levels, legatos);
		~deltas = deltas;
		~levels = levels;
		~times = times;
		~legatos = legatos;
		"Pstep(Pseq(%, 1), Pseq(%, 1), 1)".format(levels.asCompileString, deltas.asCompileString).postcs;
	};

	~patternWithoutDividers = { |code|
		var obj = ~clClass.new(~objKey),
		time = 0, division, value,
		levels = List.new, times = List.new, legatos = List.new, deltas, i,
		pat = code.findRegexp("\".*\"");
		if(pat.size == 0 or: { pat.first.size <= 0 }) {
			Error("clPatternSet: Couldn't find pattern string").throw
		};
		pat = pat[0][1];
		// pat = [pat[1 .. pat.size-2].collectAs(~divideEvents, Array)];
		pat = [~divideEvents.(pat[1 .. pat.size-2])];
		~segments = pat;
		~parseBeatsPerBar.(code, pat, $/);
		division = ~beatsPerSeg[0] / pat[0].size;
		i = 0;
		pat[0].do { |event, i|
			value = ~valueForParm.(event, obj, ~parm);
			if(value.size > 0) {
				division = division / value.size;
				value.do { |item|
					~prAddEntries.(item, levels, times, legatos, time, division, i);
					i = i + 1;
				};
			} {
				~prAddEntries.(value, levels, times, legatos, time, division, i);
				i = i + 1;
			};
		};
		#deltas, levels, legatos = ~timesToDeltas.(times, levels, legatos);
		~deltas = deltas;
		~levels = levels;
		~times = times;
		~legatos = legatos;
		"Pstep(Pseq(%, 1), Pseq(%, 1), 1)".format(levels.asCompileString, deltas.asCompileString).postcs;
	};

	~prAddEntries = { |value, levels, times, legatos, time, division, i|
		case
		{ value.isKindOf(Association) } {
			levels.add(value.key);
			times.add(time + (division * i));
			legatos.add(value.value);
		}
		{ value.notNil } {
			levels.add(value);
			times.add(time + (division * i));
			legatos.add(0.98);
		};
		[levels, times, legatos]  //.debug("after prAddEntries");
	};

	~divideEvents = { |seg|
		// for now: one char per event
		// later, this will support pitches and ties/slurs
		var stream = CollStream(seg), ch, str,
		result = Array.new;
		// seg.asCompileString.debug(">> divideEvents");
		while { (ch = stream.next).notNil } {
			if(~isPitch and: { ch.isDecDigit }) {
				// ~isPitch = true;
				str = String.with(ch);
				while { (ch = stream.next).notNil and: { "+-',~_.".includes(ch) } } {
					str = str.add(ch);
				};
				result = result.add(str);
				if(ch.notNil) { stream.pos = stream.pos - 1 };
			} {
				result = result.add(ch);
			};
		};
		// result.asCompileString.debug("<< divideEvents");
		result
	};

	// matchOverride needs refactoring
	~parseBeatsPerBar = { |code, pat, matchOverride|
		var obj = ~clClass.new(~objKey),
		quoteI = code.indexOf($"), div, total,
		matchChar = code.first ?? { matchOverride ?? { 1.asAscii } };
		// pat.debug(">> parseBeatsPerBar");
		case { matchChar.isDecDigit } {
			~beatsPerBar = code[.. quoteI - 1].interpret; // allows math for bpb
			~beatsPerSeg = [~beatsPerBar / pat.size];
		}
		// additive rhythm: all events are equal duration; assume BP's subdivision if not given
		{ matchChar == $+ } {
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
		// now the default case is the same as / so, comment out for now
		// { matchChar == $/ } {
		// 	~beatsPerBar = ~clClass.new(~objKey).beatsPerBar;
		// 	~beatsPerSeg = [~beatsPerBar / pat.size];
		// }
		{
			~beatsPerBar = ~clClass.new(~objKey).beatsPerBar; // pat.size;
			~beatsPerSeg = [~beatsPerBar / pat.size];
		};  // default case
		// ~beatsPerSeg.debug("beatsPerSeg");
		~beatsPerBar // .debug("<< parseBeatsPerBar -- bpb");
	};

	~timesToDeltas = { |times, levels, legatos|
		var deltas;
		if(levels.size == 0 or: { levels.every(_.isNil) }) {
			"%(%): No recognized values for %. Substituting a rest.".format(
				~clClass, ~objKey.asCompileString, ~parm
			).warn;
			deltas = [Rest(~beatsPerBar)];
			levels = [1];
			legatos = [1];
		} {
			deltas = (times ++ ~beatsPerBar).differentiate;
			if(times.first == 0) {
				deltas = deltas.drop(1);
			} {
				deltas[0] = Rest(times[0]);
				levels = levels.insert(0, levels[0]);
				legatos = legatos.insert(0, legatos[0]);
			};
			legatos = legatos.collect { |lg, i|
				if(lg == 0.4) { min(0.4, 0.15 / deltas[i].value) } { lg }
			};
		};
		[deltas, levels, legatos]
	};

	~valueForParm = { |event, obj, parm|
		var degree, legato = 0.9;
		if(~isPitch) {
			event = event.asString;
			case
			{ event[0].isDecDigit } {
				degree = (event[0].ascii - 48).wrap(1, 10) - 1;
				event.drop(1).do { |ch|
					switch(ch)
					{ $- } { degree = degree - 0.1 }
					{ $+ } { degree = degree + 0.1 }
					{ $, } { degree = degree - 7 }
					{ $' } { degree = degree + 7 }
					{ $~ } { legato = 1.01 }
					{ $_ } { legato = 0.9 }
					{ $. } { legato = 0.4 }
				};
				degree -> legato  // Association identifies pitch above
			}
			{ event[0] != $ } {
				Rest(0) -> legato
			}
			{ obj.valueForParm(event, parm) };
		} {
			obj.valueForParm(event, parm);
		};
	};
	// CODE GENERATION

	~buildStatement = {
		var stmt = "%(%).setPattern(%, %, %)".format(
			~clClass, ~objKey.asCompileString,
			~phrase.asCompileString, ~parm.asCompileString,
			~patString
		);
		if(~isMain) {
			stmt = stmt ++ ";\n%(%).setPattern(%, %, Pseq(%, 1))".format(
				~clClass, ~objKey.asCompileString,
				~phrase.asCompileString, \dur.asCompileString,
				~deltas.asCompileString
			)
		};
		if(~isPitch) {
			stmt = stmt ++ ";\n%(%).setPattern(%, %, Pseq(%, 1))".format(
				~clClass, ~objKey.asCompileString,
				~phrase.asCompileString, \legato.asCompileString,
				~legatos.asCompileString
			)
		};
		stmt;
	};
}.import((chucklibLiveCode: #[tokens, replaceRegexpMacros, removeRegexpDups, removeDupIndices]), #[tokens]) => PR(\clPatternSet);

// model composite-pattern elements as objects

Proto {
	~type = \seq;
	~repeats = 1;
	~prep = { |items|
		~items = items;
		currentEnvironment
	};
	~asPatString = { |stream|
		if(stream.isNil) { stream = CollStream.new };
		stream << "P" << ~type << "(";
		~itemString.(stream);
		stream << ", " << (~repeats ? 1) << ")";
		stream.collection;
	};
	~itemString = { |stream|
		if(stream.isNil) { stream = CollStream.new };
		stream << "[";
		~items.do { |item, i|
			if(i > 0) { stream << ", " };
			if(item.isKindOf(Proto)) {
				item.asPatString(stream);
			} {
				stream <<< item;
			};
		};
		stream << "]";
		stream.collection;
	};
	~clumpOperators = {
		~items = ~splitArray.(~items, $.);
		~items = ~items.collect { |item|
			case
			{ item.isString } { ~processRepeats.(item) }
			{ item.isKindOf(Array) } {
				if(item.size > 1) {
					PR(\clCompGrouping).copy.prep(item).clumpOperators;
				} {
					item = item[0];
					if(item.isKindOf(Proto)) {
						item.clumpOperators;
					} {
						if(item.isString) { ~processRepeats.(item) } { item }
					}
				};
			}
			{ item.isKindOf(Proto) } {
				item.clumpOperators;
			}
			{ item }
		};
		currentEnvironment
	};
	~splitArray = { |array, delimiter|
		var result = Array.new, subarray = Array.new;
		array.do { |item|
			if(item == delimiter) {
				result = result.add(subarray);
				subarray = Array.new;
			} {
				subarray = subarray.add(item);
			};
		};
		result.add(subarray)
	};
	~processRepeats = { |str|
		var starI, pctI, rpt;
		if(str.first.isDecDigit) {
			i = str.indexOf($%);
			if(i.notNil) {
				"%: Weights are not valid in a sequence; ignoring".format(str).warn;
				~processRepeats.(str[i+1..]);  // handle '*', or return symbol
			} {
				i = str.indexOf($*);
				if(i.isNil) {
					Error("Invalid item: Repeats without item to repeat").throw;
				};
				rpt = str[..i-1].asInteger;
				str = ~processWildcard.(str[i+1..]);
				PR(\clCompRpt).copy.prep(str, rpt);
			}
		} {
			~processWildcard.(str) // .asSymbol
		};
	};
	~processWildcard = { |str|
		var i;
		str = str.trim;
		if(str[0] == $') {
			i = str.find("'", offset: 1);
			if(i.isNil) {
				Error("%: No closing quote".format(str)).throw;
			};
			PR(\clCompWildcardItem).copy.prep(str[1 .. i-1]);
		} {
			str.asSymbol
		};
	};
} => PR(\clCompSequence);

Proto {
	~type = \rand;
	~repeats = 1;
	~prep = { |items, hasWeights(false)|
		~items = items;
		~hasWeights = hasWeights;
		if(hasWeights) { ~type = \wrand };  // I think I'm hacking more
		currentEnvironment
	};
	~clumpOperators = { currentEnvironment };
	~itemString = { |stream|
		var weights;
		if(stream.isNil) { stream = CollStream.new };
		stream << "[";
		~items.do { |item, i|
			if(i > 0) { stream << ", " };
			if(item.isKindOf(Proto)) {
				item.asPatString(stream);
			} {
				stream <<< item;
			};
		};
		stream << "]";
		if(~hasWeights) {
			weights = ~items.collect({ |item| item.tryPerform(\weight) ? 1 }).normalizeSum;
			stream << ", [";
			weights.do { |w, i|
				if(i > 0) { stream << ", " };
				stream << w;
			};
			stream << "]"
		};
		stream.collection;
	};
}.import((clCompSequence: #[asPatString/*, itemString*/])) => PR(\clCompRandom);

Proto {
	~type = \n;
	~repeats = 1;
	~prep = { |items, repeats(1), weight|
		~items = items;
		~repeats = repeats;
		~wt = weight;
		currentEnvironment
	};
	~weight = { ~wt };  // avoid notUnderstood error if weight is nil
	~clumpOperators = { currentEnvironment };
	~asPatString = { |stream|
		if(stream.isNil) { stream = CollStream.new };
		stream << "Pn(";
		if(~items.isKindOf(Proto)) {
			~items.asPatString(stream);
		} {
			stream <<< ~items;
		};
		stream << ", " << (~repeats ? 1) << ")";
		stream.collection;
	};
}.import((clCompSequence: #[itemString])) => PR(\clCompRpt);

Proto {
	~prep = { |item, weight(1)|
		~item = item;
		~weight = weight;
		currentEnvironment
	};
	~itemString = { ~item.asCompileString };
	~asPatString = { |stream|
		if(stream.isNil) { stream = CollStream.new };
		stream <<< ~item
	};
} => PR(\clCompWeightedItem);

Proto {
	~weight = 1;
	~prep = { |item/*, weight(1)*/|
		~item = item;
		// ~weight = weight;
		currentEnvironment
	};
	~itemString = { ~item.asCompileString };
	~asPatString = { |stream|
		if(stream.isNil) { stream = CollStream.new };
		stream << "Pfuncn({ ~phrases.keys.select { |key| %.matchRegexp(key.asString) }.choose }, 1)".format(~item.asCompileString);
	};
} => PR(\clCompWildcardItem);

// LATER
// Proto {
// 	~clumpOperators = { currentEnvironment };
// } => PR(\clCompWeightedRand);

Proto {
	~type = \group;
	~hasWeights = false;
	// ~repeats = 1;
	~process = { |stream|
		var ch, continue = true, str = String.new, rpt;
		~items = Array();
		while { continue and: { (ch = stream.next).notNil } } {
			case
			{ ".|".includes(ch) } {   // operators, add as chars
				if(str.size > 0) { ~items = ~items.add(str) };
				~items = ~items.add(ch);
				str = String.new;
			}
			{ ch == $( } {
				if(str.size > 0) { ~items = ~items.add(str) };
				~items = ~items.add(PR(\clCompGrouping).copy.process(stream));
				str = String.new;
			}
			{ ch == $) } {
				if(str.size > 0) { ~items = ~items.add(str) };
				continue = false;
			}
			{ ch == $' } {
				while {
					str = str ++ ch;
					ch = stream.next;
					ch.notNil and: { ch != $' }
				};
				if(ch.notNil) { str = str ++ ch };
			}
			{ "*%".includes(ch) } {
				rpt = tryNoTrace {
					~scanInt.(stream);
				} { |exc|
					if(exc.what == \nonInt) { nil } { exc.throw }
				};
				if(rpt.notNil) {
					if(ch == $%) { ~hasWeights = true };
					str = rpt ++ ch ++ str;
				} {
					"Composite pattern: Ignored invalid % string %".format(
						if(ch == $*) { "repeat" } { "weight" },
						rpt
					).warn;
				};
				// "\nCollStream state:".postln;
				// stream.collection.postln;
				// (String.fill(stream.pos, $ ) ++ "^\n").postln;
			}
			{ ch.isAlpha or: { ch.isDecDigit or: { ch == $_ } } } {
				str = str ++ ch
			}
			{
				"Composite pattern: Unexpected character % in grouping".format(ch).warn;
			};
		};
		if(continue) {
			Error("Composite pattern: Unclosed () group").throw;
		} {
			ch = stream.next;
			if(ch == $*) {
				~repeats = tryNoTrace {
					~scanInt.(stream).asInteger;
				} { |exc|
					if(exc.what == \nonInt) { 1 } { exc.throw };
				};
			} {
				stream.pos = stream.pos - 1;
			};
		};
		currentEnvironment
	};

	~clumpOperators = {
		if(~items.includes($|)) {
			~items = ~splitArray.(~items, $|);
			~items = ~items.collect { |item|
				case
				{ item.isString } { ~processRepeats.(item) }
				{ item.isKindOf(Array) } {
					if(item.size > 1) {
						PR(\clCompSequence).copy.prep(item).clumpOperators;
					} {
						if(item[0].isString) { ~processRepeats.(item[0])/*.asSymbol*/ } {
							if(item[0].isKindOf(Proto)) { item[0].clumpOperators } { item[0] }
						}
					};
				}
				{ item.isKindOf(Proto) and: { item.type == \group } } {
					item.clumpOperators;
				}
			};
			~items = PR(\clCompRandom).copy.prep(~items, ~hasWeights);
		} {
			~items = PR(\clCompSequence).copy.prep(~items).clumpOperators;
		};
		currentEnvironment
	};

	/*
	Logic tree:
* Star
** Nil
*** Pct
**** Nil
     Error
**** Non-nil
     Parent clCompRandom will handle weight
** Non-nil
*** Pct
**** Nil
     Return clCompRpt
**** Pct < Star
     Pct = .. pct-1
     Star = pct+1 .. star-1
**** Pct > Star
     Star = .. star-1
     Pct = star+1 .. pct-1
	*/

	~processRepeats = { |str|
		var starI, pctI, star, pct;
		if(str.first.isDecDigit) {
			starI = str.indexOf($*);
			pctI = str.indexOf($%);
			if(starI.isNil) {
				if(pctI.isNil) {
					Error("Invalid item: Repeats/weight without item").throw;
				} {
					// weight, but no repeats
					PR(\clCompWeightedItem).copy.prep(
						~processWildcard.(str[pctI+1..]),
						str[..pctI-1].asInteger
					)
				}
			} {
				if(pctI.isNil) {
					// repeats, but no weight: repeat object, but don't pass weight
					PR(\clCompRpt).copy.prep(
						~processWildcard.(str[starI+1..]),
						str[..starI-1].asInteger
					); // nil weights
				} {
					// repeats and weight: repeat object with both parms
					if((pctI < starI)) {
						pct = str[ .. pctI-1];
						star = str[pctI+1 .. starI-1];
						str = str[starI+1 ..];
					} {
						star = str[ .. starI-1];
						pct = str[starI+1 .. pctI-1];
						str = str[pctI+1 ..];
					};
					PR(\clCompRpt).copy.prep(~processWildcard.(str), star.asInteger, pct.asInteger);
				};
			};
		} {
			~processWildcard.(str)
		};
	};

	~scanInt = { |stream|
		var str = String.new, ch;
		while { (ch = stream.next).notNil and: { ch.isDecDigit } } {
			str = str ++ ch;
		};
		stream.pos = stream.pos - 1;
		if(str.isEmpty) {
			Exception(\nonInt).throw;
		};
		str // .asInteger;
	};

	~asPatString = { |stream|
		if(~items.isKindOf(Proto)) {
			~items.repeats_(~repeats).asPatString(stream);
		} {
			Error("Composite pattern: Group should contain a Proto but doesn't").throw;
		};
	};
}.import((clCompSequence: #[splitArray, processWildcard])) => PR(\clCompGrouping);


Proto {
	~regexp = PR(\chucklibLiveCode).replaceRegexpMacros("[+-][`spc]*[\\-0-9\\.]*|`id");
	~floatRegexp = PR(\chucklibLiveCode).replaceRegexpMacros("^`float$");
	~process = { |code|
		var parsed = code.findRegexp(~regexp),
		quant, method, keys, result = String.new;
		if(parsed.size >= 1) {
			parsed = parsed.separate { |a, b|
				(a[1].first.tryPerform(\isAlpha) ? false).not
			};
			parsed.do { |row, i|
				if("+-".includes(row.last[1].first)) {
					quant = row.last[1].drop(1);
					if(~floatRegexp.matchRegexp(quant)) {
						quant = quant.interpret;
					} {
						quant = quant.trim;
						if(quant.size == 0) {  // ok, no quant given
							quant = -1;
						} {
							"clStartStop: % is not a valid quant indicator".format(row.last).warn;
						};
					};
					if(row.last[1].first == $+) { method = \play } { method = \stop };
					keys = row.drop(-1).flop[1].collect(_.asSymbol).select { |key| BP.exists(key) };
					result = result ++ "BP(%).%(%)".format(
						keys.asCompileString,
						method, quant
					);
					if(i < (parsed.size - 1)) { code = code ++ ";\n" };
				};
			};
		} {
			Error("clStartStop: Regexp problem").throw;
		};
		result.debug("clStartStop result");
	};
}.import((chucklibLiveCode: #[tokens, replaceRegexpMacros, removeRegexpDups, removeDupIndices]), #[tokens]) => PR(\clStartStop);

Proto {
	~clClass = BP;
	~isMain = false;
	~name = "patternCopy";
	~eqCheckStr = "->";

	~process = { |code|
		~eqIndex = code.find(~eqCheckStr);
		if(~eqIndex.isNil) {
			Error("% statement has no '%': This should never happen".format(~name, ~eqCheckStr)).throw;
		};
		~parseIDs.(code);
		~buildStatement.();
	};

	~idRegexp = {
		PR(\chucklibLiveCode).replaceRegexpMacros("(`id)(\\.`id)? % (`id)".format(~eqCheckStr))
	};
	~parseIDs = { |code|
		var ids = code.findRegexp(~idRegexp.()), obj;
		if(ids.size < 4) {
			Error("%: Regexp did not find ids (%)".format(~name, ids)).throw;
		} {
			~objKey = ids[1][1].asSymbol;
			if(BP.exists(~objKey)) {
				obj = BP(~objKey);
				~srcPhrase = ids[2][1];
				if(~srcPhrase.size == 0) {
					// current
					~srcPhrase = obj.lastPhrase;  // potentially risky
				} {
					~srcPhrase = ~srcPhrase.drop(1).asSymbol;
				};
				if(obj.phrases[~srcPhrase].isNil) {
					Error("%: Source phrase % doesn't exist".format(~name, ~srcPhrase.asCompileString)).throw;
				};
				~targetPhrase = ids[3][1].asSymbol;
			};
		};
		currentEnvironment
	};

	~buildStatement = {
		var objStr = "%(%)".format(~clClass, ~objKey.asCompileString);
		"%.phrases[%] = %.phrases[%].deepCopy".format(
			objStr, ~targetPhrase.asCompileString,
			objStr, ~srcPhrase.asCompileString
		);
	};
} => PR(\clCopyPattern);

PR(\clCopyPattern).clone {
	~name = "patternXfer";
	~eqCheckStr = "->>";

	~buildStatement = {
		var objStr = "%(%)".format(~clClass, ~objKey.asCompileString),
		target = ~targetPhrase.asCompileString;
		"%.phrases[%] = %.phrases[%].deepCopy; %.setPattern('main', nil, %)".format(
			objStr, target,
			objStr, ~srcPhrase.asCompileString,
			objStr, target
		);
	};
} => PR(\clXferPattern);
);

(
Proto {
	~subdiv = 0.25;
	~numVariants = 1;
	~numToAdd = nil;
	~isPitch = false;
	~beatsPerBarSpec = "";
	~itemIsFunc = false;

	~process = { |code|
		var stream = CollStream(code),
		ch, bp;

		~idString = ~parseIDs.(stream);
		if(BP.exists(~idString[0].asSymbol).not) {
			Error("Generator statement failed: BP('%') does not exist.".format(~idString[0])).throw;
		};
		bp = BP(~idString[0].asSymbol);
		~isPitch = bp.parmIsPitch(~idString[2] ?? { bp.defaultParm });

		while { (ch = stream.next).notNil and: { ch != $" } } {
			case
			{ ch == $* } {
				~numVariants = ~parseInt.(stream);
			}
			{ ch == $+ and: { ~numToAdd.isNil } } {
				~numToAdd = ~parseInt.(stream);
				ch = stream.next;
				if(ch.notNil and: { ch.isSpace.not }) {
					~item = ~parseItem.(stream, ch);
				};
			}
			{ ch == $/ } {
				~subdiv = ~parseResolution.(stream);
			}
			{ ch.notNil and: { ch != $" } } {
				stream.pos = stream.pos - 1;
				~beatsPerBarSpec = ~skipUpToCond.(stream, { |ch| ch == $" });
			};
			~skipUpToCond.(stream);
		};
		if(ch != $") {
			Error("Generator statement: no pattern string found: %".format(code)).throw;
		};
		~string = ~skipUpToCond.(stream, { |ch| ch == $" });
		~template = ~expandString.(~string);
		~variants = Array.fill(~numVariants, { |i|
			var variant = ~makeVariant.(~template);
			~issueCommand.(variant, i);
			variant
		});
		~postVariants.(~variants);

		"nil"  // return a dummy statement to interpret
	};

	~parseIDs = { |stream|
		var str = String.new, ch;
		while { (ch = stream.next).notNil and: { " *".includes(ch).not } } {
			str = str ++ ch;
		};
		case { ch.isSpace } {
			~skipUpToCond.(stream);
		}
		{ ch == $* } {
			stream.pos = stream.pos - 1;
		}
		{ ch.isNil } { Error("Incomplete generator statement: %".format(stream.collection)).throw };
		str.split($.)  // need access to phrase name, easiest
	};

	~parseInt = { |stream|
		var str = String.new, ch;
		while { (ch = stream.next).notNil and: { ch.isDecDigit } } {
			str = str ++ ch;
		};
		if(ch.notNil) { stream.pos = stream.pos - 1 };
		str.asInteger
	};

	~parseResolution = { |stream|
		var str = String.new, ch;
		while { (ch = stream.next).notNil and: { ".0123456789/e".includes(ch) } } {
			str = str ++ ch;
		};
		if(ch.notNil) { stream.pos = stream.pos - 1 };
		str.interpret
	};

	~parseItem = { |stream, ch|
		var str = String.with(ch), funcID;
		case { ch == $' } {
			str = ~skipUpToCond.(stream, { |ch| ch == $' });  // should return the thing
			stream.next;
			str
		}
		{ ch == $\\ } {
			~itemIsFunc = true;
			funcID = ~skipUpToCond.(stream, { |ch| ch.isAlphaNum.not and: { ch != $_ } });
			ch = stream.next;
			if(ch == $() {
				~funcArgs = ~skipUpToCond.(stream, { |ch| ch == $) });
				~funcArgs = "[%]".format(~funcArgs).interpret;
				stream.next;
			} {
				~funcArgs = #[];
			};
			Func(funcID.asSymbol)
		}
		{ str };
	};

	// expand a segment if it's shorter than the subdivided beat, and evenly divides
	// otherwise assume that you gave the number of slots you want
	~expandString = { |string|
		var segs = string.split($|).collectAs(~divideEvents, Array),
		perSeg = ~subdiv.reciprocal, quotient, spaces, new;
		segs.collect { |seg|
			if(seg.size == 0) { seg = [$ ] };
			quotient = perSeg / seg.size;
			if((quotient.round >= 1) and: { (quotient absdif: quotient.round) < 0.01 }) {
				spaces = quotient.round - 1;
				new = Array(perSeg);
				seg.do { |item|
					new.add(item);
					spaces.do { new.add($ ) };
				};
				new
			} { seg }
		};
	};

	~makeVariant = { |template|
		var avail = Array(template.collect(_.size).sum),
		prevItem;
		template = template.collect(_.copy);
		template.do { |seg, i|
			seg.do { |item, j|
				if(template[i][j] == $ ) { avail.add([i, j]) };
			};
		};
		avail = avail.scramble.keep(~numToAdd ?? { 1 });
		avail.do { |indexPair|
			if(~itemIsFunc) {
				prevItem = ~scanBackward.(template, *indexPair);
				template[indexPair[0]][indexPair[1]] = ~item.eval(prevItem, *~funcArgs);
			} {
				template[indexPair[0]][indexPair[1]] = ~item;
			};
		};
		~segmentsAsString.(template)
	};

	~segmentsAsString = { |segments|
		segments.collect(_.join).join("|");
	};

	~scanBackward = { |template, segIndex, itemIndex|
		var thing;
		block { |break|
			while { segIndex >= 0 } {
				while { itemIndex >= 0 } {
					thing = template[segIndex][itemIndex];
					if(thing.notNil and: { thing != $  }) {
						break.(thing);
					} {
						itemIndex = itemIndex - 1;
					};
				};
				segIndex = segIndex - 1;
				itemIndex = template[segIndex].size - 1;
			};
		};
	};

	~issueCommand = { |variant, i|
		var id = ~idString.copy.put(1, ~idString[1] ++ i).join("."),
		cmd = "% = %\"%\"".format(id, ~beatsPerBarSpec, variant);

		// hack? Unsafe if the implementation in Func(\chucklibLiveCode) changes
		try {
			PR(\chucklibLiveCode)/*.copy?*/.process(cmd).interpret;
		} { |err|
			"Could not process variant % of %: %".format(i, ~numVariants, err.errorString).warn;
		};
	};

	~postVariants = { |variants|
		"Added:".postln;
		variants.do { |variant, i|
			"% = \"%\"\n".postf(~idString[1] ++ i, variant);
		};
	};

	~skipUpToCond = { |stream, boolFunc({ |ch| ch.isSpace.not })|
		var str = String.new, ch;
		// boolFunc.asCompileString.debug(">> skipUpToCond");
		while { (ch = stream.next)/*.debug("ch")*/.notNil and: { boolFunc.(ch).not/*.debug("loop test")*/ } } {
			str = str ++ ch;
		};
		if(ch.notNil) { stream.pos = stream.pos - 1 };
		str  // .debug("<< skipUpToCond");
	};
}.import((clPatternSet: #[divideEvents])) => PR(\clGenerator);
);

(
Proto {
	~event = (eventKey: \singleSynthPlayer);

	~defaultParm = \go;
	~parmMap = (
		go: ($x: 0)
	);
	~beatsPerBar = { ~clock.beatsPerBar };
	~division = 0.25;
	~phraseSeq = \main;
	~lastPhrase = \main;

	~prep = {
		if(~phrases.isNil) {
			~phrases = IdentityDictionary[
				\main -> PbindProxy(\dur, Pfuncn { Rest(~clock.beatsPerBar) }),
				\rest -> PbindProxy(\dur, Pfuncn { Rest(~clock.beatsPerBar) })
			];
			~phraseDurs = IdentityDictionary.new;
		};
		~userprep.();
		~postParmMap.();
		Environment.current
	};
	~freeCleanup = {
		~userfree.();
	};

	~valueForParm = { |event, parm|
		var dict = ~parmMap[parm];
		if(dict.notNil) { dict[event] };  // else nil
	};
	~parmIsPitch = { |parm|
		~parmMap[parm].notNil and: { ~parmMap[parm][\isPitch] == true }
	};

	~defaults = ();  // or Pbind
	~postDefaults = ();

	~setPattern = { |phrase, parm, pattern|
		var pat = ~phrases[phrase];
		parm = ~parmMap[parm].tryPerform(\at, \alias) ?? { parm };
		if(pat.isNil) {
			pat = PbindProxy.new;
			~phrases[phrase] = pat;
		};
		~clock.schedAbs(~clock.nextBar - 0.001, e {
			if(parm.notNil) {
				if(parm == \dur) {
					// _.value is needed to convert Rests to durations
					~phraseDurs[phrase] = pattern.list.sum(_.value);
				};
				pat.set(parm, pattern);
			} {
				// composite pattern
				~phraseSeq = pattern;
				if(~isPlaying) { ~reschedule.() };
			};
			nil
		});
	};

	~asPattern = {
		var emptyCountdown = 10, lastTime = 0;
		~reset.();
		Pchain(
			BPStream(\postDefaults), //.trace(prefix: "postDefaults: "),
			Prout { |inevent|
				var phr, pat;
				~makeStreamForKey.(\phraseSeq);
				loop {
					if(thisThread.beats > lastTime) {
						lastTime = thisThread.beats;
						~lastPhrase = phr;
					} {
						"BP(%): Empty phrase %, stopping".format(~collIndex.asCompileString, phr).warn;
						nil
					};
					phr = ~phraseSeqStream.next(inevent);
					pat = ~phrases[phr];
					// the pattern-wrapping here is not directly supported in Psym
					if(~phraseDurs[phr].notNil) {
						pat = Pfindur(~phraseDurs[phr], pat);
					};
					inevent = pat.embedInStream(inevent);
				};
			}, //.trace(prefix: "phrase: "),
			BPStream(\defaults) //.trace(prefix: "\n\ndefaults: ")
		)
	};

	~reschedule = { |quant(-1)|
		var oldStreamPlayer = ~eventStreamPlayer,
		newStreamPlayer = BP(~collIndex).asEventStreamPlayer;
		~clock.schedAbs(~clock.nextTimeOnGrid(quant) - 0.001, { oldStreamPlayer.stop });
		~clock.play(newStreamPlayer.refresh, quant);
		currentEnvironment
	};

	~reset = {
		~makeStreamForKey.(\phraseSeq);
		~makeStreamForKey.(\defaults);
		~makeStreamForKey.(\postDefaults);
	};

	~postParmMap = {
		"BP(%)'s parameter map:\n".postf(~collIndex.asCompileString);
		~parmMap.asSortedArray.do { |pair|
			if(pair[0] == ~defaultParm) { "** ".post } { "   ".post };
			"%: %\n".postf(*pair);
		};
		currentEnvironment
	};
}.import((abstractProcess: #[makeStreamForKey])) => PR(\abstractLiveCode);
)
