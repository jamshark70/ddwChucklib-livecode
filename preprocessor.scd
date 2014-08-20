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
		levels = Array.new, times = Array.new, legatos = Array.new, deltas,
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
				value = ~valueForParm.(event, obj, ~parm);
				case
				{ value.isArray } {
					levels = levels.add(value[0]);
					times = times.add(time + (division * i));
					legatos = legatos.add(value[1]);
				}
				{ value.notNil } {
					levels = levels.add(value);
					times = times.add(time + (division * i));
					legatos = legatos.add(0.98);
				};
			};
			time = time + (division * seg.size);
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
		levels = Array.new, times = Array.new, legatos = Array.new, deltas,
		pat = code.findRegexp("\".*\"");
		if(pat.size == 0 or: { pat.first.size <= 0 }) {
			Error("clPatternSet: Couldn't find pattern string").throw
		};
		pat = pat[0][1];
		// pat = [pat[1 .. pat.size-2].collectAs(~divideEvents, Array)];
		pat = [~divideEvents.(pat[1 .. pat.size-2])];
		~parseBeatsPerBar.(code, pat, $/);
		division = ~beatsPerSeg[0] / pat[0].size;
		pat[0].do { |event, i|
			value = ~valueForParm.(event, obj, ~parm);
			case
			{ value.isArray } {
				levels = levels.add(value[0]);
				times = times.add(time + (division * i));
				legatos = legatos.add(value[1]);
			}
			{ value.notNil } {
				levels = levels.add(value);
				times = times.add(time + (division * i));
				legatos = legatos.add(0.98);
			};
		};
		#deltas, levels, legatos = ~timesToDeltas.(times, levels, legatos);
		~deltas = deltas;
		~levels = levels;
		~times = times;
		~legatos = legatos;
		"Pstep(Pseq(%, 1), Pseq(%, 1), 1)".format(levels.asCompileString, deltas.asCompileString).postcs;
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
		~beatsPerBar
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
		if(event.isString) {
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
			[degree, legato]
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
			stmt = stmt ++ ";\n%(%).setPattern(%, %, Pseq(%, inf))".format(
				~clClass, ~objKey.asCompileString,
				~phrase.asCompileString, \dur.asCompileString,
				~deltas.asCompileString
			)
		};
		if(~isPitch) {
			stmt = stmt ++ ";\n%(%).setPattern(%, %, Pseq(%, inf))".format(
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
			{ item.isString } { ~processRepeats.(item)/*.asSymbol*/ }
			{ item.isKindOf(Array) } {
				if(item.size > 1) {
					PR(\clCompGrouping).copy.prep(item).clumpOperators;
				} {
					item = item[0];
					if(item.isKindOf(Proto) /*and: { item.type == \group }*/) {
						item.clumpOperators;
					} {
						if(item.isString) { ~processRepeats.(item)/*.asSymbol*/ } { item }
					}
				};
			}
			{ item.isKindOf(Proto) /*and: { item.type == \group }*/ } {
				item.clumpOperators;
			}
			{ item }
		};
		// ~items = PR(\clCompSequence).copy.prep(~items);
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
		var i;
		if(str.first.isDecDigit) {
			i = str.indexOf($*);
			if(i.isNil) {
				Error("Invalid item: Repeats without item to repeat").throw;
			};
			PR(\clCompRpt).copy.prep(str[i+1..].asSymbol, str[..i-1].asInteger);
		} {
			str.asSymbol
		};
	};
} => PR(\clCompSequence);

Proto {
	~type = \rand;
	~repeats = 1;
	~prep = { |items|
		~items = items;
		currentEnvironment
	};
	~clumpOperators = { currentEnvironment };
}.import((clCompSequence: #[asPatString, itemString])) => PR(\clCompRandom);

Proto {
	~type = \n;
	~repeats = 1;
	~prep = { |items, repeats(1)|
		~items = items;
		~repeats = repeats;
		currentEnvironment
	};
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


// LATER
// Proto {
// 	~clumpOperators = { currentEnvironment };
// } => PR(\clCompWeightedRand);

Proto {
	~type = \group;
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
			{ ch == $* } {
				rpt = tryNoTrace {
					~scanInt.(stream);
				} { |exc|
					if(exc.what == \nonInt) { nil } { exc.throw }
				};
				if(rpt.notNil) {
					~items = ~items.add(rpt ++ "*" ++ str);
					str = String.new;
				} {
					"Composite pattern: Ignored invalid repeat string %".format(rpt).warn;
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
		// ~clumpRandomOps.();  // if no randomizers, adds an extra array layer for the sequence
		if(~items.includes($|)) {
			~items = ~splitArray.(~items, $|);
			~items = ~items.collect { |item|
				case
				{ item.isString } { item.asSymbol }
				{ item.isKindOf(Array) } {
					if(item.size > 1) {
						PR(\clCompSequence).copy.prep(item).clumpOperators;
					} {
						if(item[0].isString) { item[0].asSymbol } {
							if(item[0].isKindOf(Proto)) { item[0].clumpOperators } { item[0] }
						}
						// item[0]
					};
				}
				{ item.isKindOf(Proto) and: { item.type == \group } } {
					item.clumpOperators;
				}
			};
			~items = PR(\clCompRandom).copy.prep(~items);
		} {
			~items = PR(\clCompSequence).copy.prep(~items).clumpOperators; // [~items];
		};
		currentEnvironment
	};
	// ~clumpRandomOps = {
	// 	var split;
	// 	if(~items.includes($|)) {
	// 		split = ~splitArray.(~items, $|);
	// 	} {
	// 		~items = [~items]
	// 	};
	// };

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
}.import((clCompSequence: #[splitArray])) => PR(\clCompGrouping);


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
			~phrases = (
				main: PbindProxy(\dur, Pfuncn { Rest(~clock.beatsPerBar) })
			);
		};
		~userprep.();
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
		if(pat.isNil) {
			pat = PbindProxy.new;
			~phrases[phrase] = pat;
		}/* {
			pat = pat.patterns.first;
		}*/;
		~clock.schedAbs(~clock.nextBar - 0.001, e {
			if(parm.notNil) {
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
		Pchain(
			BPStream(\postDefaults), //.trace(prefix: "postDefaults: "),
			Psym(
				BPStream(\phraseSeq).collect { |phr|
					if(thisThread.beats > lastTime) {
						lastTime = thisThread.beats;
						~lastPhrase = phr;
					} {
						"BP(%): Empty phrase %, stopping".format(~collIndex.asCompileString, phr);
						nil
					}
				},
				~phrases
			), //.trace(prefix: "phrase: "),
			BPStream(\defaults) //.trace(prefix: "\n\ndefaults: ")
		)
		// PnNilSafe(Plazy {
		// 	~phrases[\main]
		// }, inf, 20)
	};

	~reschedule = { |quant(-1)|
		var oldStreamPlayer = ~eventStreamPlayer,
		newStreamPlayer = BP(~collIndex).asEventStreamPlayer;
		~clock.schedAbs(~clock.nextTimeOnGrid(quant) - 0.001, { oldStreamPlayer.stop });
		~clock.play(newStreamPlayer.refresh, quant);
	};

	~reset = { ~makeStreamForKey.(\phraseSeq) };
}.import((abstractProcess: #[makeStreamForKey])) => PR(\abstractLiveCode);
)
