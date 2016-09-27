0+0;  // preprocessor chokes on opening paren, avoid it, that is really unfortunate

this.preProcessor = { |code|
	if("/+(".includes(code.first)) {
		\chucklibLiveCode.eval(code)
	} {
		code
	}
};

// main preprocessor
{ |code|
	if(code.first == $( and: {
		code = code.trim(" \t\n");
		code.last == $)
	}) {
		// 'code' has already lost trailing spaces
		code = code[1 .. code.size - 2].trim(" \n\t");
	};
	if("/+".includes(code.first)) {
		code = \clParseIntoStatements.eval(code);
		code.do { |stmt, i|
			case
			{ stmt.first == $/ and: { stmt[1] != $/ } } {
				code[i] = PR(\chucklibLiveCode)/*.copy?*/.process(stmt.drop(1));
			}
			{ stmt.first == $+ } {
				if(BP.exists(\clRegister).not) {
					PR(\clRegister) => BP(\clRegister);
				};
				code[i] = BP(\clRegister).process(stmt.drop(1));
			};
		};
		code.join(";\n");
	} {
		code
	}
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
				str = str ++ ch;
				switch(ch)
				{ $\\ } {
					escape = escape.not;
				}
				{ $\" } {
					if(escape) {
						escape = false;
					} {
						continue = false;
					}
				}
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
	while { (ch = ch2 ?? { code.next }).notNil and: { continue } } {
		ch2 = nil;
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
		\clMake -> "^ *make\\(.*\\)",
		\clFuncCall -> "^ *`id\\.\\(.*\\)",
		\clPassThru -> "^ *([A-Z][A-Za-z0-9_]*\\.)?`id\\(.*\\)",
		\clChuck -> "^ *([A-Z][A-Za-z0-9_]*\\.)?`id *=>.*",
		\clPatternSet -> "^ *`id(\\.|`id|`id\\*[0-9]+)* = .*",
		\clGenerator -> "^ *`id(\\.|`id)* \\*.*",
		\clXferPattern -> "^ *`id(\\.`id)?(\\*`int)? ->>",  // harder match should come first
		\clCopyPattern -> "^ *`id(\\.`id)?(\\*`int)? ->",
		\clStartStop -> "^([/`spc]*`id)+[`spc]*[+-]",
		\clPatternToDoc -> "^ *`id(\\.|`id)*(\\*[0-9]+)?[`spc]*$"
	];

	// support functions

	// ~replaceRegexpMacros.("`id(.`id)+ = .*");
	// ~replaceRegexpMacros.("blah`id");

	// ~replaceRegexpMacros.("`id(.`id)+ = .*").matchRegexp("kik.k1 = 'xxxx'");

	~replaceRegexpMacros = { |regexp|
		var key, matches;
		// should replace from right to left -- don't break indices
		matches = regexp.findRegexp("`[a-z0-9]+");
		if(matches.notEmpty) {
			~removeDupIndices.(matches).reverseDo { |found|
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
	~hasGen = false;
	// note: ~parm will be set to nil for composite patterns

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
		var i, ids, test, temp;
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
			i = test.indexOf($*);
			if(i.notNil) {
				temp = test[i+1 .. ];
				if(temp.notEmpty and: temp.every(_.isDecDigit)) {
					~numToApply = temp.asInteger;
				} {
					"%: Invalid apply number".format(test).warn;
				};
				~phrase = test[ .. i-1].asSymbol;
			} {
				~phrase = test.asSymbol;  // really? what about array types?
			};
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
		// { |code| code.includes($|) } -> \patternWithDividers,
		true -> \patternString // \patternWithoutDividers
	];

	~parsePattern = { |code|
		var case, rightHand;

		// bug here: should not include semicolon
		// proper fix: after replacing the parser, find the \clPatStringNode and get its string
		~inString = rightHand = code[~eqIndex + 1 ..].trim;
		if(rightHand.last == $;) {
			~inString = rightHand = rightHand.drop(-1);
		};
		// code.asCompileString.debug("code");

		// cases
		case = ~cases.detect { |case| case.key.(rightHand) };
		if(case.isNil) {
			Error("clPatternSet: Pattern does not match any known cases. This should never happen").throw;
		};
		case.value.envirGet.(rightHand, code);
	};

	~compositePattern = { |code|  // compositePattern does not need full statement
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
		~stream = stream;
		~group = group = PR(\clCompGrouping).copy.process(stream);
		~quant = ~getQuantFrom.(stream);
		group.clumpOperators;
		if(group.repeats.isNil) { group.repeats = inf };
		~inString = "\"\"";
		~buildStatement.();
	};

	~patternString = { |rightHand, code|  // ~patternString *does* need the full statement
		var parsed = PR(\clPatternSetNode).copy.prep(CollStream(code)),
		id, objKey, phrase, parm, bpb, stream, yieldsKeys, wrapArray = false,
		streamOne = { |stream, i|
			var phraseSym = if(i.notNil) { (phrase ++ i).asSymbol } { phrase };
			stream << "BP(" <<< objKey << ").setPhraseDur("
			<<< phraseSym << ", " << bpb << ");\n";
			stream << "BP(" <<< objKey << ").setPattern(" <<< phraseSym << ", ";
			stream <<< yieldsKeys << ", " <<< parm << ", Pseq(";
			if(wrapArray) { stream << "[ " };
			parsed.patStringNode.streamCode(stream);
			if(wrapArray) { stream << " ]" };
			stream << ", 1), " <<< rightHand << ", " << nil << ")";  // nil = quant... needed?
		};
		~parsed = parsed;
		id = parsed.idNode;
		objKey = id.objKey;
		phrase = id.phrase;
		parm = id.parm;
		if(parsed.isMain) {
			yieldsKeys = [parm, \dur];
		} {
			yieldsKeys = parm;
		};
		if(parsed.hasQuant) {
			bpb = parsed.quantNode.quant;
		};
		if(bpb.isNil) {
			if(BP.exists(objKey)) {
				bpb = (BP(objKey).clock ?? { TempoClock.default }).beatsPerBar;
			} {
				bpb = 4;  // lame default
			};
		};
		if(parsed.patStringNode.nodeType == \clPatStringNode) {
			// wrap the clPatString in the abstract generator
			// this is necessary to massage the event list into a playable stream
			parsed.patStringNode = PR(\clGeneratorNode).copy.putAll((
				bpKey: objKey,
				phrase: phrase,
				parm: parm,
				isMain: parsed.isMain,
				isPitch: parsed.isPitch,
				children: [
					PR(\clStringNode).copy.put(\string, ""),
					parsed.patStringNode  // patString is the abstract generator's only argument
				],
				name: ""
			));
			// moderate hack: I should wrap the string in a patstring -> divider -> generator
			// but I'm lazy
			wrapArray = true;
		};
		parsed.patStringNode.setTime(0, bpb);

		// write the code
		stream = CollStream.new;
		if(parsed.children[0].numToApply.isNil) {
			streamOne.(stream);
		} {
			parsed.children[0].numToApply.do { |i|
				if(i > 0) { stream << ";\n" };
				streamOne.(stream, i);
			};
		};
		stream.collection;
	};

	~unquoteString = { |str, pos = 0, delimiter = $", ignoreInParens(false)|
		var i = str.indexOf(delimiter), j, escaped = false, parenCount = 0;
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
				switch(str[j])
				{ $\\ } { escaped = escaped.not }
				{ $( } {
					if(ignoreInParens) {
						parenCount = parenCount + 1;
						escaped = true;
					} {
						escaped = false;
					};
				}
				{ $) } {
					if(ignoreInParens) {
						parenCount = parenCount - 1;
						if(parenCount < 0) {
							"unquoteString: paren mismatch in '%'".format(str).warn;
						} {
							escaped = parenCount > 0;
						};
					} {
						escaped = false;
					};
				}
				{
					if(ignoreInParens.not or: { parenCount <= 0 }) {
						escaped = false;
					};
				}
				// if(str[j] == $\\) { escaped = escaped.not } { escaped = false };
			};
			if(j - i <= 1) {
				String.new  // special case: two adjacent quotes = empty string
			} {
				str[i + 1 .. j - 1];
			};
		};
	};
	~getQuantFrom = { |stream|
		var ch, str;
		while { (ch = stream.next).notNil and: { ch.isSpace } };  // skip spaces
		if(ch == $() {
			stream.pos = stream.pos - 1;
			str = ~stringInMatchingBrackets.(stream);
			str  // return value: string, to plug into generated statement
		} { nil }
	};

	~decodePitch = { |pitchStr|
		var degree, legato = 0.9;
		case
		{ ~isPitch and: { pitchStr.isString } } {
			pitchStr = pitchStr.asString;
			case
			{ pitchStr[0].isDecDigit } {
				degree = (pitchStr[0].ascii - 48).wrap(1, 10) - 1;
				pitchStr.drop(1).do { |ch|
					switch(ch)
					{ $- } { degree = degree - 0.1 }
					{ $+ } { degree = degree + 0.1 }
					{ $, } { degree = degree - 7 }
					{ $' } { degree = degree + 7 }
					{ $~ } { legato = 1.01 }
					{ $_ } { legato = 0.9 }
					{ $. } { legato = 0.4 }
				};
				// degree -> legato  // Association identifies pitch above
				SequenceNote(degree, nil, legato)
			}
			// { "~_.".includes(pitchStr[0]) } { pitchStr[0] }  // not sure why I did this
			{ pitchStr[0] != $  } {
				// Rest(0) -> legato
				// also, now we need to distinguish between rests and replaceable slots
				SequenceNote(Rest(pitchStr[0].ascii), nil, legato)
			}
			{ nil }
		}
		{ pitchStr == $  } { nil }
		{ pitchStr }
	};

	// pitchNum is a scale degree, assuming 0 as neutral-octave tonic
	// accidentals encoded by +/- 0.1
	// currently assuming 7 notes per octave -- will have to fix this
	~encodePitch = { |seqNote|
		var pitchNum = seqNote.asFloat,
		natural = pitchNum.round,
		octave = natural div: 7,
		class = natural - (octave * 7),
		octaveChar = if(octave < 0) { $, } { $' },
		accidentalChar = if(pitchNum < natural) { $- } { $+ },
		str = (class + 1).asString;
		(pitchNum absdif: natural * 10).do { str = str ++ accidentalChar };
		octave.abs.do { str = str ++ octaveChar };
		case
		{ seqNote.length <= 0.4 } { str = str ++ "." }
		{ seqNote.length > 1 } { str = str ++ "~" };
		str
	};
	// CODE GENERATION

	~buildStatement = {
		var stmt = CollStream.new;
		if(~parm.notNil) {
			Error("\clPatternSet: Pattern string should have been handled outside of buildStatement").throw;
		} {
			"%(%).setPattern(%, %, %, %, %, %)".format(
				~clClass, ~objKey.asCompileString,
				~phrase.asCompileString, nil, nil,  // we already know ~parm is nil in this branch
				~group.asPatString, ~inString.asCompileString, ~quant
			);
		};
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
	~process = { |stream, checkDoubleStar = true|
		var ch, continue = true, str = String.new, rpt;
		if(checkDoubleStar) { ~checkDoubleStar.(stream) };
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
				~items = ~items.add(PR(\clCompGrouping).copy.process(stream, false));
				if(~items.last.tryPerform(\weight).notNil) {
					~hasWeights = true;
				};
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
			while { ch = stream.next; "*%".includes(ch) } {
				rpt = tryNoTrace {
					~scanInt.(stream).asInteger;
				} { |exc|
					if(exc.what == \nonInt) { nil } { exc.throw };
				};
				if(ch == $*) {
					~repeats = rpt ?? { 1 };
				} {
					if(rpt.notNil) {
						~weight = rpt;
					};
				};
			};
			stream.pos = stream.pos - 1;
		// };
		};
		currentEnvironment
	};

	~checkDoubleStar = { |stream|
		var str = stream.collection, regex, id, num, index;
		regex = str.findRegexp("([A-Za-z0-9_]+)\\*\\*([0-9]+)");
		if(regex.notNil) {
			// based on the above regexp, all matches should come in groups of three:
			// 0. The full matching string, e.g. "key**num"
			// 1. The first paren group (the key), e.g. "key"
			// 2. The second paren group (num)
			// It should be impossible to have multiple matches for either of the paren groups.
			// clump(3) clusters them.
			// reverseDo means that I don't have adjust indices.
			// This generates syntax like '^key0', which will be further expanded later!
			regex.clump(3).reverseDo { |triplet|
				id = triplet[1][1];
				num = triplet[2][1].asInteger;
				index = triplet[0][0];
				str = "%(%)%".format(
					if(index > 0) { str[ .. index - 1] } { "" },
					Array.fill(num, { |i| "'^%%'".format(id, i) }).join("."),
					if(index + triplet[0][1].size < str.size) {
						str[index + triplet[0][1].size .. ]
					} { "" }
				);
			};
			stream.collection = str;
		};
		stream
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
							quant = nil;
						} {
							"clStartStop: % is not a valid quant indicator".format(row.last).warn;
						};
					};
					if(row.last[1].first == $+) { method = \play } { method = \stop };
					if(row.any { |pair| pair[1] == "all" }) {
						keys = BP.keys.as(Array);
					} {
						keys = row.drop(-1).flop[1].collect(_.asSymbol).select { |key| BP.exists(key) };
					};
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
		result  // .debug("clStartStop result");
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
		PR(\chucklibLiveCode).replaceRegexpMacros("(`id)(%.`id|%.`id%*`int)? % (`id)"
			.format($\\, $\\, $\\, ~eqCheckStr))
	};
	~parseIDs = { |code|
		var ids = code.findRegexp(~idRegexp.()), obj, i;
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
					~srcPhrase = ~srcPhrase.drop(1);
					i = ~srcPhrase.indexOf($*);
					if(i.notNil) {  // discard number, if it exists
						~srcPhrase = ~srcPhrase[0 .. i-1];
					};
					~srcPhrase = ~srcPhrase.asSymbol;
				};
				if(ids[3][1].size > 0) {
					~numToCopy = ids[3][1].asInteger;
				};
				if(~numToCopy.isNil) {
					if(obj.phrases[~srcPhrase].isNil) {
						Error("%: Source phrase % doesn't exist".format(~name, ~srcPhrase.asCompileString)).throw;
					};
				} {
					if((0 .. ~numToCopy-1).any { |i| obj.phrases[(~srcPhrase ++ i).asSymbol].isNil }) {
						Error("%: Not prepared for % bars of source phrases %".format(
							~name, ~numToCopy, ~srcPhrase.asCompileString
						)).throw;
					};
				};
				~targetPhrase = ids[4][1].asSymbol;
			};
		};
		currentEnvironment
	};

	~buildStatement = {
		var objStr = "%(%)".format(~clClass, ~objKey.asCompileString);
		if(~numToCopy.isNil) {
			~buildOneStatement.(objStr, nil);
		} {
			Array.fill(~numToCopy, { |i|
				~buildOneStatement.(objStr, i)
			}).join(";\n")
		}
	};

	~buildOneStatement = { |objStr, index|
		var stmt, obj, srcPhrase, targetPhrase;
		if(index.isNil) {
			srcPhrase = ~srcPhrase.asCompileString;
			targetPhrase = ~targetPhrase.asCompileString;
		} {
			srcPhrase = "'%'".format(~srcPhrase ++ index);
			targetPhrase = "'%'".format(~targetPhrase ++ index);
		};
		stmt = "%.phrases[%] = %.phrases[%].deepCopy; %.phraseDurs[%] = %.phraseDurs[%]".format(
			objStr, targetPhrase,
			objStr, srcPhrase,
			objStr, targetPhrase,
			objStr, srcPhrase
		);
		if(~clClass.exists(~objKey)) {
			obj = ~clClass.new(~objKey);
			obj.phrases[srcPhrase.drop(1).drop(-1).asSymbol].pairs.pairsDo { |key, value|
				stmt = "%;\n%".format(stmt, ~copyPhraseStringCmd.(key, objStr, srcPhrase, targetPhrase));
			};
		};
		stmt
	};

	~copyPhraseStringCmd = { |key, objStr, srcPhrase, targetPhrase|
		// default phrases have [\key, \dur]
		// aliases need not be considered here
		if(key.isArray and: { key.last == \dur }) {
			key = key[0];
		};
		key = key.asCompileString;
		"%.prSetPhraseString(%, %, %.phraseStringAt(%, %))".format(
			objStr, targetPhrase, key,
			objStr, srcPhrase, key
		);
	};
} => PR(\clCopyPattern);

PR(\clCopyPattern).clone {
	~name = "patternXfer";
	~eqCheckStr = "->>";

	~superBuildStatement = ~buildStatement;

	// do the same as super.buildStatement, but add the phrase pattern
	~buildStatement = {
		var stmt = ~superBuildStatement.(),
		obj = ~clClass.new(~objKey),
		phraseSeq = obj.phraseSeqString;  // should work with composite patterns
		if(~numToCopy.isNil) {
			phraseSeq = phraseSeq.replace(~srcPhrase.asCompileString, ~targetPhrase.asCompileString);
		} {
			// going in reverse order should make sure e.g. m11 gets replaced before m1
			// this is still borderline risky; it may replace other syntax accidentally
			~numToCopy.reverseDo { |i|
				phraseSeq = phraseSeq.replace(
					~srcPhrase ++ i,
					~targetPhrase ++ i
				);
			};
		};
		"%;\n%(%).setPattern('main', nil, %)".format(
			stmt,
			~clClass, ~objKey.asCompileString,
			phraseSeq  // it's already a compileString
		)
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
	~reachedStringTest = { |ch| ch.isAlpha or: { "/\"".includes(ch) } };

	~process = { |code|
		var stream = CollStream(code),
		ch, bp, srcBP, srcParm;

		~idString = ~parseIDs.(stream);
		if(BP.exists(~idString[0].asSymbol).not) {
			Error("Generator statement failed: BP('%') does not exist.".format(~idString[0])).throw;
		};
		if(~idString[1].isNil) {
			Error("Generator statement failed: No phrase prefix given.").throw;
		};
		bp = BP(~idString[0].asSymbol);
		~isPitch = bp.parmIsPitch(~idString[2] ?? { bp.defaultParm });

		while { (ch = stream.next).notNil and: { ~reachedStringTest.(ch).not } } {
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
			{ ch == $% } {
				~subdiv = ~parseResolution.(stream);
			}
			{ ch.notNil and: { ~reachedStringTest.(ch).not } } {
				stream.pos = stream.pos - 1;
				~beatsPerBarSpec = ~skipUpToCond.(stream, ~reachedStringTest);
			};
			~skipUpToCond.(stream);
		};
		if(ch.isNil or: { ~reachedStringTest.(ch).not }) {
			Error("Generator statement: no pattern string found: %".format(code)).throw;
		};
		if(ch == $") {
			~string = ~skipUpToCond.(stream, { |ch| ch == $" });
		} {
			// it's either /bp.phrase.parm or phrase.parm
			~hasBPname = (ch == $/);
			if(~hasBPname.not) {
				stream.pos = stream.pos - 1;
			};
			~srcIDs = ~parseIDs.(stream, false);  // no error on 'nil' at end
			if(~hasBPname) {
				if(BP.exists(~srcIDs[0].asSymbol)) {
					srcBP = BP(~srcIDs[0].asSymbol);
					~srcIDs = ~srcIDs.drop(1);
				} {
					Error("Generator statement: Source BP(%) not found".format(~srcIDs[0])).throw;
				};
			} {
				srcBP = bp;
			};
			srcParm = ~srcIDs[1] ?? { ~idString[2] };
			~string = srcBP.phraseStringAt(~srcIDs[0].asSymbol, if(srcParm.notNil) { srcParm.asSymbol });
		};
		~template = ~expandString.(~string);
		~variants = Array.fill(~numVariants, { |i|
			var variant = ~makeVariant.(~template);
			~issueCommand.(variant, i);
			variant
		});
		~postVariants.(~variants);

		"nil"  // return a dummy statement to interpret
	};

	~parseIDs = { |stream, errorOnNil(true)|
		var str = String.new, ch;
		while { (ch = stream.next).notNil and: { " *".includes(ch).not } } {
			str = str ++ ch;
		};
		case
		{ ch.isNil } {
			if(errorOnNil) {
				Error("Incomplete generator statement: %".format(stream.collection)).throw
			};
		}
		{ ch.isSpace } {
			~skipUpToCond.(stream);
		}
		{ ch == $* } {
			stream.pos = stream.pos - 1;
		};
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
		prevItem, nextItem;
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
				nextItem = ~scanForward.(template, *indexPair);
				template[indexPair[0]][indexPair[1]] = ~item.eval(prevItem, nextItem, *~funcArgs);
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

	~scanForward = { |template, segIndex, itemIndex|
		var thing;
		block { |break|
			while { segIndex < template.size } {
				while { itemIndex < template[segIndex].size } {
					thing = template[segIndex][itemIndex];
					if(thing.notNil and: { thing != $  }) {
						break.(thing);
					} {
						itemIndex = itemIndex + 1;
					};
				};
				segIndex = segIndex + 1;
				itemIndex = 0;
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

// pass code through to the BP
Proto {
	~clClass = BP;
	~endIDChar = $(;
	~process = { |code|
		~parseIDs.(code);
		~getCode.(code);
		"%(%).%".format(~clClass, ~objKey.asCompileString, ~codeToPass);
	};

	~parseIDs = { |code|
		var i, ids, test;
		~parenIndex = code.indexOf(~endIDChar);
		// everything before ~parenIndex should be the ID string
		ids = code[.. ~parenIndex - 1].split($.).collect(_.trim);  // wslib dependency
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
			Error("clPassThru: %(%) does not exist.".format(~clClass.name, ~objKey.asCompileString)).throw;
		};
		// ignore the rest -- not looking at phrases or parms
	};

	~getCode = { |code|
		~codeToPass = ~stringInMatchingBrackets.(code[~parenIndex..]).drop(1).drop(-1);
	};

	// algorithm is not correct: brackets may be closed out of order
	~stringInMatchingBrackets = { |str|
		var stream = CollStream(str), ch, paren = 0, brackets = 0, braces = 0, hitBracket = false;
		if(str.isKindOf(Stream)) {
			stream = str;
		} {
			stream = CollStream(str);
		};
		str = String.new;
		ch = stream.next;
		while { ch.notNil } {
			str = str ++ ch;
			case
			{ ch == $( } { paren = paren + 1; hitBracket = true }
			{ ch == $) } { paren = paren - 1; hitBracket = true }
			{ ch == $[ } { brackets = brackets + 1; hitBracket = true }
			{ ch == $] } { brackets = brackets - 1; hitBracket = true }
			{ ch == ${ } { braces = braces + 1; hitBracket = true }
			{ ch == $} } { braces = braces - 1; hitBracket = true };
			if(hitBracket and: { max(max(paren, brackets), braces) == 0 }) {
				ch = nil;
			} {
				ch = stream.next
			};
		};
		if(max(max(paren, brackets), braces) == 0) {
			str
		} {
			Error("clPassThru: Brackets were not closed properly: %".format(stream.collection)).throw;
		};
	};
} => PR(\clPassThru);

PR(\clPassThru).clone {
	~endIDChar = $=;
	~process = { |code|
		~parseIDs.(code);
		~getCode.(code);
		"%(%) =>%".format(~clClass, ~objKey.asCompileString, ~codeToPass).debug("clChuck");
		// "%.eval(%)".format(~objKey.asCompileString, ~codeToPass);
	};
	~getCode = { |code|
		var i = code.find("=>");
		if(i.isNil) { Error("No '=>' in clChuck statement; this should never happen").throw };
		~codeToPass = code[i+2..];
	};
} => PR(\clChuck);

PR(\clPassThru).clone {
	~clClass = Func;
	~process = { |code|
		~parseIDs.(code);
		~getCode.(code);
		"%.eval(%)".format(~objKey.asCompileString, ~codeToPass);
	};
} => PR(\clFuncCall);

// hack: should define in a different order
PR(\clPatternSet).v.import((clPassThru: #[stringInMatchingBrackets]));

Proto {
	~clClass = BP;
	~process = { |code|
		~parseIDs.(code);
		~writeCode.();
	};
	~parseIDs = { |code|
		var match = code.findRegexp("\\([A-Za-z0-9_:]+(/[A-Za-z0-9_:]+)*\\)");
		if(match.size == 0) {
			Error("clMake: Invalid IDs in parentheses").throw;
		};
		match = match[0][1];
		match = match[1 .. match.size - 2].split($/);
		~ids = Array(match.size);
		match.do { |str|
			str = str.trim;
			if(str.size > 0) {
				if(str.includes($:)) {
					str = str.split($:).keep(2).collect { |item| item.trim.asSymbol };
					if(Fact.exists(str[0])) {
						~ids.add(Association(*str));  // factKey -> bpKey
					} {
						Error("clMake: Fact(%) does not exist".format(str[0].asCompileString)).throw;
					};
				} {
					str = str.asSymbol;
					if(Fact.exists(str)) {
						~ids.add(Association(str, Fact(str).v[\defaultName] ?? { str }));
					} {
						Error("clMake: Fact(%) does not exist".format(str.asCompileString)).throw;
					};
				};
			};
		};
		~ids
	};
	~writeCode = {
		var out = CollStream.new;
		~ids.do { |id, i|
			if(out.pos > 0) {
				out << ";\n";
			};
			if(Fact.exists(id.key)) {
				out << "Fact(%) => %(%)".format(
					id.key.asCompileString,
					~classForFactoryType.(id.key),
					id.value.asCompileString
				);
				// if(Fact(id).v[\defaultBP].notNil) {
				// 	if(BP.exists(Fact(id).v[\defaultBP])) {
				// 		out << ";\n%(%) => BP(%)".format(
				// 			~classForFactoryType.(id),
				// 			(Fact(id).v[\defaultName] ?? { id }).asCompileString,
				// 			Fact(id).v[\defaultBP].asCompileString
				// 		)
				// 	} {
				// 		"%(%) should go into BP(%), but it doesn't exists. Remember to => manually"
				// 		.format(
				// 			~classForFactoryType.(id),
				// 			(Fact(id).v[\defaultName] ?? { id }).asCompileString,
				// 			Fact(id).v[\defaultBP].asCompileString
				// 		)
				// 		.warn;
				// 	};
				// };
			};
		};
		out.collection
	};
	~classForFactoryType = { |key|
		switch(Fact(key).type)
		{ \bp } { BP }
		{ \vc } { VC }
		{ \voicer } { VC }
	};
} => PR(\clMake);

Proto {
	~clClass = BP;
	~process = { |code|
		var pos;

		if(~versionOK.isNil) {
			~versionOK = Main.versionAtLeast(3, 7) and: { Platform.ideName == "scqt" };
			PR(\clPatternToDoc).versionOK = ~versionOK;
		};
		if(~versionOK) {
			~eqIndex = code.size;  // hack: no '=' in this command syntax
			~parseIDs.(code);
			if(~numToApply.notNil) {
				~phrases = [[~phrase], (0 .. ~numToApply - 1)].flop.collect({ |array| array.join.asSymbol });
			} {
				~phrases = [~phrase]
			};

			try {
				~strings = ~phrases.collect { |phrase|
					~clClass.new(~objKey).phraseStringAt(phrase, ~parm);
				};
			} { |err|
				Error("% while looking up pattern string".format(err.errorString.asCompileString)).throw;
			};

			if(~strings.any(_.isNil)) {
				"Requested pattern hasn't been created".warn;
			} {
				if(~stepForward.isNil) {
					~checkStepForward.();
				};

				pos = 'Document'.asClass.current.selectionStart + 'Document'.asClass.current.selectionSize;
				if(~numToApply.isNil) {
					if(~stepForward and: {
						'Document'.asClass.current.string(pos - 1, 1) == "\n"
					}) { pos = pos - 1 };
					'Document'.asClass.current.string_(" = %;".format(~strings[0]), pos, 0);
				} {
					if(~parm == ~clClass.new(~objKey).defaultParm) { ~parm =nil };
					'Document'.asClass.current.string_(
						~strings.collect { |string, i|
							"/%.%% = %;\n".format(~objKey, ~phrases[i],
								if(~parm.notNil) { "." ++ ~parm } { "" },
								string
							)
						}.join,
						pos, 0
					)
				};
			};
		} {
			"Can't add the string into the document: wrong version (%) or editor (%)"
			.format(Main.version, Platform.ideName)
			.warn;
		};
		""  // the result is the side effect: don't run any code
	};

	~checkStepForward = {
		var path = Platform.userConfigDir +/+ "sc_ide_conf.yaml",
		file = File(path, "r"),
		line;
		if(file.isOpen) {
			protect {
				while { (line = file.getLine).notNil and: { line.contains("stepForwardEvaluation").not } };
			} { file.close };
			if(line.isNil) {
				~stepForward = false
			} {
				~stepForward = line.split($ ).last.interpret;
				PR(\clPatternToDoc).stepForward = ~stepForward;  // save for next time
			};
		} {
			Error("Could not open config file at %".format(path.asCompileString)).throw;
		};
		~stepForward
	}
}.import((clPatternSet: #[parseIDs])) => PR(\clPatternToDoc);


// command registers: save groups of commands, for bigger textural shifts
Proto {
	~default = \default;
	~autoResetDefault = true;
	~prep = {
		~registers = IdentityDictionary.new;
	};
	~process = { |code|
		var cmdIndex = code.detectIndex { |ch| (ch.isAlpha or: { ch.isDecDigit or: { ch == $_ } }).not },
		cmd, regId, func, outcode;
		if(cmdIndex.notNil) {
			regId = code[..cmdIndex - 1].trim;
			if(regId.isEmpty) { regId = ~default };
			regId = regId.asSymbol;
			cmd = code[cmdIndex];
			switch(cmd)
			{ $/ } {
				if(~registers[regId].isNil) {
					~emptyRegister.(regId);
				};
				"BP(%).registers[%].add(%)".format(
					~collIndex.asCompileString,
					regId.asCompileString,
					code[cmdIndex + 1 .. ].asCompileString
				);
			}
			{ $! } {
				"BP(%).emptyRegister(%)".format(
					~collIndex.asCompileString,
					regId.asCompileString
				);
			}
			{ $* } {
				outcode = CollStream.new;
				~registers[regId].do { |stmt, i|
					if(i > 0) { outcode << ";\n" };
					outcode << PR(\chucklibLiveCode).process(stmt);
				};
				if(~autoResetDefault and: { regId == ~default }) {
					~clear.(~default);
				};
				outcode.collection
			}
			{ $? } {
				"Register %\n".postf(regId.asCompileString);
				~registers[regId].do { |stmt|
					stmt.postln;
				};
				"nil"
			}
		} {
			Error("Invalid register command '%'".format(cmd)).throw;
		};
	};
	~emptyRegister = { |key|
		~registers[key] = List.new;
	};
} => PR(\clRegister);

// livecode-able process prototype

// first: there's some ugliness about pitched note lengths
// hard to work with ProtoEvent(\voicerNote).
// Here's a function to get the actual sustain time:

{ |ev|
	case
	// even more ugliness: legato branch *must* come first. Really bad design.
	{ ev[\legato].notNil } { ev[\dur] * ev[\legato] }
	{ ev[\sustain].notNil } { ev.use { ev[\sustain].value } }
	{ ev[\dur] }
} => Func(\evLength);

(
Proto {
	~event = (eventKey: \singleSynthPlayer);

	~defaultParm = \go;
	~parmMap = (
		go: ($x: 0)
	);
	~beatsPerBar = {
		if(~clock.isNil) { ~clock = TempoClock.default };
		~clock.beatsPerBar
	};
	~division = 0.25;
	// see end for default phraseSeq
	~lastPhrase = \main;

	~prep = {
		if(~phrases.isNil) {
			~phrases = IdentityDictionary[
				\main -> PbindProxy([~defaultParm, \dur], nil),
				\rest -> PbindProxy(\dur, Pfuncn { Rest(~clock.beatsPerBar) })
			];
			~phraseDurs = IdentityDictionary[
				// function is allowed here b/c Pfindur will evaluate it
				\main -> { ~clock.beatsPerBar },
				\rest -> { ~clock.beatsPerBar },
			];
			~phraseStrings = MultiLevelIdentityDictionary.new;
		};
		~userprep.();
		~postParmMap.();
		// for backward compatibility: User might have put in a stopCleanup func
		// but I need it, to clear highlights
		// so, check and move the user's function if needed.
		// This will work if stopCleanup was provided as an entry in the chuck parameter dictionary.
		if(~stopCleanup.notNil) {
			~userStopCleanup = ~stopCleanup;
		};
		~stopCleanup = {
			~clearHighlights.();
			~userStopCleanup.();
		};
		Environment.current
	};
	~freeCleanup = {
		~userfree.();
	};

	~valueForParm = { |event, parm, inEvent|
		var dict, result;
		if(~debug ?? { false }) { [event, parm, inEvent].debug(">> valueForParm") };
		result = case
		{ parm == \dur } { event }
		{ ~parmIsPitch.(parm) } {
			if(event.isKindOf(SequenceNote)) {
				if(event.length <= 0.4) {
					// this must be a function because ~dur is not populated yet!
					inEvent[\sustain] = { min(0.15, ~dur * event.length) };
				} {
					inEvent[\legato] = event.length
				};
				// not asFloat: We already know this is a SeqNote, and asFloat breaks Rests here.
				event.freq
			} {
				if(event == \rest) {
					inEvent[\legato] = 0.9;
					Rest(~parmMap[parm][\rest] ?? { 0 })
				} {
					event
				}
			};
		} {
			dict = ~parmMap[parm];
			result = if(dict.notNil) {
				if(dict[\convertFunc].notNil) {
					dict[\convertFunc].value(event, inEvent)
				} {
					dict[event]
				};
			};
			if(event == \rest) {
				Rest(result ?? { 0 })
			} {
				result  // nil if not specified
			};
		};
		if(~debug ?? { false }) { result.debug("<< valueForParm") };
		result
	};
	~parmIsPitch = { |parm|
		~parmMap[parm].notNil and: { ~parmMap[parm][\isPitch] == true }
	};

	~defaults = ();  // or Pbind
	~postDefaults = ();

	~phraseStringAt = { |phrase, parm(~defaultParm)|
		~phraseStrings.at(phrase, parm);
	};
	~prSetPhraseString = { |phrase, parm(~defaultParm), string|
		~phraseStrings.put(phrase, parm, string);
		// advise clients (e.g. GUIs) of new content
		NotificationCenter.notify(\clLiveCode, \phraseString, [~collIndex, phrase, parm, string]);
		currentEnvironment
	};
	~setPattern = { |phrase, parm, inParm, pattern, inString, newQuant|
		var pat = ~phrases[phrase],
		storeParm;
		if(parm.size > 0) {
			storeParm = parm.collect { |p|
				~parmMap[p].tryPerform(\at, \alias) ?? { p };
			}
		} {
			storeParm = ~parmMap[inParm].tryPerform(\at, \alias) ?? { parm };
		};
		if(pat.isNil) {
			pat = PbindProxy([~defaultParm, \dur], nil);
			~phrases[phrase] = pat;
		};
		if(parm.notNil) {
			~prSetPhraseString.(phrase, inParm, inString);
			~clock.schedAbs(BP(~collIndex).eventSchedTime(-1) - 0.001, e {
				case
				// does this ever happen?
				{ parm == \dur } {
					if(~debug ?? { false }) { "dur pattern".debug };
					pat.set(parm, pattern);
				}
				{ storeParm == parm } {
					if(parm.isArray) {
						if(~debug ?? { false }) { "storeParm == parm array".debug };
						// special case: This should happen only for [key, \dur] type keys
						// constructed by clPattern
						pat.set(parm, pattern.collect { |valueIDs, inEvent|
							valueIDs.collect { |valueID, i|
								~valueForParm.(valueID, parm[i], inEvent) ?? { Rest(valueID) }
							}
						});
					} {
						if(~debug ?? { false }) { "storeParm === parm single".debug };
						// single key, no alias: optimize by simply converting and returning
						pat.set(parm, pattern.collect { |valueID, inEvent|
							~valueForParm.(valueID, parm, inEvent) ?? { Rest(valueID) }
						});
					};
				}
				{ storeParm.size == 0 } {
					if(~debug ?? { false }) { "storeParm.size == 0".debug };
					// alias is a single symbol: optimize by converting and storing directly
					pat.set(parm, pattern.collect { |valueID, event|
						var result = ~valueForParm.(valueID, parm, event) ?? { Rest(valueID) };
						event.put(storeParm, result.processRest(event));
						result
					});
				}
				// arrayed *non-default* parm with alias
				{ storeParm.size > 0 and: { parm.asArray.includes(\dur).not } } {
					if(~debug ?? { false }) { "storeParm array without dur".debug };
					pat.set(parm, pattern.collect { |valueID, event|
						var result = ~valueForParm.(valueID, inParm, event);
						if(result.isNil) {
							result = [Rest(valueID)];
						} {
							result = result.asArray;
						};
						if(storeParm.size >= result.size) {
							storeParm.do { |key, i|
								event.put(key, result.wrapAt(i).processRest(event));
							};
						} {
							"Alias for % allows % values, but too many (%) were provided"
							.format(parm.asCompileString, storeParm.size, result.size)
							.warn;
						};
						result
					});
				}
				// arrayed *default* parm with alias -- assuming [id, dur] here
				{ storeParm.size > 0 } {
					if(~debug ?? { false }) { "storeParm array with dur".debug };
					pat.set(parm, pattern.collect { |valueID, event|
						var result = ~valueForParm.(valueID[0], inParm, event), sp0;
						if(result.isNil) {
							result = [Rest(valueID[0])];
						} {
							result = result.asArray;
						};
						sp0 = storeParm[0].asArray;
						if(sp0.size >= result.size) {
							sp0.do { |key, i|
								event.put(key, result.wrapAt(i).processRest(event));
							};
						} {
							"Alias for % allows % values, but too many (%) were provided"
							.format(parm.asCompileString, sp0, result.size)
							.warn;
						};
						// valueID[1] should be the dur value -- necessary to be second return value
						[result, valueID[1]]
					});
				}
				{
					// it really should be impossible to get here
					Error("BP(%): Impossible condition assigning pattern %, storeParm = %"
						.format(~collIndex.asCompileString, parm.asCompileString,
							storeParm.asCompileString
					)).throw;
				};
				nil
			});
		} {
			~clock.schedAbs(BP(~collIndex).eventSchedTime - 0.001, e {
				// composite pattern
				~phraseSeq_.(pattern);
				if(newQuant.notNil) { ~quant = newQuant.asTimeSpec };
				if(~isPlaying) { ~reschedule.() };
				nil
			});
		};
	};
	~setPhraseDur = { |phrase, dur|
		~phraseDurs[phrase] = dur;
		currentEnvironment
	};

	~clearHighlights = {
		~phrases.keysDo { |key|
			NotificationCenter.notify(~collIndex, key, false);
		};
	};

	~asPattern = {
		var phr, emptyCountdown = 10, lastTime = 0;
		~reset.();
		~playHook.();
		Pchain(
			Pif(
				Pfunc { |ev| phr == \rest or: { ev.isRest } },
				(),
				BPStream(\postDefaults), //.trace(prefix: "postDefaults: "),
			),
			Prout { |inevent|
				var pat;
				~makeStreamForKey.(\phraseSeq);
				loop {
					if(thisThread.beats > lastTime) {
						lastTime = thisThread.beats;
					} {
						"BP(%): Empty phrase %, stopping".format(~collIndex.asCompileString, phr).warn;
						nil.alwaysYield
					};
					phr = ~phraseSeqStream.next(inevent);
					pat = ~phrases[phr];
					// the pattern-wrapping here is not directly supported in Psym
					if(~phraseDurs[phr].notNil) {
						pat = Pfindur(~phraseDurs[phr], pat);
					};
					inevent = pat.embedInStream(inevent);
				};
			}, // .trace(prefix: "phrase: "),
			BPStream(\defaults) //.trace(prefix: "\n\ndefaults: ")
		)
	};

	~reschedule = { |quant|
		var oldStreamPlayer = ~eventStreamPlayer,
		newStreamPlayer = BP(~collIndex).asEventStreamPlayer;
		if(quant.isNil) { quant = ~quant ?? { BasicTimeSpec(-1) } };
		~clock.schedAbs(quant.asTimeSpec.bpSchedTime(BP(~collIndex)) - 0.001, { oldStreamPlayer.stop });
		~clock.play(newStreamPlayer.refresh, quant);
		currentEnvironment
	};

	~resetToQuant = { |quant|
		if(quant.isNil) { quant = ~quant ?? { BasicTimeSpec(-1) } };
		~clock.schedAbs(quant.asTimeSpec.bpSchedTime(BP(~collIndex)) - 0.01, e {
			~reset.();
			~reschedule.(quant);
		});
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

	~phraseSeq_ = { |pattern|
		var lastPhrase;
		pattern = pattern.asPattern;
		// for xfer pattern: each ->> command would wrap another layer of Pcollect
		// so, save the string *without* .collect
		~phraseSeqString = pattern.asCompileString;
		~phraseSeq = pattern.collect { |phr|
			~lastPhrase = lastPhrase;
			if(phr != ~lastPhrase) {
				NotificationCenter.notify(~collIndex, ~lastPhrase, false);
			};
			NotificationCenter.notify(~collIndex, phr, true);
			lastPhrase = phr;  // leave the Proto var alone, until next time
		};
	};
	~phraseSeq_.(\main);
}.import((abstractProcess: #[makeStreamForKey])) => PR(\abstractLiveCode);
);

(thisProcess.nowExecutingPath.dirname +/+ "parsenodes.scd").loadPath;
