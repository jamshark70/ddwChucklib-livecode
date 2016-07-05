
Proto {
	~begin = nil;
	~end = nil;
	~selectionStart = { ~begin };
	~selectionSize = { ~end - ~begin + 1 };
	~siblings = {
		if(~parentNode.notNil) { ~parentNode.children };  // else nil
	};
	~index = {
		var sibs = ~siblings.();
		if(sibs.notNil) { sibs.indexOf(currentEnvironment) };  // else nil
	};
	~nearbySib = { |incr = 1|
		var sibs = ~siblings.(), i;
		if(sibs.notNil) {
			i = sibs.indexOf(currentEnvironment);
			if(i.notNil) {
				sibs[i + incr]  // may be nil
			}
		};  // else nil
	};
	~setLastSelected = {
		var i = ~index.();
		if(i.notNil) { ~parentNode.tryPerform(\lastSelected_, i) };
		currentEnvironment
	};

	~prep = { |stream, parentNode|
		~parentNode = parentNode;
		~children = Array.new;
		// if(stream.peek == $/) { stream.next };
		~begin = stream.pos;
		~parse.(stream);
		if(~end.isNil) { ~end = stream.pos - 1 };
		if(~string.isNil) {
			if(~end >= ~begin) {
				~string = stream.collection[~begin .. ~end]
			} {
				~string = String.new;
			};
		};
		currentEnvironment
	};

	// utility function
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

	~idAllowed = "_";
	~getID = { |stream, skipSpaces(true)|
		var str = String.new, ch, begin;
		if(skipSpaces) { ~skipSpaces.(stream) };
		begin = stream.pos;
		while {
			ch = stream.next;
			ch.notNil and: { ch.isAlphaNum or: { ~idAllowed.includes(ch) } }
		} {
			str = str.add(ch);
		};
		if(ch.notNil) { stream.rewind(1) };
		PR(\clStringNode).copy
		.put(\parentNode, currentEnvironment)
		.put(\string, str)
		.put(\begin, begin)
		.put(\end, begin + str.size - 1);
	};
	~skipSpaces = { |stream|
		var ch;
		while {
			ch = stream.next;
			ch.notNil and: { ch.isSpace }
		};
		if(ch.notNil) { stream.rewind(1) };
	};
} => PR(\abstractClParseNode);

PR(\abstractClParseNode).clone {
	// assumes you've skipped the opening delimiter
	~parse = { |stream|
		var str = String.new, ch;
		while {
			ch = stream.next;
			ch.notNil and: { ~endTest.(ch) }
		} {
			str = str.add(ch);
		};
		if(ch.notNil) { stream.rewind(1) };
		str
	};
	~endTest = { |ch| ch == $\" };
	~symbol = { ~string.asSymbol };
} => PR(\clStringNode);

PR(\abstractClParseNode).clone {
	~types = [
		Float -> "[\\-+]?[0-9]*\\.?[0-9]+([eE][\\-+]?[0-9]+)?",
		Integer -> "(-[0-9]+|[0-9]+)"
	];
	~parse = { |stream|
		// hack into CollStream -- will fail with other IO streams
		var match,
		type = ~types.detect { |assn|
			match = stream.collection.findRegexpAt(assn.value, stream.pos);
			match.notNil
		};
		if(match.notNil) {
			~string = stream.nextN(match[1]);  // match[1] = length of match
			~value = ~string.perform(("as" ++ type.key.name).asSymbol);
		};  // else leave state variables nil
	};
} => PR(\clNumberNode);

PR(\abstractClParseNode).clone {
	~isPitch = false;
	~endChars = "|\"";
	~parse = { |stream|
		var ch;
		// stream.collection[stream.pos .. stream.pos + 10].debug(">> clDividerNode");
		~items = Array.new;
		while {
			ch = stream.next;
			ch.notNil and: { ~endChars.includes(ch).not }
		} {
			~items = ~items.add(~parseItem.(stream, ch));
		};
		if(ch.notNil) { stream.rewind(1) };
		// stream.collection[~begin .. ~begin + 10].debug("<< clDividerNode");
	};

	~parseItem = { |stream, ch|
		var new, begin;
		// [ch.asCompileString, stream.collection[stream.pos .. stream.pos + 10]].debug(">> parseItem");
		if(ch.isNil) { ch = stream.next };
		begin = stream.pos - 1;
		case
		{ ch == $\\ } {
			stream.rewind(1);
			new = PR(\clGeneratorNode).copy.put(\isPitch, ~isPitch).prep(stream, currentEnvironment);
			~children = ~children.add(new);
			new//.debug("<< parseItem");
		}
		{ ~isPitch } {
			if(ch.isDecDigit) {
				new = String.with(ch);
				while { (ch = stream.next).notNil and: { "+-',~_.".includes(ch) } } {
					new = new.add(ch);
				};
				if(ch.notNil) { stream.rewind(1) };
				~children = ~children.add(
					PR(\clStringNode).copy
					.put(\parentNode, currentEnvironment)
					.put(\string, new)
					.put(\begin, begin)
					.put(\end, begin + new.size - 1);
				);
				new//.debug("<< parseItem");
			} {
				new = String.with(ch);
				~children = ~children.add(
					PR(\clStringNode).copy
					.put(\parentNode, currentEnvironment)
					.put(\string, new)
					.put(\begin, begin)
					.put(\end, begin);  // one char only
				);
				new//.debug("<< parseItem");
			};
		} {
			new = String.with(ch);
			~children = ~children.add(
				PR(\clStringNode).copy
				.put(\parentNode, currentEnvironment)
				.put(\string, new)
				.put(\begin, begin)
				.put(\end, begin);
			);
			new//.debug("<< parseItem");
		};
	};
} => PR(\clDividerNode);

PR(\abstractClParseNode).clone {
	~isPitch = false;
	~types = [
		\clRhythmGenNode -> "^:[a-zA-Z0-9_]+\\(.*\\)",
		\clGeneratorNode -> "^\\\\[a-zA-Z0-9_]+\\(.*\\)",
		\clPatStringNode -> "^\".*\"",
		// \clStringNode -> "^\".*\"",
		\clNumberNode -> "^-?[0-9]"
	];
	~parse = { |stream|
		var name, ch;
		// stream.collection[stream.pos .. stream.pos + 10].debug(">> clGeneratorNode");
		if(stream.peek == $\\) { stream.next };
		name = ~getID.(stream);
		~children = ~children.add(name);
		~name = name.string;
		ch = stream.next;
		if(ch == $() {
			while {
				ch = stream.next;
				ch.notNil and: { ch != $) }
			} {
				stream.rewind(1);
				~children = ~children.add(~parseArg.(stream));
			};
			// if(ch.notNil) { stream.rewind(1) };
		} {
			// gen refs
			Error("Generator '%' has no argument list".format(~name)).throw;
		};
		// stream.collection[~begin .. ~begin + 10].debug("<< clGeneratorNode");
	};
	~parseArg = { |stream|
		var type, ch, new;
		// stream.collection[stream.pos .. stream.pos + 10].debug(">> parseArg");
		type = ~types.detect { |assn|
			assn.value.matchRegexp(stream.collection, stream.pos);
		};
		if(type/*.debug("type")*/.notNil) {
			new = PR(type.key).copy.put(\isPitch, ~isPitch).prep(stream, currentEnvironment);
		} {
			Error("Syntax error in % arg list".format(~name)).throw;
		};
		ch = stream.next;
		if(ch == $,) { ~skipSpaces.(stream) } {
			if(ch.notNil) { stream.rewind(1) };
		};
		// new.listArgs; "<< parseArg".debug;
		new
	};
} => PR(\clGeneratorNode);

PR(\clGeneratorNode).clone {
	~superParse = ~parse;
	~parse = { |stream|
		if(stream.peek == $:) { stream.next };
		~superParse.(stream);
	};
} => PR(\clRhythmGenNode);

PR(\abstractClParseNode).clone {
	~isPitch = false;
	~parse = { |stream|
		var str = String.new, ch, didOpenQuote = false;
		// stream.collection[stream.pos .. stream.pos + 10].debug(">> clPatStringNode");
		while {
			ch = stream.next;
			ch.notNil and: { didOpenQuote.not or: { ch != $\" } }
		} {
			~children = ~children.add(
				PR(\clDividerNode).copy.put(\isPitch, ~isPitch).prep(stream, currentEnvironment)
			);
			didOpenQuote = true;
		};
		// stream.collection[~begin .. ~begin + 10].debug("<< clPatStringNode");
	};
} => PR(\clPatStringNode);

PR(\abstractClParseNode).clone {
	~clClass = BP;
	~objExists = false;
	~phrase = \main;
	~parm = nil;  // filled in in 'parse'

	~idAllowed = "_*";
	~parse = { |stream|
		var i, ids, test, sym, temp, broke;

		broke = block { |break|
			// class (I expect this won't be used often)
			test = ~getID.(stream);
			~children = ~children.add(test);
			if(test.string.first.isUpper) {
				~clClass = test.symbol.asClass;
				if(stream.next != $.) { break.(true) };
				test = ~getID.(stream);
				~children = ~children.add(test);
			};

			// chucklib object key
			~objKey = test.symbol;  // really? what about array types?
			if(~clClass.exists(~objKey)) {
				~objExists = true;
			};
			if(stream.next != $.) { break.(true) };
			test = ~getID.(stream);
			~children = ~children.add(test);

			// phrase name
			test = test.string;
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
			if(stream.next != $.) { break.(true) };
			test = ~getID.(stream);
			~children = ~children.add(test);

			// parameter name
			~parm = test.string;
			false;
		};
		if(~parm.size == 0) {
			if(~objExists) {
				~parm = ~clClass.new(~objKey)[\defaultParm] ?? { \main };
			};
		} {
			~parm = ~parm.asSymbol;
		};
		if(broke) { stream.rewind(1) };
		currentEnvironment
	};
} => PR(\clIDNode);

PR(\abstractClParseNode).clone {
	~isPitch = false;
	~parse = { |stream|
		var id, obj;
		if(stream.peek == $/) { stream.next };
		id = PR(\clIDNode).copy.prep(stream, currentEnvironment);
		~children = ~children.add(id);
		~skipSpaces.(stream);
		if(stream.peek == $=) {
			stream.next;
		} {
			Error("clPatternSet must have '='").throw;
		};
		~skipSpaces.(stream);
		if(stream.peek == $\") {
			if(id.clClass.exists(id.objKey)) {
				obj = id.clClass.new(id.objKey);
				~isPitch = obj.tryPerform(\parmIsPitch, id.parm) ?? { false };
			};
			~children = ~children.add(
				PR(\clPatStringNode).copy.put(\isPitch, ~isPitch).prep(stream, currentEnvironment)
			);
		} {
			Error("Composite patterns not refactored yet").throw;
		};
	};
} => PR(\clPatternSetNode);
