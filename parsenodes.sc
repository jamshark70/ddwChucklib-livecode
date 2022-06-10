/**
    Chucklib-livecode: A framework for live-coding improvisation of electronic music
    Copyright (C) 2018  Henry James Harkins

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
**/

ClNumProxy {
	var <val, <valStream;

	*new { |value| ^super.new.init(value) }

	init { |value|  // anything that .asStream's and .next's to a number
		val = value;
		this.reset;
	}

	reset {
		valStream = val.asStream;
	}

	next { |inval|
		var out = valStream.next(inval);
		^if(out.notNil) {
			out
		} {
			this.reset;
			valStream.next(inval)
		};
	}
	value { |inval| ^this.next(inval) }

	// acts as its own stream
	// it's not a pattern, so it doesn't need stream independence
	// users shouldn't use this object directly as a pattern
	asStream {}
	isNumProxy { ^true }
}

ClAbstractParseNode {
	var <>begin, <>end, <>string, <>parentNode, <>children;
	var <time, <dur;
	var <>bpKey, <>isPitch = false, <>isMain = false, <>phrase, <>parm;
	var <>lastSelected;  // for GUI, not used for parsing

	*new { |stream, parentNode, properties|
		var new = super.new;
		if(properties.notNil) { new.putAll(properties) };
		^new.init(stream, parentNode)
	}

	*newEmpty { ^super.new }

	init { |stream, argParent|
		parentNode = argParent;
		children = Array.new;
		begin = stream.pos;
		this.parse(stream);
		if(end.isNil) { end = stream.pos - 1 };
		if(string.isNil) {
			if(end >= begin) {
				string = stream.collection[begin .. end]
			} {
				string = String.new;
			};
		};
	}

	// navigation
	selectionStart { ^begin }
	selectionSize { ^end - begin + 1 }
	siblings {
		^if(parentNode.notNil) { parentNode.children };  // else nil
	}
	index {
		var sibs = this.siblings;
		^if(sibs.notNil) { sibs.indexOf(this) };  // else nil
	}
	nearbySib { |incr = 1|
		var sibs = this.siblings, i;
		^if(sibs.notNil) {
			i = sibs.indexOf(this);
			if(i.notNil) {
				sibs[i + incr]  // may be nil
			}
		};  // else nil
	}
	setLastSelected {
		var i = this.index;
		if(i.notNil) { parentNode.tryPerform(\lastSelected_, i) };
	}

	// set properties
	putAll { |dict|
		dict.keysValuesDo { |key, value|
			this.perform(key.asSetter, value)
		}
	}

	// utility function
	unquoteString { |str, pos = 0, delimiter = $", ignoreInParens(false)|
		var i = str.indexOf(delimiter), j, escaped = false, parenCount = 0;
		^if(i.isNil) {
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
			};
			if(j - i <= 1) {
				String.new  // special case: two adjacent quotes = empty string
			} {
				str[i + 1 .. j - 1];
			};
		};
	}

	idAllowed { ^"_" }

	getID { |stream, skipSpaces(true)|
		var str = String.new, ch, begin;
		if(skipSpaces) { this.skipSpaces(stream) };
		begin = stream.pos;
		while {
			ch = stream.next;
			ch.notNil and: { ch.isAlphaNum or: { this.idAllowed.includes(ch) } }
		} {
			str = str.add(ch);
		};
		if(ch.notNil) { stream.pos = stream.pos - 1 };
		^ClStringNode.newEmpty
		.parentNode_(this)
		.string_(str)
		.begin_(begin)
		.end_(begin + str.size - 1)
	}
	skipSpaces { |stream|
		var ch;
		while {
			ch = stream.next;
			ch.notNil and: { ch.isSpace }
		};
		if(ch.notNil) { stream.pos = stream.pos - 1 };
	}

	streamCode { |stream| stream << string }
	setTime { |onset(0), argDur(4)|
		time = onset;
		dur = argDur;
	}
	isSpacer { ^false }
}

ClStringNode : ClAbstractParseNode {
	var <>openDelimiter = "", <>closeDelimiter = "", <>endTest;
	// assumes you've skipped the opening delimiter
	parse { |stream|
		var str = String.new, ch;
		if(endTest.isNil) { endTest = { |ch| ch == $\" } };
		while {
			ch = stream.next;
			ch.notNil and: { endTest.value(ch).not }
		} {
			str = str.add(ch);
		};
		if(ch.notNil) { stream.pos = stream.pos - 1 };
	}
	symbol { ^string.asSymbol }
	streamCode { |stream|
		stream << openDelimiter << string << closeDelimiter
	}
	isSpacer { ^string.every(_.isSpace) }
}

ClEventStringNode : ClStringNode {
	streamCode { |stream|
		stream << "(time: " << time << ", item: ";
		this.streamItem(stream);
		stream << ")";
	}

	streamItem { |stream|
		if(isPitch ?? { false }) {
			stream <<< this.decodePitch(string);
		} {
			if(string.size == 1) {
				stream <<< string[0];
			} {
				stream <<< string;
			};
		};
	}

	// whitespace placeholders and true rests end up being ClEventStringNodes
	// and you may have to call this to get a SequenceNote to render
	// so it has to be here, not in ClPitchEventNode
	decodePitch { |pitchStr|
		var degree, legato = 0.9, accent = false;
		^case
		{ isPitch and: { pitchStr.isString } } {
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
					{ $~ } { legato = inf /*1.01*/ }
					{ $_ } { legato = 0.9 }
					{ $. } { legato = 0.4 }
					{ $> } { accent = true }
				};
				// degree -> legato  // Association identifies pitch above
				SequenceNote(degree, nil, legato, if(accent) { \accent })
			}
			// { "~_.".includes(pitchStr[0]) } { pitchStr[0] }  // for articulation pools?
			{ "*@!".includes(pitchStr[0]) } { pitchStr[0] }  // placeholders for clGens etc.
			{ pitchStr[0] != $  } {
				// Rest(0) -> legato
				// also, now we need to distinguish between rests and replaceable slots
				SequenceNote(Rest(pitchStr[0].ascii), nil, legato)
			}
			{ nil }
		}
		{ pitchStr == $  } { nil }
		{ pitchStr }
	}
}

ClPitchEventNode : ClEventStringNode {
	parse { |stream|
		// while loop assumes that the pitch number has already been eaten
		var ch = stream.next, new = String.with(ch);
		while { (ch = stream.next).notNil and: { "+-',~_.>".includes(ch) } } {
			new = new.add(ch);
		};
		if(ch.notNil) { stream.pos = stream.pos - 1 };
	}

	streamItem { |stream|
		stream <<< this.decodePitch(string);
	}
}

ClChordNode : ClEventStringNode {
	*new { |stream, parentNode, properties|
		if(properties.isNil) {
			properties = IdentityDictionary.new;
		} {
			properties = properties.copy;
		};
		properties.put(\openDelimiter, $().put(\closeDelimiter, $));
		^super.new(stream, parentNode, properties)
	}
	// here, starts with first item (opening angle bracket is already skipped)
	// 'begin' is already set
	parse { |stream|
		var ch, new;
		var property = (isPitch: true);
		begin = begin - 1;
		while {
			ch = stream.peek;
			ch.notNil and: { ch != closeDelimiter }
		} {
			new = ClPitchEventNode(stream, this, property);
			children = children.add(new);
		};
		if(ch != closeDelimiter) {
			Error("Improperly closed (chord node)").throw;
		} {
			stream.next;  // used 'peek' above, have to advance past closing delimiter
			ch = stream.peek;
			if(".~>_".includes(ch)) {
				new = ClArticNode(stream, this);
				children = children.add(new);
			};
		}
	}
	streamItem { |stream|
		var legato = 0.9, accent = false,
		i = 0, size = children.size, note;
		if(children.last.isMemberOf(ClArticNode)) {
			switch(children.last.char)
			{ $~ } { legato = inf /*1.01*/ }
			{ $_ } { legato = 0.9 }
			{ $. } { legato = 0.4 };
			accent = children.last.accented;
			size = size - 1;
		};
		stream << "SequenceNote(";
		if(size > 1) { stream << "[" };
		while { children[i].isMemberOf(ClPitchEventNode) } {
			if(i > 0) { stream << ", " };
			note = children[i].decodePitch(children[i].string);
			stream << note.freq;
			i = i + 1;
		};
		if(size > 1) { stream << "]" };
		stream << ", nil, " << legato;
		if(accent) { stream << ", \\accent" };
		stream << ")";
	}
}

ClArticNode : ClStringNode {
	var artic = "_.~", accent = ">";
	var <>char, <>accented = false;

	parse { |stream|
		var ch;
		ch = stream.next;
		if(accent.includes(ch)) {
			accented = true;
			ch = stream.next;
			this.checkArtic(ch, stream);
		} {
			this.checkArtic(ch, stream);
		};
	}
	checkArtic { |ch, stream|
		case
		{ artic.includes(ch) } {
			char = ch;
			ch
		}
		{ accent.includes(ch) or: { ch == $" } } {
			char = nil;  // special case (also valid for terminating quote)
			stream.pos = stream.pos - 1;  // >~ is 2 chars; >> is two things of one char each
		}
		{
			Error("Articulation pool: Bad character $%".format(ch)).throw;
		}
	}
	streamCode { |stream|
		if(accented == true) {
			stream << "\\accent -> " <<< char;
		} {
			stream <<< char;
		};
	}
}

ClArticPoolNode : ClStringNode {
	parse { |stream|
		var ch;
		if(endTest.isNil) { endTest = { |ch| ch == $\" } };
		while {
			ch = stream.peek;
			ch.notNil and: { endTest.value(ch).not }
		} {
			children = children.add(ClArticNode(stream));
		};
		stream.next;  // eat the last quote
	}
	streamCode { |stream|
		stream << "[";
		children.do { |item, i|
			if(i > 0) { stream << ", " };
			item.streamCode(stream);
		};
		stream << "]";
	}
}

ClNumberNode : ClAbstractParseNode {
	classvar types;
	var <>value;

	*initClass {
		types = [
			// fraction: integer/integer
			{ |str|
				var slash = str.indexOf($/),  // regexp guarantees "/" is there
				numerator = str[ .. slash-1].asInteger,
				denominator = str[slash+1 .. ].asInteger;
				Rational(numerator / denominator)
			} -> "[\\-+]?[0-9]+/[0-9]+",
			// tuplet: numNotes:noteValue
			{ |str|
				var colon = str.indexOf($:),  // regexp guarantees ":" is there
				numerator = str[ .. colon-1].asFloat,
				denominator = str[colon+1 .. ].asInteger;
				// 3:2 = triplet spanning half note = 0.66667 = 4 / 2 / 3
				// 3:4 = triplet spanning quarter note = 0.3333 = 4 / 4 / 3
				// 5:4 = quintuplet spanning quarter = 0.2 = 4 / 4 / 5
				// 4:1 = 4 in a whole-note = quarter note = 1 = 4 / 4 / 1
				Rational(4 / (numerator * denominator))
			} -> "[\\-+]?[0-9]*\\.?[0-9]+([eE][\\-+]?[0-9]+)?:[0-9]+",
			_.asFloat -> "[\\-+]?[0-9]*\\.[0-9]+([eE][\\-+]?[0-9]+)?",
			_.asInteger -> "(-[0-9]+|[0-9]+)"
		];
	}
	parse { |stream|
		// hack into CollStream -- will fail with other IO streams
		var match,
		type = types.detect { |assn|
			match = stream.collection.findRegexpAt(assn.value, stream.pos);
			match.notNil
		};
		if(match.notNil) {
			string = stream.nextN(match[1]);  // match[1] = length of match
			value = type.key.value(string);
		};  // else leave state variables nil
	}
	streamCode { |stream|
		stream << "ClNumProxy(";
		if(value.isKindOf(Rational)) {
			stream <<< value.numerator << "/" <<< value.denominator;
		} {
			stream <<< value;
		};
		stream << ")";
	}
	// TODO delete? Proto:value redirects here
	next { ^value }
}

ClRangeNode : ClNumberNode {
	parse { |stream|
		var low, hi;
		low = ClNumberNode(stream, this);
		if(low.value.notNil) {
			this.skipSpaces(stream);
			2.do {
				if(stream.next != $.) {
					Error("Invalid range separator").throw;
				}
			};
			this.skipSpaces(stream);
			hi = ClNumberNode(stream, this);
			if(hi.value.isNil) {
				Error("No upper bound found in range").throw;
			};
		} {
			Error("Invalid lower bound in range").throw;
		};
		if(low.value <= hi.value) {
			children = [low, hi];
		} {
			children = [hi, low];
		};
	}
	streamCode { |stream|
		stream << "ClNumProxy(Pwhite(" // "PR(\\clNumProxy).copy.prep(Pwhite("
		<<< children[0].value << ", " <<< children[1].value << ", inf))";
	}
}

// \ins(, ".", {#[1, 4].choose}, 0.25)
// inside curly braces, brackets and quotes are allowed; SC comments aren't
ClPassthruNumberNode : ClStringNode {
	var <value;
	parse { |stream|
		var start, ch;
		if(stream.next != ${) {
			Error("Tried to parse pass-through expression not starting with a brace, should never happen")
			.throw;
		};
		start = stream.pos;  // bad interface in the \clParse helper functions
		while {
			ch = stream.next;
			ch.notNil and: { ch != $} }
		} {
			case
			{ "([{".includes(ch) } {
				ch = \clParseBracketed.eval(stream, ch, false);
			}
			{ "\'\"".includes(ch) } {
				ch = \clParseQuote.eval(stream, ch);
			};
			// else keep going by single chars
		};
		if(ch.isNil) {
			Error("Unterminated pass-through expression: %".format(
				stream.collection[start .. start + 10]
			)).throw;
		};
		value = stream.collection[start .. stream.pos - 2];
	}
	next { ^value }
	streamCode { |stream|
		// trickier than you think.
		// non-patterns --> Pfunc({ expr }); patterns --> no Pfunc.
		// we have to evaluate the expression to know which is which.
		// avoid code duplication in the generated string by using a function.
		// stream << "PR(\\clNumProxy).copy.prep(Plazy { var func = { " << ~value
		stream << "ClNumProxy(Plazy { var func = { " << value
		<< " }, thing = func.value; if(thing.isPattern) { PnNilSafe(thing, inf, 10) } { Pfunc(func) } })";
	}
}

ClDividerNode : ClAbstractParseNode {
	var <>endChars = "|\"";
	var items;
	parse { |stream|
		var ch, new;
		// stream.collection[stream.pos .. stream.pos + 10].debug(">> clDividerNode");
		items = Array.new;
		while {
			ch = stream.next;
			ch.notNil and: { endChars.includes(ch).not }
		} {
			new = this.parseItem(stream, ch);
			if(new.notNil) {
				items = items.add(new);
			};
		};
		if(ch.notNil) { stream.pos = stream.pos - 1 };
		// stream.collection[begin .. begin + 10].debug("<< clDividerNode");
	}

	parseItem { |stream, ch|
		var new, begin, test;
		// [ch.asCompileString, stream.collection[stream.pos .. stream.pos + 10]].debug(">> parseItem");
		if(ch.isNil) { ch = stream.next };
		begin = stream.pos - 1;
		^case
		{ ch == $\\ } {
			stream.pos = stream.pos - 1;
			new = ClGeneratorNode(stream, this,
				(bpKey: bpKey, isPitch: isPitch, isMain: isMain, parm: parm)
			);
			new = this.handleChain(stream, new);
			children = children.add(new);
			new//.debug("<< parseItem");
		}
		{ ch == $[ } {
			stream.pos = stream.pos - 1;
			new = ClSourceNode(stream, this,
				(bpKey: bpKey, isPitch: isPitch, isMain: isMain, parm: parm)
			);
			new = this.handleChain(stream, new);
			children = children.add(new);
			new//.debug("<< parseItem");
		}
		// code formatting: skip over CR and tab
		{ #[$\n, $\t].includes(ch) } { nil }
		// legit chains should be handled in one of the above branches
		// but if it's the start of a divider, we might already have swallowed one colon
		// so match either :\ or ::\
		// this does disallow : as a parmMap char in the following: "|::\ins() ||"
		{
			test = stream.collection.findRegexpAt(":*\\\\", stream.pos);
			// test[1] is length of match, for this case should be either :\ or ::\
			test.notNil and: { test[1] >= 2 and: { test[1] <= 3 } }
		} {
			Error(":: chain syntax applies only to generators or [source]").throw;
		}
		{ isPitch } {
			case { ch.isDecDigit } {
				stream.pos = stream.pos - 1;
				new = ClPitchEventNode(stream, this, (isPitch: true));
				children = children.add(new);
				new
				/*
				new = String.with(ch);
				while { (ch = stream.next).notNil and: { "+-',~_.>".includes(ch) } } {
					new = new.add(ch);
				};
				if(ch.notNil) { stream.pos = stream.pos - 1 };
				children = children.add(
					ClEventStringNode.newEmpty
					.parentNode_(this)
					.string_(new)
					.isPitch_(isPitch)
					.begin_(begin)
					.end_(begin + new.size - 1);
				);
				new//.debug("<< parseItem");
				*/
			}
			{ ch == $( } {
				// no, ClChordNode assumes '<' has been eaten already
				// stream.pos = stream.pos - 1;
				new = ClChordNode(stream, this/*, properties*/);
				children = children.add(new);
				new
			}
			{
				new = String.with(ch);
				children = children.add(
					ClEventStringNode.newEmpty
					.parentNode_(this)
					.string_(new)
					.isPitch_(isPitch)
					.begin_(begin)
					.end_(begin);
				);
				new//.debug("<< parseItem");
			};
		} {
			new = String.with(ch);
			children = children.add(
				ClEventStringNode.newEmpty
				.parentNode_(this)
				.string_(new)
				.isPitch_(isPitch)
				.begin_(begin)
				.end_(begin);
			);
			new//.debug("<< parseItem");
		};
	}

	handleChain { |stream, new|
		var begin, colonCount, rewindPos;
		// if(#[clGeneratorNode, clSourceNode].includes(new.nodeType.debug("nodeType")).not) {
		// 	Error(":: chain syntax applies only to generators or [source]").throw;
		// };
		if(stream.peek == $:) {
			colonCount = 0;
			rewindPos = stream.pos;
			while { stream.next == $: } { colonCount = colonCount + 1 };
			if(colonCount == 2) {
				stream.pos = stream.pos - 1;  // need next to be the backslash
				new = ClChainNode(stream, this,
					(bpKey: bpKey, isPitch: isPitch, isMain: isMain, parm: parm),
					new  // extra arg is first item in the chain
				);
			} {
				Error("'::' syntax, wrong number of colons").throw;
			};
		};
		^new
	}

	hasItems { ^children.any { |item| item.isSpacer.not } }
	streamCode { |stream|
		var needComma = false;
		if(this.hasItems) {
			if(children[0].time.isNil) { this.setTime(time, dur) };
			// no array brackets: divider delimiters are for humans, not machines
			children.do { |item, i|
				if(item.isSpacer.not) {
					if(needComma) { stream << ", " };
					item.streamCode(stream);
					needComma = true;
				};
			};
		};
	}
	setTime { |onset(0), argDur(4), extraDur(0)|
		var itemDur = argDur / max(children.size, 1), durs, lastI;
		time = onset;
		dur = argDur;
		// children should be clStringNodes or clGeneratorNodes
		durs = Array(children.size);
		children.do { |item, i|
			if(item.isSpacer and: { lastI.notNil }) {
				durs[lastI] = durs[lastI] + itemDur;
			} {
				lastI = i;
			};
			durs.add(itemDur);
		};
		if(lastI.notNil) {
			durs[lastI] = durs[lastI] + extraDur;
		};
		children.do { |item, i|
			item.setTime(onset + (i * itemDur), durs[i]);
		};
	}
}

ClGeneratorNode : ClAbstractParseNode {
	classvar types, extras;
	var <>name, <>repeats;

	*initClass {
		types = [
			// pretty sure this is deprecated
			// (type: \clRhythmGenNode, regexp: "^:[a-zA-Z0-9_]+\\(.*\\)"),
			(type: ClGeneratorNode, regexp: "^\\\\[a-zA-Z0-9_]+"),
			(type: ClPassthruNumberNode, regexp: "^\{.*\}"),
			// more specific "" test must come first
			(type: ClArticPoolNode, regexp: "^\"[._~>]+\"",
				match: { |node| node.isPitch ?? { false } },
				pre: { |stream| stream.next }  // stringnodes assume you've already dropped the opening quote
			),
			(type: ClPatStringNode, regexp: "^\".*\""),
			(type: ClRangeNode, regexp: "^-?[0-9.]+ *\\.\\. *-?[0-9]"),
			(type: ClNumberNode, regexp: "^-?[0-9]"),
			(type: ClStringNode, regexp: "^`[A-Za-z0-9_]+",
				endTest: { |ch| not(ch.isAlphaNum or: { "_`".includes(ch) }) },
				openDelimiter: $', closeDelimiter: $',
				pre: { |stream| stream.next }  // drop ` intro
			),
			(type: ClChainNode, regexp: "^::",
				pre: { |stream| stream.nextN(2) },  // drop ::
				// chain node needs to get the first sub-generator at prep time
				// also the chain replaces the last-parsed sub-generator
				parseSpecial: { |new, stream, node|
					if(node.children.last.isKindOf(ClGeneratorNode)) {
						new.init(stream, this, node.children.last);
						node.children = node.children.drop(-1);  // last is subsumed into 'new'
						new
					} {
						Error("'::' syntax is valid between two generators only").throw;
					};
				}
			),
			(type: ClStringNode, regexp: "^,",  // empty arg
				endTest: { |ch| ch == $, }
			),
		];
		extras = #[endTest, openDelimiter, closeDelimiter];
	}
	parse { |stream|
		var name, ch, newArg, testName;
		// stream.collection[stream.pos .. stream.pos + 10].debug(">> clGeneratorNode");
		if(stream.peek == $\\) { stream.next };
		name = this.getID(stream);
		testName = name.string.copy;
		testName[0] = testName[0].toUpper;
		if(PR.exists(("clGen" ++ testName).asSymbol).not) {
			Error("Incorrect generator name %".format(name.string)).throw;
		};
		children = children.add(name);
		name = name.string;
		ch = stream.next;
		if(ch == $*) {
			case
			{ this.findType(ClPassthruNumberNode)[\regexp].matchRegexp(stream.collection, stream.pos) } {
				repeats = ClPassthruNumberNode(stream, this);
			}
			{ this.findType(ClRangeNode)[\regexp].matchRegexp(stream.collection, stream.pos) } {
				repeats = ClRangeNode(stream, this);
			}
			{ this.findType(ClNumberNode)[\regexp].matchRegexp(stream.collection, stream.pos) } {
				repeats = ClNumberNode(stream, this);
			};
			ch = stream.next;
		};
		if(ch == $() {
			while {
				ch = stream.next;
				ch.notNil and: { ch != $) }
			} {
				stream.pos = stream.pos - 1;
				// note: it is now not valid to collapse to ~children.add(this.parseArg(stream))
				// because ~parseArg will modify ~children if it encounters a chain node
				// ~parseArg must finish before determining the receiver of 'add'
				newArg = this.parseArg(stream);
				children = children.add(newArg);
			};
			// if(ch.notNil) { stream.pos = stream.pos - 1 };
		} {
			// gen refs
			Error("Generator '%' has no argument list".format(name)).throw;
		};
		// stream.collection[begin .. begin + 10].debug("<< clGeneratorNode");
	}
	parseArg { |stream|
		var type, ch, new;
		// [stream.pos, stream.collection[stream.pos .. stream.pos + 10]].debug(">> parseArg");
		type = types.detect { |entry|
			(entry[\match].value(this) ?? { true }) and: {
				stream.collection.findRegexpAt(entry[\regexp], stream.pos).notNil
			}
		};
		if(type/*.debug("type")*/.notNil) {
			type[\pre].value(stream);
			new = type[\type].newEmpty
			.putAll((bpKey: bpKey, isPitch: isPitch, isMain: isMain, parm: parm));
			extras.do { |key|
				if(type[key].notNil) {
					new.perform(key.asSetter, type[key]);
				};
			};
			if(type[\parseSpecial].notNil) {
				type[\parseSpecial].value(new, stream, this);
			} {
				new.init(stream, this);
			};
		} {
			Error("Syntax error in % arg list, at '%'".format(name, stream.collection[stream.pos .. stream.pos + 10])).throw;
		};
		ch = stream.next;
		if(ch == $,) { this.skipSpaces(stream) } {
			if(ch.notNil) { stream.pos = stream.pos - 1 };
		};
		// "<< parseArg".debug;
		^new
	}

	streamCode { |stream|
		var name = children[0].string;
		this.setTime(time, dur);
		stream << "PR(\\clGen";
		if(name.size > 0) {
			stream << name[0].toUpper << name[1..];
		};
		stream << ").copy.putAll((";
		if(repeats.notNil) {
			stream << "repeats: ";
			repeats.streamCode(stream);
			stream << ", ";
		};
		stream << "bpKey: " <<< bpKey;
		stream << ", args: [ ";
		forBy(1, children.size - 1, 1) { |i|
			if(i > 1) { stream << ", " };
			if(children[i].string == "") {
				stream << "nil"
			} {
				children[i].streamCode(stream);
			};
		};
		stream << " ], dur: " << dur << ", time: " << time;
		stream << ", isPitch: " << (isPitch ?? { false });
		stream << ", isMain: " << (isMain ?? { false });
		stream << ", parm: " <<< parm;
		stream << ")).prep";
	}
	setTime { |onset(0), argDur(4)|
		var itemDur = argDur / max(children.size, 1);
		time = onset;
		dur = argDur;
		forBy(1, children.size - 1, 1) { |i|
			children[i].setTime(onset, dur);  // meaningful for gens and patstrings
		};
	}

	findType { |key|
		^types.detect { |type| type[\type] == key }
	}
}

ClChainNode : ClAbstractParseNode {
	// special constructor -- you know what kind of node you're creating
	// assumes stream.next will be the second generator

	*new { |stream, parentNode, properties, leftNode|
		var new = super.newEmpty;
		if(properties.notNil) { new.putAll(properties) };
		^new.init(stream, parentNode, leftNode)
	}

	init { |stream, argParentNode, leftNode|
		if(stream.peek != $\\) {
			Error("'::' syntax is valid between two generators only").throw;
		};
		parentNode = argParentNode;
		leftNode.parentNode = this;
		children = [leftNode];
		// if(stream.peek == $/) { stream.next };
		begin = leftNode.begin; // stream.pos;
		this.parse(stream);
		if(end.isNil) { end = stream.pos - 1 };
		if(string.isNil) {
			if(end >= begin) {
				string = stream.collection[begin .. end]
			} {
				string = String.new;
			};
		};
		this
	}

	parse { |stream|
		var new, continue = true, rewindPos, colonCount;
		// stream.collection[stream.pos .. stream.pos + 10].debug(">> clChainNode");
		while { continue } {
			if(stream.peek == $\\) {
				new = ClGeneratorNode(stream, this,
					(bpKey: bpKey, isPitch: isPitch, isMain: isMain, parm: parm)
				);
				children = children.add(new);
				if(stream.peek == $:) {
					rewindPos = stream.pos;
					colonCount = 0;
					while { stream.next == $: } { colonCount = colonCount + 1 };
					switch(colonCount)
					{ 2 } {
						stream.pos = stream.pos - 1;
					}
					{ 0 } {
						stream.pos = rewindPos;
						continue = false;
					}
					{ Error("'::' syntax, wrong number of colons").throw };
					if(stream.peek != $\\) {
						Error("'::' syntax is valid between two generators only").throw;
					};
				} {
					continue = false;
				};
			} {
				continue = false;
			}
		};
		// stream.collection[stream.pos .. stream.pos + 10].debug("<< clChainNode");
	}
	streamCode { |stream|
		if(children[0].time.isNil) { this.setTime(time, dur) };
		stream << "PR(\\clGenChain).copy.putAll((";
		stream << "bpKey: " <<< bpKey;
		stream << ", args: [ ";
		children.do { |child, i|
			if(i > 0) { stream << ", " };
			child.streamCode(stream);
		};
		stream << " ], dur: " << dur << ", time: " << time;
		stream << ", isPitch: " << (isPitch ?? { false });
		stream << ", isMain: " << (isMain ?? { false });
		stream << ", parm: " <<< parm;
		stream << ")).prep";
	}
	setTime { |onset(0), argDur(4)|
		time = onset;
		dur = argDur;
		children.do { |item| item.setTime(onset, dur) };
	}
}

// this one is gone, right?
// PR(\clGeneratorNode).clone {
// 	~superParse = ~parse;
// 	parse { |stream|
// 		if(stream.peek == $:) { stream.next };
// 		~superParse.(stream);
// 	};
// } => PR(\clRhythmGenNode);

ClPatStringNode : ClAbstractParseNode {
	parse { |stream|
		var str = String.new, ch, didOpenQuote = false;
		// stream.collection[stream.pos .. stream.pos + 10].debug(">> clPatStringNode");
		while {
			ch = stream.next;
			ch.notNil and: { didOpenQuote.not or: { ch != $\" } }
		} {
			children = children.add(
				ClDividerNode(stream, this,
					(bpKey: bpKey, isPitch: isPitch, isMain: isMain, parm: parm)
				)
			);
			didOpenQuote = true;
		};
		// stream.collection[~begin .. ~begin + 10].debug("<< clPatStringNode");
	}
	streamCode { |stream|
		var needComma = false;
		if(children[0].time.isNil) { this.setTime(time, dur) };
		stream << "[ ";
		children.do { |item, i|
			if(item.hasItems) {  // all items should be divider nodes
				if(needComma) { stream << ", " };
				item.streamCode(stream);
				needComma = true;
			};
		};
		stream << " ]";
	}
	setTime { |onset(0), argDur(4)|
		var itemDur = argDur / max(children.size, 1), i, extraDur = 0, first;
		time = onset;
		dur = argDur;
		// all children should be divider nodes
		// what about items spanning a division? reverse order
		i = children.size - 1;
		children.reverseDo { |item|
			var itemOnset = onset + (i * itemDur);
			item.setTime(itemOnset, itemDur, extraDur);
			if(item.children.size == 0) {
				extraDur = extraDur + itemDur;
			} {
				first = item.children.detect { |ch| ch.isSpacer.not };
				if(first.notNil) {
					extraDur = first.time - itemOnset;
				} {
					extraDur = extraDur + itemDur;
				}
			};
			i = i - 1;
		};
	}
}

ClSourceNode : ClPatStringNode {
	parse { |stream|
		var str = String.new, ch;
		// stream.collection[stream.pos .. stream.pos + 10].debug(">> clPatStringNode");
		if(stream.peek != $[) {
			Error("Invalid bracketed source string").throw;
		};
		while {
			ch = stream.next;
			ch.notNil and: { ch != $] }
		} {
			children = children.add(ClDividerNode(stream, this,
				(bpKey: bpKey, isPitch: isPitch, isMain: isMain, parm: parm, endChars: "|]")
			));
		};
		if(ch != $]) {
			Error("Unterminated bracketed source string").throw;
		};
		// stream.collection[~begin .. ~begin + 10].debug("<< clPatStringNode");
	}
	streamCode { |stream|
		var needComma = false;
		this.setTime(time, dur);
		stream << "PR(\\clGenSrc).copy.putAll((";
		stream << "bpKey: " <<< bpKey;
		stream << ", args: [ [ ";
		children.do { |child, i|
			if(child.hasItems) /*{
				stream << "nil"
			}*/ {
				if(needComma) { stream << ", " };
				child.streamCode(stream);
				needComma = true;
			};
		};
		stream << " ] ], dur: " << dur << ", time: " << time;
		stream << ", isPitch: " << (isPitch ?? { false });
		stream << ", isMain: " << (isMain ?? { false });
		stream << ", parm: " <<< parm;
		stream << ")).prep";
	}
}

ClIDNode : ClAbstractParseNode {
	var <>clClass, <>objKey, <>objExists = false, /*<>phrase = \main,*/ <>numToApply;
	// var <>parm = nil;  // filled in in 'parse'
	var <>idAllowed = "_*";

	parse { |stream|
		var i, ids, test, sym, temp, broke;

		broke = block { |break|
			// class (I expect this won't be used often)
			test = this.getID(stream);
			children = children.add(test);
			if(test.string.first.isUpper) {
				clClass = test.symbol.asClass;
				if(stream.next != $.) { break.(true) };
				test = this.getID(stream);
				children = children.add(test);
			} {
				clClass = BP;
			};

			// chucklib object key
			objKey = test.symbol;  // really? what about array types?
			if(clClass.exists(objKey)) {
				objExists = true;
			};
			if(stream.next != $.) { break.(true) };
			test = this.getID(stream);
			children = children.add(test);

			// phrase name
			test = test.string;
			if(test.size == 0) {
				phrase = \main;
			} {
				i = test.indexOf($*);
				if(i.notNil) {
					temp = test[i+1 .. ];
					if(temp.notEmpty and: temp.every(_.isDecDigit)) {
						numToApply = temp.asInteger;
					} {
						"%: Invalid apply number".format(test).warn;
					};
					phrase = test[ .. i-1].asSymbol;
				} {
					phrase = test.asSymbol;  // really? what about array types?
				};
			};
			if(stream.next != $.) { break.(true) };
			test = this.getID(stream);
			children = children.add(test);

			// parameter name
			parm = test.string;
			false;
		};
		if(phrase.isNil or: { phrase == '' }) {
			phrase = \main;
		};
		if(parm.size == 0) {
			if(objExists) {
				parm = clClass.new(objKey)[\defaultParm];
			};
		} {
			parm = parm.asSymbol;
		};
		if(broke) { stream.pos = stream.pos - 1 };
	}

	streamCode { |stream|
		"clIDNode:streamCode not yet implemented".warn;
		stream << "ClIDNode";
	}
}

ClPatStringQuantNode : ClAbstractParseNode {
	var <>additiveRhythm, <>quant;
	parse { |stream|
		var str = String.new, ch;
		while {
			ch = stream.next;
			ch.notNil and: { ch != $" }
		} {
			str = str.add(ch);
		};
		if(ch.notNil) { stream.pos = stream.pos - 1 };
		string = str;
		additiveRhythm = str[0] == $+;
		quant = str[additiveRhythm.asInteger ..].interpret;
	}
}

ClPatternSetNode : ClAbstractParseNode {
	var <>hasQuant = false;
	// getters, because ~children positions may vary
	idNode { ^children[0] }
	quantNode {
		^if(hasQuant) { children[1] } { nil };
	}
	patStringNode {
		^if(hasQuant) { children[2] } { children[1] };
	}
	// caller needs to wrap the outermost patStringNode in a generatorNode: need setter
	patStringNode_ { |node|
		if(hasQuant) {
			children[2] = node;
		} {
			children[1] = node;
		};
	}
	parse { |stream|
		var id, obj;
		if(stream.peek == $/) { stream.next };
		id = ClIDNode(stream, this);
		children = children.add(id);
		this.skipSpaces(stream);
		if(stream.peek == $=) {
			stream.next;
		} {
			Error("clPatternSet must have '='").throw;
		};
		this.skipSpaces(stream);
		if(stream.peek == $() {
			Error("Composite patterns not refactored yet").throw;
		};
		if(stream.peek.isDecDigit or: { stream.peek == $+ }) {
			children = children.add(ClPatStringQuantNode(stream, this));
			hasQuant = true;
		};
		if(stream.peek == $\") {
			if(id.clClass.exists(id.objKey)) {
				obj = id.clClass.new(id.objKey);
				try {
					isPitch = obj.parmIsPitch(id.parm) ?? { false };
					isMain = id.parm == obj.defaultParm ?? { false };
				};
			};
			children = children.add(
				ClPatStringNode(stream, this,
					(bpKey: id.objKey, isPitch: isPitch, isMain: isMain, parm: id.parm)
				)
			);
		}
	}

	// setPattern { |phrase, parm, inParm, pattern, inString, newQuant| };

	// maybe caller should be responsible for this
	streamCode { |stream|
		// var id = ~children[0];
		// stream << id.clClass.name << "(" <<< id.name << ").setPattern(";
		// stream <<< id.phrase << ", "
	}
	setTime { |onset(0), argDur(4)|
		time = onset;
		dur = argDur;
		this.patStringNode.setTime(onset, dur);
	}
}
