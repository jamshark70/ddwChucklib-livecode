(
Proto {
	~bounds = Window.screenBounds;
	~windowTitle = "Pattern editor";
	~backColor = Color.gray(0.078);
	~windowBackground = Color.gray(0.2);
	~stringColor = Color.white;
	// ~isActive = true;
	~textFont = Font("Inconsolata", 24);
	~updateDefer = 0.1;

	~prep = {
		~makeGui.();
		~scanBPs.();
		~notifier = NotificationCenter.register(\clLiveCode, \phraseString, ~collIndex, e {
			|objKey, phrase, parm, string|
			(e { ~updatePatString.(objKey, phrase, parm, string) }).defer;
		});
	};
	~freeCleanup = {
		// ~isActive = false;
		~notifier.remove;
		~win.onClose_(nil).close;
		"freed".debug;
	};

	~makeGui = {
		~win = Window(~windowTitle, ~bounds.value).background_(~windowBackground);
		~pages = Array.new;
		~pageMenu = PopUpMenu()
		.background_(~backColor).stringColor_(~stringColor)
		// .items_(~pages[0, 2..])
		.action_(e { |view| ~setPage.(view.value) });
		~win.layout = VLayout(
			~pageMenu,
			~stackLayout = StackLayout(/**~pages[1, 3..]*/)
		);
		~newPage.(\main);
		~win.front;
		~win.onClose = e {
			~win.onClose = nil;
			// if(~isActive ? true) {
			BP(~collIndex).free;
			// };
		};
	};

	~newPage = { |key|
		var i = block { |break|
			~pages.pairsDo { |existingKey, page, i|
				case
				{ key == existingKey } {
					Error("BP(%): Page at % already exists".format(
						~collIndex.asCompileString,
						key.asCompileString
					)).throw;
				}
				{ key < existingKey } {
					break.(i)
				};
			};
			nil
		};
		var origItem;
		var newPage = TextView()
		// .background_(~backColor)
		.palette_(QPalette.auto.base_(~backColor).baseText_(~stringColor))
		.string_("x")
		.font_(~textFont);
		if(i.isNil) {
			i = ~pages.size div: 2;
			~pages = ~pages.add(key).add(newPage);
			~stackLayout.add(newPage);
			~pageMenu.items = ~pageMenu.items.add(key.asString);
		} {
			origItem = ~pageMenu.items[~pageMenu.value];
			~pages = ~pages.insert(i, newPage).insert(i, key);
			i = i div: 2;
			~stackLayout.insert(newPage, i);
			~pageMenu.items = ~pageMenu.items.insert(i, key.asString);
			~pageMenu.value = ~pageMenu.items.indexOfEqual(origItem);
		};
		// (e {
		// 	newPage
		// 	.string_("x")
		// 	.stringColor_(~stringColor)
		// }).defer(0.1);
		i
	};

	~updatePatString = { |objKey, phrase, parm, string|
		var page, start, length, pageView;
		if(string.size > 0) {
			if(~isDefaultParm.(objKey, parm)) { parm = nil };
			#page, start, length = ~findStringPos.(objKey, phrase, parm);
			[page, start, length].debug("position to insert");
			// safety check?
			pageView = ~pages[page * 2 + 1];
			// if(phrase == \main) { phrase = nil };
			{
				pageView.setString(
					"/%.%% = %;\n".format(
						objKey,
						phrase, // if(phrase.notNil) { "." ++ phrase } { "" },
						if(parm.notNil) { "." ++ parm } { "" },
						string
					).postcs,
					start, length
				);
			}.defer(~updateDefer);
		};
	};

	// note, this should always return a position, even if it's a place for a new pattern
	// return: [page_index, start, length]
	~findStringPos = { |objKey, phrase, parm|
		var phrKey, phrIndex, pageI, pageStr, keyIndex, j, toFind, lastComparison;
		#phrKey, phrIndex = ~phraseAndIndex.(phrase);
		pageI = ~pageIndexForPhrase.(phrKey);
		if(pageI.isNil) {
			pageI = ~newPage.(phrKey.asSymbol);
		};
		pageStr = ~pages[pageI * 2 + 1].string;
		toFind = "/%%%".format(objKey,
			if(phrase.notNil) { "." ++ phrase } { "" },
			if(parm.notNil) { "." ++ parm } { "" }
		).debug("searching for");
		if(phrIndex.notNil) {
			keyIndex = block { |break|
				pageStr.findRegexp("^// ([0-9]+)").debug("search for headings").pairsDo { |fullMatch, indexMatch|
					[indexMatch[1].asInteger, phrIndex.asInteger, indexMatch[1].asInteger >= phrIndex.asInteger].debug("oh bloody fucking hell");
					if(indexMatch[1].asInteger >= phrIndex.asInteger) {
						j = fullMatch[0];
						break.(indexMatch);
					}
				};
				j = 0;
				nil
			};
			if(keyIndex.size > 0) {
				[keyIndex[1].asInteger, phrIndex.asInteger, keyIndex[1].asInteger != phrIndex.asInteger].debug("test to add a fucking heading");
			};
			if(keyIndex.size < 2 or: { keyIndex[1].asInteger != phrIndex.asInteger }) {
				// insert new heading
				~pages[pageI * 2 + 1].setString("// " ++ phrIndex ++ "\n\n", j, 0);
				pageStr = pageStr.insert(j, "// " ++ phrIndex ++ "\n\n");
				// ~pages[pageI * 2 + 1].stringColor_(~stringColor).string_(pageStr);
			};
			j = j + phrIndex.size + 4;
		} {
			j = 0;
		};
		[j, pageStr.size].debug("before while");
		keyIndex = -10;
		while { j < pageStr.size and: { j - keyIndex > 1 and: {
			[j, pageStr[j .. j+15].asCompileString].debug("testing");
			pageStr.findRegexpAt("// [0-9]", j).isNil and: {
				(lastComparison = ~compareStringAt.(pageStr, toFind, j).debug("comparison")) > 0
			}
		} } } {
			keyIndex = j;
			while { j < pageStr.size and: { pageStr[j] != $\n } } {
				j = j + 1
			};
			j = j + 1;
		};
		if(j >= pageStr.size) {
			[pageI, j - 2, 0].debug("past document end");
		} {
			if(lastComparison == 0) {
				keyIndex = j; // = keyIndex;
				while { j < pageStr.size and: { pageStr[j] != $\n } } {
					j = j + 1;
				};
				[pageI, keyIndex, j - keyIndex + 1].debug("match")
			} {
				if(pageStr.findRegexpAt("/[a-zA-Z]", j).isNil) { j = j - 1 };
				[pageI, j, 0].debug("non-match, insert")
			};
		};
	};

	~phraseAndIndex = { |phrase|
		var j, phrIndex;
		phrase = phrase.asString;
		j = phrase.size - 1;
		while { j >= 0 and: { phrase[j].isDecDigit } } {
			j = j - 1;
		};
		if(j < 0) {
			Error("BP(%): Phrase key % has no letters".format(
				~collIndex.asCompileString, phrase.asCompileString
			)).throw;
		};
		if(j < (phrase.size - 1) and: { phrase[j+1].isDecDigit }) {
			phrIndex = phrase[j+1 ..];
			phrase = phrase[.. j];
		};
		[phrase, phrIndex]
	};

	~pageIndexForPhrase = { |phrase|
		phrase = phrase.asString;
		block { |break|
			~pages.pairsDo { |key, page, i|
				if(key.asString == phrase) {
					break.(i div: 2);
				};
			};
			nil
		};
	};

	~setPage = { |index|
		~pageMenu.value = index;
		~stackLayout.index = index;
	};

	~isDefaultParm = { |objKey, parm|
		BP.exists(objKey) and: { parm == BP(objKey).defaultParm }
	};

	~compareStringAt = { |sourceStr, findStr, i|
		block { |break|
			findStr.do { |ch, j|
				if(ch < sourceStr[i+j]) { break.(-1) } {
					if(ch > sourceStr[i+j]) { break.(1) }
				};
			};
			0
		};
	};
} => PR(\clEditGui);
)

BP(\edit).free; PR(\clEditGui).chuck(BP(\edit), nil, (bounds: Rect(800, 50, 500, 600)));
BP(\edit).pages

/edit(updateDefer)
/edit(updateDefer = 0.1)

// test

/k(free)

(
PR(\abstractLiveCode).chuck(BP(\k), nil, (
	defaultParm: \amp,
	parmMap: (amp: ($-: 0.5, $.: 0.1))
));

PR(\abstractLiveCode).chuck(BP(\j), nil, (
	defaultParm: \amp,
	parmMap: (amp: ($-: 0.5, $.: 0.1))
));
)

/k = "----";

/k = "|-  -|--| -  ";
1+1

/k.main0 = "-| -|-|";

/k.main3 = "-| -|-|";
/j.main3 = " - -";

/k.main2 = "-| -|-|";

/j.main1 = "-";
/k.main1 = "--";


/k.a0 = "-|   -| -- |";

/k.d1 = "-| -|-  -| -  ";