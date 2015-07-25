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
		~bookmarks = MultiLevelIdentityDictionary.new;
		~bookmarkIndex = IdentityDictionary.new;
		~markUpdaters = IdentityDictionary.new;
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
		~markUpdaters.do { |upd| upd.remove };
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
		var newPage = PR(\rangeTextView).copy
		.put(\background, ~backColor)
		.put(\stringColor, ~stringColor)
		.prep(nil, nil),
		view = newPage.view;
		// .background_(~backColor)
		view.palette_(QPalette.auto.base_(~backColor).baseText_(~stringColor))
		// .string_("x")
		.font_(~textFont);
		~markUpdaters[key.asSymbol] = SimpleController(newPage)
		.put(\markMoved, e { |obj, what, old, new|
			~updateOneBookmark.(old, new);
		});
		if(i.isNil) {
			i = ~pages.size div: 2;
			~pages = ~pages.add(key).add(newPage);
			~stackLayout.add(view);
			~pageMenu.items = ~pageMenu.items.add(key.asString);
		} {
			origItem = ~pageMenu.items[~pageMenu.value];
			~pages = ~pages.insert(i, newPage).insert(i, key);
			i = i div: 2;
			~stackLayout.insert(view, i);
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
		var page, start, length, pageView, patString;
		if(string.size > 0) {
			if(~isDefaultParm.(objKey, parm)) { parm = nil };
			#page, start, length = ~findStringPos.(objKey, phrase, parm);
			[page, start, length].debug("position to insert");
			// safety check?
			pageView = ~pages[page * 2 + 1];
			patString = "/%.%% = %;\n".format(
				objKey,
				phrase, // if(phrase.notNil) { "." ++ phrase } { "" },
				if(parm.notNil) { "." ++ parm } { "" },
				string
			).postcs;
			~bookmarks.put(~pages[page * 2].asSymbol, phrase, parm.asSymbol,
				[start, start + patString.size - 1]);
			~bookmarkIndex[start] = [~pages[page * 2].asSymbol, phrase, parm.asSymbol];
			~bookmarkIndex[start + patString.size - 1] = ~bookmarkIndex[start];
			// if(phrase == \main) { phrase = nil };
			{
				pageView.setString(patString, start, length);
				pageView.addBookmark(start, start + patString.size - 1);
			}.defer(~updateDefer);
		};
	};

	// note, this should always return a position, even if it's a place for a new pattern
	// return: [page_index, start, length]
	~findStringPos = { |objKey, phrase, parm|
		var phrKey, phrIndex, pageI, page, pageStr, mark, keyIndex, j;
		var toFind, lastComparison;
		#phrKey, phrIndex = ~phraseAndIndex.(phrase);
		pageI = ~pageIndexForPhrase.(phrKey);
		if(pageI.isNil) {
			pageI = ~newPage.(phrKey.asSymbol);
		};
		page = ~pages[pageI * 2 + 1];
		pageStr = page.string;
		if(~isDefaultParm.(objKey, parm)) { parm = nil };
		mark = ~bookmarks.at(objKey, phrKey, parm.asSymbol);
		if(mark.notNil) {
			[pageI, mark[0], mark[1] - mark[0] + 1]
		} {  // find space for the new string
			// find index heading
			if(phrIndex.notNil) {
				keyIndex = ~indexHeadInString.(pageStr, phrIndex);
				if(keyIndex.notNil) {
					if(keyIndex[1] == phrIndex) {
						j = keyIndex[0] + keyIndex[1].size + 1;
					} {
						j = keyIndex[0] - 3;
						page.setString("// " ++ phrIndex ++ "\n\n", j, 0);
						pageStr = pageStr.insert(j, "// " ++ phrIndex ++ "\n\n");
						j = j + phrIndex.size + 3;
					};
				} {
					Error("keyIndex isNil: I think we shouldn't ever get here").throw;
				};
			} {
				j = 0;  // un-sectioned strings at top
			};

			// find alpha order
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
				[pageI, max(0, j - 2), 0].debug("<< past document end");
			} {
				if(lastComparison == 0) {
					keyIndex = j; // = keyIndex;
					while { j < pageStr.size and: { pageStr[j] != $\n } } {
						j = j + 1;
					};
					[pageI, keyIndex, j - keyIndex + 1].debug("<< match")
				} {
					if(pageStr.findRegexpAt("/[a-zA-Z]", j).isNil) { j = j - 1 };
					[pageI, j, 0].debug("<< non-match, insert")
				};
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

	~indexHeadInString = { |string, phrIndex|
		block { |break|
			string.findRegexp("^// ([0-9]+)").debug("search for headings").pairsDo { |fullMatch, indexMatch|
				[indexMatch[1].asInteger, phrIndex.asInteger, indexMatch[1].asInteger >= phrIndex.asInteger].debug("oh bloody fucking hell");
				if(indexMatch[1].asInteger >= phrIndex.asInteger) {
					// j = fullMatch[0];
					break.(indexMatch);
				}
			};
			// j = pageStr.size - 1;
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

	~updateOneBookmark = { |old, new|
		var mIndex = ~bookmarkIndex[old], pair;
		if(mIndex.notNil) {
			~bookmarkIndex.removeAt(old);
			~bookmarkIndex.put(new, mIndex);
			pair = ~bookmarks.at(*mIndex);
			if(pair.notNil) {
				pair.do { |item, i|
					if(item == old) { pair[i] = new };
				};
			};
		};
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

BP(\edit).pages[1].string.size
BP(\edit).pages[1].string[23 .. ]

BP(\edit).pages[1].str.asCompileString

BP(\edit).pages[1].visibleBookmarks = [0, 17];
BP(\edit).pages[1].visibleBookmarks = [];
BP(\edit).pages[1].drawBookmarks;

BP(\edit).bookmarks
BP(\edit).bookmarkIndex

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