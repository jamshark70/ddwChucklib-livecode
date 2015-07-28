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
	~defaultPhrIndex = " - Unnumbered - ";

	~prep = {
		// ~bookmarks = MultiLevelIdentityDictionary.new;
		// ~bookmarkIndex = IdentityDictionary.new;
		// ~markUpdaters = IdentityDictionary.new;
		~makeGui.();
		~scanBPs.();
		~notifier = NotificationCenter.register(\clLiveCode, \phraseString, ~collIndex, e {
			|objKey, phrase, parm, string|
			if(parm != \dur) {
				(e { ~updatePatString.(objKey, phrase, parm, string) }).defer;
			};
		});
	};
	~freeCleanup = {
		// ~isActive = false;
		~notifier.remove;
		~pages.do(_.free);
		// ~markUpdaters.do { |upd| upd.remove };
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
			~pages.do { |page, i|
				var existingKey = page.name;
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
		var newPage = PR(\clGuiPage).copy.name_(key).prep(~stackLayout, i);
		if(i.isNil) {
			i = ~pages.size;
			~pages = ~pages.add(newPage);
			~pageMenu.items = ~pageMenu.items.add(key.asString);
		} {
			origItem = ~pageMenu.items[~pageMenu.value];
			~pages = ~pages.insert(i, newPage);
			~pageMenu.items = ~pageMenu.items.insert(i, key.asString);
			~pageMenu.value = ~pageMenu.items.indexOfEqual(origItem);
		};
		newPage.newSection(~defaultPhrIndex);
		i
	};

	~updatePatString = { |objKey, phrase, parm, string|
		var phrKey, phrIndex, pageI, page, fullPhrID, section, str, newStr;
		if(~isDefaultParm.(objKey, parm)) { parm = nil };
		#phrKey, phrIndex = ~phraseAndIndex.(phrase);
		pageI = ~pageIndexForPhrase.(phrKey);
		if(pageI.isNil) {
			pageI = ~newPage.(phrKey.asSymbol);
		};
		page = ~pages[pageI];
		if(phrIndex.isNil) { phrIndex = ~defaultPhrIndex };
		section = page.sectionForPhraseIndex(phrIndex);
		if(section.isNil) {
			section = page.newSection(phrIndex);
		};
		fullPhrID = "/%.%%".format(
			objKey,
			phrase, // if(phrase.notNil) { "." ++ phrase } { "" },
			if(parm.notNil) { "." ++ parm } { "" }
		);
		str = section.stringForPhrase(fullPhrID);
		newStr = "% = %;".format(fullPhrID, string);
		if(str.isNil) {
			str = section.addString(newStr);
		} {
			str.string = newStr;
		};
		currentEnvironment
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
			~pages.do { |page, i|
				if(page.name.asString == phrase) {
					break.(i);
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

BP(\edit).newPage(\z);
BP(\edit).newPage(\a);

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

e = BP(\edit);
z = e.pages[0].sections[0][0];
z.strings.do(_.listVars);

e.pages[0].sectionForPhraseIndex(nil)
e.phraseAndIndex("main").postcs


/k = "----";  // k, j: OK; j, k: inserts one char early, messes up bookmarks
/j = " - -";

/k = "|-  -|--| -  ";
/j = "|-  -||-";
1+1

/k.main0 = "-| -|-|";

/k.main3 = "-| -|-|";
/j.main3 = " - -";

/k.main2 = "-| -|-|";

/j.main1 = "-";
/k.main1 = "--";


/k.a0 = "-|   -| -- |";

/k.d1 = "-| -|-  -| -  ";