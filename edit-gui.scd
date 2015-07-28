// first: TEXTVIEW WRAPPERS for content change tracking
(
Proto {
	~background = Color.white;
	~stringColor = Color.black;
	~markColor = Color(0.8, 0, 0);
	~font = Font("Inconsolata", 14);
	~textParms = #[font, background, stringColor];
	~str = "";

	~prep = { |parentView(~parentView), bounds(~bounds)|
		~parentView = parentView;
		~bounds = bounds;
		~view = TextView(parentView, bounds)
		.string_(~str)
		.keyUpAction_(e { |... args| ~prKeyUpAction.(*args) })
		.mouseUpAction_(e { |... args| ~prMouseUpAction.(*args) });
		~textParms.().do { |key|
			~view.perform(key.asSetter, key.envirGet);
		};
		// save time for platform lookup
		// this is to detect ctrl-C and ctrl-v
		// but in OSX, it's cmd-c and cmd-v
		~ctrlMod = if(thisProcess.platform.name == \osx) { 1048576 } { 262144 };
		~bookmarks = List.new;
		currentEnvironment
	};

	~addBookmark = { |pos|
		var i = ~bookmarks.detectIndex { |mark| mark >= pos };
		case
		{ i.isNil } { ~bookmarks.add(pos) }
		{ ~bookmarks[i] != pos } {
			~bookmarks.insert(i, pos);
		};
		~drawBookmarks.();
	};
	~removeBookmark = { |pos|
		~bookmarks.remove(pos);
		~drawBookmarks.();
	};

	~background_ = { |color|
		~background = color;
		currentEnvironment
	};
	~drawBookmarks = {
		~view.background_(~background).stringColor_(~stringColor);
		~bookmarks.do { |mark|
			~view.setStringColor(~markColor, mark, 1);
		};
		currentEnvironment
	};

	~setString = { |string, start, length, stringObj|
		var end = start + length;
		~view.setString(string, start, length);
		~str = ~view.string;
		~updateBookmarks.(start, string.size - length, ~str, (length > 0).asInteger, stringObj);
	};
	~string = { ~view.string };
	// DO NOT OVERRIDE
	~prKeyUpAction = { |view, char, mod, unicode, keycode, key|
		var newString, ascii;
		if(char.notNil) {
			newString = view.string;
			~updateBookmarks.(~selectionStart, newString.size - ~str.size, newString);
		};
		if(newString.notNil) {
			~str = newString;
		};
		~getSelection.();
		~keyUpAction.(view, char, mod, unicode, keycode, key);
	};
	~prMouseUpAction = { |view, x, y, mod|
		~getSelection.();
		~mouseUpAction.(view, x, y, mod);
	};
	~getSelection = {
		~selectionStart = ~view.selectionStart;
		~selectionSize = ~view.selectionSize;
		[~selectionStart, ~selectionSize]
	};
	~updateBookmarks = { |start, delta = 0, string, startOffset(0), stringObj|
		var oldMark, newMark;
		if(start.notNil and: { delta != 0 }) {
			currentEnvironment.changed(\contentChanged, start, delta, string, stringObj);
			start = start + startOffset;
			block { |break|
				~bookmarks.reverseDo { |mark, i|
					if(mark < start) {
						break.()
					} {
						oldMark = ~bookmarks[~bookmarks.size - i - 1];
						newMark = mark + delta;
						~bookmarks[~bookmarks.size - i - 1] = newMark;
					};
				};
			};
			~drawBookmarks.();
		};
	};
} => PR(\bookmarkTextView);

PR(\bookmarkTextView).clone {
	~addBookmark = { |start, end|
		var i = ~bookmarks.detectIndex { |mark| mark >= start };
		case
		{ i.isNil } { ~bookmarks.add(start).add(end) }
		{ ~bookmarks[i] != start } {
			if(i.even and: { end <= ~bookmarks[i+1] }) {
				~bookmarks.insert(i, end).insert(i, start);
			} {
				"Range (% .. %) overlaps with another range".format(start, end).warn;
			};
		} {
			"Range (% .. %) overlaps with another range".format(start, end).warn;
		};
		~drawBookmarks.();
	};
	~removeBookmark = { |start|
		var i = ~bookmarks.detectIndex { |mark| mark >= start };
		if(i.notNil) {
			~bookmarks.removeAt(i);
			~bookmarks.removeAt(i);
		};
		~drawBookmarks.();
	};
	~drawBookmarks = {
		~view.background_(~background).stringColor_(~stringColor);
		~visibleBookmarks.pairsDo { |start, end|
			~view.setStringColor(~markColor, start, end - start + 1);
		};
		currentEnvironment
	};
} => PR(\rangeTextView);


// SUPPORT OBJECTS

Proto {
	~string = "";
	~pos = 0;
	~existing = false;

	~length = { ~string.size };
	~end = { ~pos + ~string.size };

	~string_ = { |newStr|
		var oldSize = ~string.size;
		~string = newStr;
		currentEnvironment.changed(\string, newStr, oldSize, currentEnvironment);
		currentEnvironment
	};
	~silentString_ = { |newStr|
		~string = newStr;
		currentEnvironment
	};
} => PR(\clGuiString);

Proto {
	~name = "";
	~prep = {
		~strings = List.new;
		currentEnvironment
	};
	~free = {
		currentEnvironment.changed(\modelWasFreed);
	};

	~addString = { |str|
		var newStr, i;
		if(str.last == $\n) {
			str = str.drop(-1);
		};
		newStr = PR(\clGuiString).copy.string_(str);
		i = ~strings.detectIndex { |item| item.string > str };
		if(i.isNil) {
			newStr.pos = ~strings.sum(_.length) + ~strings.size;
			~strings.add(newStr);
		} {
			newStr.pos = ~strings[i].pos;
			~strings.insert(i, newStr);
		};
		currentEnvironment.changed(\addString, newStr);
	};
	~removeStrings = { |match|
		var c = ~strings.size - 1;
		~strings.reverseDo { |str, i|
			if(str.string.contains(match)) {
				~strings.removeAt(c - i);
				currentEnvironment.changed(\removeString, str);
			};
		};
		currentEnvironment
	};

	~stringForPhrase = { |match|
		~strings.detect { |item| item.string.contains(match) };
	};

	~sortStrings = {
		~strings.sort { |a, b| a.pos < b.pos };
		currentEnvironment.changed(\reset, ~strings);
	};

	~contentChanged = { |start, delta, string, stringObj|
		block { |break|
			~strings.reverseDo { |str, i|
				if(str !== stringObj) {
					if(str.pos < start) {
						if(str.end >= start) {
							// silentString_ b/c the change is coming from the GUI
							str.silentString_(string[str.pos .. str.end - 1 + delta]);
						};
						break.();
					} {
						if(str.existing) {
							str.pos = str.pos + delta;
						} {
							str.existing = true
						};
					};
				};
			};
		};
		currentEnvironment
	};
} => PR(\clGuiSection);

Proto {
	~textViewKey = \rangeTextView;
	~minHeight = 250;
	~prep = { |model, layout, insertIndex|
		var textview;
		~model = model;
		~view = PR(~textViewKey).copy.prep(nil, nil);
		~view.view.minHeight_(~minHeight);
		if(insertIndex.notNil) {
			layout.insert(~view.view, insertIndex);
		} {
			layout.add(~view.view);
		};
		model.addDependant(currentEnvironment);
		~view.addDependant(currentEnvironment);
		currentEnvironment
	};
	~free = {
		~view.removeDependant(currentEnvironment);
		~view.remove;
		~model.strings.do(_.removeDependant(currentEnvironment));
		~model.removeDependant(currentEnvironment);
		currentEnvironment
	};
	~update = { |obj, what ... args|
		switch(what)
		{ \contentChanged } {
			~model.contentChanged(*args);
		}
		{ \string } {  // obj is the \clGuiString
			~view.setString(args[0], obj.pos, args[1], obj);
		}
		{ \addString } {
			args = args[0];
			~view.setString(args.string ++ $\n, args.pos, 0);
			args.addDependant(currentEnvironment);  // receive changes from this \clGuiString
		}
		{ \removeString } {
			args = args[0];
			~view.setString("", args.pos, args.length + 1);
			args.removeDependant(currentEnvironment);
		}
		{ \reset } {
			"\clGuiSectionView:reset, implement later".warn;
		}
		{ \modelWasFreed } {
			~free.();
		};
	};
	~textParms = { PR(~textViewKey).textParms };
} => PR(\clGuiSectionView);

Proto {
	~prep = { |layout, insertIndex|
		~view = ScrollView();
		if(insertIndex.notNil) {
			layout.insert(~view, insertIndex);
		} {
			layout.add(~view);
		};
		~layout = VLayout();
		~view.canvas_(View().layout_(~layout));
		~sections = List.new;
		currentEnvironment
	};
	~free = {
		~sections.do { |pair| pair[0].free };
	};
	~sectionForPhraseIndex = { |phrIndex|
		var pair = ~sections.detect { |pair| pair[0].name == phrIndex };
		if(pair.notNil) { pair[0] } { nil };
	};
	~newSection = { |name|
		var i = ~sections.detectIndex { |pair| pair[0].name > name },
		newSect = PR(\clGuiSection).copy.name_(name.asString).prep,
		label = StaticText().string_(name),
		newGui = PR(\clGuiSectionView).copy
		.putAll(~getTextParms.())
		.prep(newSect, ~layout, if(i.notNil) { i * 2 });

		if(i.notNil) {
			~layout.insert(label, i * 2);
			~sections.insert(i, [newSect, newGui]);
		} {
			~layout.insert(label, ~sections.size * 2);
			~sections.add([newSect, newGui]);
		};
		newSect
	};
	~textParms = { PR(\clGuiSectionView).textParms };
	~getTextParms = {
		var out = IdentityDictionary.new;
		~textParms.().do { |key| out.put(key, key.envirGet) };
		out
	};
} => PR(\clGuiPage);


// MAIN GUI

Proto {
	~bounds = Window.screenBounds;
	~windowTitle = "Pattern editor";
	~backColor = Color.gray(0.078);
	~windowBackground = Color.gray(0.2);
	~stringColor = Color.white;
	~textFont = Font("Inconsolata", 24);
	~updateDefer = 0.1;
	~defaultPhrIndex = " - Unnumbered - ";

	~prep = {
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
		~notifier.remove;
		~pages.do(_.free);
		~win.onClose_(nil).close;
		"freed".debug;
	};

	~makeGui = {
		~win = Window(~windowTitle, ~bounds.value).background_(~windowBackground);
		~pages = Array.new;
		~pageMenu = PopUpMenu()
		.background_(~backColor).stringColor_(~stringColor)
		.action_(e { |view| ~setPage.(view.value) });
		~win.layout = VLayout(
			~pageMenu,
			~stackLayout = StackLayout()
		);
		~newPage.(\main);
		~win.front;
		~win.onClose = e {
			~win.onClose = nil;
			BP(~collIndex).free;
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
			phrase,
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