// first: TEXTVIEW WRAPPERS for content change tracking
(
var getTextProperties = {
	var result = IdentityDictionary.with(\textProperties -> ~textProperties);
	~textProperties.do { |key|
		result.put(key, key.envirGet);
	};
	result
};

Proto {
	~background = Color.white;
	~stringColor = Color.black;
	~markColor = Color(0.8, 0, 0);
	~font = Font("Inconsolata", 14);
	~markFont = ~font.boldVariant;
	~textProperties = #[font, background, stringColor];
	~str = "";
	~displayBookmarks = false;

	~prep = { |parentView(~parentView), bounds(~bounds)|
		var currentEnv = currentEnvironment;
		~parentView = parentView;
		~bounds = bounds;
		~view = TextView(parentView, bounds)
		.string_(~str)
		.keyUpAction_(e { |... args| ~prKeyUpAction.(*args) })
		.mouseUpAction_(e { |... args| ~prMouseUpAction.(*args) })
		.focusGainedAction_({ currentEnv.changed(\focused, true) })
		.focusLostAction_({ currentEnv.changed(\focused, false) });
		~textProperties.().do { |key|
			~view.tryPerform(key.asSetter, key.envirGet);
		};
		~view.palette = QPalette.system.baseText_(~stringColor).base_(~background);
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
	~focus = { |bool| ~view.focus(bool) };

	~highlight = { |highlightOn(false), start, size|
		defer(e {
			~view.setStringColor(
				if(highlightOn) { ~markColor } { ~stringColor },
				start, size
			);
			~view.setFont(
				if(highlightOn) { ~markFont } { ~font },
				start, size
			);
		});
		currentEnvironment
	};
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
			if(~displayBookmarks) { ~drawBookmarks.() };
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

	~prep = {
		if(~objKey.notNil and: { ~phrase.notNil }) {
			NotificationCenter.register(
				~objKey.asSymbol, ~phrase.asSymbol, currentEnvironment,
				e { |highlight(false)|
					currentEnvironment.changed(\highlight, highlight)
				}
			)
		};
		currentEnvironment
	};
	~free = {
		if(~objKey.notNil and: { ~phrase.notNil }) {
			NotificationCenter.unregister(~objKey.asSymbol, ~phrase.asSymbol, currentEnvironment);
		};
		currentEnvironment
	};

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
	~defaultString = "\n\n//--- scratch\n";
	~prep = {
		~strings = List.new;
		currentEnvironment
	};
	~free = {
		~strings.do(_.free);
		currentEnvironment.changed(\modelWasFreed);
	};

	~addString = { |str, objKey, phrase|
		var newStr, i;
		if(str.last == $\n) {
			str = str.drop(-1);
		};
		newStr = PR(\clGuiString).copy
		.objKey_(objKey).phrase_(phrase)
		.prep
		.string_(str);
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
				str.free;
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
	~getTextProperties = getTextProperties;
} => PR(\clGuiSection);

Proto {
	~textViewKey = \rangeTextView;
	~minHeight = 250;
	~prep = { |model, layout, insertIndex|
		var textview;
		~model = model;
		// model.getTextProperties.keysValuesDo { |key, value|
		// 	key.envirPut(value);
		// };
		~view = PR(~textViewKey).copy
		.putAll(model.getTextProperties)
		.str_(model.defaultString ? "")
		.prep(nil, nil);
		~view.view.minHeight_(~minHeight);
		if(insertIndex.notNil) {
			layout.insert(~view.view, insertIndex);
		} {
			layout.add(~view.view);
		};
		model.addDependant(currentEnvironment);
		~view.addDependant(currentEnvironment);
		~view.keyUpAction = e { |view, char|
			if(char.notNil and: { char.ascii == 27 }) {
				~model.changed(\escKey);
			};
		};
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
		// [obj, what, args].debug(">> clGuiSectionView:update");
		switch(what)
		{ \contentChanged } {
			~model.contentChanged(*args);
		}
		{ \highlight } {
			~view.highlight(args[0], obj.pos, obj.length);
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
		{ \focus } {
			~view.focus(args[0] ? true)
		}
		{ \focused } {
			// I have to change the message name,
			// or ~model.changed recurs infinitely back to here
			~model.changed(\focusedSection, args[0], ~model)
		}
		{ \modelWasFreed } {
			~free.();
		};
		// [obj, what, args].debug("<< clGuiSectionView:update");
	};
	// ~textProperties = { PR(~textViewKey).textProperties };
	// ~getTextProperties = getTextProperties;
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
		~view.canvas_(View().background_(~scrollBackground).layout_(~layout));
		// ~view.background_(~scrollBackground);
		~view.palette_(QPalette.new.base_(~scrollBackground));
		~sections = List.new;
		currentEnvironment
	};
	~free = {
		~sections.do { |pair| pair[0].removeDependant(currentEnvironment).free };
	};
	~sectionForPhraseIndex = { |phrIndex|
		var pair = ~sections.detect { |pair| pair[0].name == phrIndex };
		if(pair.notNil) { pair[0] } { nil };
	};
	~labelForPhraseIndex = { |phrIndex|
		var i = ~sections.detectIndex { |pair| pair[0].name == phrIndex };
		if(i.notNil) {
			~view.children[i * 2 + 1]
		};
	};
	~newSection = { |name|
		var i = ~sections.detectIndex { |pair| pair[0].name > name },
		newSect = PR(\clGuiSection).copy
		.name_(name.asString)
		.putAll(~getTextProperties.())
		.prep,
		label = StaticText().string_(name).stringColor_(~stringColor),
		newGui = PR(\clGuiSectionView).copy.prep(newSect, ~layout, if(i.notNil) { i * 2 });

		if(i.notNil) {
			~layout.insert(label, i * 2);
			~sections.insert(i, [newSect, newGui]);
		} {
			~layout.insert(label, ~sections.size * 2);
			~sections.add([newSect, newGui]);
		};
		newSect.addDependant(currentEnvironment);
		newSect
	};
	~textProperties = { ~textProperties ?? { PR(\clEditGui).textProperties } };
	~getTextProperties = getTextProperties;
	~updateForwards = IdentitySet.with(\escKey, \focusedSection);
	~update = { |obj, what ... args|
		if(~updateForwards.includes(what)) {
			currentEnvironment.changed(what, *args);
		};
	};
} => PR(\clGuiPage);

Proto {
	~maxWidth = 120;
	~prep = { |model|
		~oldPage = model.currentPage;
		~oldVisible = model.pageVisibleOrigin(~oldPage);
		~view = TextField().maxWidth_(~maxWidth)
		.keyUpAction_(e { |view, char|
			if(char.notNil) {
				case
				{ char.isPrint } {
					currentEnvironment.changed(\setVisible, view.string);
				}
				{ char.ascii == 27 } {
					currentEnvironment.changed(\resetVisible, ~oldPage, ~oldVisible);
					~remove.();
				};
			};
		})
		.action_(e { |view|
			currentEnvironment.changed(\setVisible, view.string, \done);
			~remove.();
		});
		model.menuLayout.add(~view);
		currentEnvironment
	};
	~remove = {
		~view.remove;
		currentEnvironment.changed(\popUpRemoved);
	};
	~focus = { |bool|
		~view.focus(bool);
		currentEnvironment
	};
} => PR(\clGuiPopUpSelector);

// MAIN GUI

Proto {
	~bounds = Window.screenBounds;
	~windowTitle = "Pattern editor";
	~backColor = Color.gray(0.078);
	~windowBackground = Color.gray(0.15);
	~scrollBackground = Color.gray(0.3);
	// ~stringColor = Color.white;
	// ~textFont = Font("Inconsolata", 24);
	~updateDefer = 0.1;
	~defaultPhrIndex = " - Unnumbered - ";

	// textview properties, to pass down
	~background = Color.gray(0.15);
	~stringColor = Color.white;
	~markColor = Color(0.5, 1, 0.5);  // Color(0.8, 0, 0);
	~font = Font("Inconsolata", 18);
	~markFont = ~font.boldVariant;
	~textProperties = #[background, scrollBackground, stringColor, markColor, font, markFont];
	~getTextProperties = getTextProperties;

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
		if(~selectField.notNil) { ~selectField.removeDependant(currentEnvironment) };
		~pages.do { |page| page.removeDependant(currentEnvironment).free };
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
			~menuLayout = HLayout(~pageMenu),
			~stackLayout = StackLayout()
		);
		~newPage.(\main);
		~pages[0].sections[0][0].dependants;
		~pages[0].sections[0][0].changed(\focus, true);
		~setPage.(0);
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
		var newPage = PR(\clGuiPage).copy
		.name_(key)
		.putAll(~getTextProperties.())
		.prep(~stackLayout, i);
		if(i.isNil) {
			i = ~pages.size;
			~pages = ~pages.add(newPage);
			~pageMenu.items = ~pageMenu.items.add(key.asString);
		} {
			origItem = ~pageMenu.items[~pageMenu.value];
			~pages = ~pages.insert(i, newPage);
			~pageMenu.items = ~pageMenu.items.insert(i, key.asString);
			~currentPage = ~pageMenu.items.indexOfEqual(origItem);
			~pageMenu.value = ~currentPage;
		};
		newPage.addDependant(currentEnvironment);
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
			str = section.addString(newStr, objKey, phrase);
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
		~currentPage = index;
		~pageMenu.value = index;
		~stackLayout.index = index;
	};
	~pageVisibleOrigin = { |index|
		~pages[index].view.visibleOrigin;
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

	// typing navigation
	~popUpSelector = {
		if(~selectField.isNil) {
			~selectField = PR(\clGuiPopUpSelector).copy.prep(currentEnvironment);
			~selectField.addDependant(currentEnvironment);
			~selectField.focus(true);
		} {
			~selectField.focus(true);
		};
	};

	~findVisible = { |str|
		var regex, phrKey, phrIndex, pageIndex, label;
		if(str.notNil) {
			regex = str.findRegexp("([0-9]+)$");
			if(regex.notEmpty) {
				phrIndex = regex[1][1];
				if(regex[1][0] > 0) {
					phrKey = str[ .. regex[1][0] - 1];
				} {
					pageIndex = ~currentPage;
				}
			} {
				phrKey = str;
			};
			[pageIndex, phrKey, phrIndex];
			if(pageIndex.isNil) {
				pageIndex = block { |break|
					~pages.do { |page, i|
						if(page.name.asString.beginsWith(phrKey)) { break.(i) };
					};
					nil
				};
			};
			if(pageIndex.notNil) {
				~setPage.(pageIndex);
				~label = ~pages[pageIndex].labelForPhraseIndex(phrIndex ?? { ~defaultPhrIndex });
				if(~label.notNil) {
					~pages[pageIndex].view.visibleOrigin = Point(0, ~label.bounds.top);
				};
			};
		};
		[phrKey, phrIndex]
	};

	~update = { |obj, what ... args|
		var phrKey, phrIndex;
		switch(what)
		{ \escKey } {
			~popUpSelector.();
		}
		{ \setVisible } {
			#phrKey, phrIndex = ~findVisible.(args[0]);
			if(args[1] == \done /*and: { phrIndex.notNil }*/) {
				~pages[~currentPage].sectionForPhraseIndex(phrIndex ?? { ~defaultPhrIndex })
				.changed(\focus, true);
			};
		}
		{ \resetVisible } {
			~setPage.(args[0]);
			~pages[args[0]].view.visibleOrigin = args[1];
			if(~currentSection.notNil) {
				~currentSection.changed(\focus, true)
			};
		}
		{ \popUpRemoved } {
			~selectField.removeDependant(currentEnvironment);
			~selectField = nil;
		}
		{ \focusedSection } {
			if(args[0]) {
				~currentSection = args[1];
			};
		};
	};
} => PR(\clEditGui);
)
