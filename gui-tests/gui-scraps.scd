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
				j = pageStr.size - 1;
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
			[pageI, j - 2, 0].debug("<< past document end");
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
				if(keyIndex.isNil) {
					j = pageStr.size;
					keyIndex = ["", j];
				} {
					keyIndex[0] = keyIndex[0][3..];
				};
				if(keyIndex[1] == phrIndex) {
					j = keyIndex[0] + keyIndex[1].size + 4;
				} {
					j = keyIndex[0];
					page.setString("// " ++ phrIndex ++ "\n\n", j, 0);
					pageStr = pageStr.insert(j, "// " ++ phrIndex ++ "\n\n");
					j = j + phrIndex.size + 4;
				};
			} {
				j = 0;  // un-sectioned strings at top
			};

			// find alpha order
			[j, pageStr.size].debug("before while");
			toFind = "/%%%".format(objKey,
				if(phrase.notNil) { "." ++ phrase } { "" },
				if(parm.notNil) { "." ++ parm } { "" }
			).debug("searching for");

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
				j = pageStr.size - 1;
				while { j > 0 and: { pageStr[j] == $\n } } { j = j - 1 };
				if(j > 0 and: { j.debug("last non-newline") == (pageStr.size - 1).debug("last char index") }) {  // need to add a newline
					pageStr = pageStr ++ $\n;
					page.setString("\n", j, 0);  // also updates bookmarks
				};
				[pageI, (j + 2).clip(0, pageStr.size), 0].debug("<< past document end");
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
			~bookmarks.put(objKey, phrase, parm.asSymbol,
				[start, start + patString.size - 1]);
			~bookmarkIndex[start] = [objKey, phrase, parm.asSymbol];
			~bookmarkIndex[start + patString.size - 1] = ~bookmarkIndex[start];
			// if(phrase == \main) { phrase = nil };
			{
				pageView.setString(patString, start, length);
				pageView.addBookmark(start, start + patString.size - 1);
			}.defer(~updateDefer);
		};
	};
