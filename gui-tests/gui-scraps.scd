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
