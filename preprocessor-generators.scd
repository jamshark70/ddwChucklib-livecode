var saveType = AbstractChuckArray.defaultSubType;

// cl-livecode generator objects

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

AbstractChuckArray.defaultSubType = \clGenerator;

protect {

	// clGen is abstract, *and* serves as the generic timed-sequence generator
	Proto({
		~bpKey = nil;  // supply to 'use' this environment while inside the pattern
		~args = nil;   // override with an array
		~parm = \value;
		~isMain = false;
		~addRestToEmpty = true;
		~protoID = \clGen;
		~isClGen = true;
		~canBePool = false;
		~repeats = 1;

		// responsible for parsing any time-sequence strings
		// each generator may have different ones
		// generic one assumes only ~args[0]
		~prep = {
			// ~baseItems = ~parse.(~args[0]);
			~baseItems = ~args[0];
			~checkPoolArgs.();
			currentEnvironment
		};

		~doProcess = {
			var temp;
			if(~repository.isNil) {
				if(BP.exists(~bpKey)) {
					if(BP(~bpKey)[\genRepository].notNil) {
						~repository = BP(~bpKey)[\genRepository];
					} {
						~repository = IdentityDictionary.new;
						BP(~bpKey)[\genRepository] = ~repository;
					};
				};
				if(~repository[~parm].isNil and: {
					temp = BP(~bpKey).parmMap[~parm].tryPerform(\at, \default);
					temp.notNil
				}) {
					~repository[~parm] = (time: 0, item: temp);
				};
			};
			~process.();
			if(~items.size > 0) {
				if(~repository[~parm].isNil) {
					// this happens only the first time the phrase is invoked in a new BP:
					// if nothing on the downbeat, S&H the first real item
					~repository[~parm] = ~items.first;
				} {
					~repository[~parm] = ~items.last;
				};
			};
			~items
		};
		~process = {
			~items = ~getUpstreamItems.();
		};
		~unprocess = {
			if(~items.notNil) { ~cachedItems = ~items };
			~items = nil;
		};
		// .value polymorphism: we may need items from an array or a clgen
		// Proto maps .value onto ~next
		~next = { ~process.() };
		~getUpstreamItems = { |key(\baseItems)|
			var out;
			// ">> % getUpstreamItems".format(~protoID).debug;
			out = key.envirGet.asArray
			.collect { |item| item.value }
			.flatten(1)
			.collect { |item|
				if(item[\wildcard].isNil) { item[\wildcard] = item[\item] };
				item
			};
			// "<< % getUpstreamItems".format(~protoID).debug;
			// out.postcs
		};
		~updateDeltas = {
			var prevItem, default;
			if(BP.exists(~bpKey) and: { ~parm != BP(~bpKey).defaultParm }) {
				if(~items.isEmpty) {
					~deltas = [~dur];
					prevItem = ~repository[~parm];
					if(prevItem.notNil) {
						prevItem = prevItem.item
					} {
						default = BP(~bpKey).parmMap[~parm][\default];
						if(default.notNil) {
							prevItem = default
						} {
							// suppresses event
							// but that's better than returning nil and stopping the process
							// for politeness, tell the user what happened
							"BP(%) parameter % has no items and no default; substituting \\rest"
							.format(~bpKey.asCompileString, ~parm.asCompileString)
							.warn;
							prevItem = \rest
						};
					};
					~items = [(item: prevItem, time: 0)];
				} {
					~deltas = (~items.collect(_[\time]) ++ ~dur).differentiate;
					if(~items[0][\time] == 0) {
						~deltas = ~deltas.drop(1);
					} {
						~deltas[0] = ~items[0][\time];
						prevItem = ~repository[~parm];
						if(prevItem.notNil) { prevItem = prevItem.item };
						~items = ~items.copy.insert(0, (item: prevItem, time: 0));
					};
				};
			} {
				if(~items.isEmpty) {
					~deltas = [Rest(~dur)];
					~items = [(item: \rest, time: 0, initialRest: true)];
				} {
					~deltas = (~items.collect(_[\time]) ++ ~dur).differentiate;
					if(~items[0][\time] == 0) {
						~deltas = ~deltas.drop(1);
					} {
						~deltas[0] = Rest(~items[0][\time]);
						~items = ~items.copy.insert(0, (item: \rest, time: 0, initialRest: true));
					};
				};
			};
			case
			{ BP.exists(~bpKey) and: { BP(~bpKey)[\swing].notNil } } {
				~deltas = BP(~bpKey)[\swing].mapDeltaArray(~deltas);
			}
			{ Library.at(\globalSwing).notNil } {
				~deltas = Library.at(\globalSwing).mapDeltaArray(~deltas);
			};
			~items = ~items.collect { |item, i| item.copy.put(\dur, ~deltas[i]) };
			// ~items.do { |item, i| item[\dur] = ~deltas[i] };
		};
		~asPattern = { |bp|  // the BP() object -- asPattern should be called @ top level only
			var pat, rest, didProcess = false;
			if(~items.isNil) {
				~items = ~doProcess.(bp);
				~updateDeltas.();
				// "clGen items: ".post; ~items.postcs;
				didProcess = true;
			};
			// if(~items[0].time > 0) {
			// 	rest = [(time: 0, dur: ~items[0].time, item: Rest(~items[0].item))];
			// };
			// "rest ++" also works if 'rest' is nil
			pat = Pseq(rest ++ ~items, 1);
			if(BP.exists(~bpKey) and: { ~parm != BP(~bpKey).defaultParm }) {
				pat = pat.collect { |ev, inEvent|
					if(ev[\initialRest] == true) {
						inEvent.put(\initialRest, true);
					};
					if(ev[\item].isNil) {
						// incumbent on you to put the parm into ~defaults
						[inEvent[~parm], ev[\dur]]
					} {
						[ev[\item], ev[\dur]]
					}
				};
			} {
				pat = pat.collect { |ev, inEvent|
					if(ev[\initialRest] == true) {
						inEvent.put(\initialRest, true);
					};
					[ev[\item], ev[\dur]]
				};
			};
			if(~parm.notNil) {
				~repository[~parm] = ~items.last;
			};
			if(didProcess) { ~unprocess.() };
			if(~isMain) {
				pat
			} {
				PstepDurPair(pat, 1)
			}
		};
		~embedInStream = { |inval|
			var dur = ~dur.(),
			startTime = thisThread.clock.beats,
			next, pitch, bp, rout;
			if(BP.exists(~bpKey)) {  // this also avoids 'nil'
				bp = BP(~bpKey);
			};

			~asPattern.(bp).embedInStream(inval);
		};
		~asStream = {
			Routine { |inval| ~embedInStream.(inval) }
		};
		~canEmbed = true;  // do not override!

		~yieldsKeys = {
			if(~isMain) { [~parm, \dur] } { ~parm };
			// if(~isPitch.()) { [~parm, \legato] } { ~parm }
		};

		// problem: in an item, only 'time' is sure to be set
		// dur must be calculated -- this should be a general support method
		~getIndexDur = { |index|
			if(index < (~items.size - 1)) {
				// there's another item after this one
				~items[index+1].time - ~items[index].time
			} {
				// last item; this generator's ending time index - item time
				~time + ~dur - ~items[index].time
			};
		};

		if('DDWSnippets'.asClass.notNil) {
			~snippet = { |key, value| 'DDWSnippets'.asClass.put(key, value) };
		};

		// articulation support; not all generators will use this,
		// but inheritance is complex enough, just put it here
		~artic = ($~: inf /*1.01*/, $_: 0.9, $.: 0.4,
			$>: { |note| note.args = \accent; nil }  // don't change length
		);
		~isValidArtic = { |artic|
			~artic[artic.value].notNil or: {
				artic.tryPerform(\key) == \accent or: {
					if(artic.tryPerform(\canBePool) ?? { false }) {
						artic.poolArgs.every { |item|  // poolArgs is array-of-args
							// each arg may be a Proto or an array
							item.asArray.every(~isValidArtic)
						};
					} {
						if(artic.size > 0) { artic.every(~isValidArtic) } { false }
					};
				};
			}
		};
		~checkArticPool = { |pool|
			// for now, shortcut, assume Association's key is \accent
			// because there is no other usage currently
			pool.do { |item|
				if(~isValidArtic.(item).not) {
					Error("Invalid articulation pool").throw;
				};
			};
			true
		};
		// quite confusing interface.
		// 'artic' may be a char (. _ ~) *or* an association (accent -> char or nil)
		// but also, stutt may need to clear the accent flag, so...
		// accent: if nil, no change; otherwise, should be boolean
		~applyArtic = { |artic, note, accent|
			var didCopy = false, item;
			if(note.isKindOf(SequenceNote)) {
				item = artic.tryPerform(\key);
				if(item.notNil) {
					note = note.copy.args_(item);
					didCopy = true;
				};
				item = ~artic[artic.value];
				if(item.notNil) {
					if(didCopy.not) { note = note.copy };
					note.length = item;
				};
				if(note.length.isNil) { note.length = 0.9 };
				if(accent.notNil) {
					if(accent) {
						note.args = \accent
					} {
						note.args = nil
					}
				};
			} {
				"Generator tried to apply articulation '%' to '%'"
				.format(artic, note).warn;
			};
			note
		};
		~isNumProxy = { |object|
			object.tryPerform(\isNumProxy) == true or: { object.isNumber }
		};
		~poolArgs = nil;
		~checkPoolArgs = { |inPool = false|
			var poolArgs = ~poolArgs.().asArray, pool;
			// [~protoID, inPool].debug(">> checkPoolArgs");
			// check myself, before recursion
			if(inPool and: { not(~canBePool ?? { false }) }) {
				Error("Invalid generator '%' in pool".format(~protoID)).throw;
			};
			~args.do { |obj, i|
				// [obj, i].debug("checking");
				case
				{ obj.tryPerform(\isClGen) ?? { false } } {
					obj.checkPoolArgs(poolArgs.includes(obj));
				}
				{ obj.isArray } {
					pool = poolArgs.includes(obj);
					obj.do { |item|
						if(item.tryPerform(\isClGen) ?? { false }) {
							item.checkPoolArgs(pool)
						};
					};
				};
			};
			// [~protoID, inPool].debug("<< checkPoolArgs");
		};
		~checkEmpty = { |obj, str|
			if(obj.size == 0) {
				Error("Values expected for %, but was empty".format(str)).throw;
			};
		};
	}, parentKeys: #[canEmbed, protoID, isClGen, patternStringKeys, patternStringFuncs])
	.import((clPatternSet: #[decodePitch])) => PR(\clGen);

	PR(\clGen).copy => PR(\clGenTest);

	// "\seq("rhythm", "itemseq", "replaceItems", >0 to reset)
	PR(\clGen).clone {
		~poolArgs = { [~args[1]] };
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1].asArray;
			~checkEmpty.(~pool, "pool");
			if(~args[2].isSequenceableCollection) {
				~replaceItems = ~args[2].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			// if(~args[3].isNumber) {
			// 	~doReset = ~args[3] > 0;
			// } {
			// 	~doReset = false;
			// };
			~doReset = ~args[3] ?? { 0 };
			currentEnvironment
		};

		~process = {
			if(~doReset.next > 0 or: { ~seqStream.isNil }) {
				~makeStream.();
			};
			~items = ~getUpstreamItems.();
			~modifySource.();
			~items
		};
		// assumes ~items has already been populated
		~modifySource = {
			~items.do { |item, i|
				// includesEqual: SequenceNotes compare pitch only! Use pitch for matching
				if(~replaceItems.includesEqual(item.item)) {
					// some replacers might need access to the state
					~currentItem = item;
					~currentIndex = i;
					// copy is critical here!
					// 18-1014: wildcard-replacers update the wildcard
					~items[i] = item.copy.put(\wildcard, item[\item])
					.put(\item, ~seqStream.next(item));
				};
			};
		};

		// makeStream should always be infinite
		~makeStream = {
			~seqStream = Pseq(~pool.asArray, inf).collect(_.item).asStream;
		};
		// embedInStream is for generators embedded in pools, should use repeats
		~embedInStream = { |inval|
			Pseq(~pool, ~repeats).embedInStream(inval)
		};
		~canBePool = true;
		~helpString = "\\seq(\"items\")";
		~snippet.("seq", "\\seq(##src, items, wildcards, reset##)");
	} => PR(\clGenSeq);

	PR(\clGenSeq).clone {
		// ~asPattern = { Prand(~args[0], inf) };
		~makeStream = {
			~seqStream = Prand(~pool.asArray, inf).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			Prand(~pool, ~repeats).embedInStream(inval)
		};
		~helpString = "\\rand(\"timing\", \"items\")";
		~snippet.("rand", "\\rand(##src, items, wildcards##)");
	} => PR(\clGenRand);

	PR(\clGenSeq).clone {
		~makeStream = {
			~seqStream = Pxrand(~pool.asArray, inf).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			Pxrand(~pool, ~repeats).embedInStream(inval)
		};
		~helpString = "\\xrand(\"items\")";
		~snippet.("xrand", "\\xrand(##src, items, wildcards, reset##)");
	} => PR(\clGenXrand);

	PR(\clGenSeq).clone {
		~poolArgs = nil;
		// The whole point of a "reference" generator is:
		// Every instance should share the same stream.
		// Pdefn(key).asStream follows SC convention and makes
		// independent streams. So I need my own
		// stream repository: ~global is like a classvar here.
		~global = IdentityDictionary.new;
		~repository = { PR(\clGenGdefn).global };
		~makeStream = {
			var stream, repository = ~repository.();
			if(repository[~pool].isNil) {
				repository[~pool] = Pdefn(~pool).asStream;
			};
			stream = repository[~pool];
			// Insurance against failure: auto-reset if the stream ends.
			// 10 nils in a row, stop (don't infloop)
			~seqStream = Routine { |inval|
				var next, tries, warned = false;
				loop {
					tries = 10;
					while {
						next = stream.next(inval);
						next.isNil and: { tries > 0 }
					} {
						tries = tries - 1;
						stream.reset;
					};
					if(next.isNil) {
						if(warned.not) {
							"\\pdefn(, `%) stream ended, rests will be substituted".format(~pool).warn;
							warned = true;
						};
					} {
						warned = false;
					};
					inval = next.yield;
				};
			};
		};
		~helpString = "\\gdefn(, `key)";
		~snippet.("gdefn", "\\gdefn(, `##key##, \"wildcards\", reset)");
	} => PR(\clGenGdefn);

	// similar, but each BP has its own stream repository
	PR(\clGenGdefn).clone {
		~repository = {
			if(BP.exists(~bpKey)) {
				if(BP(~bpKey)[\clGenPdefn].isNil) {
					BP(~bpKey)[\clGenPdefn] = IdentityDictionary.new;
				};
				BP(~bpKey)[\clGenPdefn]
			} {
				if(~bpKey.isNil) {
					Error("clGenPdefn: ~bpKey must be populated").throw;
				} {
					Error("clGenPdefn: BP(%) must exist".format(~bpKey.asCompileString)).throw;
				};
			};
		};
		~snippet.("pdefn", "\\pdefn(, `##key##, \"wildcards\", reset)");
	} => PR(\clGenPdefn);

	PR(\clGenSeq).clone {
		~superprep = ~prep;
		~prep = {
			~superprep.();
			~doReset = 0;  // reset is meaningless for wrand anyway
			currentEnvironment
		};
		~makeStream = {
			~seqStream = Pwrand(~pool.asArray, ~weightPattern.(), inf).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			Pwrand(~pool, ~weightPattern.(), ~repeats).embedInStream(inval)
		};
		~weightPattern = {
			var firstNum = ~args.detectIndex { |item| ~isNumProxy.(item) };
			if(firstNum.isNil) {
				// optimization: constant weights, don't need a stream
				Array.fill(~pool.size, ~pool.size.reciprocal);
			} {
				// these are number proxies, should respond to asStream
				Ptuple(~args[firstNum..].extend(~pool.size, 1))
				.asStream.collect(_.normalizeSum);
			};
		};
		~helpString = "\\wrand(\"items\", weight0, weight1...)";
		~snippet.("wrand", "\\wrand(##src, items, wildcards, weights...##)");
	} => PR(\clGenWrand);

	PR(\clGenSeq).clone {
		~makeStream = {
			~seqStream = Pn(Pshuf(~pool.asArray, 1), inf).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			Pn(Pshuf(~pool, 1), ~repeats).embedInStream(inval)
		};
		~helpString = "\\shuf(\"items\")";
		~snippet.("shuf", "\\shuf(##src, items, wildcards, reset##)");
	} => PR(\clGenShuf);

	PR(\clGenSeq).clone {
		~poolArgs = { [~args[2]] };
		~canBePool = false;  // inheritance here is messy...
		~defaults = [[], [(item: $*)], #[-2, -1, 1, 2].collect({ |int| (item: int) }), 0, 7, "_"];
		~argNames = #[baseItems, replaceItems, intervalPool, fallbackMin, fallbackMax, articPool];
		~artic = ($~: inf /*1.01*/, $_: 0.9, $.: 0.4,
			$>: { |note| note.args = \accent; nil }  // don't change length
		);
		~doReset = 0;
		~prep = {
			~argNames.do { |key, i|
				key.envirPut(~args[i] ?? { ~defaults[i] });
			};
			~checkArticPool.(~articPool);  // throws error on failure
			~replaceItems = ~replaceItems.collect { |item| item.item };
			~intervalStream = Prand(~intervalPool, inf).asStream.collect { |item| item.item.asFloat };
			~articPool = ~articPool.reject(_ == $");
			currentEnvironment
		};
		~makeStream = {
			~seqStream = Pfunc {
				var i = ~currentIndex, pitch, prev, new, newLen;
				while {
					i = i - 1;
					i >= 0 and: {
						~items[i].item.isKindOf(SequenceNote).not or: {
							// in here, ~items[i].item must be a SequenceNote
							~items[i].item.freq.isRest
						}
					}
				};
				prev = ~items[i];
				if(prev.isNil) {
					pitch = rrand(~fallbackMin.next, ~fallbackMax.next);
				} {
					pitch = prev.item.asFloat + ~intervalStream.next;
				};
				new = SequenceNote(pitch);
				new = ~applyArtic.(~articPool.choose, new);
				new
			}.asStream;
		};
		~snippet.("pitch", "\\pitch(##src, replaceItems, intervalPool, fallbackMin, fallbackMax, articPool##)");
	} => PR(\clGenPitch);

	PR(\clGenPitch).clone {
		~defaults = [[], [(item: $*)], #[-2, -1, 1, 2].collect({ |int| (item: int) }), 0, 7, "_", 1];
		~argNames = #[baseItems, replaceItems, intervalPool, fallbackMin, fallbackMax, articPool, connect];
		~doReset = 1;
		~makeStream = {
			~seqStream = Routine {
				// first init only is from previous item
				var pitch, prev, i, new, newLen;
				if(~connect.next > 0) {
					// prev item might be a rest, if it's a placeholder from \ins
					// so, scan backward through rests; normally this won't loop
					i = ~currentIndex;
					while {
						i = i - 1;
						i >= 0 and: { ~items[i].item.isRest }
					};
					prev = ~items[i];
				};
				if(prev.isNil) {
					prev = rrand(~fallbackMin.next, ~fallbackMax.next);
				} {
					prev = prev.item;
				};
				loop {
					pitch = prev.asFloat + ~intervalStream.next;
					prev = pitch;  // loop is always going forward
					new = SequenceNote(pitch);
					new = ~applyArtic.(~articPool.choose, new);
					new.yield
				}
			}
		};
		~snippet.("pitch2", "\\pitch2(##src, replaceItems, intervalPool, fallbackMin, fallbackMax, articPool, connect##)");
	} => PR(\clGenPitch2);

	// transposition: choose interval from pool.
	// 1 = no transposition. 1' = octave up, 1, = octave down
	// can limit to wildcards
	PR(\clGen).clone {
		~poolArgs = { [~args[1]] };
		~defaults = [[], [(item: SequenceNote(0, nil, 0.9))], nil];
		~argNames = #[baseItems, intervalPool, wildcards];
		~prep = {
			~argNames.do { |key, i|
				key.envirPut(~args[i] ?? { ~defaults[i] });
			};
			~intervalStream = Prand(~intervalPool, inf).asStream.collect { |item| item.item.asFloat };
			~wildcards = ~wildcards.collect { |item| item.item };
			currentEnvironment
		};
		~process = {
			var endTime = ~time + ~dur;
			~items = ~getUpstreamItems.();
			~items.do { |item, i|
				if(item.time >= ~time and: {
					item.time < endTime and: {
						~wildcards.isNil or: { ~wildcards.includesEqual(item.wildcard) }
					}
				}) {
					~items[i] = ~apply.(item);
				};
			};
			~items
		};
		~apply = { |item|
			var interval = ~intervalStream.next;
			if(item.item.isKindOf(SequenceNote) and: {
				item.item.isRest.not
			}) {
				item.copy
				.put(\item, item.item.copy.freq_(item.item.asFloat + interval));
			} { item };
		};
		~snippet.("xpose", "\\xpose(##src, intervalPool, wildcards##)");
	} => PR(\clGenXpose);

	PR(\clGenSeq).clone {
		~poolArgs = { [~args[1]] };
		~canBePool = false;
		~snippet.("prev", "\\prev(##src, pool, replaceItems, staccDur##)");
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1] ?? { [] };
			// empty pool = ignored
			~poolStream = if(~pool.size > 0) { Prand(~pool, inf).asStream } { nil };
			if(~args[2].notNil) {
				~replaceItems = ~args[2].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			// rhythmic values <= ~staccDur will be made staccato, otherwise _
			// ~staccDur = 0 disables this feature
			// ignored for non-pitch parameters (because, no concept of articulation)
			if(~isNumProxy.(~args[3])) {
				~staccDur = ~args[3];
			} {
				~staccDur = 0;
			};
			~doReset = 0;  // required by superclass
			currentEnvironment
		};
		~makeStream = {
			// currentIndex is set here -- that's important
			~seqStream = Pfunc { |item|
				var new, temp;
				// find previous item
				var prevIndex = ~currentIndex - 1;
				while { prevIndex >= 0 and: {
					~replaceItems.includesEqual(~items[prevIndex].item)
					or: {
						if(BP.exists(~bpKey)) {
							BP(~bpKey).valueIsRest(~items[prevIndex].item, ~parm)
						} {
							false
						}
					}
				} } {
					prevIndex = prevIndex - 1;
				};
				if(prevIndex < 0) {
					// if none, choose from pool
					new = ~pool.next;
					if(new.notNil) {
						new = new.item.copy;
					} {
						// no previous item, pool is empty:
						// look up last played item
						if(BP.exists(~bpKey)) {
							temp = BP(~bpKey)[\genRepository];
							if(temp.notNil) {  // ~repository might be nil
								temp = temp[~parm];
								if(temp.notNil) {
									new = temp.item.copy;
								};
							};
						};
					};
					if(new.isNil) {
						new = Rest(0);
						"BP(%): \\prev(): No previous item could be found, substituting rest"
						.format(~bpKey.asCompileString)
						.warn;
					};
				} {
					// else repeat previous (this should be the normal case)
					new = ~items[prevIndex].item.copy
				};
				if(new.isKindOf(SequenceNote)) {
					if(~getIndexDur.(~currentIndex) <= ~staccDur.next) {
						new = ~applyArtic.($., new);
					} {
						if(new.length.isNil) {
							new = ~applyArtic.($_, new);
						};
					};
				};
				new
			}.asStream;
		};
	}.import((clGenPitch: #[artic])) => PR(\clGenPrev);

	PR(\clGenSeq).clone {
		~doReset = 0;
		~canBePool = false;
		~poolArgs = { [~args[1]] };
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1].asArray;
			~checkEmpty.(~pool, "pool");
			~numToAdd = ~args[2] ?? { 1 };
			if(~isNumProxy.(~numToAdd).not) {
				Error("numToAdd '%' is not a number".format(~numToAdd)).throw;
			};
			~quant = ~args[3] ?? { 0.25 };
			~poolStream = Prand(~pool, inf).asStream.collect(_.item);
			currentEnvironment
		};
		~modifySource = {
			var avail = ~getAvail.(~items, ~quant.next).scramble;
			avail = avail.keep(min(~numToAdd.next, avail.size).asInteger).sort;
			avail.size.do { |i|
				var newTime = avail[i],
				new;
				// should be guaranteed but, protect against mistakes
				if(newTime.notNil) {
					new = ~poolStream.next;
					if(new.notNil) {
						~insertItem.((item: new, wildcard: new, src: \ins, time: newTime));
					};
				};
			};
			~items
		};
		// 'new' should be an event with time: -- dur: comes from updateDeltas at ending stage
		~insertItem = { |new|
			var insertI = ~items.detectIndex { |item| new[\time] < item[\time] };
			if(insertI.isNil) {
				~items = ~items.add(new);
			} {
				~items = ~items.insert(insertI, new);
			};
			currentEnvironment
		};
		~getAvail = { |items, quant, tolerance = 0.001|
			var i = 0, j = 0, time, avail = List.new,
			times = items.collect({ |item| item[\time] - ~time }) ++ [~dur];
			time = i * quant;
			while {
				time < (~dur - tolerance)
			} {
				if(time absdif: times[j] < tolerance) {
					if(items[j][\initialRest] == true) { avail.add(time) };
					// 'time' is matched at times[j], so we must advance by at least 1
					j = j + 1;
				} {
					avail.add(time);
					// times[j] is somewhere in the future; may not need to advance yet
				};
				time = (i+1) * quant;
				// keep advancing 'j' as long as times[j] is earlier than the next 'time'
				// notNil: if ~dur is, say, 3.25 and quant is 0.5, then j may have advanced
				// past the end of the array. Stop immediately in that case
				while { times[j].notNil and: { time - times[j] >= tolerance } } {
					j = j + 1;
				};
				i = i + 1;
			};
			avail + ~time
		};
		~snippet.("ins", "\\ins(##src, pool, numToAdd, quant##)");
	} => PR(\clGenIns);

	// quant is minimum duration to divide
	// divisor is how many to split into
	// divisible allows e.g. 1.5 times units
	// (divisible == quant means durations quant, quant * 2, quant * 3 etc. can be divided)
	// (divisible == 2*quant means durations quant, quant * 3, quant * 5 etc. can be divided)
	// (divisible == 0 means *only* quant can be divided)
	// divisor may randomize once per bar (I might change this later but it's a bigger rewrite)
	PR(\clGenIns).clone {
		~superprep = ~prep;
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1];
			~replaceItems = ~args[2] ?? { ~pool };
			~numToAdd = ~args[3] ?? { 1 };
			if(~isNumProxy.(~numToAdd).not) {
				Error("numToAdd '%' is not a number".format(~numToAdd)).throw;
			};
			~quant = ~args[4] ?? { 0.25 };
			~divisor = ~args[5] ?? { 2 };
			// if(~divisor <= 1) {
			// 	Error("\\clGenDiv divisor must be >= 1.0").throw;
			// };
			~divisible = ~args[6] ?? { Pfunc({ |quant| quant }) };
			~replaceItems = ~replaceItems.collect(_.item);
			~poolItems = ~pool.collect(_.item);
			if(~poolItems.size > 0) {
				~poolStream = Prand(~poolItems, inf).asStream;
			};
			currentEnvironment
		};
		// copy/paste, pshaw
		~process = {
			~items = ~getUpstreamItems.();
			~modifySource.();
			~items
		};
		~modifySource = {
			var avail = ~getAvail.(~items, ~quant.next).sort;
			avail.size.do { |i|
				var newTime = avail[i],
				new;
				// should be guaranteed but, protect against mistakes
				if(newTime.notNil) {
					new = ~poolStream.next ?? { ~findPreviousForTime.(newTime) };
					if(new.notNil) {
						// I am not sure about wildcard: new
						~insertItem.((item: new, wildcard: new, src: \div, time: newTime));
					};
				};
			};
			~items
		};
		~getAvail = { |items, quant, tolerance = 0.001|
			var durs = (items.collect(_.time) ++ ~dur).differentiate.drop(1),
			avail, numToAdd,
			blocks, testUnit = 0, blockDur, i, div;
			avail = ~items.collect { |item|
				~replaceItems.isEmpty or: { ~replaceItems.includesEqual(item.item) }
			};
			numToAdd = min(~numToAdd.next, avail.count { |bool| bool });
			blocks = Array(numToAdd);
			blockDur = quant;
			div = ~divisible.next(quant);
			while { numToAdd > 0 and: { blockDur < (~dur - tolerance) } } {
				i = durs.collectIndices { |dur, i| avail[i] and: { dur == blockDur } };
				if(i.isEmpty) {
					if(div == 0) {
						blockDur = ~dur + 1;  // force stop: avoid infinite loop
					} {
						testUnit = testUnit + 1;
						blockDur = quant + (div * testUnit);
					};
				} {
					i = i.choose;
					avail[i] = false;
					blocks.add(i);
					numToAdd = numToAdd - 1;
				};
			};
			// now get times for these indices
			div = ~divisor.next;
			if(div <= 1) {
				"In BP(%), \\clGenDiv divisor must be >= 1.0, substituting 2.0"
				.format(~bpKey.asCompileString).warn;
			};
			blockDur = (1 .. div - 1) / div;
			blocks.collect { |i|
				~items[i].time + (~getIndexDur.(i) * blockDur)
			}.flat;
		};
		~findPreviousForTime = { |time|
			var prev;  // optimization, don't scan back unless you have to
			i = ~items.detectIndex { |item|
				if(item.time >= time) {
					true
				} {
					prev = item;
					false
				}
			};
			if(prev.notNil) {
				prev.item
			} {
				// in theory this should never happen,
				// because all 'avail' times should be > incoming event times
				~items.first.item
			}
		};
		~snippet.("div", "\\div(##src, pool, replaceItems, numToAdd, quant, divisor, divisible##)");
	} => PR(\clGenDiv);

	// \stutt(source, match, numToAdd, quant, prob, insItem "-")
	// choose numToAdd of "pool" items and of these, add more items after it
	PR(\clGenIns).clone {
		~poolArgs = { [~args[5]] };
		~prep = {
			~defaultNames.do { |key, i|
				var value = ~args[i] ?? { ~defaults[i].value },
				error;
				if(value.isKindOf(Exception)) { value.throw };
				error = ~validation[i];
				// validation key
				if(error.notNil) { error = error.envirGet.value(value, key) };
				// validation result
				if(error.notNil) { Error(error).throw };
				key.envirPut(value);
			};
			// oh, I don't like this workaround.
			// Prand(~insPool) must not get a number as the inval, or it dies
			// but the function must get the number! So I have to pass it conditionally
			// Whose fault is that? Dictionary:embedInStream
			~insPool = ~insPool.asArray;
			~passIndex = ~insPool.size <= 0;
			~insStream = if(~passIndex.not) {
				Prand(~insPool, inf).asStream.collect(_.item);
			} {
				{ |srcIndex| // will only be called internally
					var new = ~items[srcIndex].item;
					if(new.isKindOf(SequenceNote)) {
						// all stuttered notes should be staccato and non-accented
						new = ~applyArtic.($., new, false);
					};
					new
				}
			};
			~matchItems = ~match.asArray.collect(_.item);
			if(~wildcards.isNil) {
				~wildcardCheck = true;
			} {
				~wildcards = ~wildcards.collect(_.item);
				~wildcardCheck = { |item| ~wildcards.includesEqual(item) };
			};
			currentEnvironment
		};
		~defaultNames = #[baseItems, match, numToAdd, quant, prob, insPool, wildcards];
		~defaults = [nil, nil, 1, 0.25, 0.75, nil, nil];
		// expand this later
		~validation = [nil, nil, \isNumProxy, \isNumProxy, \isNumProxy, nil, nil];
		~isNumProxy = { |thing, key|
			if(thing.tryPerform(\isNumProxy) != true) {
				"BP(%): %'s % input expected number"
				.format(~bpKey.asCompileString, ~protoID, key.asCompileString)
			} { nil };  // else no error
		};
		~canStutter = { |item, index, quant|
			BP(~bpKey).valueIsRest(item.item, ~parm).not and: {
				~getIndexDur.(index) >= (2 * quant) and: {
					~matchItems.isEmpty or: { ~matchItems.includesEqual(item.item) }
					and: { ~wildcardCheck.(item[\wildcard]) }
				}
			}
		};
		~modifySource = {
			var nextTime, time, times, new, quant, prob;
			// later, we check index vs ~items.size
			// but ~items.size changes as we add new items
			// so, save the original input size
			var inputSize = ~items.size;
			quant = ~quant.next;
			prob = ~prob.next;
			// get source indices
			~items.collectIndices { |item, index| ~canStutter.(item, index, quant) }
			.scramble.keep(~numToAdd.next).do { |index|
				var didAdd = false;
				time = ~items[index].time;
				nextTime = if(index < (inputSize - 1)) {
					~items[index + 1].time
				} {
					~time + ~dur  // this generator's ending time
				};
				while {
					time = time + quant;
					time < nextTime
				} {
					if(prob.coin) {
						times = times.add([time, index]);
						didAdd = true;
					};
				};
				if(didAdd and: { ~isPitch ?? { false } }) {
					if(~items[index].item.isKindOf(SequenceNote)) {
						~items[index].item = ~applyArtic.($., ~items[index].item);
					};
				};
			};
			times.do { |pair|
				new = ~insStream.value(if(~passIndex) { pair[1] });  // else nil
				~items = ~items.add((item: new, wildcard: new, src: \stutt, time: pair[0]));
			};
			~items.sort { |a, b| a.time < b.time }
		};
		~makeStream = 0;  // ~seqStream is not used here
		~snippet.("stutt", "\\stutt(##src, match, numToAdd, quant, prob, insItem \"-\", wildcards##)");
	}.import((clGenPitch: #[artic])) => PR(\clGenStutt);

	PR(\clGenStutt).clone {
		~poolArgs = { [~args[2]] };
		~defaultNames = #[baseItems, maxDur, insPool, wildcards];
		~defaults = [nil, 0.5, [(item: $x, time: 0)], nil];
		~validation = [nil, \isNumProxy, nil, nil];
		~process = {
			var lastEvent, localItems, new, maxDur;
			~items = ~getUpstreamItems.();
			localItems = ~items.copy;
			(localItems.collect(_.time) ++ [~time + ~dur]).do { |time, i|
				maxDur = ~maxDur.next;
				if(lastEvent.notNil and: {
					lastEvent[\initialRest] != true
					and: { ~wildcardCheck.(lastEvent[\wildcard])
						and: { time - lastEvent.time > maxDur }
					}
				}) {
					new = ~insStream.value;
					~insertItem.((item: new, wildcard: new, src: \choke, time: lastEvent.time + maxDur));
				};
				lastEvent = localItems[i];
			};
			~items
		};
		~protoID = \clGenChoke;
		~snippet.("choke", "\\choke(##src, maxDur, pool, wildcards##)");
	}.import((clGenIns: #[insertItem])) => PR(\clGenChoke);

	PR(\clGenIns).clone {
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1];
			~quant = ~args[2] ?? { 0.25 };
			~weights = ~args[3..];
			if(~weights.last.isSymbol) {
				~initRest = true;
				~weights = ~weights.drop(-1);
			} {
				~initRest = false;
			};
			~weights = Ptuple(~weights, inf).asStream.collect(_.normalizeSum);
			~poolStream = Prand(~pool.asArray, inf).collect(_.item).asStream;
			currentEnvironment
		};
		~modifySource = {
			var avail, didInsertFirst = false,
			quant = ~quant.next, weights = ~weights.next;
			~availPat = Pseries(
				~time,
				Pconst(~dur, Pwrand(Array.series(weights.size, quant, quant), weights, inf)),
				inf
			);
			avail = ~getAvail.(~items, quant);
			avail.size.do { |i|
				var newTime = avail[i],
				new;
				// should be guaranteed but, protect against mistakes
				if(newTime.notNil) {
					if(newTime == ~time and: { ~initRest and: { 0.5.coin } }) {
						// this will always touch only the first item of this generator
						if(~items[0].isNil) {
							~insertItem.((item: \rest, time: newTime));
						};  // else leave the existing item alone
					} {
						new = ~poolStream.next;
						if(new.notNil) {
							~insertItem.((item: new, wildcard: new, src: \delta, time: newTime));
							if(newTime == ~time) { didInsertFirst = true };
						};
					};
				};
			};
			~items
		};
		~getAvail = { |items, quant|
			// for now, just blow away existing items
			// future: recursive testing to avoid existing items
			~availPat.asStream.all;
		};
		~insertItem = { |new|
			var insertI = ~items.detectIndex { |item| new[\time] <= item[\time] };
			if(insertI.isNil) {
				~items = ~items.add(new);
			} {
				if(new[\time] < ~items[insertI][\time]) {
					~items = ~items.insert(insertI, new);
				} {
					~items[insertI] = new;
				};
			};
			currentEnvironment
		};
		~snippet.("delta", "\\delta(##src, pool, quant, weights...##)");
	} => PR(\clGenDelta);

	// non-metric generators

	// Pool
	// Max duration
	// Factor (min duration = max / factor)
	// Min number of notes
	// Max number of notes
	// Distribution type
	// Parameters (except range, calculated as above)
	PR(\clGenIns).clone {
		~distributions = IdentityDictionary[
			\lin -> { |lo, hi| Pwhite(lo, hi, inf) },
			\exp -> { |lo, hi| Pexprand(lo, hi, inf) },
			// "exponential beta": beta distribution weighted toward low values (for rhythm)
			\expb -> { |lo, hi, spread(0.2), n(64)|
				// base**i is now basically a 1/x function with the right endpoints
				var base = (lo / hi) ** (n - 1).reciprocal,
				table = Array.fill(n, { |i|
					var x = (i+1) / (n+1);
					(((1 - x) ** (spread - 1)) * (x ** (spread - 1))) * (base ** i)
				}).asRandomTable;
				Pfunc { table.tableRand.linlin(0.0, 1.0, lo, hi) }
			},
			// hard to balance toward low values for rhythm
			\beta -> { |lo, hi, a(1), b(1)| Pbeta(lo, hi, a, b, inf) },
			// \cauchy -> Pcauchy,  // mean-spread, can't support
			// \gauss -> Pgauss,
			\hp -> { |lo, hi| Phprand(lo, hi, inf) },
			\lp -> { |lo, hi| Plprand(lo, hi, inf) }
		];
		~numToAdd = 1e10;  // hack: ~modifySource does min(~numToAdd, avail.size)
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1].asArray;
			~checkEmpty.(~pool, "pool");
			~maxDelta = ~args[2] ?? { 1 };
			~factor = ~args[3] ?? { 2 };
			~minNotes = ~args[4] ?? { 3 };
			~maxNotes = ~args[5] ?? { 1e10 };
			~distribution = ~args[6] ?? { \exp };
			~parameters = ~args[7..];

			#[maxDelta, factor, minNotes, maxNotes].do { |key|
				var value = key.envirGet;
				if(~isNumProxy.(value).not) {
					Error("%: % '%' is not a number".format(~protoID, key, value)).throw;
				};
			};

			~pclass = ~distribution.asClass;
			if(~pclass.isNil or: { ~pclass.isPattern.not }) {
				~pclass = ~distributions[~distribution];
				if(~pclass.isNil) {
					Error("%: Invalid distribution '%'".format(~protoID, ~distribution)).throw;
				};
			};

			~poolStream = Prand(~pool.asArray, inf).collect(_.item).asStream;
			currentEnvironment
		};
		~getAvail = { |items, quant, tolerance = 0.001|
			var avail = List.new,
			times = items.collect({ |item| item[\time] - ~time }) ++ [~dur],
			now = 0, maxDelta, minNotes, maxNotes, i = 0, j = 0;

			if(~pattern.isNil or: { ~doReset > 0 }) {
				maxDelta = ~maxDelta.next;  // won't update, this is a mistake
				~pattern = ~pclass.value(maxDelta / ~factor.next, maxDelta,
					*(~parameters.collect(_.next))
				);
				~stream = ~pattern.asStream;
			};

			minNotes = ~minNotes.next;
			maxNotes = ~maxNotes.next;
			while {
				avail.size < maxNotes and: {
					now < (~dur - tolerance) or: { avail.size < minNotes }
				}
			} {
				avail = avail.add(now);
				now = now + ~stream.next;
			};

			// scale to generator duration;
			// 'now' should be the sum of the deltas
			avail = avail * (~dur / now);

			// delete time points that are already used and return
			~checkDups.(avail, times, tolerance) + ~time
		};

		~checkDups = { |avail, times, tolerance|
			var i = 0, j = 0;
			while { i < avail.size } {
				if(avail[i] absdif: times[j] < tolerance) {
					avail.removeAt(i);  // and don't advance 'i'
				} {
					while { j < times.size and: { avail[i] - times[j] >= tolerance } } {
						j = j + 1;
					};
					if(j >= times.size) {
						i = avail.size;  // force stop
					} {
						if(times[j] - avail[i] >= tolerance ) {
							i = i + 1;
						};
					};
				};
			};
			avail
		};

		~snippet.("rDelta", "\\rDelta(##src, pool, maxDelta, factor, minNotes, maxNotes, distribution, parameters##)");
		~protoID = \clGenRDelta;
	} => PR(\clGenRDelta);

	// pool, avg delta, factor(>1 = accel to midpoint), midpoint, curve
	PR(\clGenRDelta).clone {
		~numToAdd = 1e10;  // hack: ~modifySource does min(~numToAdd, avail.size)
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1].asArray;
			~checkEmpty.(~pool, "pool");
			~initDelta = ~args[2] ?? { 1 };
			~midDelta = ~args[3] ?? { 0.1 };
			~midpoint = ~args[4] ?? { 1 };
			~curve = ~args[5] ?? { 0 };

			#[initDelta, midDelta, midpoint, curve].do { |key|
				var value = key.envirGet;
				if(~isNumProxy.(value).not) {
					Error("%: % '%' is not a number".format(~protoID, key, value)).throw;
				};
			};

			~poolStream = Prand(~pool.asArray, inf).collect(_.item).asStream;
			currentEnvironment
		};

		~getAvail = { |items, quant, tolerance = 0.001|
			var times = items.collect({ |item| item[\time] - ~time }) ++ [~dur],
			start = ~initDelta.next, midLevel = ~midDelta.next,
			mid = ~midpoint.next, curve = ~curve.next,
			env = Env([start, midLevel, start], [mid, 1.0 - mid], [curve, curve.neg]),
			avail = List.new, now = 0;
			while { now < (~dur - tolerance) } {
				avail.add(now);
				now = now + env.at(now / ~dur);
			};
			~checkDups.(avail * (~dur / now), times, tolerance) + ~time
		};

		~snippet.("ramp", "\\ramp(##src, pool, initDelta, midDelta, midpoint, curve##)");
		~protoID = \clGenRamp;
	} => PR(\clGenRamp);

	PR(\clGenIns).clone {
		~poolArgs = nil;
		~superprep = ~prep;
		~prep = {
			~superprep.();
			// pool comes from superclass, but it isn't a "pool"
			// it's a list of matching items
			~matchItems = ~pool.collect(_.item);
			~matchStream = Pn(Pshuf(~matchItems, 1), inf).asStream;
			currentEnvironment
		};
		~modifySource = {
			var quant = ~quant.next,
			possShifts = (quant ?? { 0.25 }) * [-1, 1],
			numToShift = (~args[2] ?? { 1 }).next;
			// check number of source items present in the match pool (but only if match was explicitly given)
			// it's possible to ask for 4 shifts but the source has only two shiftable items
			// reduce to 2 in that case
			if(~matchItems.notNil) {
				numToShift = min(numToShift, ~items.count { |item| ~matchItems.includes(item.item) });
			};
			~endTime = ~time + ~dur;  // traverseShifts needs this value, many times
			~traverseShifts.(numToShift, ~items, ~matchItems.copy, possShifts);
			~items
		};

		// This must be recursive. Consider: \shift("..|||", "..", 2, 0.25).
		// If it first moves 0.5 to 0.25, there is nowhere for 0 to go.
		// The correct solution is to backtrack and move 0.5 to 0.75.
		// Backtracking = recursion.
		~traverseShifts = { |numToShift, items, match, shiftArray, shiftedIndices(List.new)|
			var shiftItem, shiftIndex, shift, newTime, oldItem, tries;

			if(numToShift > 0) {
				shiftIndex = nil;
				tries = ~matchItems.size * 2;
				while { shiftIndex.isNil and: { tries > 0 } } {
					shiftItem = ~matchStream.next;
					shiftIndex = ~items.collectIndices { |item, i|
						item.item == shiftItem and: { shiftedIndices.includes(i).not }
					};
					shiftIndex = shiftIndex.choose;  // nil if array is empty
					tries = tries - 1;
				};
				shiftArray = shiftArray.rotate(2.rand);
				shift = block { |break|
					shiftArray.do { |sh|
						newTime = ~items[shiftIndex][\time] + sh;
						if(newTime.inclusivelyBetween(~time, ~endTime - 0.001) and: {
							newTime != ~items[shiftIndex + sh.sign].tryPerform(\at, \time)
						}) {
							oldItem = ~items[shiftIndex];
							~items[shiftIndex] = ~items[shiftIndex].copy.put(\time, newTime);
							match.remove(shiftItem);
							shiftedIndices.add(shiftIndex);
							if(numToShift > 1) {
								if(~traverseShifts.(numToShift - 1, ~items, match, shiftArray, shiftedIndices)) {
									break.(newTime);
								}
							} {
								break.(newTime);
							};
							// if we didn't break for success, undo for next iteration
							match = match.add(shiftItem);
							shiftedIndices.remove(shiftIndex);
							~items[shiftIndex] = oldItem;
						};
					};
					nil
				};
				shift.notNil  // true if successful
			} {
				true  // calling this with numToShift == 0 --> do nothing, success
			}
		};
		~snippet.("shift", "\\shift(##src, match, numToShift, quant##)");
	} => PR(\clGenShift);

	PR(\clGen).clone {
		~poolArgs = nil;
		~prep = {
			~baseItems = ~args[0];
			~quant = ~args[1];
			if(~isNumProxy.(~quant).not) {
				Error("\rot() quant must be a number").throw;
			};
			~match = ~args[2].collect(_.item);  // also handles nil; not really a pool
			currentEnvironment
		};
		~process = {
			var quant = ~quant.next,
			firstI, endTime = ~time + ~dur,
			newItems,
			testFunc = if(~match.size > 0) {
				{ |item|
					~match.includesEqual(item[\item]) or: {
						~match.includesEqual(item[\wildcard])
					}
				}
			} {
				true  // if no pool, rotate everything
			},
			rotFunc = { |item|
				var newTime;
				if(testFunc.value(item) and: {
					newTime = (item[\time] + quant).wrap(~time, endTime);
					// note that do vs reverseDo below means we need to check only items already done
					newItems.every { |x| x[\time] != newTime }
				}) {
					newItems.add(item.copy.put(\time, newTime));
				} {
					newItems.add(item)
				};
			};
			~items = ~getUpstreamItems.();
			newItems = Array(~items.size);
			if(quant > 0) {
				~items.reverseDo(rotFunc);
			} {
				~items.do(rotFunc);
			};
			~items = newItems.sort { |a, b| a[\time] < b[\time] };
		};
		~snippet.("rot", "\\rot(##src, quant, match##)");
	} => PR(\clGenRot);

	PR(\clGen).clone {
		~poolArgs = { [~args[1]] };
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1] ?? { "_" };
			~checkArticPool.(~pool);
			~wildcards = ~args[2];
			~override = ~args[3] ?? { 1 };
			if(~wildcards.notNil) {
				~wildcards = ~wildcards.collect(_.item);
			};
			~poolStream = Prand(~pool.asArray, inf).asStream;
			// Possible future feature
			// ~durSplit = ~args[1];
			// if(~durSplit.isNumber.not) {
			// 	Error("\artic() durSplit must be a number").throw;
			// };
			// ~shortPool = ~args[2];
			// ~longPool = ~args[3] ?? { ~args[2] };
			currentEnvironment
		};
		~process = {
			~items = ~getUpstreamItems.();
			~items.collect(~apply);
		};
		// for chain-in-pool
		~apply = { |item|
			var artic, new, newLen;
			if(item.item.isKindOf(SequenceNote) and: {
				~wildcards.isNil or: { ~wildcards.includes(item.wildcard) }
			}) {
				artic = ~poolStream.next;
				new = item.item.copy;
				new = ~applyArtic.(artic, new);
				item.copy.put(\item, new);
			} {
				item
			};
		};
		~snippet.("artic", "\\artic(##src, articPool, wildcards##)");
	}.import((clGenPitch: #[artic])) => PR(\clGenArtic);

	PR(\clGen).clone {
		~poolArgs = { [~args[1]] };
		~prep = {
			~srcParm = ~args[0];
			~pool = ~args[1].asArray;
			~srcBP = ~args[2] ?? { ~bpKey };
			if(BP.exists(~srcBP)) {
				// will throw DoesNotUnderstand if there's no defaultParm
				if(~srcParm.isNil) { ~srcParm = BP(~srcBP).defaultParm };
			} {
				Error("\\unis: BP(%) doesn't exist".format(~srcBP.asCompileString)).throw;
			};
			~poolStream = if(~pool.size > 0) { Prand(~pool, inf).asStream } { nil };
			currentEnvironment
		};
		~process = {
			var srcBP, i, endTime = ~time + ~dur, new;
			~items = ~getUpstreamItems.();
			if(BP.exists(~srcBP)) {
				srcBP = BP(~srcBP);
				// lookup must be here: the parm index might be different in different phrases
				i = ~getParmIndex.(srcBP, srcBP.lastPhrase, ~srcParm);
				if(i.notNil and: { srcBP.phrases[srcBP.lastPhrase].pairs[i].source.isKindOf(Pcollect) }) {
					srcBP.phrases[srcBP.lastPhrase].pairs[i].source.pattern.list[0].cachedItems
					.select { |item|
						item[\initialRest] != true and: {
							item[\time] >= ~time and: { item[\time] < endTime }
						}
					}
					.do { |item|
						// if no item already at the source's time slot
						if(~items.detect { |x| x.time == item.time }.isNil) {
							new = ~poolStream.next ?? { item };
							new = new.item;
							~insertItem.((item: new, wildcard: new, src: \unis, time: item.time));
						};
					};
				};
			};
			~items
		};
		~getParmIndex = { |srcBP, phrase, parm|
			var i;
			if(~srcParm == srcBP.defaultParm) {
				1
			} {
				i = srcBP.phrases[phrase].pairs.indexOf(parm);
				if(i.notNil) { i + 1 } { nil }
			};
		};
		~protoID = \clGenUnis;
		~snippet.("unis", "\\unis(##srcParm, pool, srcBP##)");
	}.import((clGenIns: #[insertItem])) => PR(\clGenUnis);

	// PR(\clGen).clone {
	//
	// } => PR(\clGenLeadTo);

	// \fork(items, "\gen()| \gen()|...")
	PR(\clGen).clone {
		~prep = {
			~baseItems = ~args[0];
			~generators = ~args[1];
			currentEnvironment
		};
		~process = {
			var lastGenTime = 0;
			~items = ~getUpstreamItems.();
			~newItems = Array.new;
			~generators.do { |gen, i|
				var start, end, partition;
				if(gen.tryPerform(\isClGen) ?? { false }) {
					start = gen.time;
					end = start + gen.dur;
					// any stray items?
					if(start - lastGenTime > 0.0001) {
						~newItems = ~newItems.add(~items.select { |item|
							item[\time] >= lastGenTime and: { item[\time] < start }
						});
					};
					partition = ~items.select { |item|
						item[\time] >= start and: { item[\time] < end }
					};
					gen.baseItems = partition;
					~newItems = ~newItems.add(gen.process);
					lastGenTime = gen.time + gen.dur;
				};
			};
			~newItems = ~newItems.add(~items.select { |item|
				item[\time] >= lastGenTime
			});
			~items = ~newItems.flatten(1);
		};

		// pool validation: tricky because non-pool gens are allowed here
		// but each should be validated
		~checkPoolArgs = { |inPool = false|
			var checkOne = { |item|
				if(item.isKindOf(Proto)) {
					item.checkPoolArgs(false)  // not in pool
				};
			};
			if(inPool) {
				Error("\\fork may not be used in a pool").throw;
			};
			~baseItems.do(~checkOne);
			~generators.do(~checkOne);
		};

		~snippet.("fork", "\\fork(##src, genString##)");
	} => PR(\clGenFork);

	PR(\clGen).clone {
		~prep = {
			var stream, thing;
			~baseItems = ~args[0];
			~list = Array.new;
			~weights = Array.new;
			stream = Pseq(~args.drop(1), 1).asStream;
			thing = stream.next;
			while { thing.notNil } {
				if(thing.isSequenceableCollection) {
					~list = ~list.add(thing);
					thing = stream.next;
					if(~isNumProxy.(thing)) {
						~weights = ~weights.add(thing);
						thing = stream.next;  // always leave 'thing' at the next item
					} {
						~weights = ~weights.add(1);
					};
				} {
					"Invalid item in \\oneof(), skipping".warn;
				};
			};
			~weights = ~weights.normalizeSum;
			currentEnvironment
		};
		~process = {
			var str, end;
			~items = ~getUpstreamItems.();
			str = ~list.wchoose(~weights);
			// any generators inside, provide source items within gen's timespan and evaluate
			str = str.collect { |item|
				if(item.tryPerform(\isClGen) ?? { false }) {
					end = item.time + item.dur;
					item.baseItems = ~items.select { |it|
						it.time >= item.time and: { it.time < end }
					};
					item.process;
				} {
					item
				};
			};
			~items = str.flatten(1);
		};
		~snippet.("oneof", "\\oneof(##src, genStrings/probs##)");
	} => PR(\clGenOneof);

	// \gen1(...)::\gen2(...)::\gen3(...)
	// gen1 processed items -> gen2 processed items -> gen3
	PR(\clGen).clone {
		~canBePool = true;  // pitch procs can chain \artic or \xpose in a pool
		~prep = {
			currentEnvironment  // just use ~args
		};
		~process = {
			var items;
			// ~baseItems is nil if chain is the outermost generator
			// but if chain is inside a fork, the fork will provide baseItems
			if(~baseItems.notNil) { items = ~getUpstreamItems.() };
			~args.do { |gen|
				if(items.notNil) {
					// this is corrupting the initial state,
					// but this generator will only ever be processed here
					gen.baseItems = items;
				};
				items = gen.process;
			};
			~items = items;
		};

		~embedInStream = { |inval|
			~args[0].asStream.collect { |item|
				(1 .. ~args.size - 1).do { |i|
					item = ~args[i].apply(item);
				};
				item
			}.embedInStream(inval);
		};

		~poolable = #[clGenArtic, clGenXpose];
		~checkPoolArgs = { |inPool = false|
			if(inPool) {
				if(not(~args[0].tryPerform(\canBePool) ?? { false })) {
					Error("Chain first generator '%' may not be used in a pool".format(~args[0][\protoID])).throw;
				} {
					~args[0].checkPoolArgs(true);
				};
				(1 .. ~args.size - 1).do { |i|
					if(~poolable.includes(~args[i][\protoID]).not) {
						Error("Chain filter generator '%' may not be used in a pool".format(~args[i][\protoID])).throw;
					};
				};
			} {
				~args.do { |item|
					if(item.isKindOf(Proto)) {
						item.checkPoolArgs(false)  // not in pool, but subgens might have pools
					};
				};
			};
		};
	} => PR(\clGenChain);

	PR.allOfType(\clGenerator).do { |pr|
		pr[\protoID] = pr.collIndex;
	};
} { AbstractChuckArray.defaultSubType = saveType };
