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
		~resetFlag = false;

		// responsible for parsing any time-sequence strings
		// each generator may have different ones
		// generic one assumes only ~args[0]
		~prep = {
			// oh that is ugly...
			if(~protoID == \clGen) {
				~baseItems = ~args[0];
			};
			~localPrep.();
			~checkPoolArgs.();
			~checkChildrenReset.();
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
		~updateDeltas = { |items, key = \dur|
			var deltas;
			var prevItem, default;
			if(BP.exists(~bpKey) and: { ~parm != BP(~bpKey).defaultParm }) {
				if(items.isEmpty) {
					deltas = [~dur];
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
					items = [(item: prevItem, time: 0)];
				} {
					deltas = (items.collect(_[\time]) ++ ~dur).differentiate;
					if(items[0][\time] == 0) {
						deltas = deltas.drop(1);
					} {
						deltas[0] = items[0][\time];
						prevItem = ~repository[~parm];
						if(prevItem.notNil) { prevItem = prevItem.item };
						items = items.copy.insert(0, (item: prevItem, time: 0));
					};
				};
			} {
				if(items.isEmpty) {
					deltas = [Rest(~dur)];
					items = [(item: \rest, time: 0, initialRest: true)];
				} {
					deltas = (items.collect(_[\time]) ++ ~dur).differentiate;
					if(items[0][\time] == 0) {
						deltas = deltas.drop(1);
					} {
						deltas[0] = Rest(items[0][\time]);
						items = items.copy.insert(0, (item: \rest, time: 0, initialRest: true));
					};
				};
			};
			case
			{ BP.exists(~bpKey) and: { BP(~bpKey)[\swing].notNil } } {
				deltas = BP(~bpKey)[\swing].mapDeltaArray(deltas);
			}
			{ Library.at(\globalSwing).notNil } {
				deltas = Library.at(\globalSwing).mapDeltaArray(deltas);
			};
			items.collect { |item, i|
				var copy = item.copy.put(key, deltas[i]);
				if(copy[\dur].isNil) {
					copy.put(\dur, deltas[i])
				};
				copy
			};
		};
		~asPattern = { |bp|  // the BP() object -- asPattern should be called @ top level only
			var pat, rest, didProcess = false;
			// var startTime = Main.elapsedTime;
			if(~items.isNil) {
				~items = ~doProcess.(bp);
				~items = ~updateDeltas.(~items, \delta);
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
						[inEvent[~parm], ev[\delta]]
					} {
						[ev[\item], ev[\delta]]
					}
				};
			} {
				pat = pat.collect { |ev, inEvent|
					if(ev[\initialRest] == true) {
						inEvent.put(\initialRest, true);
					};
					[ev[\item], ev[\delta], ev[\dur]]
				};
			};
			if(~parm.notNil) {
				~repository[~parm] = ~items.last;
			};
			if(didProcess) { ~unprocess.() };
			// (Main.elapsedTime - startTime).debug("BP(%) pattern processing time".format(bp.collIndex));
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
		~getParmIndex = { |parm|
			var bp = BP(~bpKey);
			var i = bp.phrases[bp.lastPhrase].pairs.detectIndex { |value, i|
				i.even and: {
					value == parm or: {
						value.asArray.includes(parm)
					}
				}
			};
			if(i.isNil) {
				Error("BP(%): parm '%' is not found in pairs array (should never happen)"
					.format(~bpKey.asCompileString, parm)
				).throw;
			} {
				i + 1
			}
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
			if(artic.tryPerform(\isClGen) ?? { false }) {
				if(artic.tryPerform(\canBePool) ?? { false }) {
					artic.poolArgs.every { |item|  // poolArgs is array-of-args
						// each arg may be a Proto or an array
						item.asArray.every(~isValidArtic)
					};
				} {
					if(artic.size > 0) { artic.every(~isValidArtic) } { false }
				};
			} {
				~artic[artic.value].notNil or: {
					artic.tryPerform(\key) == \accent
				}
			};
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
		// it's impossible for the parser to isolate an articpool containing a gen
		// because the parser doesn't know which args are expected to be artic
		// so, reconvert after the fact -- eventually maybe lose articpool nodes
		// Accent+artic may render as two items with SequenceNotes... re-squish them too
		~convertPoolToArtic = { |pool|
			var in = pool.asArray;
			var out = Array(in.size);
			var str = CollStream(in);
			var item, ch, ch2, j;
			while {
				item = str.next;
				item.notNil
			} {
				case
				{ item.isKindOf(Char) or: { item.isKindOf(Association) } } {
					out.add(item);
				}
				{ item.tryPerform(\isClGen) ?? { false } } {
					out.add(item.convertArticPools)  // returns self
				}
				{ item.isKindOf(Dictionary) and: { item[\item].isKindOf(SequenceNote) } } {
					ch = item[\item].asFloat.asInteger.asAscii;
					case { ch == $> } {
						item = str.peek;
						case { item.isKindOf(Dictionary) and: { item[\item].isKindOf(SequenceNote) } } {
							ch2 = item[\item].asFloat.asInteger.asAscii;
							if("~_.".includes(ch2)) {
								out.add(\accent -> ch2);  // ">_" branch
								str.next;
							} {
								Error("Wrong character in artic pool").throw;
								// out.add(ch2);  // ">" branch... loop back and reprocess ch2
							};
						}
						{ item.tryPerform(\isClGen) ?? { false } } {
							out.add(item.convertArticPools);  // returns self
							str.next;
						}
						{ Error("Wrong character in artic pool").throw }
					}
					{ "~_.".includes(ch) } {
						out.add(ch);
					}
					{ Error("Wrong character in artic pool").throw }
				}
				{ Error("Wrong character in artic pool").throw }
			};
			if(pool.tryPerform(\isClGen) ?? { false }) {
				out = out[0];
			};
			out
		};
		~convertArticPools = {
			// I messed up the data structure here, didn't think I'd have to write back
			~poolArgs.value.do { |pool|
				var i = ~args.indexOf(pool);
				pool = ~convertPoolToArtic.(pool);
				~args.put(i, pool);
				~pool = pool;  // currently all pool-able generators store the source here
			};
			currentEnvironment
		};
		// quite confusing interface.
		// 'artic' may be a char (. _ ~) *or* an association (accent -> char or nil)
		// but also, stutt may need to clear the accent flag, so...
		// accent: if nil, no change; otherwise, should be boolean
		~applyArtic = { |artic, note, accent|
			var didCopy = false, item;
			// there is a really spectacularly stupid feature of Association
			// where it creates an array when embedded into a stream
			// so I have to check for that and un-array it
			if(artic.isArray) { artic = artic[0] };
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
		~checkChildrenReset = {
			var reset = false;
			~poolArgs.().do { |obj, i|
				reset = reset or: { ~checkChildReset.(obj) };
			};
			~resetFlag = ~resetFlag or: reset;
		};
		~checkChildReset = { |child|
			case
			{ child.tryPerform(\isClGen) ?? { false } } {
				child.resetFlag
			}
			{ child.isKindOf(SequenceableCollection) } {
				child.any { |item|
					item.tryPerform(\isClGen) ?? { false }
					and: { item.resetFlag == true }
				};
			}
			{ false };  // else, normal item cannot specify reset
		};
		~checkEmpty = { |obj, str|
			if(obj.size == 0) {
				Error("Values expected for %, but was empty".format(str)).throw;
			};
		};
	}, parentKeys: #[canEmbed, protoID, isClGen, patternStringKeys, patternStringFuncs])
	.import((clPatternSet: #[decodePitch])) => PR(\clGen);

	// PR(\clGen).copy => PR(\clGenTest);

	PR(\clGen).clone {
		~localPrep = {
			~baseItems = ~args[0];
			currentEnvironment
		};
		~process = {
			~items = ~getUpstreamItems.();
		};
	} => PR(\clGenSrc);

	// "\seq("rhythm", "itemseq", "replaceItems", >0 to reset)
	PR(\clGen).clone {
		~poolArgs = { [~args[0]] };
		~localPrep = {
			~pool = ~args[0].asArray;
			~checkEmpty.(~pool, "pool");
			if(~args[1].isSequenceableCollection) {
				~replaceItems = ~args[1].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			// if(~args[2].isNumber) {
			// 	~doReset = ~args[2] > 0;
			// } {
			// 	~doReset = false;
			// };
			~doReset = ~args[2] ?? { 0 };
			~offset = ~args[3] ?? { 0 };
			#[doReset, offset].do { |key|
				if(~isNumProxy.(key.envirGet).not) {
					Error("%: % should be a number proxy".format(~protoID, key)).throw;
				};
			};
			currentEnvironment
		};

		~process = {
			if(~doReset.next > 0 or: { ~seqStream.isNil }) {
				~makeStream.();
			};
			~doResetFlag.();  // really just for \pitch() at this time
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
					~items[i] = item.copy
					.put(\wildcard, item[\wildcard] ?? { item[\item] })
					.put(\item, ~seqStream.next(item));
				};
			};
		};

		// makeStream should always be infinite
		~makeStream = {
			~seqPattern = Pseq(~pool.asArray, inf).collect(_.item);
			~seqStream = ~seqPattern.asStream;
		};
		// embedInStream is for generators embedded in pools, should use repeats
		~embedInStream = { |inval|
			Pseq(~pool, { ~repeats.next }, { ~offset.next }).embedInStream(inval)
		};
		~canBePool = true;
		~helpString = "\\seq(\"items\")";
		~snippet.("seq", "\\seq(##items, wildcards, reset##)");
	} => PR(\clGenSeq);

	PR(\clGenSeq).clone {
		~superPrep = ~localPrep;
		~localPrep = {
			~superPrep.();
			// ser common case is "play 3, 4, 5 .. n notes of the sequence"
			// filling in upper bound must be lazy
			if(~repeats.respondsTo(\hi)) {
				if(~repeats.hi == \upperBound) {
					~repeats.hi = max(1, ~repeats.lo).roundUp(~pool.size);
				};
			};
			currentEnvironment
		};
		~embedInStream = { |inval|
			Pser(~pool, { ~repeats.next }, { ~offset.next }).embedInStream(inval)
		};
		// makeStream should always be infinite
		~makeStream = {
			~seqStream = Pn(Pser(~pool.asArray, { ~repeats.next }, { ~offset.next }), inf).collect(_.item).asStream;
		};
		~snippet.("ser", "\\ser(##items, wildcards, reset##)");
	} => PR(\clGenSer);

	PR(\clGenSeq).clone {
		// seq ~prep defines a numeric ~offset
		// let's reuse that for the number of repeats
		~makeStream = {
			~seqStream = Pdup(
				Pfunc { max(1, ~offset.next) },
				Prand(~pool.asArray, inf)
			).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			Pdup(Pfunc { max(1, ~offset.next) }, Prand(~pool)).embedInStream(inval)
		};
		~snippet.("repeat1", "\\repeat1(##items, wildcards, reset, repeats##)");
	} => PR(\clGenRepeat1);

	PR(\clGenSeq).clone {
		// ~asPattern = { Prand(~args[0], inf) };
		~makeStream = {
			~seqStream = Prand(~pool.asArray, inf).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			Prand(~pool, { ~repeats.next }).embedInStream(inval)
		};
		~helpString = "\\rand(\"timing\", \"items\")";
		~snippet.("rand", "\\rand(##items, wildcards##)");
	} => PR(\clGenRand);

	PR(\clGenSeq).clone {
		~localPrep = {
			~pool = ~args[0].asArray;
			~checkEmpty.(~pool, "pool");
			~numToPlay = ~args[1] ?? { inf };
			~numToSkip = ~args[2] ?? { 0 };
			~doReset = ~args[4] ?? { 0 };
			#[numToPlay, numToSkip, doReset].do { |key|
				if(~isNumProxy.(key.envirGet).not) {
					Error("BP(%): % should be a number"
						.format(~bpKey.asCompileString, key)
					).throw
				};
			};
			if(~numToSkip.value == inf) {
				Error("BP(%): numToSkip must not be inf").throw;
			};
			if(~args[3].isSequenceableCollection) {
				~replaceItems = ~args[3].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			currentEnvironment
		};
		~makeStream = {
			~seqStream = ~localPattern.value(inf).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			~localPattern.value(~repeats).embedInStream(inval)
		};
		~localPattern = { |repeats|  // "makeStream" needs to override ~repeats
			Prout { |inval|
				var stream = Pseq(~pool, inf).asStream;
				var next;
				block { |break|
					repeats.next.do {
						// this may be inf because of yielding
						~numToPlay.next.do {
							next = stream.next(inval);
							if(next.isNil) { break.() };
							inval = next.embedInStream(inval);
						};
						// this must not be inf!
						~numToSkip.next.do {
							// stop upon nil, else just throw the value away
							if(stream.next(inval).isNil) { break.() };
						};
					};
				};
				inval
			};
		};
		~snippet.("skip", "\\skip(##items, numToPlay, numToSkip, wildcards, reset##)");
	} => PR(\clGenSkip);

	PR(\clGenSkip).clone {
		~repeats = inf;
		~localPrep = {
			~weights = ~args[0].asArray;
			~checkEmpty.(~weights, "weights");
			if(~args[1].isSequenceableCollection) {
				~replaceItems = ~args[1].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			~doReset = ~args[2] ?? { 0 };
			if(~isNumProxy.(~doReset).not) {
				Error("BP(%): voss doReset must be a number"
					.format(~bpKey.asCompileString)
				).throw;
			};
			currentEnvironment
		};
		~localPattern = { |repeats|
			~numericPattern.(repeats).collect { |next|
				(item: SequenceNote(next, nil, 0.9))
			}
		};
		~numericPattern = { |repeats(inf)|
			var prev;
			var tuple = Ptuple(
				~weights.collect { |item, i|
					item = max((i == 0).binaryValue, item.item.asFloat.asInteger.abs);
					Pdup((1 << i), Pxrand((item.neg .. item), inf))
				},
				inf
			);
			Pfin(repeats.next,
				tuple.collect(_.sum)
				// note that this cannot infloop because
				// all generators at minimum produce -1, 0, 1 in some order
				// and Pxrand cannot repeat values
				// so the i == 0 generator must produce an unequal value
				.reject { |next| next == prev }
				.collect { |next|
					prev = next
				}
			)
		};
		~snippet.("voss", "\\voss(##weights, wildcards##)");
	} => PR(\clGenVoss);

	PR(\clGenVoss).clone {
		~localPattern = { |repeats|
			Pdiff(~numericPattern.(repeats))
			.collect { |next|
				(item: SequenceNote(next, nil, 0.9))
			}
		};
		~snippet.("vossdiff", "\\vossdiff(##weights, wildcards##)");
	} => PR(\clGenVossdiff);

	PR(\clGenRand).clone {
		~canBePool = false;
		~modifySource = {
			~items.do { |item, i|
				if(~replaceItems.includesEqual(item[\wildcard])) {
					// some replacers might need access to the state
					~currentItem = item;
					~currentIndex = i;
					// copy is critical here!
					// 18-1014: wildcard-replacers update the wildcard
					~items[i] = item.copy
					.put(\wildcard, item[\wildcard] ?? { item[\item] })
					.put(\item, ~seqStream.next(item));
				};
			};
		};
		~snippet.("replace", "\\replace(##items, wildcards##)");
	} => PR(\clGenReplace);

	PR(\clGenSeq).clone {
		// I think not necessary
		~poolArgs = { ~args[[0, 2]] };
		~superPrep = ~localPrep;
		~localPrep = {
			~superPrep.value;
			~restItem = ~args[2] ?? {
				if(~isPitch ?? { false }) {
					[(item: SequenceNote(Rest(120), ~dur, ~dur, 0.5))]
				} {
					[(item: $x)]
				};
			};
			~restItemStream = Prand(~restItem, inf).asStream;  // not sure
			~setTrueRest.(3);
			~allWildcards = true;
			(1..2).do { |i|
				~allWildcards = ~allWildcards and: {
					~args[i].every { |item|
						"*@!".includes(item[\item])
					}
				}
			};
		};
		~setTrueRest = { |index|
			// use ~args if given; use parmMap if exists; otherwise assume true
			~trueRest = ~args[index] ?? {
				BP(~bpKey).parmMap[~parm][\trueRest] ?? { 1 }
			};
		};
		~canBePool = false;
		~modifySource = {
			~items.do { |item, i|
				var next, restItem;
				if(~replaceItems.includesEqual(item[\wildcard])) {
					// some replacers might need access to the state
					~currentItem = item;
					~currentIndex = i;
					next = ~seqStream.next(item);
					if(~isRestChar.(next)) {
						restItem = ~restItemStream.next(item)[\item];
						if(~trueRest.next != 0) {
							if(restItem.isRest.not) {
								restItem = Rest(restItem);
							};
						} {
							restItem = restItem.dereferenceOperand;
						};
						// copy is critical here!
						// 18-1014: wildcard-replacers update the wildcard
						~items[i] = item.copy
						.put(\wildcard,
							if(~allWildcards) {
								restItem
							} {
								item[\wildcard] ?? { item[\item] }
							}
						)
						.put(\item, restItem);
					};
				};
			};
		};
		~isRestChar = { |item|
			if(item.isKindOf(Dictionary)) {
				item = item[\item];
			};
			// comment may be totally wrong for = vs -
			// tricky: for isPitch parameters,
			// "." compiles to an articulation pool,
			// which is totally incompatible with this usage

			// btw: why did I say item == $. here? Everywhere else, '-' is the rest
			"-.".includes(item) or: { item.tryPerform(\asFloat) == 45 }
		};
		~snippet.("rest", "\\rest(##switch, wildcards, restItem, trueRest##)");
	} => PR(\clGenRest);

	PR(\clGenRest).clone {
		~wildcardCheck = [2, 3];
		~localPrep = {
			// superPrep needs: pool, replaceItems, doReset, offset
			// this is faking an object as if you had written
			// \rest(\repeat1(\seq("=-", , , {Ppatlace(...)})))
			~numNotes = ~args[0] ?? { ClNumProxy(1) };
			~numRests = ~args[1] ?? { ClNumProxy(1) };
			~setTrueRest.(4);
			~doReset = ~args[5] ?? { 0 };
			#[numNotes, numRests, doReset].do { |key|
				if(~isNumProxy.(key.envirGet).not) {
					Error("%: % should be a number proxy".format(~protoID, key)).throw;
				};
			};
			if(~args[2].isSequenceableCollection) {
				~replaceItems = ~args[2].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			~pool = Pdup(
				// because ClNumProxy can stream
				Ppatlace([~numNotes, ~numRests], inf),
				Pseq("=-".collectAs({ |char, i|
					var item;
					if(~isPitch ?? { false }) {
						item = SequenceNote(Rest(char.ascii));
					} {
						item = char;
					};
					(item: item, dur: ~dur * 0.5, time: i * (~dur * 0.5))
				}, Array), inf)
			);

			~restItem = ~args[3] ?? {
				if(~isPitch ?? { false }) {
					[(item: SequenceNote(Rest(120), ~dur, ~dur, 0.5))]
				} {
					[(item: $x)]
				};
			};
			~restItemStream = Prand(~restItem, inf).asStream;  // not sure

			~allWildcards = true;
			(2..3).do { |i|
				~allWildcards = ~allWildcards and: {
					~args[i].every { |item|
						"*@!".includes(item[\item])
					}
				}
			};
		};

		~snippet.("restn", "\\restn(##numEvents##, numRests, wildcards, restItem, trueRest, reset)");
	} => PR(\clGenRestn);

	PR(\clGenSeq).clone {
		~poolArgs = { ~args[0..1] };
		~resetFlag = true;  // special case, these two
		~repeats = inf;
		~localPrep = {
			~first = ~args[0].asArray;
			~checkEmpty.(~first, "first");
			~pool = ~args[1].asArray;
			~checkEmpty.(~pool, "pool");
			if(~args[2].isSequenceableCollection) {
				~replaceItems = ~args[2].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			~doReset = ~args[3] ?? { 0 };
			~offset = ~args[4] ?? { 0 };
			#[doReset, offset].do { |key|
				if(~isNumProxy.(key.envirGet).not) {
					Error("%: % should be a number proxy".format(~protoID, key)).throw;
				};
			};
			currentEnvironment
		};
		~makeStream = {
			~seqStream = Pseq([
				Pseq(~first, 1),
				Prand(~pool, inf)
			]).collect(_.item).asStream;
		};
		// embedInStream is for generators embedded in pools, should use repeats
		~embedInStream = { |inval|
			Pseq([
				Pseq(~first, 1),
				Prand(~pool, { ~repeats.next }, { ~offset.next })
			]).embedInStream(inval)
		};
		~helpString = "\\first(\"first\", \"items\")";
		~snippet.("first", "\\first(##first, items, wildcards, reset##)");
	} => PR(\clGenFirst);

	PR(\clGen).clone {
		~poolArgs = [];
		~localPrep = {
			if(~args[0].isSequenceableCollection) {
				~replaceItems = ~args[0].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			~prob = ~args[1] ?? { 0.0 };
			~negate = ~args[2] ?? { 0 };
			if(~isNumProxy.(~prob).not) {
				Error("%: prob should be a number proxy".format(~protoID)).throw;
			};
			if(~isNumProxy.(~negate).not) {
				Error("%: negate should be a number proxy".format(~protoID)).throw;
			};
			currentEnvironment
		};
		~process = {
			var didDelete = false;
			~items = ~getUpstreamItems.();
			~items.do { |item, i|
				var condition = (
					~replaceItems.includesEqual(item[\wildcard])
					or: { ~replaceItems.includesEqual(item[\item]) }
				) and: {
					// why '.not'? 0.75 should be 75% chance of keeping it
					// (higher number -> more event density)
					~prob.next.coin.not
				};
				if(~negate.next > 0) {
					condition = condition.not;
				};
				if(condition) {
					didDelete = true;
					~items[i] = nil;
				};
			};
			if(didDelete) {
				~items.reject(_.isNil)
			} {
				~items
			};
		};
		~snippet.("delete", "\\delete(##wildcards, keepProbability, negate##)");
	} => PR(\clGenDelete);

	PR(\clGenSeq).clone {
		// xrand is stateful so, if no repeats are given, it should go forever
		// \xrand*n() will override
		~repeats = inf;
		~makeStream = {
			~seqStream = Pxrand(~pool.asArray, inf).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			Pxrand(~pool.asArray, { ~repeats.next }).embedInStream(inval)
		};
		~helpString = "\\xrand(\"items\")";
		~snippet.("xrand", "\\xrand(##items, wildcards, reset##)");
	} => PR(\clGenXrand);

	PR(\clGenSeq).clone {
		~poolArgs = nil;
		~superprep = ~localPrep;
		~localPrep = {
			~superprep.();
			~pool = ~args[0];  // 'super' wrapped this in an array
			if(Pdefn.all[~pool].isNil) {
				Error("'%': gdefn can't find Pdefn key '%'".format(~bpKey, ~args[0])).throw;
			};
			currentEnvironment
		};
		// The whole point of a "reference" generator is:
		// Every instance should share the same stream.
		// Pdefn(key).asStream follows SC convention and makes
		// independent streams. So I need my own
		// stream repository: ~global is like a classvar here.
		~global = IdentityDictionary.new;
		~repository = { PR(\clGenGdefn).global };
		~makeStream = {
			~seqStream = Routine { |inval|
				~embedInStream.(inval)
			};
		};
		~embedInStream = { |inval|
			var next, tries, warned = false;
			var stream, repository = ~repository.();
			if(repository[~pool].isNil) {
				repository[~pool] = Pdefn(~pool).asStream;
			};
			stream = repository[~pool];
			loop {
				// Insurance against failure: auto-reset if the stream ends.
				// 10 nils in a row, stop (don't infloop)
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
				inval = next.yield;  // may yield nil if pdefn ends
			};
		};
		~helpString = "\\gdefn(, `key)";
		~snippet.("gdefn", "\\gdefn(`##key##, \"wildcards\", reset)");
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
		~snippet.("pdefn", "\\pdefn(`##key##, \"wildcards\", reset)");
	} => PR(\clGenPdefn);

	PR(\clGenSeq).clone {
		~superprep = ~localPrep;
		~localPrep = {
			~superprep.();
			~doReset = 0;  // reset is meaningless for wrand anyway
			currentEnvironment
		};
		~patternClass = Pwrand;
		~makeStream = {
			~seqStream = ~patternClass.new(~pool.asArray, ~weightPattern.(), inf).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			~patternClass.new(~pool, ~weightPattern.(), ~repeats).embedInStream(inval)
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
		~snippet.("wrand", "\\wrand(##items, wildcards, weights...##)");
	} => PR(\clGenWrand);

	PR(\clGenWrand).clone {
		~patternClass = Pwxrand;

		// see xrand comment: xrand is stateful; need to preserve the stream
		~repeats = inf;

		~helpString = "\\wxrand(\"items\", weight0, weight1...)";
		~snippet.("wxrand", "\\wxrand(##items, wildcards, weights...##)");
	} => PR(\clGenWxrand);

	PR(\clGenSeq).clone {
		~repeats = \unset;  // \shuf*num() will override this
		~makeStream = {
			~seqStream = Pn(Pshuf(~pool.asArray, 1), inf).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			// This is uglier than I'd like, but unavoidable.
			// It's mandatory not to check the pool size until
			// after the environment is fully initialized.
			if(~repeats == \unset) {
				~repeats = ~pool.size;
			};
			if(~shuffler.isNil) {
				~shuffler = Pnshuf(~pool, ~repeats, false);
			};
			// non-local is OK because this Pnshuf exists only in this generator
			~shuffler.embedInStream(inval)
		};
		~helpString = "\\shuf(\"items\")";
		~snippet.("shuf", "\\shuf(##items, wildcards, reset##)");
	} => PR(\clGenShuf);

	PR(\clGenShuf).clone {
		~makeStream = {
			~seqStream = Pn(Pshuf(~pool.asArray, 1), inf).collect(_.item).asStream;
		};
		~embedInStream = { |inval|
			Pfin(~repeats, Pn(Pshuf(~pool, 1), inf)).embedInStream(inval)
		};
		~helpString = "\\shufn(\"items\")";
		~snippet.("shufn", "\\shuf(##items, wildcards, reset##)");
	} => PR(\clGenShufn);

	PR(\clGenSeq).clone {
		~localPrep = {
			~pool = ~args[0].asArray;
			~checkEmpty.(~pool, "pool");
			case
			{ ~isNumProxy.(~args[1]) } {
				~stepStream = (Pfunc { ~args[1].next }.reject(_ == 0)).asStream;
			}
			// should be possible only for item strings
			// how to check if it's a pitch string?
			{ ~args[1].isSequenceableCollection } {
				~stepStream = Prand(~args[1], inf).collect { |item|
					item[\item].asFloat
				}.reject(_ == 0).asStream;
			}
			{ Error("\\walk(): Invalid step sequence").throw };
			if(~args[2].isSequenceableCollection) {
				~replaceItems = ~args[2].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			~offset = ~args[3] ?? { -1 };
			~wrap = ~args[4] ?? { 0 };
			#[wrap, offset].do { |key|
				if(~isNumProxy.(key.envirGet).not) {
					Error("%: % should be a number proxy".format(~protoID, key)).throw;
				};
			};
			~doReset = 0;

			currentEnvironment
		};
		// reimplement Pwalk because of a turnaround bug
		~walkPattern = {
			Prout { |inval|
				var pos = ~offset.next, newPos;
				var directionStream = Pseq([1, -1], inf).asStream;
				var direction = directionStream.next;
				var step, val, wrap;
				var count;
				if(pos < 0) {
					pos = ~lastPos ?? { 0 };
				};
				// default ~repeats == 1 should be inf
				// all explicit repeats are passed as ClNumProxy
				if(~repeats.tryPerform(\isNumProxy) == true) {
					count = ~repeats.next;
				} {
					count = inf;
				};
				while {
					count > 0 and: {
						step = ~stepStream.next(inval);
						step.notNil
					}
				} {
					newPos = pos + (step * direction);
					if(newPos < 0 or: { newPos >= ~pool.size }) {
						if(~wrap.next != 0) {
							newPos = newPos.wrap(0, ~pool.size - 1);
						} {
							newPos = newPos.fold(0, ~pool.size - 1);
							direction = directionStream.next(inval);
						};
					};
					inval = ~pool.wrapAt(pos).yield;
					pos = newPos;
					count = count - 1;
				};
				~lastPos = pos;
				inval
			}
		};
		~makeStream = {
			~seqStream = ~walkPattern.value.collect(_.item).asStream
		};
		~embedInStream = { |inval|
			~walkPattern.().embedInStream(inval)
		};

		~helpString = "\\walk(\"items\", steps, wildcards, offset, wrap)";
		~snippet.("walk", "\\walk(\"##items##\", steps, wildcards, offset, wrap)");
	} => PR(\clGenWalk);

	PR(\clGenSeq).clone({
		~poolArgs = { [~args[1]] };
		~canBePool = false;  // inheritance here is messy...
		~defaults = [/*[],*/ [(item: $*)], #[-2, -1, 1, 2].collect({ |int| (item: int) }), 0, 7, "_"];
		~argNames = #[/*baseItems,*/ replaceItems, intervalPool, fallbackMin, fallbackMax, articPool];
		~artic = ($~: inf /*1.01*/, $_: 0.9, $.: 0.4,
			$>: { |note| note.args = \accent; nil }  // don't change length
		);
		~doReset = 0;
		~localPrep = {
			~argNames.do { |key, i|
				key.envirPut(~args[i] ?? { ~defaults[i] });
			};
			~checkArticPool.(~articPool);  // throws error on failure
			~replaceItems = ~replaceItems.collect { |item| item.item };

			~intervalPool = ~intervalPool.asArray;  // don't die if gen isn't wrapped in ""
			~iReset = ~checkChildReset.(~intervalPool);
			~intervalStream = Prand(~intervalPool, inf).asStream.collect { |item| item.item.asFloat };
			~articPool = ~articPool.asArray.reject(_ == $");
			~aReset = ~checkChildReset.(~articPool);
			~articStream = Prand(~articPool, inf).asStream;
			if(not(~iReset or: { ~aReset })) {
				// here: neither subpattern specified reset
				// therefore both of them should follow the pdelta's resetFlag
				~iReset = ~aReset = ~resetFlag;
			};

			currentEnvironment
		};
		~doResetFlag = {
			if(~iReset) { ~intervalStream.reset };
			if(~aReset) { ~articStream.reset };
		};
		~makeStream = {
			~seqStream = Pfunc {
				var pitch, new;
				pitch = ~getPrev.(~currentIndex) + ~intervalStream.next;
				new = SequenceNote(pitch);
				new = ~applyArtic.(~articStream.next, new);
				new
			}.asStream;
		};
		~getPrev = { |index|
			var prev, bp, cache;
			prev = ~findLastPitch.(~items, index);
			if(prev.isNil) {
				bp = BP(~bpKey);
				if(~patIndex.isNil) {
					~patIndex = ~getParmIndex.(~parm);
				};

				// any of these could fail
				// 'try' would be easier
				// but 'try' collects a backtrace on error
				// and backtraces are DANGEROUS
				if(bp[\prevPhrase].notNil) {
					cache = bp.phrases[bp[\prevPhrase]];
				};
				if(cache.notNil and: { ~patIndex.isNumber }) {
					cache = cache.tryPerform(\pairs).tryPerform(\at, ~patIndex);
				};
				if(cache.notNil) { cache = cache.tryPerform(\source) };
				if(cache.notNil) { cache = cache.tryPerform(\pattern) };
				if(cache.notNil) { cache = cache.tryPerform(\list).tryPerform(\at, 0) };
				case
				{ cache.isKindOf(Proto) } {
					cache = cache[\cachedItems]
				}
				{ cache.notNil } {
					cache = cache.tryPerform(\at, \cachedItems)
				};
				if(cache.isNil) {
					rrand(~fallbackMin.next, ~fallbackMax.next);
				} {
					prev = ~findLastPitch.(cache, cache.size);
					if(prev.isNil) {
						rrand(~fallbackMin.next, ~fallbackMax.next);
					} {
						prev.item.asFloat
					}
				}
			} {
				prev.item.asFloat
			}
		};
		~findLastPitch = { |items, index|
			while {
				index = index - 1;
				index >= 0 and: {
					items[index].item.isKindOf(SequenceNote).not or: {
						// in here, items[i].item must be a SequenceNote
						items[index].item.freq.isRest
					}
				}
			};
			items[index]
		};
		~snippet.("pitch", "\\pitch(##replaceItems, intervalPool, fallbackMin, fallbackMax, articPool##)");
	}) => PR(\clGenPitch);

	PR(\clGenPitch).clone {
		~defaults = [/*[],*/ [(item: $*)], #[-2, -1, 1, 2].collect({ |int| (item: int) }), 0, 7, "_", 1];
		~argNames = #[/*baseItems,*/ replaceItems, intervalPool, fallbackMin, fallbackMax, articPool, connect];
		~doReset = 1;
		~makeStream = {
			~seqStream = Routine {
				// first init only is from previous item
				var pitch, prev, i, new;
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
					new = ~applyArtic.(~articStream.next, new);
					new.yield
				}
			}
		};
		~snippet.("pitch2", "\\pitch2(##replaceItems, intervalPool, fallbackMin, fallbackMax, articPool, connect##)");
	} => PR(\clGenPitch2);

	// transposition: choose interval from pool.
	// 1 = no transposition. 1' = octave up, 1, = octave down
	// can limit to wildcards
	PR(\clGen).clone {
		~poolArgs = { [~args[0]] };
		~defaults = [/*[],*/ [(item: SequenceNote(0, nil, 0.9))], nil];
		~argNames = #[/*baseItems,*/ intervalPool, wildcards];
		~localPrep = {
			~argNames.do { |key, i|
				key.envirPut(~args[i] ?? { ~defaults[i] });
			};
			~intervalStream = Prand(~intervalPool.asArray, inf).asStream.collect { |item| item.item.asFloat };
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
		~snippet.("xpose", "\\xpose(##intervalPool, wildcards##)");
	} => PR(\clGenXpose);

	PR(\clGenSeq).clone {
		~poolArgs = { [~args[0]] };
		~canBePool = false;
		~snippet.("prev", "\\prev(##pool, replaceItems, staccDur##)");
		~localPrep = {
			~pool = ~args[0] ?? { [] };
			// empty pool = ignored
			~poolStream = if(~pool.size > 0) { Prand(~pool, inf).asStream } { nil };
			if(~args[1].notNil) {
				~replaceItems = ~args[1].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			// rhythmic values <= ~staccDur will be made staccato, otherwise _
			// ~staccDur = 0 disables this feature
			// ignored for non-pitch parameters (because, no concept of articulation)
			if(~isNumProxy.(~args[2])) {
				~staccDur = ~args[2];
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
					new = ~poolStream.next;
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
		~poolArgs = { [~args[0]] };
		~localPrep = {
			~pool = ~args[0].asArray;
			~checkEmpty.(~pool, "pool");
			~numToAdd = ~args[1] ?? { 1 };
			if(~isNumProxy.(~numToAdd).not) {
				Error("numToAdd '%' is not a number".format(~numToAdd)).throw;
			};
			~quant = ~args[2] ?? { 0.25 };
			~poolStream = Prand(~pool, inf).asStream.collect(_.item);
			currentEnvironment
		};
		~modifySource = {
			var avail = ~getAvail.(~items, ~quant.next).scramble;
			avail = avail.keep(min(~numToAdd.next, avail.size).asInteger).sort;
			if(~resetFlag) { ~poolStream.reset };
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
		// 'new' should be an event with time:
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
		~snippet.("ins", "\\ins(##pool, numToAdd, quant##)");
	} => PR(\clGenIns);

	// \euclid(pool, subdivision, increment, initial)
	PR(\clGenIns).clone {
		~numToAdd = 0x7FFFFFFF;  // always add all avail
		~localPrep = {
			~pool = ~args[0].asArray;
			~checkEmpty.(~pool, "pool");
			~quant = ~args[1] ?? { 0.25 };
			~increment = ~args[2] ?? { 3 };
			~initial = ~args[3] ?? { 0 };
			#[quant, increment, initial].do { |key|
				if(~isNumProxy.(key.envirGet).not) {
					Error("clGenEuclid: '%' should be a number".format(key)).throw;
				};
			};
			~poolStream = Prand(~pool, inf).asStream.collect(_.item);
			currentEnvironment
		};
		~getAvail = { |items, quant, tolerance = 0.001|
			var q = quant.next,
			num = (~dur / q).asInteger,  // must round down
			times = items.collect({ |item| item[\time] - ~time }) ++ [~dur],
			counter = ~initial.next % num,
			increment = ~increment.next % num,
			avail = List.new,
			j = 0;
			num.do { |i|
				// times, to reduce impact of rounding error
				var now = q * i;

				if(now absdif: times[j] < tolerance) {
					if(items[j][\initialRest] == true and: { counter < increment }) {
						avail.add(now)
					};
					// 'time' is matched at times[j], so we must advance by at least 1
					j = j + 1;
				} {
					if(counter < increment) { avail.add(now) };
					// times[j] is somewhere in the future; may not need to advance yet
				};

				// advance j if needed
				now = q * (i+1);
				while { times[j].notNil and: { now - times[j] >= tolerance } } {
					j = j + 1;
				};

				// here's the Euclid part
				counter = counter + increment;
				// after this, counter should be less than increment,
				// signaling to add the next slot
				// this may be a slight deviation from the standard Euclidean rhythm technique
				if(counter >= num) { counter = counter - num };
			};
			avail + ~time
		};
		~snippet.("euclid", "\\euclid(##pool##, quant, increment, initial)");
	} => PR(\clGenEuclid);

	PR(\clGenIns).clone {
		~numToAdd = 0x7FFFFFFF;  // always add all avail
		~localPrep = {
			~pool = ~args[0].asArray;
			~checkEmpty.(~pool, "pool");
			~quant = ~args[1] ?? { 0.25 };
			~minimum = ~args[2] ?? { 1 };
			#[quant, minimum].do { |key|
				if(~isNumProxy.(key.envirGet).not) {
					Error("clGenGolomb: '%' should be a number".format(key)).throw;
				};
			};
			~poolStream = Prand(~pool, inf).asStream.collect(_.item);
			~lastMin = 1;
			currentEnvironment
		};
		~getAvail = { |items, quant, tolerance = 0.001|
			var num = (~dur / quant).round.asInteger;
			var min = ~minimum.next.round.asInteger;
			var deltas = ~getGolombDeltas.(num, min);
			var times = items.collect({ |item| item[\time] - ~time }) ++ [~dur];
			var timeIncr = 0, j = 0;
			var avail = List.new;

			if(deltas.isNil) {
				"Golomb deltas failed for min = %, retrying with %"
				.format(min, ~lastMin)
				.warn;
				deltas = ~getGolombDeltas.(num, ~lastMin);
				if(deltas.isNil) {
					deltas = ~getGolombDeltas.(num, 1);  // should always succeed
				};
			} {
				~lastMin = min;
			};
			deltas.scramble.do { |d|
				var now = quant * timeIncr;

				if(now absdif: times[j] < tolerance) {
					if(items[j][\initialRest] == true) {
						avail.add(now)
					};
					// 'time' is matched at times[j], so we must advance by at least 1
					j = j + 1;
				} {
					avail.add(now);
					// times[j] is somewhere in the future; may not need to advance yet
				};

				// advance j if needed
				timeIncr = timeIncr + d;
				now = quant * timeIncr;
				while { times[j].notNil and: { now - times[j] >= tolerance } } {
					j = j + 1;
				};
			};
			avail + ~time
		};

		~getGolombDeltas = { |num, min|
			var try = ~cache.at(num, min);
			if(try.notNil) {
				try
			} {
				try = ~calcGolombDeltas.(num, min);
				if(try.notNil) {
					~cache.put(num, min, try);
					try
				};
			};
		};
		~calcGolombDeltas = { |n, m = 1|  // m = minimum delta
			var root = ((sqrt(1 - (4 * (m - m.squared - (2*n)))) - 1) / 2);
			var rootInt = root.trunc.asInteger;
			var residual;
			var i, j;

			// now sum(1..root+1) >= n
			var deltas = (m .. rootInt+1);

			if(root > rootInt) {
				// we want sum up to rootInt+1 so
				// ((k+1)(k+2)) - (m(m-1)) = k.squared + 3k - 2 - m.squared + m
				residual = ((rootInt.squared + (3*rootInt) + 2 - m.squared + m) div: 2) - n;
				if(deltas.includes(residual)) {
					deltas.remove(residual);
				} {
					// not sure if this is optimal but you can get out of jail
					// by adding two of the choices until a+b-residual > rootInt+1
					// this requires at least 3 values in the array; fail otherwise
					if(deltas.size >= 3) {
						#i, j = block { |break|
							deltas.do { |a, i|
								(i+1 .. deltas.size - 1).do { |j|
									var b = deltas[j];
									if(a + b - residual > (rootInt+1)) {
										break.([i, j]);
									};
								};
							};
							#[nil, nil]
						};
						if(i.notNil) {
							deltas[i] = deltas[i] + deltas[j] - residual;
							deltas.removeAt(j);
						} {
							deltas = nil
						};
					} {
						deltas = nil;
					};
				};
			} {
				deltas = deltas.keep(rootInt - m + 1)
			};

			deltas
		};

		// classvar
		~cache = MultiLevelIdentityDictionary.new;

		~snippet.("golomb", "\\golomb(##pool##, quant, minimum)");
	}.moveFunctionsToParent(#[cache]) => PR(\clGenGolomb);

	// quant is minimum duration to divide
	// divisor is how many to split into
	// divisible allows e.g. 1.5 times units
	// (divisible == quant means durations quant, quant * 2, quant * 3 etc. can be divided)
	// (divisible == 2*quant means durations quant, quant * 3, quant * 5 etc. can be divided)
	// (divisible == 0 means *only* quant can be divided)
	// divisor may randomize once per bar (I might change this later but it's a bigger rewrite)
	PR(\clGenIns).clone {
		~superprep = ~localPrep;
		~localPrep = {
			~pool = ~args[0];
			~replaceItems = ~args[1];  // should allow nil to match everything
			~numToAdd = ~args[2] ?? { 1 };
			if(~isNumProxy.(~numToAdd).not) {
				Error("numToAdd '%' is not a number".format(~numToAdd)).throw;
			};
			~quant = ~args[3] ?? { 0.25 };
			~divisor = ~args[4] ?? { 2 };
			// if(~divisor <= 1) {
			// 	Error("\\clGenDiv divisor must be >= 1.0").throw;
			// };
			~divisible = ~args[5] ?? { Pfunc({ |quant| quant }).asStream };
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
			if(~resetFlag) { ~poolStream.reset };
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
		// for div, dur must match blockDur exactly
		~testDur = { |dur, blockDur, quant, tolerance|
			(dur absdif: blockDur) < tolerance
		};
		~getAvail = { |items, quant, tolerance = 0.001|
			var durs = (items.collect(_.time) ++ ~dur).differentiate.drop(1),
			avail, numToAdd,
			blocks, testUnit = 0, blockDur, i, div;
			if(~replaceItems.size == 0) {
				avail = Array.fill(~items.size, true);
			} {
				avail = ~items.collect { |item|
					~replaceItems.isEmpty or: { ~replaceItems.includesEqual(item.item) }
				};
			};
			numToAdd = min(~numToAdd.next, avail.count { |bool| bool });
			blocks = Array(numToAdd);
			blockDur = quant.next;
			div = ~divisible.next(quant);
			while { numToAdd > 0 and: { blockDur < (~dur - tolerance) } } {
				i = durs.collectIndices { |dur, i| avail[i] and: {
					~testDur.(dur, blockDur, quant, tolerance)
				} };
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
			blocks.collect { |i|
				div = ~divisor.next;
				if(div <= 1) {
					"In BP(%), \\clGenDiv divisor must be >= 1.0, substituting 2.0"
					.format(~bpKey.asCompileString).warn;
				};
				blockDur = (1 .. div - 1) / div;
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
		~snippet.("div", "\\div(##pool, replaceItems, numToAdd, quant, divisor, divisible##)");
	} => PR(\clGenDiv);

	PR(\clGenDiv).clone {
		~testDur = { |dur, blockDur, quant, tolerance|
			dur.inclusivelyBetween(blockDur, blockDur + quant)
		};
		~snippet.("div2", "\\div2(##pool, replaceItems, numToAdd, quant, divisor, divisible##)");
	} => PR(\clGenDiv2);

	// \stutt(source, match, numToAdd, quant, prob, insItem "-")
	// choose numToAdd of "pool" items and of these, add more items after it
	PR(\clGenIns).clone {
		~poolArgs = { [~args[4]] };
		~localPrep = {
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
		~defaultNames = #[/*baseItems,*/ match, numToAdd, quant, prob, insPool, wildcards, replaceWildcard];
		~defaults = [/*nil,*/ nil, 1, 0.25, 0.75, nil, nil, nil];
		// expand this later
		~validation = [/*nil,*/ nil, \isNumProxy, \isNumProxy, \isNumProxy, nil, nil, nil];
		~isNumProxy = { |thing, key|
			if(thing.tryPerform(\isNumProxy) != true) {
				"BP(%): %'s % input expected number"
				.format(~bpKey.asCompileString, ~protoID, key.asCompileString)
			} { nil };  // else no error
		};
		~canStutter = { |item, index, quant|
			item.item.isRest.not and: {
				~getIndexDur.(index) >= (2 * quant) and: {
					~matchItems.isEmpty or: { ~matchItems.includesEqual(item.item) }
					and: { ~wildcardCheck.(item[\wildcard]) }
				}
			}
		};
		~modifySource = {
			var times = ~getTimes.();  // [time, index] pairs unless overridden
			var valueStream = ~getValueStream.();
			times.do { |pair, i|
				var new = valueStream.next(pair);
				~items = ~items.add((
					item: new,
					wildcard: if(~replaceWildcard.isNil) { new }
					{ ~replaceWildcard.choose.item },
					src: \stutt,  // DON'T HARDCODE THIS
					time: pair[0]
				));
			};
			~items.quickSort { |a, b| a.time < b.time }
		};
		~getTimes = {
			var nextTime, time, times, new, quant, prob, goCondition;
			// later, we check index vs ~items.size
			// but ~items.size changes as we add new items
			// so, save the original input size
			var inputSize = ~items.size;
			quant = ~quant.next;
			prob; // = ~prob.next;

			// get source indices
			~items.collectIndices { |item, index| ~canStutter.(item, index, quant) }
			.scramble.keep(~numToAdd.next).quickSort(_ < _).do { |index|
				// var didAdd = false;
				var numAdded = 0;
				time = ~items[index].time;
				nextTime = if(index < (inputSize - 1)) {
					~items[index + 1].time
				} {
					~time + ~dur  // this generator's ending time
				};
				// probably need to pass more stuff here?
				goCondition = ~makeGoCondition.(time, index);
				while {
					time = time + quant;
					time < nextTime and: { goCondition.value(time, numAdded) }
				} {
					prob = ~prob.next;
					if(prob.coin) {
						times = times.add([time, index]);
						numAdded = numAdded + 1;
					};
				};
				if(numAdded > 0 and: { ~isPitch ?? { false } }) {
					if(~items[index].item.isKindOf(SequenceNote)) {
						~items[index].item = ~applyArtic.($., ~items[index].item);
					};
				};
			};
			times
		};
		~makeGoCondition = { |time, index|
			true
		};
		// Routine is needed for stuttPitch
		// Other approach would be to build an array of values
		// but that's an extra iteration.
		// The routine does the same work with one fewer loop.
		~getValueStream = {
			if(~resetFlag) { ~insStream.reset };
			Routine { |pair|
				loop {
					pair = yield(~insStream.value(if(~passIndex) { pair[1] }));  // else nil
				};
			};
		};
		~makeStream = 0;  // ~seqStream is not used here
		~snippet.("stutt", "\\stutt(##match, numToAdd, quant, prob, insItem \"-\", wildcards, \"*\"##)");
	}.import((clGenPitch: #[artic])) => PR(\clGenStutt);

	PR(\clGenStutt).clone {
		~modifySource = {
			var nextTime, time, times, new, quant, prob, availIndices, added = 0, didAdd, index;
			var toAdd = ~numToAdd.next,
			attempts = toAdd * 3;
			// later, we check index vs ~items.size
			// but ~items.size changes as we add new items
			// so, save the original input size
			var inputSize = ~items.size;
			availIndices = ~items.collectIndices { |item, index| ~canStutter.(item, index, 0.001) };
			while {
				attempts = attempts - 1;
				attempts >= 0 and: {
					added < toAdd and: { availIndices.notEmpty }
				}
			} {
				didAdd = false;
				quant = ~quant.next;
				prob = ~prob.next;
				index = availIndices.select { |i| ~getIndexDur.(i) >= (2 * quant) }.choose;
				if(index.notNil) {
					availIndices.remove(index);
					time = ~items[index].time;
					nextTime = if(index < (inputSize - 1)) {
						~items[index + 1].time
					} {
						~time + ~dur  // this generator's ending time
					};
					while {
						time = time + quant;
						time - nextTime < 0.001  // tolerance
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
			};
			times.do { |pair|
				new = ~insStream.value(if(~passIndex) { pair[1] });  // else nil
				~items = ~items.add((item: new, wildcard: new, src: \stutt, time: pair[0]));
			};
			~items.sort { |a, b| a.time < b.time }
		};
		~snippet.("stutt2", "\\stutt2(##match, numToAdd, quant, prob, insItem \"-\", wildcards##)");
	} => PR(\clGenStutt2);

	// for each, add up to n-1 new things
	// e.g. addLimit = 3 means a total of 3 events following from a source event
	PR(\clGenStutt).clone {
		~defaultNames = #[/*baseItems,*/ match, numToAdd, quant, prob, addLimit, insPool, wildcards, replaceWildcard];
		~defaults = [/*nil,*/ nil, 1, 0.25, 0.75, 1000, nil, nil, nil];
		// expand this later
		~validation = [/*nil,*/ nil, \isNumProxy, \isNumProxy, \isNumProxy, \isNumProxy, nil, nil, nil];
		~makeGoCondition = { |time, index|
			var addLimit = max(0, ~addLimit.next - 1);
			{ |time, numAdded| numAdded < addLimit }
		};
		~snippet.("stuttN", "\\stuttN(##match, numToAdd, quant, prob, numInclusive, insItem \"-\", wildcards, \"*\"##)");
	} => PR(\clGenStuttN);

	PR(\clGenStuttN).clone {
		~defaultNames = #[/*baseItems,*/ match, numToAdd, quant, prob, addLimit, insPool, articPool, wildcards, replaceWildcard];
		~defaults = [/*nil,*/ nil, 1, 0.25, 0.75, 1000, nil, nil, nil, nil];
		// expand this later
		~validation = [/*nil,*/ nil, \isNumProxy, \isNumProxy, \isNumProxy, \isNumProxy, nil, nil, nil, nil];

		~superPrep = ~localPrep;
		~localPrep = {
			~superPrep.();
			if(~articPool.isNil) {
				~articPool = [$.];
			};
			~articPool = ~articPool.reject(_ == $");
			~articStream = Prand(~articPool.asArray, inf).asStream;
			currentEnvironment
		};

		~getValueStream = {
			if(~passIndex) {
				// note: this branch doesn't make sense.
				// A pitch accumulator assumes that you've provided an
				// interval pool. We get here if you didn't.
				// Shouldn't happen -- but, if we don't provide a branch,
				// then you get errors during a show. So at least don't die.
				Routine { |pair|
					loop {
						// we know passIndex is true, so just pass it
						pair = yield(~insStream.value(pair[1]));  // else nil
					};
				}
			} {
				Routine { |pair|
					var lastIndex, lastPitch, new;
					loop {
						if(pair[1] != lastIndex) {
							lastIndex = pair[1];
							lastPitch = ~items[pair[1]].item;
						};
						new = SequenceNote(lastPitch.asFloat + ~insStream.next.asFloat);
						// this test is COMPLETE BULLSHIT
						// because ~canStutter should prohibit rests from even
						// getting in here! I think I got it
						// but I have a show so I'm leaving it in for extra insurance
						if(lastPitch.isRest) {
							new.freq = Rest(new.freq);
						};
						new = ~applyArtic.(~articStream.next, new);
						lastPitch = new;
						pair = new.yield;
					};
				};
			};
		};

		~snippet.("stuttPitch", "\\stuttPitch(##match, numToAdd, quant, prob, numInclusive, intervalPool, articPool, wildcards, \"*\"##)");
	} => PR(\clGenStuttPitch);

	// for each, add up to a total duration
	PR(\clGenStutt).clone {
		~defaultNames = #[/*baseItems,*/ match, numToAdd, quant, prob, addDur, insPool, wildcards, replaceWildcard];
		~defaults = [/*nil,*/ nil, 1, 0.25, 0.75, 1000, nil, nil, nil];
		// expand this later
		~validation = [/*nil,*/ nil, \isNumProxy, \isNumProxy, \isNumProxy, \isNumProxy, nil, nil, nil];
		~makeGoCondition = { |time, index|
			var nextTime = if(index < (~items.size - 1)) {
				~items[index + 1].time
			} {
				~time + ~dur  // this generator's ending time
			};
			// lesser of available dur vs user limited dur
			nextTime = min(nextTime, time + ~addDur.next);
			{ |time| time < nextTime }
		};
		~snippet.("stuttDur", "\\stuttDur(##match, numToAdd, quant, prob, addDur, insItem \"-\", wildcards, \"*\"##)");
	} => PR(\clGenStuttDur);

	PR(\clGenStutt).clone {
		~poolArgs = { [~args[1]] };
		~defaultNames = #[/*baseItems,*/ maxDur, insPool, wildcards];
		~defaults = [/*nil,*/ 0.5, [(item: $x, time: 0)], nil];
		~validation = [/*nil,*/ \isNumProxy, nil, nil];
		~process = {
			var lastEvent, localItems, new, maxDur;
			if(~resetFlag) { ~insStream.reset };
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
		~snippet.("choke", "\\choke(##maxDur, pool, wildcards##)");
	}.import((clGenIns: #[insertItem])) => PR(\clGenChoke);

	PR(\clGenIns).clone {
		~localPrep = {
			~pool = ~args[0];
			~quant = ~args[1] ?? { 0.25 };
			~weights = ~args[2..];
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
			var avail;
			~resetPool.();  // for subclasses
			avail = ~getAvail.(~items);
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
						};
					};
				};
			};
			~items
		};
		~resetPool = {
			if(~resetFlag) { ~poolStream.reset };
		};
		~getAvail = { |items|
			var quant = ~quant.next, weights = ~weights.next,
			now = ~time, end = ~time + ~dur,
			avail = Array.new, tolerance = 0.001,
			delta, tempWeights, next = 0, i, deleted;
			~deltaPool = Array.series(weights.size, quant, quant);
			~deltaStream = Pwrand(~deltaPool, Pfunc { |w| w }, inf).asStream;
			while { now < (end - tolerance) } {
				if(items.every { |item| (next absdif: item[\time]) > tolerance }) {
					avail = avail.add(now);
				};
				delta = ~deltaStream.next(weights);
				next = now + delta;
				if(items.any { |item| (next absdif: item[\time]) < tolerance }) {
					// there was already something there; try other options
					tempWeights = weights.copy;
					deleted = 0;
					while { deleted < (weights.size - 1) } {
						i = ~deltaPool.indexOf(delta);
						// tried this one and failed, so delete its probability
						tempWeights = tempWeights.put(i, 0).normalizeSum;
						deleted = deleted + 1;
						delta = ~deltaStream.next(tempWeights);
						next = now + delta;
						if(items.every { |item| (next absdif: item[\time]) < tolerance }) {
							deleted = inf;  // got one, drop out
						};
					};
					// if succeeded, then the slot at 'next' is empty
					// 'next' may have failed, but no matter, check for the next one
				};
				now = next;
			};
			avail
		};
		~insertItem = { |new|
			var insertI = ~items.detectIndex { |item| new[\time] < item[\time] };
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
		~snippet.("delta", "\\delta(##pool, quant, weights...##)");
	} => PR(\clGenDelta);

	PR(\clGenDelta).clone({
		// can't use Char:digit because I want 0 in keyboard order, not logical order
		~charMap = "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		~localPrep = {
			~pool = ~args[0];
			~quant = ~args[1] ?? { 0.25 };
			~dpool = ~args[2];
			~poolStream = Prand(~pool.asArray, inf).collect(_.item).asStream;
			~dpool = Prand(~dpool.asArray, inf);
			~dpoolStream = ~dpool.asStream;
			~resetCount = 0;  // see ~getAvail; trap infinite loops
			~initRest = false;  // see superclass modifySource
			~remainder = 0;

			// prepare reset flags for the 2 pool streams
			~pReset = ~checkChildReset.(~pool);
			~dpReset = ~checkChildReset.(~dpool.list);  // dpool is a Prand
			// want: pReset.not and: dpReset.not (3 ops)
			// which == not(preset or: dpReset) (2 ops)
			if(not(~pReset or: { ~dpReset })) {
				// here: neither subpattern specified reset
				// therefore both of them should follow the pdelta's resetFlag
				~pReset = ~dpReset = ~resetFlag;
			};

			currentEnvironment
		};
		~resetPool = {
			// not checking resetFlag
			// bc the two child flags should be false if resetFlag was false
			if(~pReset) { ~poolStream.reset };
			if(~dpReset) { ~dpoolStream.reset };
		};
		// modifySource is inherited
		// getAvail has the new logic
		~getAvail = { |items|
			var avail = List.new,
			// also works if ~remainder > ~dur
			// avail is empty in that case, and remainder is reduced the next time
			now = ~time + ~remainder,
			end = ~time + ~dur,
			tolerance = 0.001,
			quant = ~quant.next,
			item, index;
			if(quant != ~oldQuant) {
				~oldQuant = quant;
				if(quant.size > 0) {
					~quantMap = quant;
				} {
					~quantMap = quant * (1..36);  // allow letters and numbers as digits
				};
			};
			while { now < (end - tolerance) } {
				if(items.every { |item| (now absdif: item[\time]) > tolerance }) {
					avail.add(now);
				};
				item = ~dpoolStream.next;
				if(item.notNil) {
					index = ~deltaIndexFromItem.(item[\item]);
					if(index.isNumber) {
						now = now + (~quantMap[index] ?? {
							Error("pdelta: error accessing quantMap").throw;
						});
					} {
						Error("pdelta: could not resolve delta pool").throw;
					};
				} {
					if(~resetCount < 10) {
						~resetCount = ~resetCount + 1;
						~dpoolStream = ~dpool.asStream;
					} {
						Error("pdelta: delta pool failed too many times").throw;
					}
				};
			};
			~remainder = now - end;
			avail
		};
		~deltaIndexFromItem = { |item|
			case
			{ item.isKindOf(SequenceNote) } {
				item = item.asFloat;
				// char 0 --> value 9.0; char A --> value 65.0
				if(item >= 10) {
					item - 55  // 65.0 --> 10.0
				} {
					item
				}
			}
			{ item.isKindOf(Char) } {
				~charMap.indexOf(item.toUpper) ?? {
					Error("pdelta: invalid character $" ++ item).throw;
				}
			}
			{
				"pdelta: invalid digit %".format(item.asCompileString).warn;
				1
			};
		};
		~snippet.("pdelta", "\\pdelta(##pool, quant, deltapool##)");
	}).import((clGenPitch: #[checkChildReset])) => PR(\clGenPdelta);

	// \timePt(pool, timepts, quant, ignoreCollisions)
	// ignoreCollisions == 0: if the next timepoint is occupied,
	// stop and wait for the next bar to render this timepoint
	// != 0: if it's occupied, just go ahead with the next point
	// (pretend the collision didn't happen)
	PR(\clGenPdelta).clone {
		~tolerance = 0.001;
		~initRest = false;
		~numericDefaults = [0.5, 0];
		~localPrep = {
			~pool = ~args[0];
			~timepts = ~args[1];
			#[quant, ignoreCollisions].do { |name, i|
				var j = i + 2;
				case
				{ ~args[j].isNil } {
					name.envirPut(~numericDefaults[i])
				}
				{ ~isNumProxy.(~args[j]) } {
					name.envirPut(~args[j])
				} {
					Error("BP(%) \\timePt() expected number for '%'"
						.format(~bpKey.asCompileString, name)
					).throw;
				};
			};
			~poolStream = Prand(~pool.asArray, inf).collect(_.item).asStream;
			~dPoolStream = Prand(~timepts.asArray, inf).asStream;

			// prepare reset flags for the 2 pool streams
			~pReset = ~checkChildReset.(~pool);
			~dpReset = ~checkChildReset.(~timepts);
			// want: pReset.not and: dpReset.not (3 ops)
			// which == not(preset or: dpReset) (2 ops)
			if(not(~pReset or: { ~dpReset })) {
				// here: neither subpattern specified reset
				// therefore both of them should follow the timePt()'s resetFlag
				~pReset = ~dpReset = ~resetFlag;
			};

			currentEnvironment
		};
		~getAvail = { |items|
			var point, lastPt = -1, time;
			var quant = ~quant.next;
			var ignore = ~ignoreCollisions.next;
			var avail = Array.new;
			block { |break|
				while {
					point = ~nextTimePoint.();
					point.notNil and: {
						point = ~deltaIndexFromItem.(point[\item]);
						point > lastPt
					}
				} {
					time = quant * point;
					// got collision?
					if(items.any { |item| item[\time] absdif: time < ~tolerance }) {
						if(ignore == 0) {
							break.();
						} {
							~pending = nil;  // continue with next timePt item
						}
					} {
						avail = avail.add(time);
						lastPt = point;
						~pending = nil;
					}
				};
			};
			avail
		};
		// collision recovery
		~nextTimePoint = {
			var out;
			if(~pending.notNil) {
				out = ~pending;
				~pending = nil;
				out
			} {
				~pending = ~dPoolStream.next;
			}
		};
		~snippet.("timePt", "\\timePt(##pool, timepoints, quant, ignoreCollisions##)");
	} => PR(\clGenTimePt);

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
		~localPrep = {
			~pool = ~args[0].asArray;
			~checkEmpty.(~pool, "pool");
			~maxDelta = ~args[1] ?? { 1 };
			~factor = ~args[2] ?? { 2 };
			~minNotes = ~args[3] ?? { 3 };
			~maxNotes = ~args[4] ?? { 1e10 };
			~distribution = ~args[5] ?? { \exp };
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

		~snippet.("rDelta", "\\rDelta(##pool, maxDelta, factor, minNotes, maxNotes, distribution, parameters##)");
		~protoID = \clGenRDelta;
	} => PR(\clGenRDelta);

	// pool, avg delta, factor(>1 = accel to midpoint), midpoint, curve
	PR(\clGenRDelta).clone {
		~numToAdd = 1e10;  // hack: ~modifySource does min(~numToAdd, avail.size)
		~localPrep = {
			~pool = ~args[0].asArray;
			~checkEmpty.(~pool, "pool");
			~initDelta = ~args[1] ?? { 1 };
			~midDelta = ~args[2] ?? { 0.1 };
			~midpoint = ~args[3] ?? { 1 };
			~curve = ~args[4] ?? { 0 };

			if(~isNumProxy.(~curve).not and: { Env.shapeNames[~curve].isNil }) {
				Error("%: curve '%' is invalid".format(~protoID, ~curve)).throw;
			};

			#[initDelta, midDelta, midpoint].do { |key|
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

		~snippet.("ramp", "\\ramp(##pool, initDelta, midDelta, midpoint, curve##)");
		~protoID = \clGenRamp;
	} => PR(\clGenRamp);

	PR(\clGenIns).clone {
		~poolArgs = nil;
		~superprep = ~localPrep;
		~localPrep = {
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
			numToShift = (~args[1] ?? { 1 }).next;
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
		~snippet.("shift", "\\shift(##match, numToShift, quant##)");
	} => PR(\clGenShift);

	PR(\clGen).clone {
		~poolArgs = nil;
		~localPrep = {
			~quant = ~args[0];
			if(~isNumProxy.(~quant).not) {
				Error("\rot() quant must be a number").throw;
			};
			~match = ~args[1].collect(_.item);  // also handles nil; not really a pool
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
		~snippet.("rot", "\\rot(##quant, match##)");
	} => PR(\clGenRot);

	PR(\clGen).clone {
		~poolArgs = { [~args[0]] };
		~localPrep = {
			~convertArticPools.();
			~pool = ~args[0] ?? { "_" };
			~checkArticPool.(~pool);
			~wildcards = ~args[1];
			~override = ~args[2] ?? { 1 };
			if(~wildcards.notNil) {
				~wildcards = ~wildcards.collect(_.item);
			};
			~poolStream = Prand(~pool.asArray, inf).asStream;
			// Possible future feature
			// ~durSplit = ~args[0];
			// if(~durSplit.isNumber.not) {
			// 	Error("\artic() durSplit must be a number").throw;
			// };
			// ~shortPool = ~args[1];
			// ~longPool = ~args[2] ?? { ~args[1] };
			currentEnvironment
		};
		~process = {
			if(~resetFlag) { ~poolStream.reset };
			~items = ~getUpstreamItems.();
			~items.collect(~apply);
		};
		// for chain-in-pool
		~apply = { |item|
			var artic, new;
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
		~snippet.("artic", "\\artic(##articPool, wildcards##)");
	}.import((clGenPitch: #[artic])) => PR(\clGenArtic);

	// \articSplit(wildcards, dur0, articpool0, dur1, articpool1, ... articpoolDefault)
	// durs are arranged in decreasing order
	// match: given dur <= event dur, and last dur is 0 (so shortest notes will match last)
	// so \articSplit("*", 0.5, "_", "~"):
	// event dur = 0.5: do match vs 0.5, apply "_"
	// event dur = 0.25, no match vs 0.5, do match 0, apply "~"
	// so now the meaning is: "0.5 and greater, use '_', less than 0.5, use '~'"
	PR(\clGenArtic).clone {
		~poolArgs = {
			var out = ~args[2, 4..];
			if(~args.size.even) {
				out = out.add(~args.last);
			};
			out;
		};
		~localPrep = {
			var i = 1;
			var sortValues, order;
			~convertArticPools.();
			~wildcards = ~args[0];
			if(~wildcards.notNil) {
				~wildcards = ~wildcards.collect(_.item);
			};
			~pools = Array(~args.size + 1 div: 2);
			while {
				i < ~args.size
			} {
				if(i == (~args.size - 1)) {
					if(i.odd) {
						~checkArticPool.(~args[i]);
						~pools.add([0, ~args[i]]);
					} {
						"BP(%): articSplit ignoring extra value"
						.format(~bpKey.asCompileString).warn;
					};
					i = i + 1;
				} {
					if(~isNumProxy.(~args[i])) {
						~checkArticPool.(~args[i+1]);
						~pools.add([~args[i], ~args[i+1]]);
						i = i + 2;
					} {
						Error("BP(%): articSplit should alternate durations and pools"
							.format(~bpKey.asCompileString)
						).throw;
					};
				};
			};
			if(~pools.last[0] != 0) {
				~pools.add([0, ["_"]]);
			};
			sortValues = ~pools.collect { |pair| pair[0].value };
			order = sortValues.order { |a, b| a > b };
			~pools = ~pools[order];
			~poolStreams = ~pools.collect { |pair|
				Prand(pair[1].asArray, inf).asStream;
			};
			currentEnvironment
		};
		~apply = { |item, i|
			var artic, new, j, dur;
			if(item.item.isKindOf(SequenceNote) and: {
				~wildcards.isNil or: { ~wildcards.includes(item.wildcard) }
			}) {
				dur = ~durAtIndex.(i);
				j = ~pools.detectIndex { |pair| pair[0].value <= dur };
				artic = ~poolStreams.clipAt(j).next;
				new = item.item.copy;
				new = ~applyArtic.(artic, new);
				item.copy.put(\item, new);
			} {
				item
			};
		};
		// [\dur] is not available yet... have to calculate *rolls eyes*
		~durAtIndex = { |i|
			if(i < (~items.size - 1)) {
				~items[i+1][\time] - ~items[i][\time]
			} {
				~dur - ~items[i][\time]
			}
		};
		~snippet.("articSplit", "\\articSplit(##wildcards##, 0.5, \"_\", \".\")");
	} => PR(\clGenArticSplit);

	PR(\clGen).clone {
		~poolArgs = { [~args[0]] };
		~localPrep = {
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
			var srcBP, i, endTime = ~time + ~dur, new, wildcard;
			~items = ~getUpstreamItems.();
			if(~resetFlag) { ~poolStream.reset };
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
							wildcard = item[\wildcard];
							new = new.item;
							~insertItem.((item: new, wildcard: wildcard ?? { new }, src: \unis, time: item.time));
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

	// extract (selected) wildcards: throw away "item" values
	// if you supply wildcards, match only those; otherwise match all
	// if an item has no wildcard, don't touch it
	// useful pattern: \unis(...)::\wildcards(...)
	PR(\clGen).clone {
		~localPrep = {
			~wildcards = ~args[0];
			if(~wildcards.notNil) {
				~wildcards = ~wildcards.collect(_.item);
			};
			~negate = ~args[1] ?? { 0 };
			if(~isNumProxy.(~negate).not) {
				Error(
					"BP(%): negate should be a number".format(~bpKey.asCompileString)
				).throw
			};
			currentEnvironment
		};
		~process = {
			var end = ~time + ~dur;
			var includes;
			~items = ~getUpstreamItems.();
			~items.do { |item|
				if(item[\time] >= ~time and: { item[\time] < end
					and: {
						item[\wildcard].notNil and: {
							~wildcards.isNil or: {
								includes = ~wildcards.includesEqual(item[\wildcard]);
								if(~negate.next > 0) {
									includes.not
								} {
									includes
								}
							}
						}
					}
				}) {
					item[\item] = item[\wildcard];
				};
			};
		};
		~snippet.("wildcards", "\\wildcards(##wildcards, negate##)");
	} => PR(\clGenWildcards);

	// PR(\clGen).clone {
	//
	// } => PR(\clGenLeadTo);

	// \fork(items, "\gen()| \gen()|...")
	PR(\clGen).clone {
		~localPrep = {
			~generators = ~args[0];
			currentEnvironment
		};
		~process = {
			~items = ~getUpstreamItems.();
			~items = ~processOne.(~items, ~generators);
		};
		// factored out for subclass
		~processOne = { |items, patternString|
			var lastGenTime = 0, newItems = Array.new;
			patternString.do { |gen, i|
				var start, end, partition;
				if(gen.tryPerform(\isClGen) ?? { false }) {
					start = gen.time;
					end = start + gen.dur;
					// any stray items?
					if(start - lastGenTime > 0.0001) {
						newItems = newItems.addAll(items.select { |item|
							item[\time] >= lastGenTime and: { item[\time] < start }
						});
					};
					partition = items.select { |item|
						item[\time] >= start and: { item[\time] < end }
					};
					gen.baseItems = partition;
					newItems = newItems.addAll(gen.process);
					lastGenTime = gen.time + gen.dur;
				}
			};
			newItems = newItems.addAll(~items.select { |item|
				item[\time] >= lastGenTime
			});
			newItems
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
				Error("%% may not be used in a pool".format(
					$\\, ~protoID.asString.drop(5).toLower
				)).throw;
			};
			~args.do { |a| a.do(checkOne) };  // ~args for subclass
		};

		~snippet.("fork", "\\fork(##genString##)");
	} => PR(\clGenFork);

	PR(\clGenFork).clone {
		~poolArgs = { ~strings };
		~localPrep = {
			~source = ~args[0];
			~strings = ~args[1..].collect { |str|
				if(str.every { |item| item.tryPerform(\isClGen) ?? { false } }) {
					str  // all are clGens, pass through OK
				} {
					// replace with \rand() (compatible with other pool strings)
					// the [] wrapper represents the "" string
					[PR(\clGenRand).copy.putAll((
						// str is already an array, but needs 'doReset' and 'offset'
						args: [str, nil, ClNumProxy(0), ClNumProxy(0)],
						bpKey: ~bpKey,
						parm: ~parm,
						time: ~time,
						dur: ~dur,
						isPitch: ~isPitch,
						isMain: ~isMain
					)).prep]
				};
			};
			~layerIDs = Array.fill(~strings.size, { UniqueID.next });
			~checkPoolArgs.();
			currentEnvironment
		};
		~process = {
			var sourceItems = ~getUpstreamItems.();
			var lookupSet = sourceItems.as(IdentitySet);
			var items, new;
			// prevent items from leaking in from the previous bar
			~items = sourceItems;
			~strings.do { |patString, i|
				items = sourceItems.copy;
				items = ~processOne.(items, ~source);  // common source
				items = ~processOne.(items, patString);
				items = ~updateDeltas.(items, \dur);
				items.do { |item|
					if(lookupSet.includes(item).not) {
						item[\layerID] = ~layerIDs[i];
						new = new.add(item);
					};
				};
			};
			// as in Ppar, delta may need to be different from dur
			// dur was set above
			~items = ~updateDeltas.((sourceItems ++ new).sort { |a, b| a[\time] < b[\time] }, \delta);
		};
		~snippet.("par", "\\par(##source, string0, string1...##)");
	} => PR(\clGenPar);

	PR(\clGen).clone {
		~localPrep = {
			var stream, thing;
			~list = Array.new;
			~weights = Array.new;
			stream = Pseq(~args, 1).asStream;
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
		~snippet.("oneof", "\\oneof(##genStrings/probs##)");
	} => PR(\clGenOneof);

	// \gen1(...)::\gen2(...)::\gen3(...)
	// gen1 processed items -> gen2 processed items -> gen3
	PR(\clGen).clone {
		~canBePool = true;  // pitch procs can chain \artic or \xpose in a pool
		~localPrep = {
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
