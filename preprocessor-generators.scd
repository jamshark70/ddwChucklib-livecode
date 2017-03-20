var saveType = AbstractChuckArray.defaultSubType;

// cl-livecode generator objects

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

		// responsible for parsing any time-sequence strings
		// each generator may have different ones
		// generic one assumes only ~args[0]
		~prep = {
			// ~baseItems = ~parse.(~args[0]);
			~baseItems = ~args[0];
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
			out = key.envirGet.asArray.collect { |item| item.value }.flatten(1);
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
	}, parentKeys: #[canEmbed, protoID, isClGen, patternStringKeys, patternStringFuncs])
	.import((clPatternSet: #[decodePitch])) => PR(\clGen);

	PR(\clGen).copy => PR(\clGenTest);

	// "\seq("rhythm", "itemseq", "replaceItems", >0 to reset)
	PR(\clGen).clone {
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1];
			if(~args[2].notNil) {
				~replaceItems = ~args[2].collect { |item| item.item };
			} {
				~replaceItems = [$*];
			};
			if(~args[3].isNumber) {
				~doReset = ~args[3] > 0;
			} {
				~doReset = false;
			};
			currentEnvironment
		};

		~process = {
			if(~doReset or: { ~seqStream.isNil }) {
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
					~items[i] = item.copy.put(\item, ~seqStream.next(item));
				};
			};
		};

		~makeStream = {
			~seqStream = Pseq(~pool, inf).collect(_.item).asStream;
		};
		~helpString = "\\seq(\"items\")";
		~snippet.("seq", "\\seq(##src, items, wildcards, reset##)");
	} => PR(\clGenSeq);

	PR(\clGenSeq).clone {
		// ~asPattern = { Prand(~args[0], inf) };
		~makeStream = {
			~seqStream = Prand(~pool, inf).collect(_.item).asStream;
		};
		~helpString = "\\rand(\"timing\", \"items\")";
		~snippet.("rand", "\\rand(##src, items, wildcards##)");
	} => PR(\clGenRand);

	PR(\clGenSeq).clone {
		~makeStream = {
			~seqStream = Pxrand(~pool, inf).collect(_.item).asStream;
		};
		~helpString = "\\xrand(\"items\")";
		~snippet.("xrand", "\\xrand(##src, items, wildcards, reset##)");
	} => PR(\clGenXrand);

	PR(\clGenSeq).clone {
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
		~snippet.("gdefn", "\\gdefn(##`key##)");
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
		~snippet.("pdefn", "\\pdefn(##`key##)");
	} => PR(\clGenPdefn);

	PR(\clGenSeq).clone {
		~superprep = ~prep;
		~prep = {
			~superprep.();
			~doReset = false;  // reset is meaningless for wrand anyway
			currentEnvironment
		};
		~makeStream = {
			var firstNum = ~args.detectIndex { |item| item.isNumber },
			weights;
			if(firstNum.isNil) {
				weights = Array.fill(~pool.size, ~pool.size.reciprocal);
			} {
				weights = ~args[firstNum..].extend(~pool.size, 1).normalizeSum;
			};
			~seqStream = Pwrand(~pool, weights, inf).collect(_.item).asStream;
		};
		~helpString = "\\wrand(\"items\", weight0, weight1...)";
		~snippet.("wrand", "\\wrand(##src, items, wildcards, weights...##)");
	} => PR(\clGenWrand);

	PR(\clGenSeq).clone {
		~makeStream = {
			~seqStream = Pn(Pshuf(~pool, 1), inf).collect(_.item).asStream;
		};
		~helpString = "\\shuf(\"items\")";
		~snippet.("shuf", "\\shuf(##src, items, wildcards, reset##)");
	} => PR(\clGenShuf);

	PR(\clGenSeq).clone {
		~defaults = [[], [(item: $*)], #[-2, -1, 1, 2].collect({ |int| (item: int) }), 0, 7, "_"];
		~argNames = #[baseItems, replaceItems, intervalPool, fallbackMin, fallbackMax, articPool];
		~artic = ($~: inf /*1.01*/, $_: 0.9, $.: 0.4);
		~doReset = false;
		~prep = {
			~argNames.do { |key, i|
				key.envirPut(~args[i] ?? { ~defaults[i] });
			};
			if(~articPool.isString.not) {
				Error("\\pitch() invalid articPool").throw;
			};
			~replaceItems = ~replaceItems.collect { |item| item.item };
			~intervalPool = ~intervalPool.collect { |item| item.item.asFloat };
			~articPool = ~articPool.reject(_ == $");
			currentEnvironment
		};
		~makeStream = {
			~seqStream = Pfunc {
				var i = ~currentIndex, pitch, prev;
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
					pitch = rrand(~fallbackMin, ~fallbackMax);
				} {
					pitch = prev.item.asFloat + ~intervalPool.choose;
				};
				SequenceNote(pitch, nil, ~artic[~articPool.choose] ?? { 0.9 })
			}.asStream;
		};
		~snippet.("pitch", "\\pitch(##src, replaceItems, intervalPool, fallbackMin, fallbackMax, articPool##)");
	} => PR(\clGenPitch);

	PR(\clGenPitch).clone {
		~defaults = [[], [(item: $*)], #[-2, -1, 1, 2].collect({ |int| (item: int) }), 0, 7, "_", 1];
		~argNames = #[baseItems, replaceItems, intervalPool, fallbackMin, fallbackMax, articPool, connect];
		~doReset = true;
		~makeStream = {
			~seqStream = Routine {
				// first init only is from previous item
				var pitch, prev, i;
				if(~connect > 0) {
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
					prev = rrand(~fallbackMin, ~fallbackMax);
				} {
					prev = prev.item;
				};
				loop {
					pitch = prev.asFloat + ~intervalPool.choose;
					prev = pitch;  // loop is always going forward
					SequenceNote(pitch, nil, ~artic[~articPool.choose] ?? { 0.9 }).yield
				}
			}
		};
		~snippet.("pitch2", "\\pitch2(##src, replaceItems, intervalPool, fallbackMin, fallbackMax, articPool, connect##)");
	} => PR(\clGenPitch2);

	PR(\clGenSeq).clone {
		~doReset = false;
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1];
			~numToAdd = ~args[2] ?? { 1 };
			~quant = ~args[3] ?? { 0.25 };
			~poolStream = Prand(~pool.collect(_.item), inf).asStream;
			currentEnvironment
		};
		~modifySource = {
			var avail = ~getAvail.(~items, ~quant).scramble;
			min(~numToAdd, avail.size).do { |i|
				var newTime = avail[i],
				new;
				// should be guaranteed but, protect against mistakes
				if(newTime.notNil) {
					new = ~poolStream.next;
					if(new.notNil) {
						~insertItem.((item: new, time: newTime));
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
				time < ~dur
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

	PR(\clGenIns).clone {
		~superprep = ~prep;
		~prep = {
			~superprep.();
			~poolItems = ~pool.collect(_.item);
			~poolStream = Pn(Pshuf(~poolItems, 1), inf).asStream;
			currentEnvironment
		};
		~modifySource = {
			var possShifts = (~quant ?? { 0.25 }) * [-1, 1],
			numToShift = ~args[2] ?? { 1 };
			// check number of source items present in the pool (but only if pool was explicitly given)
			// it's possible to ask for 4 shifts but the source has only two shiftable items
			// reduce to 2 in that case
			if(~poolItems.notNil) {
				numToShift = min(numToShift, ~items.count { |item| ~poolItems.includes(item.item) });
			};
			~traverseShifts.(numToShift, ~items, ~poolItems.copy, possShifts);
			~items
		};

		// This must be recursive. Consider: \shift("..|||", "..", 2, 0.25).
		// If it first moves 0.5 to 0.25, there is nowhere for 0 to go.
		// The correct solution is to backtrack and move 0.5 to 0.75.
		// Backtracking = recursion.
		~traverseShifts = { |numToShift, items, pool, shiftArray, shiftedIndices(List.new)|
			var shiftItem, shiftIndex, shift, newTime, oldItem, tries;

			shiftIndex = nil;
			tries = ~poolItems.size * 2;
			while { shiftIndex.isNil and: { tries > 0 } } {
				shiftItem = ~poolStream.next;
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
					if(newTime.inclusivelyBetween(0, ~dur - 0.001) and: {
						newTime != ~items[shiftIndex + sh.sign].tryPerform(\at, \time)
					}) {
						oldItem = ~items[shiftIndex];
						~items[shiftIndex] = ~items[shiftIndex].copy.put(\time, newTime);
						pool.remove(shiftItem);
						shiftedIndices.add(shiftIndex);
						if(numToShift > 1) {
							if(~traverseShifts.(numToShift - 1, ~items, pool, shiftArray, shiftedIndices)) {
								break.(newTime);
							}
						} {
							break.(newTime);
						};
						// if we didn't break for success, undo for next iteration
						pool = pool.add(shiftItem);
						shiftedIndices.remove(shiftIndex);
						~items[shiftIndex] = oldItem;
					};
				};
				nil
			};
			shift.notNil  // true if successful
		};
		~snippet.("shift", "\\shift(##src, pool, numToShift, quant##)");
	} => PR(\clGenShift);

	PR(\clGen).clone {
		~prep = {
			~baseItems = ~args[0];
			~quant = ~args[1];
			if(~quant.isNumber.not) {
				Error("\rot() quant must be a number").throw;
			};
			currentEnvironment
		};
		~process = {
			var firstI, endTime = ~time + ~dur;
			~items = ~getUpstreamItems.();
			~items = ~items.collect { |item|
				item.copy.put(\time, (item[\time] + ~quant).wrap(~time, endTime));
			}.sort { |a, b| a[\time] < b[\time] };
		};
		~snippet.("rot", "\\rot(##src, quant##)");
	} => PR(\clGenRot);

	PR(\clGen).clone {
		~prep = {
			~baseItems = ~args[0];
			~pool = ~args[1] ?? { "_" };
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
			~items.collect { |item|
				var artic;
				if(item.item.isKindOf(SequenceNote)) {
					artic = ~pool.choose;
					item.copy.put(\item, item.item.copy.length_(~artic[artic] ?? { ~artic[$_] }));
				} {
					item
				};
			};
		};
		~snippet.("artic", "\\artic(##src, articPool##)");
	}.import((clGenPitch: #[artic])) => PR(\clGenArtic);

	PR(\clGen).clone {
		~prep = {
			~srcParm = ~args[0];
			~pool = ~args[1];
			~srcBP = ~args[2] ?? { ~bpKey };
			if(BP.exists(~srcBP)) {
				// will throw DoesNotUnderstand if there's no defaultParm
				if(~srcParm.isNil) { ~srcParm = BP(~srcBP).defaultParm };
			} {
				Error("\\unis: BP(%) doesn't exist".format(~srcBP.asCompileString)).throw;
			};
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
							if(~pool.notNil) {
								new = ~pool.choose.item;
							} {
								new = item.item;
							};
							~insertItem.((item: new, time: item.time));
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
		~snippet.("fork", "\\fork(##src, genString##)");
	} => PR(\clGenFork);

	PR(\clGen).clone {
		~prep = {
			var stream, thing;
			~baseItems = ~args[0];
			~list = Array.new;
			~weights = Array.new;
			stream = Pseq(~args, 1).drop(1).asStream;
			thing = stream.next;
			while { thing.notNil } {
				if(thing.isSequenceableCollection) {
					~list = ~list.add(thing);
					thing = stream.next;
					if(thing.isNumber) {
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
	} => PR(\clGenChain);

	PR.allOfType(\clGenerator).do { |pr|
		pr[\protoID] = pr.collIndex;
	};
} { AbstractChuckArray.defaultSubType = saveType };
