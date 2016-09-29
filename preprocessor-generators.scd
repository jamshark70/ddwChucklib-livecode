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

		// responsible for parsing any time-sequence strings
		// each generator may have different ones
		// generic one assumes only ~args[0]
		~prep = {
			// ~baseItems = ~parse.(~args[0]);
			~baseItems = ~args[0];
			currentEnvironment
		};

		~process = {
			~items = ~getUpstreamItems.();
		};
		~unprocess = {
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
			~deltas = (~items.collect(_[\time]) ++ ~dur).differentiate;
			if(~items[0][\time] == 0) {
				~deltas = ~deltas.drop(1);
			} {
				~deltas[0] = Rest(~items[0][\time]);
				~items = ~items.copy.insert(0, (item: \rest, time: 0, initialRest: true));
			};
			~items = ~items.collect { |item, i| item.copy.put(\dur, ~deltas[i]) };
			// ~items.do { |item, i| item[\dur] = ~deltas[i] };
		};
		~asPattern = { |bp|  // the BP() object -- asPattern should be called @ top level only
			var pat, rest, didProcess = false;
			if(~items.isNil) {
				~items = ~process.(bp);
				~updateDeltas.();
				// "clGen items: ".post; ~items.postcs;
				didProcess = true;
			};
			// if(~items[0].time > 0) {
			// 	rest = [(time: 0, dur: ~items[0].time, item: Rest(~items[0].item))];
			// };
			// "rest ++" also works if 'rest' is nil
			pat = Pseq(rest ++ ~items, 1).collect { |ev|
				[ev[\item], ev[\dur]]
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

		// code generation
		~protoID = \clGen;
		~isClGen = true;
		~patternStringKeys = #[bpKey, args, dur, isPitch, isMain, parm];
		~asPatternString = { |stream(CollStream.new)|
			var needComma = false, obj;
			stream << "PR(" <<< ~protoID << ").copy.putAll((";
			~patternStringKeys.do { |key|
				obj = key.envirGet;
				if(obj.notNil) {
					if(needComma) { stream << ", " };
					stream << key << ": ";
					~objectToStream.(obj, stream);
					needComma = true;
				};
			};
			stream << ")).prep";
			stream.collection
		};
		~objectToStream = { |obj, stream|
			block { |break|
				~patternStringFuncs.do { |pair|
					if(pair.key.value(obj)) {
						break.(pair.value.value(obj, stream));
					};
				};
			};
		};
		~patternStringFuncs = [
			{ |obj| obj.tryPerform(\isClGen) ? false } -> { |obj, stream|
				obj.asPatternString(stream);
			},
			{ |obj| obj.isString } -> { |obj, stream|
				stream <<< obj
			},
			{ |obj| obj.isSequenceableCollection } -> { |obj, stream|
				if(obj.isMemberOf(Array).not) {
					stream << obj.class.name;
				};
				stream << "[";
				obj.do { |item, i|
					if(i > 0) { stream << ", " };
					~objectToStream.(item, stream);
				};
				stream << "]";
			},
			{ true } -> { |obj, stream| stream <<< obj }
		];
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
				~replaceItems = [$.];
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
					// copy is critical here!
					~items[i] = item.copy.put(\item, ~seqStream.next);
				};
			};
		};

		~makeStream = {
			~seqStream = Pseq(~pool, inf).collect(_.item).asStream;
		};
		~helpString = "\\seq(\"items\")";
	} => PR(\clGenSeq);

	PR(\clGenSeq).clone {
		// ~asPattern = { Prand(~args[0], inf) };
		~makeStream = {
			~seqStream = Prand(~pool, inf).collect(_.item).asStream;
		};
		~helpString = "\\rand(\"timing\", \"items\")";
	} => PR(\clGenRand);

	PR(\clGenSeq).clone {
		~makeStream = {
			~seqStream = Pxrand(~pool, inf).collect(_.item).asStream;
		};
		~helpString = "\\xrand(\"items\")";
	} => PR(\clGenXrand);

	PR(\clGenSeq).clone {
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
	} => PR(\clGenWrand);

	PR(\clGenSeq).clone {
		~makeStream = {
			~seqStream = Pn(Pshuf(~pool, 1), inf).collect(_.item).asStream;
		};
		~helpString = "\\shuf(\"items\")";
	} => PR(\clGenShuf);

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
	} => PR(\clGenIns);

	PR(\clGenIns).clone {
		~superprep = ~prep;
		~prep = {
			~superprep.();
			~poolItems = ~pool.collect(_.item);
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
			var shiftItem, shiftIndex, shift, newTime, oldItem;

			shiftIndex = nil;
			while { shiftIndex.isNil } {
				shiftItem = pool.choose;
				shiftIndex = ~items.collectIndices { |item, i|
					item.item == shiftItem and: { shiftedIndices.includes(i).not }
				};
				shiftIndex = shiftIndex.choose;  // nil if array is empty
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
	} => PR(\clGenShift);

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
			~items = ~getUpstreamItems.();
			~newItems = Array(~generators.size);
			~generators.do { |gen, i|
				var start, end, partition;
				if(gen.tryPerform(\isClGen) ?? { false }) {
					start = gen.time;
					end = start + gen.dur;
					partition = ~items.select { |item|
						item[\time] >= start and: { item[\time] < end }
					};
					gen.baseItems = partition;
					~newItems.add(gen.process);
				};
			};
			~items = ~newItems.flatten(1);
		};
	} => PR(\clGenFork);

	PR.allOfType(\clGenerator).do { |pr|
		pr[\protoID] = pr.collIndex;
	};
} { AbstractChuckArray.defaultSubType = saveType };
