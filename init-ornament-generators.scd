
PR(\clGen).clone {
	// args: [string, minDur, itemGen:rhythmGen]
	~asPattern = {
		var parsed;
		if(~args[0] != ~currentBase) {
			~currentBase = ~args[0];
			parsed = ~parse.(~currentBase);
			~items = parsed[0];
			// ~deltas = parsed[1];
		};
		Pseq(~items).collect { |srcItem, inval|
			var item, delta, stream, newPair, elapsed = 0, tolerance;
			item = srcItem[\item];
			delta = srcItem[\delta];
			if(item != $  and: { delta >= ~args[1] }) {
				tolerance = ~args[3] ?? { 0.001 };
				if(~args[2].tryPerform(\isClGen) ?? { false }) {
					~args[2].inItem = item;
					~args[2].inDelta = delta;
				};
				stream = ~args[2].asStream;
				while {
					newPair = stream.next(inval);
					newPair.notNil and: { (elapsed + newPair[1]).round(tolerance) < delta }
				} {
					elapsed = elapsed + newPair[1];
					inval = [item, newPair[1]].yield;
					item = newPair[0];
				};
				[item, delta - elapsed]
			} {
				[item, delta]
			};
		};
	};
	~parse = { |array|
		var bpb = if(BP.exists(~bpKey)) { BP(~bpKey).beatsPerBar } { thisThread.clock.beatsPerBar },
		beatsPerSeg, beatsPerItem, items = List.new/*, times = List.new*/, deltas;
		array = ~arraySplit.(array, $|);
		beatsPerSeg = bpb / array.size;
		array.do { |seg, i|
			if(seg.size > 0) {
				beatsPerItem = beatsPerSeg / seg.size;
				seg.do { |item, j|
					if(item.notNil and: { item != $  }) {
						items.add((item: item, time: (beatsPerSeg * i) + (beatsPerItem * j)));
					};
				};
			};
		};
		if(items.size > 0) {
			deltas = (items.collect(_[\time]) ++ [min(~dur, bpb)]).differentiate;
			if(items[0][\time] == 0) {
				deltas = deltas.drop(1);
			} {
				items = items.insert(0, (
					item: if(~isPitch.()) {
						SequenceNote(Rest(0), nil, 0.9)
					} {
						Rest(0)
					},
					time: 0,
					initialRest: true
				));
			};
			items.do { |item, i| item[\delta] = deltas[i] };
			[items/*, deltas, times*/]  // retain array for possible future extensions
		} {
			[List.new]
		};
	};
	~arraySplit = { |array, delimiter|
		var new = Array.new, subArray = Array.new;
		array.do { |item|
			if(item == delimiter) {
				new = new.add(subArray);
				subArray = Array.new;
			} {
				subArray = subArray.add(item);
			};
		};
		new.add(subArray);
	};
	~yieldsKeys = {
		// (if(~isPitch.()) { [~parm, \legato] } { [~parm] })
		// ++ #[dur]
		[~parm, \dur]
	};
	~protoID = \clGenOrn;
} => PR(\clGenOrn);


PR(\clGenOrn).clone({
	// superhack
	~itemIsRest = { |item|
		item.isRest or: { item.isKindOf(SequenceNote) and: { item.freq.isKindOf(Rest) } }
	};
	~asPattern = {
		var parsed;
		if(~args[0] != ~currentBase) {
			~currentBase = ~args[0];
			parsed = ~parse.(~currentBase);
			~items = parsed[0];
			// ~deltas = parsed[1];
		};
		Prout { |inval|
			var items = Pseq(~items.collect(_[\item])).asStream,
			deltas = Pseq(~items.collect(_[\delta])).asStream,
			item, delta, phraseDur, insertItems, insertDeltas,
			prevItem = items.next(inval),
			intervalStream = ~args[4].valueGen.asStream.collect(_.freq),
			rhythmStream = ~args[4].rhythmGen.asStream;
			while {
				item = items.next(inval);
				delta = deltas.next(inval);
				delta.notNil  // item will be nil on last iteration
			} {
				if(delta >= ~args[1] and: { ~itemIsRest.(prevItem).not }) {
					phraseDur = (delta * ~args[2]).round(~args[3]);  // func? stream?
					if(item.isNil or: { ~itemIsRest.(item) }) {
						// next item is empty or rest: move away from prevItem
						inval = PcollectFinal(
							{ |pair| pair[0].length = 1.01; pair },
							Ptuple([
								Pseries(prevItem, intervalStream, inf),
								Pseq([delta - phraseDur, Pconst(phraseDur, rhythmStream)])
							]),
							{ |pair| pair[0].length = 0.55; pair }
						).embedInStream(inval);
					} {
						// next item exists: lead to its pitch by reverse series
						insertDeltas = Pconst(phraseDur, rhythmStream).asStream.all;
						insertItems = Pseries(
							item,
							intervalStream,
							inf
						).drop(1).asStream.nextN(insertDeltas.size).reverse;
						inval = Ptuple([
							PcollectFinal(
								{ |note| note.length = 1.01 },
								Pseq([prevItem, Pseq(insertItems)]),
								{ |note| note.length = 0.55 }
							),
							Pseq([delta - phraseDur, Pseq(insertDeltas)])
						]).embedInStream(inval);
					};
				} {
					inval = [prevItem, delta].yield;
				};
				prevItem = item;
			};
		};
	};
	~protoID = \clGenBass;
}).import((clPatternSet: #[encodePitch], clRhythmGen: #[convertPitch])) => PR(\clGenBass);


PR(\clGen).clone {
	~defaultRule = [true -> { |item, now| [item, ~inDelta] }];  // this may be wrong
	~defaultDelta = 0.25;
	~defaultItem = $.;
	~asPattern = {
		~rules = Library.at(\rules, ~args[0]) ?? { ~defaultRule };
		~ruleStorage = nil;  // safe place for rules to save temp stuff
		Prout { |inval|
			var item = ~inItem ?? { ~defaultItem }, beat, match, newPair;
			// need first item before while; rule returns *next*, not "yield now"
			while {
				beat = thisThread.clock.beatInBar;  // possibly risky for lookahead patterns
				match = ~rules.detect { |rule| rule.key.value(item, beat) };
				match.notNil
			} {
				// evaluate rule: returns [newItem, delta]
				newPair = match.value.value(item, beat) ?? { [Rest(0), 1] };
				inval = newPair.yield;
				item = newPair[0];
			};
		}
	};
	~yieldsKeys = { [~parm, \dur] };
	~protoID = \clGenRule;
} => PR(\clGenRule);


// generators based on modifying a source pattern string

PR(\clGen).clone {
	~asPattern = {
		~getSource.();
		~modifySource.();
		~source
	};
	~srcKeys = #[items/*, deltas, times*/];
	~getSource = { |srcIndex(0)|
		var parsed;
		case
		{ ~args[srcIndex].tryPerform(\isClGen) ?? { false } } {
			~args[srcIndex].dur = ~dur;
			~source = ~args[srcIndex].asPattern;
			~srcKeys.do { |key|
				key.envirPut(~args[srcIndex].tryPerform(key).copy);
			};
		}
		{ ~args[srcIndex].isSequenceableCollection } {
			~constrainDur.(~parse.(~args[srcIndex])).do { |array, i|
				~srcKeys[i].envirPut(array);
			};
			~source = ~makeSource.();
		}
		{ Error("Generator source % is invalid".format(~args[srcIndex])).throw };
		~source
	};
	~constrainDur = { |parsed, tolerance = 0.001|
		var items, lastValidIndex;
		if(~itemIndex.isNil) {
			~itemIndex = ~srcKeys.indexOf(\items);
		};
		items = parsed[~itemIndex];
		// 'detectIndex' is easier to write
		// but the most common case is, all times are < ~dur.
		// reverse scanning means the common case will check one item and stop!
		lastValidIndex = block { |break|
			items.reverseDo { |item, i|
				if((item[\time] - ~dur).round(tolerance) < 0) {
					break.(items.size - i - 1)
				};
			};
			nil
		};
		if(lastValidIndex.isNil) {
			// all items are after ~dur, so remove them all
			parsed = Array.fill(parsed.size, { List.new });
		} {
			if(lastValidIndex < (items.size - 1)) {
				parsed = parsed.flop[ .. lastValidIndex].flop;
			};  // else, all time values are < ~dur, no change needed
		};
		parsed
	};
	~makeSource = {
		if(~items.size == 0) {
			Pn(Event.silent(~dur), 1)
		} {
			Pseq(~items.collect { |item| [item[\item], item[\delta]] }, 1)
		};
	};
	// for subclasses:
	~updateDeltas = {
		~deltas = (~items.collect(_[\time]) ++ ~dur).differentiate;
		if(~items[0][\time] == 0) {
			~deltas = ~deltas.drop(1);
		} {
			~deltas[0] = Rest(~items[0][\time]);
			~items = ~items.insert(0, (item: \rest, time: 0, initialRest: true));
		};
		~items.do { |item, i| item[\delta] = ~deltas[i] };
	};
	~isPitch = false;
	~protoID = \clGenSrc;
	~helpString = "\\src(\"source\")";
}.import((clGenOrn: #[parse, arraySplit, yieldsKeys])) => PR(\clGenSrc);

// [source, shiftable items, numToShift, shiftDur]
PR(\clGenSrc).clone {
	~modifySource = {
		var pool = (~args[1] ?? { ~items.collect(_[\item]) }).copy,
		possShifts = (~args[3] ?? { 0.25 }) * [-1, 1],
		numToShift = min(pool.size, (~args[2] ?? { 1 }));
		~traverseShifts.(numToShift, ~items, pool, possShifts);
		~updateDeltas.();
		~source = ~makeSource.();
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
	~protoID = \clGenShift;
	~helpString = "\\shift(\"source\", \"shiftable items\", numToShift, shiftDur)";
} => PR(\clGenShift);

// [source, insertable items, numToAdd, quant]
PR(\clGenSrc).clone {
	~getFromPool = { |pool|
		var i;
		if(pool.size > 0) {
			i = pool.size.rand;
			if(pool[i].tryPerform(\isClGen) ?? { false }) {
				pool[i]
			} { pool.takeAt(i) };
		};  // else return nil
	};
	~getNew = { |pool, insertI|
		var new = ~getFromPool.(pool);
		if(new.tryPerform(\isClGen) ?? { false }) {
			// different usage: here a clGen is not a pattern-maker,
			// but a value-maker
			new.getValue(pool, insertI, ~items);
		} {
			new
		};
	};
	~numToAdd = { |pool, avail|
		min(~args[2] ?? { 1 }, avail.size)
	};
	~poolIndex = 1;
	~quantIndex = 3;
	~modifySource = {
		var pool, avail, quant, insertI, new;
		if(~itemPool.isNil) {
			~itemPool = ~args[~poolIndex.value] ?? { ~invertProbs.(~items.collect(_[\item])) };
		};
		pool = ~itemPool.copy;
		quant = ~args[~quantIndex.value] ?? { 0.25 };
		avail = ~getAvail.(quant).scramble;
		block { |break|
			~numToAdd.(pool, avail).do { |i|
				insertI = ~items.detectIndex { |item| avail[i] < item[\time] };
				new = ~getNew.(pool, insertI ?? { ~items.size });
				if(new.isNil) { break.() };
				if(insertI.isNil) {
					~items.add((item: new, time: avail[i]));
				} {
					~items.insert(insertI, (item: new, time: avail[i]));
				};
			};
		};
		~updateDeltas.();
		~source = ~makeSource.();
	};
	// fallback: tend to add less-frequent items
	~invertProbs = { |array|
		array = array.copy.sort.separate { |a, b| a != b };
		array = array.sort { |a, b| a.size < b.size };
		array.collect { |subarray, i|
			Array.fill(subarray.size, array[array.size - 1 - i][0])
		}.flatten(1)
	};
	~getAvail = { |quant, tolerance = 0.001|
		var i = 0, j = 0, time, avail = List.new,
		times = ~items.collect(_[\time]) ++ [~dur];
		time = i * quant;
		while {
			time < ~dur
		} {
			if(time absdif: times[j] < tolerance) {
				if(~items[j][\initialRest] == true) { avail.add(time) };
				// 'time' is matched at times[j], so we must advance by at least 1
				j = j + 1;
			} {
				avail.add(time);
				// times[j] is somewhere in the future; may not need to advance yet
			};
			time = (i+1) * quant;
			// keep advancing 'j' as long as times[j] is earlier than the next 'time'
			while { time - times[j] >= tolerance } {
				j = j + 1;
			};
			i = i + 1;
		};
		avail
	};
	~protoID = \clGenIns;
	~helpString = "\\ins(\"source\", \"insertable items\", numToAdd, quant)";
} => PR(\clGenIns);

// [source, insertable, dur, quant, prob]
PR(\clGenIns).clone {
	~getFromPool = { |pool| pool[pool.size.rand] };
	~numToAdd = { |pool, avail| avail.size };
	~getAvail = { |quant, tolerance = 0.001|
		var i, j = 0, time, avail = List.new,
		prob = ~args[4] ?? { 0.8 },
		times = ~items.collect(_[\time]) ++ [~dur];
		i = max(0, ~dur - (~args[2] ?? { 1 }));
		j = times.detectIndex { |time| time >= i };
		i = round(i / quant);
		while {
			time = i * quant;
			time < ~dur
		} {
			if(time absdif: times[j] < tolerance) {
				j = j + 1;
			} {
				if(prob.coin) { avail.add(time) };
				if(time - times[j] >= tolerance) {
					j = j + 1;
				}
			};
			i = i + 1;
		};
		avail
	};
	~protoID = \clGenFill;
	~helpString = "\\fill(\"source\", \"insertable items\", fillDur, quant, probability)";
} => PR(\clGenFill);

PR(\clGenIns).clone {
	~poolIndex = 4;
	~quantIndex = 2;
	~numToAdd = { |pool, avail| avail.size };
	~artic = ($~: 1.01, $_: 0.9, $.: 0.4);
	~getFromPool = { |pool| pool[pool.size.rand] ?? { 0.4 } };
	~getNew = { |pool, insertI|
		var prev = ~items[insertI - 1], artic;
		if(~isPitch.()) {
			artic = ~getFromPool.(pool);
			if(artic.tryPerform(\isClGen) ?? { false }) {
				// different usage: here a clGen is not a pattern-maker,
				// but a value-maker
				artic = artic.getValue(pool, insertI, ~items);
			};
			prev[\item].copy.length_(~artic[artic] ?? { 0.4 })
		} {
			prev[\item].copy
		};
	};
	~getAvail = { |quant, tolerance = 0.001|
		var initRestCount = (~items[0][\initialRest] == true).asInteger;
		~order = ~items.collectIndices { |item|
			var x = item[\item];
			item[\initialRest] != true and: {
				if(~isPitch) {
					item.freq.isRest.not
				} {
					item.isRest.not
				}
			}
		}
		// Array.series(~items.size - initRestCount, initRestCount, 1)
		.scramble.keep(~args[1] ?? { 1 });
		~order.collect { |i|
			~getOneAvail.(
				~items[i][\time],
				if(~items[i + 1].notNil) { ~items[i + 1][\time] } { ~dur }
				- ~items[i][\time],
				quant, tolerance
			)
		}.flat;
	};
	~getOneAvail = { |start, dur, quant, tolerance = 0.001|
		var i = 1, time, avail = List.new,
		prob = ~args[3] ?? { 0.8 };
		while {
			time = i * quant;
			time < dur
		} {
			if(prob.coin) {
				avail.add(time + start);
			};
			i = i + 1;
		};
		avail
	};
	// stutter: don't invert probabilities
	~invertProbs = { |array|
		if(~isPitch.()) {
			array.reject { |item| item.freq.isRest }.collect(_.length)
		} { array };
	};
	~protoID = \clGenStutt;
	// ~helpString = "\\ins(\"source\", \"insertable items\", numToAdd, quant)";
	~helpString = "\\stutt(\"source\", numToStutter, quant, probability, articulationPool)";
} => PR(\clGenStutt);

// [stepSize, jumpSize, narrowThresh, fallbackMin, fallbackMax, articPool]
PR(\clGen).clone {
	~defaults = [2, 3, 2, 0, 7, "~_."];
	~getValue = { |pool, insertI, items|
		var prev = items[insertI - 1], next = items[insertI], interval, pitch, sign;
		~setDefaults.();
		pitch = case
		{ prev.isNil } {
			if(next.isNil) {
				rrand(~args[3], ~args[4]).round  // accidentals?
			} {
				// insert at start: attach to next
				next.item.asFloat + (rrand(1, ~args[0]).round * #[-1, 1].choose)
			};
		}
		{ next.isNil } {
			prev.item.asFloat + (rrand(1, ~args[0]).round * #[-1, 1].choose)
		}
		{
			// both non-nil: if close, jump; else fit in the middle
			prev = prev.item.asFloat;
			next = next.item.asFloat;
			interval = next - prev;
			if(abs(interval) <= ~args[2]) {
				pitch = [prev, next].rotate(2.rand);
				sign = sign(pitch[0] - pitch[1]);
				if(sign == 0) { sign = 1 };  // bias upward for repeated pitches
				pitch[0] + (rrand(~args[0], ~args[0] + ~args[1]).round * sign)
			} {
				rrand(min(prev, next), max(prev, next)).round
			}
		};
		SequenceNote(pitch, nil, ~artic[~args[5].choose] ?? { 0.9 })  // replace later
	};
	~artic = ($~: 1.01, $_: 0.9, $.: 0.4);
	~setDefaults = {
		if(~args.size < ~defaults.size) {
			~args = ~args.extend(~defaults.size);
		};
		~defaults.do { |default, i|
			if(~args[i].isNil) { ~args[i] = ~defaults[i] };
		};
		~setDefaults = 0;  // don't do this again in this instance
	};
	~protoID = \clGenPitch;
	~helpString = "\\pitch(stepSize, jumpSize, narrowThresh, fallbackMin, fallbackMax, articPool)";
} => PR(\clGenPitch);

{ |key|
	if(key.notNil) {
		key = key.asString;
		key[0] = key[0].toUpper;
		key = ("clGen" ++ key).asSymbol;
		if(PR.exists(key)) {
			PR(key).v[\helpString] ?? {
				"PR(%) has no help string".format(key.asCompileString);
			}
		} {
			"PR(%) doesn't exist".format(key.asCompileString).warn;
		};
	} {
		PR.keys.as(Array).sort.do { |key|
			if(PR(key).v[\helpString].notNil) { PR(key).v[\helpString].postln };
		};
		""
	};
} => Func(\genhelp);
