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
			~baseItems = ~parse.(~args[0]);
			currentEnvironment
		};

		~process = {
			~items = ~baseItems;  // copy if you're modifying
		};
		~asPattern = { |bp|  // the BP() object
			var pat;
			~process.();
			pat = Pseq(~items, 1).collect { |ev|
				[ev[\item], ev[\dur]]
			};
			if(~isMain) {
				pat
			} {
				PstepDurPair(pat.trace(prefix: "item/dur pair: "), 1)
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

		// this is temporary, for early testing before replacing the upstream parser
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
				items.do { |item, i| item[\dur] = deltas[i] };
				// [items/*, deltas, times*/]  // retain array for possible future extensions
				items.debug("parse return: processed deltas");
			} {
				if(~addRestToEmpty ? false) {
					List[(
						item: if(~isPitch.()) {
							SequenceNote(Rest(0), nil, 0.9)
						} {
							Rest(0)
						},
						time: 0,
						dur: min(~dur, bpb),
						initialRest: true
					)].debug("parse return: empty, added rest")
				} {
					[List.new].debug("parse return: empty")
				};
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

	PR(\clGen).clone {
		// optional argument 'bp' to access process variables
		~asPattern = { Pseq(~args[0], inf) };
		~helpString = "\\seq(\"items\")";
	} => PR(\clGenSeq);

	PR(\clGen).clone {
		~asPattern = { Prand(~args[0], inf) };
		~helpString = "\\rand(\"items\")";
	} => PR(\clGenRand);

	PR(\clGen).clone {
		~asPattern = { Pxrand(~args[0], inf) };
		~helpString = "\\xrand(\"items\")";
	} => PR(\clGenXrand);

	PR(\clGen).clone {
		~asPattern = { Pwrand(~args[0], ~args[1..].normalizeSum, inf) };
		~helpString = "\\wrand(\"items\", weight0, weight1...)";
	} => PR(\clGenWrand);

	PR(\clGen).clone {
		~asPattern = { Pn(Pshuf(~args[0], 1), inf) };
		~helpString = "\\shuf(\"items\")";
	} => PR(\clGenShuf);

	PR(\clGen).clone {
		~asPattern = {
			var weights = ~args[1..].normalizeSum;
			Pwrand((1 .. weights.size), weights, inf) * ~args[0]
		};
		~helpString = "\\randRhy(baseDur, weight1, weight2...)";
	} => PR(\clGenRandRhy);

	// non-default parameter with rhythm generator: Use Pstep
	PR(\clGen).clone({
		~valueGen = 0;
		~rhythmGen = 1;

		~embedInStream = { |inval|
			var dur = ~dur.(), durStream;
			~prepareChildren.(inval);
			durStream = if(dur.notNil and: { dur < inf }) {
				Pconst(dur, ~rhythmGen).asStream
			} { ~rhythmGen.asStream };
			PstepDur(~valueGen.asStream, durStream, 1).embedInStream(inval)
		};
		~prepareChildren = {
			#[inItem, inDelta, dur].do { |key|
				var thing = key.envirGet;
				if(thing.notNil) {
					#[valueGen, rhythmGen].do { |childKey|
						if(childKey.envirGet.tryPerform(\isClGen) ?? { false }) {
							childKey.envirGet.put(key, thing);
						};
					};
				};
			};
		};

		~protoID = \clStepGen;
		~patternStringKeys = #[valueGen, rhythmGen, dur, /*isPitch,*/ parm];
		~isPitch = { ~valueGen.tryPerform(\isPitch) ?? { false } };
	}, parentKeys: #[protoID, patternStringKeys]) => PR(\clStepGen);

	// default parm must expose 'dur' separately: Use Ptuple
	PR(\clStepGen).clone({
		~embedInStream = { |inval|
			var dur = ~dur.(), durStream;
			~prepareChildren.(inval);
			durStream = if(dur.notNil and: { dur < inf }) {
				Pconst(dur, ~rhythmGen).asStream
			} { ~rhythmGen.asStream };
			Ptuple([~valueGen.asStream, durStream], 1).embedInStream(inval)
		};

		~protoID = \clRhythmGen;

		~yieldsKeys = {
			[~parm, \dur]
			// if(~isPitch.()) { [~parm, \legato, \dur] } { [~parm, \dur] };
		};
	}, parentKeys: #[protoID, patternStringKeys]) => PR(\clRhythmGen);

	PR.allOfType(\clGenerator).do { |pr|
		pr[\protoID] = pr.collIndex;
	};
} { AbstractChuckArray.defaultSubType = saveType };
