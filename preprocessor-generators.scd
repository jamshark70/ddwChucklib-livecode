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
			~items = ~getDownstreamItems.();
		};
		~unprocess = {
			~items = nil;
		};
		// .value polymorphism: we may need items from an array or a clgen
		// Proto maps .value onto ~next
		~next = { ~process.() };
		~getDownstreamItems = { |key(\baseItems)|
			var out;
			// ">> % getDownstreamItems".format(~protoID).debug;
			out = key.envirGet.collect { |item| item.value }.flatten(1);
			// "<< % getDownstreamItems".format(~protoID).debug;
			// out.postcs
		};
		~updateDeltas = {
			~deltas = (~items.collect(_[\time]) ++ ~dur).differentiate;
			if(~items[0][\time] == 0) {
				~deltas = ~deltas.drop(1);
			} {
				~deltas[0] = Rest(~items[0][\time]);
				~items = ~items.insert(0, (item: \rest, time: 0, initialRest: true));
			};
			~items.do { |item, i| item[\dur] = ~deltas[i] };
		};
		~asPattern = { |bp|  // the BP() object -- asPattern should be called @ top level only
			var pat, rest, didProcess = false;
			if(~items.isNil) {
				~items = ~process.(bp);
				~updateDeltas.();
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
			~items = ~getDownstreamItems.();
			~items.do { |item, i|
				// includesEqual: SequenceNotes compare pitch only! Use pitch for matching
				if(~replaceItems.includesEqual(item.item)) {
					// copy is critical here!
					~items[i] = item.copy.put(\item, ~seqStream.next);
				};
			};
			~items
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

	PR(\clGen).clone {
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

	PR(\clGen).clone {
		~makeStream = { ~seqStream = Pn(Pshuf(~args[0], 1), inf) };
		~helpString = "\\shuf(\"items\")";
	} => PR(\clGenShuf);

	PR.allOfType(\clGenerator).do { |pr|
		pr[\protoID] = pr.collIndex;
	};
} { AbstractChuckArray.defaultSubType = saveType };
