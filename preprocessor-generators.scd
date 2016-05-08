var saveType = AbstractChuckArray.defaultSubType;

// cl-livecode generator objects

AbstractChuckArray.defaultSubType = \clGenerator;

protect {

	Proto({
		// use Func collection for generator "plugins"
		// i.e., avoid creating a PR "subclass" for every generator func
		// ~func = \rand;
		~bpKey = nil;  // supply to 'use' this environment while inside the pattern
		~args = nil;   // override with an array
		~parm = \value;

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
			~parm
			// if(~isPitch.()) { [~parm, \legato] } { ~parm }
		};

		// code generation
		~protoID = \clGen;
		~isClGen = true;
		~patternStringKeys = #[bpKey, args, dur, isPitch, parm];
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
			stream << "))";
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
