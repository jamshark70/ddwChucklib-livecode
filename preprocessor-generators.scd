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

		~convertPitch = { |next|
			var pitch;
			pitch = ~decodePitch.(next);
			if(pitch.notNil) {
				[pitch.key, pitch.value]  // [degree, legato]
			} {
				"Generator produced pitch string '%', could not decode"
				.format(next).warn;
				nil
			};
		};

		~embedInStream = { |inval|
			var dur = ~dur.(),
			startTime = thisThread.clock.beats,
			next, pitch, bp, rout;
			if(BP.exists(~bpKey)) {  // this also avoids 'nil'
				bp = BP(~bpKey);
			};

			if(~isPitch.()) {
				~asPattern.(bp).collect(~convertPitch).embedInStream(inval);
			} {
				~asPattern.(bp).embedInStream(inval);
			};
		};
		~asStream = {
			Routine { |inval| ~embedInStream.(inval) }
		};
		~canEmbed = true;  // do not override!

		~yieldsKeys = {
			if(~isPitch.()) { [~parm, \legato] } { ~parm }
		};

		// code generation
		~protoID = \clGen;
		~isClGen = true;
		~patternStringKeys = #[func, bpKey, args, dur, isPitch, parm];
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
	} => PR(\clGenSeq);

	PR(\clGen).clone {
		~asPattern = { Prand(~args[0], inf) };
	} => PR(\clGenRand);

	PR(\clGen).clone {
		~asPattern = { Pxrand(~args[0], inf) };
	} => PR(\clGenXrand);

	PR(\clGen).clone {
		~asPattern = {
			var weights = ~args[1..].normalizeSum;
			Pwrand((1 .. weights.size), weights, inf) * ~args[0]
		};
	} => PR(\clGenRandRhy);

	// non-default parameter with rhythm generator: Use Pstep
	PR(\clGen).clone({
		~valueGen = 0;
		~rhythmGen = 1;

		~embedInStream = { |inval|
			Pstep(~valueGen.asStream, ~rhythmGen.asStream, 1).embedInStream(inval)
		};

		~protoID = \clStepGen;
		~patternStringKeys = #[valueGen, rhythmGen, dur, isPitch, parm];
	}, parentKeys: #[protoID, patternStringKeys]) => PR(\clStepGen);

	// if isPitch, Ptuple yields [[pitch, legato], dur]
	// but it's not necessarily safe to just "flat"ten it
	~convertPitch = { |next| next[0] ++ next[1] };

	// default parm must expose 'dur' separately: Use Ptuple
	PR(\clStepGen).clone({
		~embedInStream = { |inval|
			var pat = Ptuple([~valueGen.asStream, ~rhythmGen.asStream], 1);
			if(~isPitch.()) { pat = pat.collect(~convertPitch) };
			pat.embedInStream(inval)
		};

		~protoID = \clRhythmGen;

		~yieldsKeys = {
			if(~isPitch.()) { [~parm, \legato, \dur] } { [~parm, \dur] };
		};
	}, parentKeys: #[protoID, patternStringKeys]) => PR(\clRhythmGen);

	PR.allOfType(\clGenerator).do { |pr|
		pr[\protoID] = pr.collIndex;
	};
} { AbstractChuckArray.defaultSubType = saveType };