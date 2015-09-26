var saveType = AbstractChuckArray.defaultSubType;

// cl-livecode generator objects

AbstractChuckArray.defaultSubType = \clGenerator;

protect {

	Proto({
		// use Func collection for generator "plugins"
		// i.e., avoid creating a PR "subclass" for every generator func
		~func = \rand;
		~bpKey = nil;  // supply to 'use' this environment while inside the pattern
		~args = nil;   // override with an array

		// instance should override
		~dur = {
			thisThread.clock.tryPerform(\beatsPerBar) ?? { TempoClock.beatsPerBar }
		};
		~tolerance = 0.001;

		// time constraint must be implemented by a separate Routine
		~embedInStream = { |inval|
			var dur = ~dur.(),
			startTime = thisThread.clock.beats,
			next, bpEnvir, rout;
			if(BP.exists(~bpKey)) {  // this also avoids 'nil'
				bpEnvir = BP(~bpKey).v;
			};
			if(dur == inf) {
				// optimize: no time constraint, just embed
				~embedIt.(inval, bpEnvir);
			} {
				// 'rout' is analagous to "pattern.asStream" in, say, Pfin
				rout = Routine { |inval| ~embedIt.(inval, bpEnvir) };
				while { (thisThread.clock.beats - startTime).round(~tolerance) < dur
					and: { (next = rout.next(inval)).notNil }
				} {
					inval = next.yield;
				};
			}
		};
		~embedIt = { |inval, runInEnvir|
			// copy: don't corrupt () in parent event, if it wasn't overridden
			// need local vars because we might use ~args and ~func in a different envir
			var args = ~args.copy.proto_((gen: currentEnvironment)),
			func = ~func;
			// func should return last inval
			if(runInEnvir.notNil) {
				runInEnvir.use { Func(func).v.valueArray(args).embedInStream(inval) }
			} {
				Func(func).v.valueArray(args).embedInStream(inval)
			};
		};
		~asStream = {
			Routine { |inval| ~embedInStream.(inval) }
		};
		~canEmbed = true;  // do not override!

		// code generation
		~protoID = \clGen;
		~isClGen = true;
		~patternStringKeys = #[func, bpKey, args];
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
	}, parentKeys: #[canEmbed, protoID, isClGen, patternStringKeys, patternStringFuncs]) => PR(\clGen);

	// non-default parameter with rhythm generator: Use Pstep
	PR(\clGen).clone({
		~valueGen = 0;
		~rhythmGen = 1;

		~embedInStream = { |inval|
			Pstep(~valueGen.asStream, ~makeRhythmStream.(), 1).embedInStream(inval)
		};
		~makeRhythmStream = {
			var dur = min(~valueGen.dur, ~rhythmGen.dur),
			rStream = ~rhythmGen.asStream;
			if(dur != inf) {
				Pconst(dur, rStream, ~tolerance).asStream;
			} {
				rStream
			};
		};

		~protoID = \clStepGen;
		~patternStringKeys = #[valueGen, rhythmGen];
	}, parentKeys: #[protoID, patternStringKeys]) => PR(\clStepGen);

	// default parm must expose 'dur' separately: Use Ptuple
	PR(\clStepGen).clone({
		~embedInStream = { |inval|
			Ptuple([~valueGen.asStream, ~makeRhythmStream.()], 1).embedInStream(inval)
		};

		~protoID = \clRhythmGen;
	}, parentKeys: #[protoID, patternStringKeys]) => PR(\clRhythmGen);

} { AbstractChuckArray.defaultSubType = saveType };
