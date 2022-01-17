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

// these assume to be run within a chucklib-livecode BP
// the BP supplies ~valueForParm

PCllPattern : FilterPattern {
	var <>handler;
	*new { |pattern, handler|
		^super.newCopyArgs(pattern, handler)
	}

	embedInStream { |inval|
		var stream = pattern.asStream;
		var value;
		while {
			value = stream.next(inval);
			value.notNil
		} {
			inval = this.processValue(value, inval).yield;
		};
		^inval
	}

	// higher level hook, for subclasses handling arrays
	processValue { |valueID, inEvent|
		^this.processOneValue(valueID, inEvent)
	}

	// valueID is the cll representation (character or SequenceNote)
	processOneValue { |valueID, inEvent|
		^handler.valueForParm(valueID, inEvent) ?? { Rest(valueID) }
	}
}

PCllDefaultNoAliasPattern : PCllPattern {
	// key and valueID are arrays
	processValue { |valueID, inEvent|
		var out;
		out = valueID.copy;
		out[0] = this.processOneValue(out[0], inEvent);
		^out
	}
}

PCllSimpleAliasPattern : PCllPattern {
	processValue { |valueID, inEvent|
		var result = this.processOneValue(valueID, inEvent);
		inEvent.put(handler.storeParm, result);
		^result
	}
}

PCllNondefaultArrayAliasPattern : PCllPattern {
	processValue { |valueID, inEvent|
		var result = handler.valueForParm(valueID, inEvent);
		var storeParm = handler.storeParm;
		if(result.isNil) {
			result = [Rest(valueID)];
		} {
			result = result.asArray;
		};
		if(storeParm.size >= result.size) {
			storeParm.do { |key, i|
				inEvent.put(key, result.wrapAt(i));
			};
		} {
			"Alias for % allows % values, but too many (%) were provided"
			.format(storeParm.asCompileString, storeParm.size, result.size)
			.warn;
		};
		^result
	}
}

PCllDefaultArrayAliasPattern : PCllPattern {
	processValue { |valueID, inEvent|
		var result = handler.valueForParm(valueID[0], inEvent), sp0;
		var storeParm = handler.storeParm;
		if(result.isNil) {
			result = [Rest(valueID[0])];
		} {
			result = result.asArray;
		};
		if(storeParm.size >= result.size) {
			storeParm.do { |key, i|
				inEvent.put(key, result.wrapAt(i));
			};
		} {
			"Alias for % allows % values, but too many (%) were provided"
			.format(storeParm.asCompileString, storeParm, result.size)
			.warn;
		};
		// valueID[1] should be the dur value -- necessary to be second return value
		// if 'result' is one item, unbubble reverses the 'asArray' earlier
		^[result.unbubble, valueID[1], valueID[2]]
	}
}


// parameter handlers
CllParmHandlerFactory {
	// the logic to decode the sub-cases is still a bit ugly
	// but at least 1/ it's now concentrated in this one location
	// and 2/ refactored as nested ifs
	*new { |parm, bpKey|
		var bp;
		var map;

		if(BP.exists(bpKey)) {
			bp = BP(bpKey);
		} {
			Error("Can't create parm handler because BP(%) doesn't exist"
			.format(bpKey.asCompileString)).throw;
		};

		map = bp.parmMap[parm];
		if(map.isNil and: { #[delta, dur].includes(parm).not }) {
			Error("BP(%): Can't create handler for nonexistent parm '%'"
				.format(bpKey.asCompileString, parm)).throw;
		};

		if(map.tryPerform(\at, \alias).isNil) {
			// non-aliased cases (2)
			if(bp.parmIsDefault(parm)) {
				^CllDefaultNoAliasParm(parm, bpKey, map)
			} {
				^CllParm(parm, bpKey, map)
			};
		} {
			// aliased cases: simple or arrayed?
			if(map[\alias].size == 0) {
				// simple
				if(bp.parmIsDefault(parm)) {
					^CllDefaultArrayAliasParm(parm, bpKey, map)
				} {
					^CllSimpleAliasParm(parm, bpKey, map)
				}
			} {
				// arrayed
				if(bp.parmIsDefault(parm)) {
					^CllDefaultArrayAliasParm(parm, bpKey, map)
				} {
					^CllNonDefaultArrayAliasParm(parm, bpKey, map)
				}
			};
		}
	}
}

CllParm {
	var <parm, bpKey, map;
	var <storeParm, <patternParm;
	var isDefault;
	var valueLookup, isRest;

	// bpKey should have been validated in the factory
	// I won't recheck it redundantly here
	*new { |parm, bpKey, map|
		[parm, bpKey, map].debug("creating " ++ this.name);
		^super.newCopyArgs(parm, bpKey, map).init
	}

	init {
		isDefault = BP(bpKey).parmIsDefault(parm);
		storeParm = map[\alias] ?? { parm };
		if(isDefault) {
			patternParm = [parm, \delta, \dur];
		} {
			patternParm = parm;
		};
		if(BP(bpKey).parmIsPitch(parm)) {
			valueLookup = \pitchLookup;
			isRest = \pitchIsRest;
		} {
			valueLookup = \dictLookup;
			isRest = \otherIsRest;
		};
	}

	// this will vary based on subclasses
	wrapPattern { |pattern|
		^PCllPattern(pattern, this)
	}

	// these should be consistent:
	// the subcases for value conversion don't depend on aliasing
	// but rather on pitched vs nonpitched
	// I'd rather not explode 5 classes into 10 though
	valueForParm { |event, inEvent|
		^this.perform(valueLookup, event, inEvent)
	}
	pitchLookup { |event, inEvent|
		var convert = map.tryPerform(\at, \convertFunc);
		^if(event.isKindOf(SequenceNote)) {
			// only defaultParm should influence articulation
			if(isDefault) {
				if(event.length <= 0.4) {
					// this must be a function because ~dur is not populated yet!
					inEvent[\sustain] = { min(0.15, ~dur * event.length) };
				} {
					inEvent[\legato] = event.length
				};
				// press args into service for accents, may change the spec later
				if(event.args == \accent) {
					inEvent[\accent] = true;
				};
			};
			// not .asFloat: We already know this is a SeqNote, and asFloat breaks Rests here.
			convert.(event, inEvent) ?? { event.freq }
		} {
			if(event == \rest or: { event.class == Char }) {
				if(isDefault) {
					inEvent[\legato] = 0.9;
				};
				event = Rest(~parmMap[parm][\rest] ?? { 0 });
			};
			convert.(event, inEvent) ?? { event }
		};
	}
	dictLookup { |event, inEvent|
		var result = if(map.notNil) {
			if(map[\convertFunc].notNil) {
				map[\convertFunc].value(event, inEvent)
			} {
				map[event]
			};
		};
		^if(result == \rest or: { event == \rest }) {
			Rest(result ?? { 0 })
		} {
			result  // nil if not specified
		};
	}

	valueIsRest { |event, inEvent|
		^this.perform(isRest, event, inEvent)
	}
	notRest { ^false }
	pitchIsRest { |event|
		^if(event.respondsTo(\freq)) {
			event.freq.isRest
		} {
			// if we get here -- not a SequenceNote -- then it should be a Char
			// if not a Char, assume rest
			event.tryPerform(\isAlpha) ?? { true }
		}
	}
	otherIsRest { |event, inEvent|
		var result;
		^if(map.notNil) {
			result = if(map[\convertFunc].notNil) {
				map[\convertFunc].(event, inEvent);
			} {
				map[event]
			};
			result.isRest or: { result.isNil }
		} {
			event.isRest
		}
	}
}

CllDefaultNoAliasParm : CllParm {
	wrapPattern { |pattern|
		^PCllDefaultNoAliasPattern(pattern, this)
	}
}

CllSimpleAliasParm : CllParm {
	wrapPattern { |pattern|
		^PCllSimpleAliasPattern(pattern, this)
	}
}

CllDefaultArrayAliasParm : CllParm {
	wrapPattern { |pattern|
		^PCllDefaultArrayAliasPattern(pattern, this)
	}
}

CllNonDefaultArrayAliasParm : CllParm {
	wrapPattern { |pattern|
		^PCllNondefaultArrayAliasPattern(pattern, this)
	}
}
