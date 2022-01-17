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
	var <>parm, <>storeParm, <>inParm;
	*new { |pattern, parm, storeParm, inParm|
		^super.newCopyArgs(pattern, parm, storeParm, inParm)
	}

	embedInStream { |inval|
		var stream = pattern.asStream;
		var value;
		while {
			value = stream.next(inval);
			value.notNil
		} {
			inval = this.processValue(parm, value, inval).yield;
		};
		^inval
	}

	// higher level hook, for subclasses handling arrays
	processValue { |key, valueID, inEvent|
		^this.processOneValue(key, valueID, inEvent)
	}

	// valueID is the cll representation (character or SequenceNote)
	processOneValue { |key, valueID, inEvent|
		^ ~valueForParm.(valueID, key, inEvent) ?? { Rest(valueID) }
	}
}

PCllArrayPattern : PCllPattern {
	// key and valueID are arrays
	processValue { |key, valueID, inEvent|
		^valueID.collect { |value, i|
			this.processOneValue(key[i], value, inEvent)
		}
	}
}

PCllSimpleAliasPattern : PCllPattern {
	processValue { |key, valueID, inEvent|
		var result = this.processOneValue(key, valueID, inEvent);
		inEvent.put(storeParm, result);
		^result
	}
}

PCllNondefaultArrayAliasPattern : PCllPattern {
	processValue { |key, valueID, inEvent|
		var result = ~valueForParm.(valueID, inParm, inEvent);
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
			.format(parm.asCompileString, storeParm.size, result.size)
			.warn;
		};
		^result
	}
}

PCllDefaultArrayAliasPattern : PCllPattern {
	processValue { |key, valueID, inEvent|
		var result = ~valueForParm.(valueID[0], inParm, inEvent), sp0;
		if(result.isNil) {
			result = [Rest(valueID[0])];
		} {
			result = result.asArray;
		};
		sp0 = storeParm[0].asArray;
		if(sp0.size >= result.size) {
			sp0.do { |key, i|
				inEvent.put(key, result.wrapAt(i));
			};
		} {
			"Alias for % allows % values, but too many (%) were provided"
			.format(parm.asCompileString, sp0, result.size)
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
				^CllArrayParm(parm, bpKey, map)
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
	var parm, bpKey, map;
	var isDefault;
	var valueLookup, isRest;

	// bpKey should have been validated in the factory
	// I won't recheck it redundantly here
	*new { |parm, bpKey, map|
		^super.newCopyArgs(parm, bpKey, map).init
	}

	init {
		isDefault = BP(bpKey).parmIsDefault(parm);
		case
		{ #[delta, dur].includes(parm) } {
			valueLookup = \passThrough;
			isRest = \notRest;
		}
		{ BP(bpKey).parmIsPitch(parm) } {
			valueLookup = \pitchLookup;
			isRest = \pitchIsRest;
		}
		{
			valueLookup = \dictLookup;
			isRest = \otherIsRest;
		};
		this.init2;
	}
	init2 {}

	// this will vary based on subclasses
	wrapPattern { |pattern, parm, storeParm, inParm|
		^PCllPattern(pattern, parm, storeParm, inParm)
	}

	// these should be consistent:
	// the subcases for value conversion don't depend on aliasing
	// but rather on pitched vs nonpitched
	// I'd rather not explode 5 classes into 10 though
	valueForParm { |event, parm, inEvent|
		^this.perform(valueLookup, event, parm, inEvent)
	}
	passThrough { |value| ^value }
	pitchLookup { |event, parm, inEvent|
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
	dictLookup { |event, parm, inEvent|
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

	valueIsRest { |event, parm, inEvent|
		^this.perform(isRest, event, parm, inEvent)
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
	otherIsRest { |event, parm, inEvent|
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

CllArrayParm : CllParm {
	wrapPattern { |pattern, parm, storeParm, inParm|
		^PCllArrayPattern(pattern, parm, storeParm, inParm)
	}
}

CllSimpleAliasParm : CllParm {
	wrapPattern { |pattern, parm, storeParm, inParm|
		^PCllSimpleAliasPattern(pattern, parm, storeParm, inParm)
	}
}

CllDefaultArrayAliasParm : CllParm {
	wrapPattern { |pattern, parm, storeParm, inParm|
		^PCllDefaultArrayAliasPattern(pattern, parm, storeParm, inParm)
	}
}

CllNonDefaultArrayAliasParm : CllParm {
	wrapPattern { |pattern, parm, storeParm, inParm|
		^PCllNondefaultArrayAliasPattern(pattern, parm, storeParm, inParm)
	}
}
