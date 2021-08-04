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

/*
TempoClock.tempo = 84/60;
/changeKey.(\ddor);

/hpl(debug = true);
/hpl = "[13  ]";

/hpl+
*/
