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

// "decodes" pairs of [value, delta] into step sequences, for non-default parameters

PstepDurPair : Pstep {
	var <>tolerance;

	*new { |pairs, repeats = 1, tolerance = 0.001|
		^super.newCopyArgs(pairs, nil, repeats).init.tolerance_(tolerance);
	}

	embedInStream { |inval|
		var itemStream, durStream, pair, item, dur, nextChange = 0, elapsed = 0;
		repeats.value(inval).do {
			itemStream = list.asStream;
			while {
				pair = itemStream.next(inval);
				if(pair.notNil) {
					#item, dur = pair;
					item.notNil and: { dur.notNil }
				} { false }  // terminate if stream was nil
			} {
				nextChange = nextChange + dur;
				// 'elapsed' increments, so nextChange - elapsed will get smaller
				// when this drops below 'tolerance' it's time to move on
				while { (nextChange - elapsed) >= tolerance } {
					elapsed = elapsed + inval.delta;
					inval = item.embedInStream(inval);
				};
			};
		};
		^inval
	}

	// not properly part of a pattern, but I need to install hooks to load the environment
	// then you can do e.g. \loadCl.eval
	*initClass {
		Class.initClassTree(AbstractChuckArray);
		Class.initClassTree(Library);
		Library.put(\cl, \path, this.filenameSymbol.asString.dirname);
		Library.put(\cl, \files, [
			"preprocessor.scd", "preprocessor-generators.scd",
			"helper-funcs.scd"
		]);
		Library.put(\cl, \extras, ["edit-gui.scd", "mobile-objects.scd", "nanoktl-objects.scd"]);
		if(File.exists(Quarks.folder +/+ "ddwLivecodeInstruments")) {
			Library.put(\cl, \instr, (Quarks.folder +/+ "ddwLivecodeInstruments/*.scd").pathMatch);
			{
				(Quarks.folder +/+ "ddwLivecodeInstruments/*.scd").pathMatch.do { |path|
					path.loadPath
				};
			} => Func(\loadClInstr);
		};

		{ |files|
			var dir = Library.at(\cl, \path);
			files.do { |name| (dir +/+ name).loadPath };
		} => Func(\loadClFiles);

		{ \loadClFiles.eval(Library.at(\cl, \files)) } => Func(\loadCl);
		{ \loadClFiles.eval(Library.at(\cl, \extras)) } => Func(\loadClExtras);
		{ #[loadCl, loadClExtras, loadClInstr].do(_.eval) } => Func(\loadAllCl);
	}
}


// used in clGenSeq for matching items

+ Rest {
	== { |that|
		if(that.isKindOf(Rest).not) { ^false };
		^(this.value == that.value)
	}
}

+ Object {
	processRest {}
}


// preset support
+ Fact {
	addPreset { |key, presetDef|
		Library.put(\cl, \presets, this.collIndex, key, presetDef);
		Fact.changed(\addPreset, this.collIndex, key);
	}

	presets {
		^Library.at(\cl, \presets, this.collIndex)
	}

	presetAt { |key|
		^Library.at(\cl, \presets, this.collIndex, key)
	}

	*savePresets { |force = false|
		var write = {
			Library.at(\cl, \presets).writeArchive(Platform.userConfigDir +/+ "chucklibPresets.txarch");
		};
		if(force) {
			write.value
		} {
			if(Library.at(\cl, \presetsLoaded) == true) {
				write.value
			} {
				"Presets not previously loaded from disk. Load first to avoid data loss".warn;
			}
		}
	}

	*loadPresets {
		var allPresets = Object.readArchive(Platform.userConfigDir +/+ "chucklibPresets.txarch");
		var curPresets = Library.at(\cl, \presets) ?? {
			IdentityDictionary.new
		};
		var conflicts = IdentityDictionary.new;
		allPresets.keysValuesDo { |factName, presets|
			if(curPresets[factName].isNil) {
				curPresets[factName] = presets;
			} {
				presets.keysValuesDo { |presetName, values|
					if(curPresets[factName][presetName].isNil) {
						curPresets[factName][presetName] = values;
					} {
						conflicts[factName] = conflicts[factName].add(presetName);
					};
				};
			};
		};
		if(conflicts.notEmpty) {
			"The following presets loaded from disk already existed in memory.
The memory version is retained; the disk version was not loaded.".warn;
			conflicts.keysValuesDo { |factName, presetKeys|
				"Fact(%): %\n".postf(factName.asCompileString, presetKeys);
			};
		};
		Library.put(\cl, \presets, allPresets);
		Library.put(\cl, \presetsLoaded, true);
	}
}

// local only, not preserved with Factories
+ VC {
	addPreset { |key, presetDef|
		if(env[\presets].isNil) {
			env[\presets] = IdentityDictionary.new;
		};
		env[\presets].put(key, presetDef);
	}
}
