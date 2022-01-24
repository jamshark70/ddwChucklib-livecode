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
		if(this.isVoicer) {
			if(value[\presets].isNil) {
				value[\presets] = IdentityDictionary.new;
			};
			value[\presets].put(key, presetDef);
		} {
			"Fact(%) is a BP type; presets are not supported"
			.format(~collIndex.asCompileString)
			.warn;
		}
	}

	*savePresets {
		var allPresets = IdentityDictionary.new;
		this.all.do { |fact|
			allPresets.put(fact.collIndex, fact.value[\presets]);
		};
		allPresets.writeArchive(Platform.userConfigDir +/+ "chucklibPresets.txarch");
	}

	// should call after creating Fact objects
	*loadPresets {
		var allPresets = Object.readArchive(Platform.userConfigDir +/+ "chucklibPresets.txarch");
		var failed = IdentitySet.new;
		allPresets.keysValuesDo { |key, presets|
			if(this.exists(key)) {
				this.new(key).value[\presets] = presets;
			} {
				failed.add(key);
			};
		};
		if(failed.size > 0) {
			"These Factories were missing:".postln;
			failed.asArray.sort.postln;
		};
	}
}

+ VC {
	addPreset { |key, presetDef|
		if(env[\presets].isNil) {
			env[\presets] = IdentityDictionary.new;
		};
		env[\presets].put(key, presetDef);
	}
}
