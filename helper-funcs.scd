
Library.put(\clLivecode, \setupbars, \addToDoc, Main.versionAtLeast(3, 7) and: { Platform.ideName == "scqt" });
// common helper funcs
// when GUI window is front, /bars.(...) should NOT affect current document
Library.put(\clLivecode, \setupbars, \guiIsFront, false);

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


{
	// Library.at(\clLivecode, \setupbars, \addToDoc) ?? { false }
	// and: {
	// 	not(Library.at(\clLivecode, \setupbars, \guiIsFront) ?? { false })
	// }
	true
} => Func(\clBarFuncMayAddToDoc);

{ |key, n(4), prefix("m"), meters|
	var phrases = [[prefix], (0 .. n-1)].flop.collect(_.join),
	stepForward, doc, pos, method;
	meters = meters.asArray;
	phrases.do { |phrase, i|
		\chucklibLiveCode.eval("/%.% = %\"\";".format(key, phrase, meters.wrapAt(i) ?? { "" })).interpret;
	};
	if(\clBarFuncMayAddToDoc.eval) {
		stepForward = PR(\clPatternToDoc)[\stepForward];
		if(stepForward.isNil) {
			stepForward = PR(\clPatternToDoc).checkStepForward;
		};

		doc = PR(\clPatternToDoc).currentDoc;
		pos = doc.selectionStart + doc.selectionSize;
		method = if(doc.isKindOfByName('Document')) { 'string_' } { 'setString' };
		if(stepForward.not and: { doc.isKindOfByName('Document') }) { pos = pos + 1 };
		doc.perform(method,
			"\n" ++ (phrases.collect { |phr| "/%.% = \"\";\n".format(key, phr) }.join),
			pos, 0
		);
	} {
		"Added " ++ phrases
	}
} => Func(\setupbars);

{ |key, n(4), prefix("m"), phrases|
	var argPhrases = phrases, str;
	phrases ?? {
		phrases = [[prefix], (0 .. n-1)].flop.collect(_.join);
	};
	str = "/% = (%)".format(
		key,
		// '^prefix' supports easy variations:
		// /proc.m00 = ...
		// /proc.m01 = ... and so on
		phrases.collect { |phr| "'^%'".format(phr) }.join($.),
	);
	if(argPhrases.isNil) { str = "%(%)".format(str, n.neg) };  // set BP's quant
	\chucklibLiveCode.eval(str).interpret;
} => Func(\setm);

{ |key, n(4), prefix("m")|
	\setm.eval(key, n, prefix);
	\setupbars.eval(key, n, prefix);
} => Func(\bars);

// setMeterAtBeat is unnecessarily hard to use
// if last 2 are omitted, it will wait until the next barline to switch the meter
{ |beatsPerBar, clock(TempoClock.default), barBeat(clock.nextBar), newBaseBarBeat(barBeat)|
	clock.schedAbs(barBeat, {
		clock.setMeterAtBeat(beatsPerBar, newBaseBarBeat);
		nil
	});
} => Func(\changeMeter);

{ |tempo, clock(TempoClock.default), barBeat(clock.nextBar)|
	clock.schedAbs(barBeat, {
		if(clock.isKindOfByName('BeaconClock')) {
			clock.setGlobalTempo(tempo, barBeat);
		} {
			clock.tempo = tempo;
		};
		nil
	});
} => Func(\changeTempo);

// assign-voicer functions
{ |vcKey, bpKey|
	if(bpKey.notNil) {
		if(BP.exists(bpKey)) {
			VC(vcKey) => BP(bpKey);
		} {
			"VC(%) should go into BP(%), but it doesn't exist. Remember to => manually"
			.format(
				vcKey.asCompileString,
				bpKey.asCompileString
			)
			.warn;
		};
	};
} => Func(\vcToDefaultBP);

// apply a voicer to the default event prototype
{ |ev|
	var mixer;
	if(ev[\voicer].notNil) {
		ev.put(\instrument, ev[\voicer].nodes[0].defname);
		mixer = ev[\voicer].bus.asMixer;
		if(mixer.notNil) {
			ev.put(\group, mixer.synthgroup)
			.put(\out, mixer.inbus.index);
		};
		ev[\voicer].nodes[0].initArgs.pairsDo { |key, value|
			// difficult. If the event has a default, the event's default takes precedence.
			if(ev[key].isNil) { ev.put(key, value) };
		};
		ev[\voicer].globalControls.keysValuesDo { |key, value|
			ev.put(key, value.asMap);
		};
	};
	ev
} => Func(\voicer);
