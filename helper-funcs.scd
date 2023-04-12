
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
	strings, stepForward, doc, pos, method;
	meters = meters.asArray;
	strings = phrases.collect { |phrase, i|
		"/%.% = %\"\";".format(key, phrase, meters.wrapAt(i) ?? { "" })
	};
	phrases.do { |phrase, i|
		\chucklibLiveCode.eval(strings[i]).interpret;
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
			"\n" ++ strings.join("\n"),
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

{ |mode = \default|
	if(Mode.exists(mode)) {
		"changeKey: Mode(%) => Mode('default')\n".postf(mode.asCompileString);
		Mode(mode) => Mode(\default);
	} {
		"changeKey: Mode(%) does not exist".format(mode.asCompileString).warn;
	};
} => Func(\changeKey);

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

// intended for use in \preset
{ |bp|
	if(BP.exists(bp)) {
		BP(bp).event[\voicer]
	} {
		Error("BP(%) doesn't exist, so, can't find voicer".format(bp.asCompileString)).throw;
	}
} => Func(\bpAsVoicer);

{ |bp|
	var voicer = \bpAsVoicer.eval(bp);
	var vc;
	if(voicer.isNil) {
		Error("\\preset: BP(%) doesn't have a voicer".format(bp.asCompileString)).throw;
	};
	vc = VC.collection.detect { |vc| vc.value === voicer };
	if(vc.isNil) {
		Error("\\preset: BP(%)'s voicer can't be found in VC".format(bp.asCompileString)).throw;
	};
	vc
} => Func(\bpAsVC);

{ |preset, bp|
	var vc, presetDef, proc;
	var gcs;
	vc = \bpAsVC.eval(bp);
	presetDef = vc.env[\presets].tryPerform(\at, preset) ?? {
		// vc.env[\collIndex] is the Factory's collIndex
		Library.at(\cl, \presets, vc.env[\collIndex], preset)
	};
	if(presetDef.isNil) {
		Error("\\preset: VC(%) does not define preset %".format(
			vc.collIndex.asCompileString,
			preset.asCompileString
		)).throw;
	};
	proc = BP(bp);  // already validated by \bpAsVC
	gcs = vc.v.globalControls;
	// pairsDo supports both SeqColl and Dictionary
	presetDef.pairsDo { |key, value|
		switch(key)
		{ \makeParms } { nil }  // no-op
		{ \postAction } {
			vc.use { value.value(vc) }
		}
		{
			proc.set(key, value);
			// presets should be able to override globalcontrol defaults
			// but not with patterns
			if(gcs[key].notNil and: { value.isNumber }) {
				gcs[key].value = value
			};
		};
	};
	proc
} => Func(\preset);

{ |parent, bounds(Rect(800, 200, 500, 400))|
	var view = TreeView(parent, bounds);
	var watcher;
	view.columns_(["ID", "preset"]).setColumnWidth(0, 140);
	if(parent.isNil) { view.front };

	// populate
	Fact.keys.as(Array).sort.do { |key|
		var fact = Fact(key);
		var presets, factoryItem;
		if(fact.isVoicer) {
			presets = fact.presets;
			if(presets.size > 0) {
				factoryItem = view.addChild([fact.collIndex.asString]);
				presets.keys.as(Array).sort.do { |presetKey|
					var str = String.streamContentsLimit({ |stream|
						fact.presetAt(presetKey).printOn(stream)
					}, 50);
					factoryItem.addChild([presetKey.asString, str]);
				};
			};
		};
	};

	// this is absolutely not modularized in a good way
	watcher = SimpleController(Fact)
	.put(\addPreset, { |obj, what, factKey, presetKey|
		// queue these? or assume the clock will do it
		defer {
			var fIndex, pIndex, fItem, pItem;
			var fKeyStr = factKey.asString;
			var pKeyStr;
			var presetStr = String.streamContentsLimit({ |stream|
				Fact(factKey).presetAt(presetKey).printOn(stream)
			}, 50);

			fIndex = block { |break|
				view.numItems.do { |i|
					if(view.itemAt(i).strings[0] == fKeyStr) {
						break.(i);
					}
				};
				nil
			};
			if(fIndex.notNil) {
				pKeyStr = presetKey.asString;
				fItem = view.itemAt(fIndex);
				// super-oldskool while loop
				// because there's no 'numChildren' method for TreeViewItem
				pIndex = 0;
				while {
					fItem.childAt(pIndex).notNil and: {
						fItem.childAt(pIndex).strings[0] != pKeyStr
					}
				} {
					pIndex = pIndex + 1;
				};
				pItem = fItem.childAt(pIndex);
				if(pItem.notNil) {
					pItem.strings = [pKeyStr, presetStr]
				} {
					fItem.addChild([pKeyStr, presetStr]);
				}
			} {
				fItem = view.addChild([fKeyStr]);
				fItem.addChild([pKeyStr, presetStr]);
			};
		}
	});

	view.onClose = { watcher.remove };
	view.beginDragAction = { |view|
		var item = view.currentItem;
		var str, parent;
		if(item.notNil) {
			str = item.strings;
			// child item or parent item?
			if(str.size >= 2) {
				parent = item.parent;
				"/make(%:name(preset:%%)/".format(
					parent.strings[0],
					$\\,
					item.strings[0]
				);
			} { nil }
		} { nil }
	};

	view
} => Func(\presetBrowser);

// record several mixers with sample-accurate onset
// for simplicity/livecode use, create a folder in recordingsDir
{ |... mixers|
	var path, result, servers;
	{
		path = Platform.recordingsDir +/+ "tracks_" ++ Date.getDate.stamp;
		result = File.mkdir(path);
		if(result) {
			servers = mixers.collect(_.server).as(Set);
			if(servers.size > 1) {
				"Recording on multiple servers; sample-accurate sync is not guaranteed".warn;
			};
			mixers.do { |mixer|
				mixer.prepareRecord(path +/+ mixer.name ++ ".aiff", "AIFF");
			};
			servers.do(_.sync);
			servers.do { |server|
				server.makeBundle(0.2, {
					mixers.do { |mixer|
						if(mixer.server == server) {
							mixer.startRecord;
						};
					};
				});
			};
		} {
			"Directory '%' is not empty; not recording".format(path).warn;
		};
	}.fork(SystemClock);
} => Func(\syncRecord);

{
	MixerChannel.servers.do { |mixers|
		mixers.do { |mixer|
			if(mixer.isRecording) { mixer.stopRecord };
		};
	};
} => Func(\stopRecord);


// finally publish my GUI functions
{ |controller(\nanoTouch), midiDevice("The default search string should fail")|  // or \mix16Touch
	var sBounds = Window.screenBounds, touchExtent, w, lay, touchParent, v, left,
	midiIndex,
	path = Platform.userAppSupportDir +/+ "cll-cheatsheet.txt",
	file, str;

	if(File.exists(path)) {
		file = File(path, "r");
		if(file.isOpen) {
			protect {
				str = file.readAllString;
			} { file.close }
		} {
			"can't open cheatsheet.txt".warn;
		};
	};

	if(MBM.exists(0).not) {
		MIDIClient.init;
		midiIndex = MIDIClient.sources.detectIndex { |endpt|
			endpt.device.containsi(midiDevice)
		};
		MIDIPort.init(midiIndex.asArray);
		MIDIBufManager(nil, 0) => MBM.prNew(0);
	};

	BP(#[touchGui, touch]).free;
	PR(controller).chuck(BP(\touch), nil, (pingDebug: false));

	touchExtent = PR(\abstractTouchGUI).calcExtent(BP(\touch).v);
	w = Window("control panel",
		Rect(sBounds.width - touchExtent.x, 0, touchExtent.x, sBounds.height)
	);
	lay = VLayout(
		touchParent = View().fixedSize_(touchExtent + Point(-4, 4)),
		StaticText().fixedHeight_(3),  // spacer
		PR(\controlList) => BP(\clist),
	).margins_(2).spacing_(4);
	if(str.notNil) {
		lay.add(StaticText().fixedHeight_(3));  // spacer
		v = TextView().string_(str)
		// for some stupid reason, I have to set the text before colors
		.background_(Color.black).stringColor_(Color.white);
		lay.add(v);
		lay.add(Button(/*w, Rect(2, 2, w.view.bounds.width - 4, 20)*/)
			.states_([["save"]])
			.action_({
				var file = File(path, "w");
				protect {
					if(file.isOpen) {
						file.putString(v.string);
					} {
						"can't open cheatsheet.txt for writing".warn;
					};
				} { file.close };
			})
		);
	};

	w.layout = lay;

	PR(\abstractTouchGUI).chuck(BP(\touchGui), nil, (
		model: BP(\touch).v,
		parentView: touchParent
	));
	PR(\chuckOSC) => BP(\chuckOSC);
	t = BP(\chuckOSC).v;  // must chuck into the proto, not the BP
	w.front;

	~cleanup = {
		var dontFree = #[chuckOSC, touchGui, touch, clist];
		VC.all.free;
		BP.all.do { |bp|
			if(dontFree.includes(bp.collIndex).not) {
				bp.free;
			};
		};
	};

	NotificationCenter.notify(\clInterface, \ready);
	t
} => Func(\makeController);

{
	var sBounds = Window.screenBounds, touchExtent = PR(\abstractTouchGUI).calcExtent(BP(\touch).v),
	left;

	PR(\clGuiString).doHighlighting = false;  // like setting a classvar
	left = max(180, 0.5 * (sBounds.width - 800));
	~editWindow = Window("code editor",
		// Rect.aboutPoint(sBounds.center, min(400, sBounds.width * 0.5), sBounds.height * 0.5)
		// -8? Apparently border is extra
		Rect(left, 0, sBounds.width - touchExtent.x - left - 8, sBounds.height)
	);
	~editWindow.layout = VLayout(
		HLayout(
			nil,
			Button().fixedSize_(Size(80, 20))
			.states_([
				["autosave"],
				["autosave", Color.white, Color.green(0.45)]
			])
			.action_(inEnvir { |view|
				~editor.view.autoSave = view.value > 0;
			}),
			Button().fixedSize_(Size(80, 20))
			.states_([["load"]])
			.action_(inEnvir {
				~editor.view.doAutoSave;  // if window is not currently empty
				FileDialog(inEnvir { |path|
					var file, str;
					file = File(path, "r");
					if(file.isOpen) {
						protect {
							str = file.readAllString;
						} {
							file.close;
						};
						~editor.view.setString(str, 0, ~editor.view.str.size);
					} {
						"Error opening '%'".format(path).warn;
					};
				}, fileMode: 1, acceptMode: 0, stripResult: true,
				path: ~editor.view.autoSavePath
				);
			})
		)
	);
	// I don't really need a model, but the GUI needs it
	~editModel = PR(\clGuiSection).copy
	.defaultString_("")
	.putAll(PR(\clEditGui).v.getTextProperties)
	.prep;
	~editor = PR(\clGuiSectionView).copy.prep(~editModel, ~editWindow.layout, nil);
	~editor.view.autoSave = false;
	// experimental hack: watch for focus -- must do this before .front!
	~focusWatcher = SimpleController(~editor.view)
	// this should be OK for multiple views
	// because the sequence is always a/ lose old focus, then b/ gain new focus
	.put(\focused, { |view, what, bool|
		if(bool) {
			PR(\clPatternToDoc).activeView = view.view;
		} {
			PR(\clPatternToDoc).activeView = nil;  // clear to revert to Document
		};
	})
	.put(\didFree, inEnvir { ~focusWatcher.remove });
	~editWindow.onClose_(inEnvir { ~editModel.free }).front;
	Library.put(\clLivecode, \setupbars, \addToDoc, false);
	~editor
} => Func(\makeCodeWindow);

{ |controller(\nanoTouch), midiDevice|
	\makeController.eval(controller, midiDevice);
	\makeCodeWindow.eval;
} => Func(\cllGui);

{ |vcKey, index = 0, exclude(#[]), overwrite = true|
	var t = thisProcess.interpreter.t;
	var i = index;
	if(VC.exists(vcKey)) {
		block { |break|
			VC(vcKey).globalControlsByCreation.do { |gc|
				// also check i < size
				if(i >= t.faderKeys.size) {
					"VC(%).globalControls[%]: Ran out of GUI slots"
					.format(vcKey.asCompileString, gc.name.asCompileString)
					.warn;
					break.(nil)
				};
				if(overwrite or: { t.maps[t.faderKeys[i]].isNil }) {
					if(exclude.includes(gc.name).not) {
						gc.chuck(t, i);
						i = i + 1;
					};
				};
			};
		};
	} {
		"gcChuck: VC(%) doesn't exist".format(vcKey.asCompileString).warn;
	};
} => Func(\gcChuck);


{ |str, i|
	var match;
	var alphaNum = { |chr|
		chr.respondsTo(\isAlphaNum) and: { chr.isAlphaNum }
	};
	// allow cursor to trail the identifier
	if(alphaNum.(str[i]).not) {
		i = i - 1
	};
	while { i > 0 and: { alphaNum.(str[i]) } } {
		i = i - 1;
	};
	// now we should be one char before the symbol
	i = i + 1;
	match = str.findRegexpAt("([A-Za-z0-9]+)", i);
	[i, match]
} => Func(\strGetIdentifier);
