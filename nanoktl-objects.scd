// nanoKontrol interface objects

var saveSubtype = AbstractChuckArray.defaultSubType;

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

if(PR.exists(\abstractTouch).not) {
	(thisProcess.nowExecutingPath.dirname +/+ "mobile-objects.scd").load;
};

protect {
	PR(\mix16Touch).clone {
		~uid = nil;
		~chan = nil;
		// flat array of pairs: ccnum, oscpath, ccnum, oscpath etc.
		// for GUI definition below, zero-padding the index is important!
		~pathTable = [
			[108, 109, 110, 111, 114, 115, 116, 117, 5, 80, 82, 83, 85, 104, 106, 105],
			[21, 22, 23, 24, 25, 26, 27, 28, 2, 3, 4, 6, 8, 9, 11, 12],
		].collect { |row, i|
			var prefix = #["/1/fader", "/1/toggle"][i];
			row.collect { |ccnum, j| [ccnum, (prefix ++ (j+1).asPaddedString(2)).asSymbol] }
		}.flat;
		~keepAlive = false;
		~prep = {
			~pathsForCC = Array.newClear(128);
			~pathTable.pairsDo { |ccnum, path|
				~pathsForCC[ccnum] = path;
			};
			if(MIDIClient.initialized.not) { MIDIIn.connectAll };
			~resp = MIDIFunc.cc(inEnvir { |value, ccnum, chan|
				var path;
				path = ~pathsForCC[ccnum];
				if(path.notNil) {
					// downstream may use 'nil' to detect that it's coming from MIDI
					~respond.([path, value / 127.0], SystemClock.seconds, nil, nil)
				};
			}, chan: ~chan, srcID: ~uid);
			~data = IdentityDictionary.new;  // save all incoming data by oscpath
			~syncSigns = IdentityDictionary.new;
			if(~labels.isNil) { ~labels = IdentityDictionary.new };
			~setDataKeys.();
			currentEnvironment
		};
		~freeCleanup = {
			~resp.free;
			NotificationCenter.notify(currentEnvironment, \modelWasFreed);
		};

		~setLabel = { |oscpath, label|
			if(label.isNil) { label = "" /*oscpath.asString.split($/).last*/ };
			~labels[oscpath] = label;
			NotificationCenter.notify(currentEnvironment, \any, [[\label, oscpath, label]]);
			currentEnvironment
		};

		// setValue and related methods supply local address
		// so, if there's an address, we know we should override saved value
		// if replyAddr is nil, the new value comes from MIDI
		// and we should sync up if there's a saved value

		~respond = { |msg, time, replyAddr, recvPort, guiOnly(false)|
			if(msg[0].asString[3] == $f) {
				~respondFader.(msg, time, replyAddr, recvPort, guiOnly);
			} {
				~respondToggle.(msg, time, replyAddr, recvPort, guiOnly);
			};
		};

		// syncSigns:
		// nil = not synced, check before changing real value
		// 0 = synced, pass value through
		// -1 = MIDI value is less than old value
		~respondFader = { |msg, time, replyAddr, recvPort, guiOnly(false)|
			var args = [msg, time, replyAddr, recvPort];
			var sign, data;
			// [msg, time, replyAddr, recvPort, guiOnly].debug("respond");
			if(replyAddr.notNil) {
				if(
					(msg[1] ?? { -2 }) absdif: (~data[msg[0]] ?? { -3 }) > 0.007  // 1/127 ~= 0.00787
				) {
					~syncSigns[msg[0]] = nil;
				};
				~prUpdateControl.(msg, args, guiOnly);
			} {
				// MIDI: check sync
				sign = ~syncSigns[msg[0]];
				case
				{ sign == 0 } {
					~prUpdateControl.(msg, args, guiOnly);
				}
				{ sign.isNil } {
					data = ~data[msg[0]];
					if(data.notNil) {
						~syncSigns[msg[0]] = sign(msg[1] - data);
					} {
						// no existing data, MIDI is first to set
						~syncSigns[msg[0]] = 0;
						~prUpdateControl.(msg, args, guiOnly);
					}
				}
				{
					// sync should get close or cross over
					data = ~data[msg[0]];
					if(data absdif: msg[1] <= 0.016 or: {  // 2/127 ~= 0.016
						sign(msg[1] - data) != sign
					}) {
						~syncSigns[msg[0]] = 0;
						~prUpdateControl.(msg, args, guiOnly);
					};
				};
			};
		};

		// toggle:
		// if never used before (nothing in ~data), assume we are switching on
		// otherwise read value and flip
		~respondToggle = { |msg, time, replyAddr, recvPort, guiOnly(false)|
			var args = [msg, time, replyAddr, recvPort];
			var old;
			if(replyAddr.notNil) {  // again, if setting from client (not MIDI)
				~prUpdateControl.(msg, args, guiOnly);
			} {
				old = ~data[msg[0]];
				if(old.isNil) {
					msg[1] = 1;  // assume flipping on
				} {
					msg[1] = (old == 0).asInteger;
				};
				~prUpdateControl.(msg, args, guiOnly);
			};
		};

		~prUpdateControl = { |msg, args, guiOnly(false)|
			if(~saveKeys.includes(msg[0])) {
				~saveValueFromMsg.(msg);
			};
			NotificationCenter.notify(currentEnvironment, \any, args);
			if(guiOnly.not) {
				NotificationCenter.notify(currentEnvironment, msg[0], args);
			};
		};

		~saveValueFromMsg = { |msg|
			if(msg.size == 2) {
				// "saveValueFromMsg".debug;
				~data[msg[0]] = msg[1] //.dump
			} {
				~data[msg[0]] = msg[1..];
			};
		};

		~fullWidth = 200;
		~sliderWidth = ~fullWidth - (2 * (~buttonExtent.x + ~gap.x));
		~tabSpecs = [
			"Tab1", {
				var out = Array(64),  // 8 rows: button, slider, button, knob with names
				origin = ~gap.copy,
				oneBound = Rect(0, 0, ~sliderWidth, ~buttonExtent.y),
				buttonExtent = ~buttonExtent,
				gap = ~gap.y,
				// must save colors because GUI object init runs in another proto
				ltAqua = ~ltAqua, aqua = ~aqua,
				green = ~green, ltGreen = ~ltGreen;
				8.do { |i|
					out.add("/1/toggle%".format((i+1).asPaddedString(2)).asSymbol).add((
						bounds: Rect.fromPoints(
							origin + Point(gap.neg, 0),
							origin + Point(buttonExtent.x - gap, buttonExtent.y)
						),
						class: Button,
						init: { |view|
							// init func runs in the touchGui environment
							view.states_([[" ", nil, ltAqua], ["", nil, aqua]])
							.receiveDragHandler_(inEnvir { |view|
								// we don't have access to the 't' touch object
								// notification assumes there's just one (untested with multiple)
								NotificationCenter.notify(~model, \receiveDrag, [View.currentDrag, i]);
							})
						},
					))
					.add("/1/fader%".format((i+1).asPaddedString(2)).asSymbol).add((
						bounds: Rect.fromPoints(
							origin + Point(buttonExtent.x, 0),
							origin + oneBound.extent
						),
						class: Slider,
						// I considered implementing receive-drag on the slider too but it didn't work
						init: { |view| view.knobColor_(green) },
						spec: [0, 1]
					))
					.add("/1/toggle%".format((i+9).asPaddedString(2)).asSymbol).add((
						bounds: Rect.fromPoints(
							origin + Point(gap + ~sliderWidth, 0),
							origin + Point(buttonExtent.x + ~sliderWidth + gap, buttonExtent.y)
						),
						class: Button,
						init: { |view|
							// init func runs in the touchGui environment
							view.states_([[" ", nil, ltAqua], ["", nil, aqua]])
							.receiveDragHandler_(inEnvir { |view|
								// we don't have access to the 't' touch object
								// notification assumes there's just one (untested with multiple)
								NotificationCenter.notify(~model, \receiveDrag, [View.currentDrag, i+8]);
							})
						},
					))
					.add("/1/fader%".format((i+9).asPaddedString(2)).asSymbol).add((
						bounds: Rect.fromPoints(
							origin + Point(~fullWidth - buttonExtent.x, 0),
							origin + Point(~fullWidth, buttonExtent.y)
						),
						class: Knob,
						// I considered implementing receive-drag on the slider too but it didn't work
						init: { |view|
							view.color_([ltGreen, green, ltGreen, green])
							.mode_(\vert)
						},
						spec: [0, 1]
					));
					origin.y = origin.y + oneBound.height + gap;
				};
				out
			}.value
		];

		~chuckOSCKeys = {
			var allKeys = ~saveKeys.collect(_.asString),
			keys, faderKeys, toggleKeys;
			keys = sort(allKeys.select({ |key|
				key[1] == $1 and: { "ft".includes(key[3]) }
			}).as(Array));
			faderKeys = keys.select { |key| key[3] == $f }.collect(_.asSymbol);
			toggleKeys = keys.select { |key| key[3] == $t }.collect(_.asSymbol);
			(keys: keys, faderKeys: faderKeys, toggleKeys: toggleKeys)
		};
	} => PR(\nanoTouch);  // 'touch' is not accurate for MIDI, but reflects object design
} {
	AbstractChuckArray.defaultSubType = saveSubtype;
};
