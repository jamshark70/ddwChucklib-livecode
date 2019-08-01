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
		~pathTable = [
			1, '/2/fader1',
			2, '/2/toggle1',
			3, '/2/fader2',
			4, '/2/toggle2'
		];
		~keepAlive = false;
		~prep = {
			~pathsForCC = Array.newClear(128);
			~pathTable.pairsDo { |ccnum, path|
				~pathsForCC[ccnum] = path;
			};
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

		// syncSigns:
		// nil = not synced, check before changing real value
		// 0 = synced, pass value through
		// -1 = MIDI value is less than old value
		~respond = { |msg, time, replyAddr, recvPort, guiOnly(false)|
			var args = [msg, time, replyAddr, recvPort];
			var sign, data;
			[msg, time, replyAddr, recvPort, guiOnly].debug("respond");
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
				"saveValueFromMsg".debug;
				~data[msg[0]] = msg[1].dump
			} {
				~data[msg[0]] = msg[1..];
			};
		};
	} => PR(\nanoTouch);  // 'touch' is not accurate for MIDI, but reflects object design
} {
	AbstractChuckArray.defaultSubType = saveSubtype;
};
