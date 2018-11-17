// mobile control interface objects

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

protect {
	AbstractChuckArray.defaultSubType = \mobile;

	Proto {
		~addr = nil;  // address to match
		~pingSetsAddr = true;  // auto filter on /ping receipt
		~sendPort = 9000;
		~pingDebug = true;
		~keepAlive = true;
		~prep = {
			~respFunc = e { |msg, time, replyAddr, recvPort|
				if(replyAddr.matches(~addr) or: { msg[0] == '/ping' }) {
					// if(msg[0] != '/accxyz') { msg.debug("raw") };
					~respond.(msg, time, replyAddr, recvPort)
				};
			};
			thisProcess.addOSCRecvFunc(~respFunc);
			~data = IdentityDictionary.new;  // save all incoming data by oscpath
			if(~labels.isNil) { ~labels = IdentityDictionary.new };
			~setDataKeys.();
			if(~keepAlive) { ~startAliveThread.() };
			Environment.current
		};
		~freeCleanup = {
			~stopAliveThread.();
			thisProcess.removeOSCRecvFunc(~respFunc);
			NotificationCenter.notify(currentEnvironment, \modelWasFreed);
		};
		~startAliveThread = {
			~stopAliveThread.();
			~aliveThread = Routine {
				loop {
					if(~addr.notNil) {
						~addr.sendMsg("/alive");
					};
					10.wait;
				};
			};
			currentEnvironment
		};
		~stopAliveThread = { ~aliveThread.stop; };

		~setLabel = { |oscpath, label|
			if(label.isNil) { label = "" /*oscpath.asString.split($/).last*/ };
			~labels[oscpath] = label;
			NotificationCenter.notify(currentEnvironment, \any, [[\label, oscpath, label]]);
			currentEnvironment
		};
		// for single-value controls (common case)
		~setValue = { |oscpath, value|
			if(~sendAddr.notNil) {
				~sendAddr.sendMsg(oscpath, value);
			};
			~respond.([oscpath, value], SystemClock.beats, NetAddr.localAddr, NetAddr.langPort);
		};
		~setGUIValue = { |oscpath, value|
			if(~sendAddr.notNil) {
				~sendAddr.sendMsg(oscpath, value);
			};
			~respond.([oscpath, value], SystemClock.beats, NetAddr.localAddr, NetAddr.langPort, true);
		};
		// for multiple-value controls (rare case)
		~setValues = { |oscpath ... values|
			if(~sendAddr.notNil) {
				~sendAddr.sendMsg(oscpath, *values);
			};
			~respond.([oscpath] ++ values, SystemClock.beats, NetAddr.localAddr, NetAddr.langPort);
		};
		~setGUIValues = { |oscpath ... values|
			if(~sendAddr.notNil) {
				~sendAddr.sendMsg(oscpath, *values);
			};
			~respond.([oscpath] ++ values, SystemClock.beats, NetAddr.localAddr, NetAddr.langPort, true);
		};

		~respond = { |msg, time, replyAddr, recvPort, guiOnly(false)|
			var args = [msg, time, replyAddr, recvPort];
			if(~saveKeys.includes(msg[0])) {
				if(msg.size == 2) {
					~data[msg[0]] = msg[1]
				} {
					~data[msg[0]] = msg[1..];
				};
			};
			NotificationCenter.notify(currentEnvironment, \any, args);
			if(guiOnly.not) {
				NotificationCenter.notify(currentEnvironment, msg[0], args);
			};
			if(msg[0] == '/ping' /*and: { ~addr.isNil }*/) {
				if(~pingDebug) { replyAddr.debug("/ping: set mobile IP") };
				~addr = replyAddr;
				~sendAddr = NetAddr(~addr.ip, ~sendPort);
			};
		};

		~setDataKeys = {
			var new = IdentitySet.new;
			new.addAll(~dataKeys);
			~tabSpecs.pairsDo { |name, viewspecs|
				viewspecs.pairsDo { |oscpath, viewspec|
					new.add(oscpath)
				}
			};
			~saveKeys = new;
		};
		~viewChanged = { |path, value|
			if(value.size > 0) {
				~setValues.(path, *value)
			} {
				~setValue.(path, value)
			};
			currentEnvironment
		};
	} => PR(\abstractTouch);

	// this has GUI stuff in it -- maybe revisit
	PR(\abstractTouch).clone {
		~white = Color.white;
		~black = Color.black;
		~tabBackground = Color.gray(0.3);

		~yellow = Color(0.7, 0.7, 0.2); // Color.yellow(0.6);
		~ltYellow = Color.yellow(0.6, 0.6, 0.2); // ~yellow.blend(~black, 0.7);
		~yellowFor2D = Color(0.4, 0.4, 0.2);

		~aqua = Color(0, 0.4, 1.0);
		~ltAqua = ~aqua.blend(~black, 0.7);

		~purple = Color(1.0, 0.2, 1.0);
		~ltPurple = ~purple.blend(~black, 0.7);

		~green = Color(0.3, 1.0, 0.3);
		~ltGreen = ~green.blend(~black, 0.7);

		~red = Color(0.8, 0, 0);
		~ltRed = ~red.blend(~white, 0.7);

		~buttonExtent = Point(25, 25);
		~labelHeight = 16;
		~labelFont = Font.default.copy.pixelSize_(11);
		~sliderWidth = 200;
		~xyExtent = Point(140, 145);
		~gap = Point(5, 5);

		~sliders = { |n = 1, prefix = "/1", oneBound(Rect(0, 0, 400, 50)), buttonExtent(Point(50, 50)), gap = 10, bcolor, bltColor, scolor, sltColor, startI = 0, nameStartI = 1|
			var out = Array(n * 4),
			togStr = prefix ++ "/toggle",
			slStr = prefix ++ "/fader",
			origin = Point(gap, gap + oneBound.top);
			oneBound = oneBound.copy.top_(0);
			n.do { |i|
				out.add((togStr ++ (i+nameStartI)).asSymbol)
				.add((
					bounds: Rect.fromPoints(origin, origin + buttonExtent),
					class: Button,
					init: { |view|
						// init func runs in the touchGui environment
						view.states_([[" ", nil, bltColor], ["", nil, bcolor]])
						.receiveDragHandler_(e { |view|
							// we don't have access to the 't' touch object
							// notification assumes there's just one (untested with multiple)
							NotificationCenter.notify(~model, \receiveDrag, [View.currentDrag, i+startI]);
						})
					},
				))
				.add((slStr ++ (i+nameStartI)).asSymbol)
				.add((
					bounds: Rect.fromPoints(origin + Point(buttonExtent.x + gap, 0), origin + oneBound.extent),
					class: Slider,
					// I considered implementing receive-drag on the slider too but it didn't work
					init: { |view| view.knobColor_(scolor) },
					spec: [0, 1]
				));
				origin.y = origin.y + oneBound.height + gap;
			};
			out
		};

		~tabSpecs = [
			"Tab1", {
				var out = ~sliders.(1, "/1", oneBound: Rect(0, 0, ~sliderWidth, ~buttonExtent.y), buttonExtent: ~buttonExtent, gap: ~gap.x, bcolor: ~yellow, bltColor: ~ltYellow, scolor: ~yellow, sltColor: ~ltYellow, startI: 0),
				last = out.last;
				out = out ++ ~sliders.(2, "/1", oneBound: Rect(0, last.bounds.bottom, ~sliderWidth, ~buttonExtent.y), buttonExtent: ~buttonExtent, gap: ~gap.x, bcolor: ~aqua, bltColor: ~ltAqua, scolor: ~aqua, sltColor: ~ltAqua, nameStartI: 2, startI: 1);
				last = out.last;
				out = out.grow(out.size + (7*2));
				5.do { |i|
					out.add(("/1/push" ++ (i+1)).asSymbol)
					.add((
						bounds: Rect(~gap.x, last.bounds.bottom + ~gap.y, ~buttonExtent.x, ~buttonExtent.y),
						class: Button,
						init: e { |view|
							view.states = [[" ", nil, ~ltPurple], ["", nil, ~purple]];
						},
					));
					last = out.last;
				};
				// last = out.last;
				out.add('/1/fader4').add((
					bounds: Rect(last.bounds.right + ~gap.x, out[11].bounds.bottom + ~gap.y, ~buttonExtent.x, ~xyExtent.y),
					class: Slider,
					init: e { |view| view.knobColor_(~purple) },
					spec: [1, 0]
				));
				last = out.last;
				out.add('/1/xy').add((
					bounds: Rect(last.bounds.right + ~gap.x, out[11].bounds.bottom + ~gap.y, ~xyExtent.x, ~xyExtent.y),
					class: Slider2D,
					init: e { |view| view.background_(~yellowFor2D).knobColor_(~yellow) },
					updater: { |view, x, y| view.setXY(x, 1.0 - y) }
				));
				out
			}.value,
			"Tab2", ~sliders.(
				8, "/2", oneBound: Rect(0, 0, ~sliderWidth, ~buttonExtent.y),
				buttonExtent: ~buttonExtent, gap: ~gap.y,
				bcolor: ~aqua, bltColor: ~ltAqua, scolor: ~green, sltColor: ~ltGreen,
				startI: 0
			),
			"Tab3", ~sliders.(
				8, "/3", oneBound: Rect(0, 0, ~sliderWidth, ~buttonExtent.y),
				buttonExtent: ~buttonExtent, gap: ~gap.y,
				bcolor: ~aqua, bltColor: ~ltAqua, scolor: ~red, sltColor: ~ltRed,
				startI: 8
			),
		];

	} => PR(\mix16Touch);

	Proto {
		~windowName = "TouchOSC";
		~minExtent = Point(150, 0);
		~font = { ~model[\labelFont] ?? { Font.default.copy.size_(14) } };

		~prep = { |model/*, parentView*/|
			if(model.notNil) { ~model = model };
			// ~parentView = parentView;
			~font = ~font.value;
			~notification = NotificationCenter.register(~model, \any, Environment.current, e { |msg|
				~respond.(~model, msg);  // args[0] == msg
			});
			~freeNotify = NotificationCenter.register(~model, \modelWasFreed, Environment.current, e {
				~free.();
			});
			~tabSpecs = ~model.tabSpecs.deepCopy;
			~makeWindow.();
			~makeTabs.();
			~window.front;
			Environment.current;
		};
		~free = {
			~parentView.remove;
			if(~iMadeWindow and: { ~window.notNil and: { ~window.isClosed.not } }) {
				~window.close;
			};
			~window = nil;
			~notification.remove;
			~freeNotify.remove;
		};
		~freeCleanup = { ~free.() };

		~makeWindow = {
			var temp;
			~maxExtent = ~calcExtent.();
			if(~parentView.isNil) {
				~window = Window(~windowName, ~windowBoundsFromExtent.(~maxExtent));
				~parentView = ~window.view;
				~iMadeWindow = true;
			} {
				temp = ~parentView;
				while { temp.parent.notNil } { temp = temp.parent };
				// now temp should be a TopView
				~window = temp.findWindow;
				~iMadeWindow = false;
			};
			~parentView.onClose = e {
				if(~window.notNil) { ~window.onClose = nil };
				~free.();
			};
		};
		~calcExtent = { |overrideModel|
			var maxPt = ~minExtent, specs;
			if(overrideModel.notNil) {
				specs = overrideModel.tabSpecs;
			} {
				specs = ~tabSpecs ?? { ~model.tabSpecs };
			};
			specs.pairsDo { |name, viewSpecs|
				viewSpecs.pairsDo { |oscpath, spec|
					maxPt = max(maxPt, spec.bounds.rightBottom);
				}
			};
			maxPt + 20
		};
		~windowBoundsFromExtent = { |extent|
			var sb = Window.screenBounds;
			// was: Rect.aboutPoint(Window.screenBounds.center, extent.x / 2, extent.y / 2)
			Rect(sb.right - extent.x, sb.center.y - (extent.y / 2), extent.x, extent.y)
		};
		~makeTabs = {
			~tabs = TabbedView(~parentView, ~parentView.bounds.insetBy(2, 2), ~tabSpecs[0, 2 ..])
			.backgrounds_([~model.tabBackground]);
			~tabSwitch = Array.fill(~tabs.views.size, { |i| ("/" ++ (i+1)).asSymbol });
			~views = IdentityDictionary.new;
			~labels = IdentityDictionary.new;
			~tabSpecs.pairsDo { |name, viewSpecs, i|
				~fillTab.(i div: 2, viewSpecs);
			};

			// I found it's too easy to drag-focus tab 1
			// but maybe useful to drag-select other tabs
			~tabs.tabViews[0].canReceiveDragHandler = nil;

			// if you switch tab onscreen, it should switch on the phone too
			// where to get the address? no time now
			~tabFocusActive = true;
			~tabs.focusActions = Array.fill(~tabs.views.size, { |i|
				e {
					if(~tabFocusActive and: { ~model.addr.notNil }) {
						~model.sendAddr.sendMsg(~tabSwitch[i]);
					};
				}
			});
		};

		~fillTab = { |index, specs|
			var parent = ~tabs.views[index], view;
			specs.pairsDo { |oscpath, spec|
				view = spec.copy;
				view[\view] = view[\class].new(parent, view[\bounds]);
				view[\init].value(view.view);
				view[\spec] = view[\spec].asSpec;
				if(view[\class] == Slider2D) {
					view.view.action = e { |vw|
						// this is not exactly right
						~model.tryPerform(\viewChanged, oscpath, view[\spec].map([vw.x, vw.y]));
					};
				} {
					view.view.action = e { |vw|
						~model.tryPerform(\viewChanged, oscpath, view[\spec].map(vw.value));
					};
				};
				~views[oscpath] = view;
				~viewHook.(oscpath, view, parent);
			};
		};

		~viewHook = { |oscpath, view, parent|
			var label;
			if(~model.labels[oscpath].notNil) {
				label = ~model.labels[oscpath]
			} {
				label = "" // oscpath.asString.split($/).last;
			};
			~labels[oscpath] = StaticText(parent, view.bounds.setExtent(view.bounds.width, ~model.labelHeight))
			.align_(\center)
			.font_(~font)
			.string_(label);
		};

		~respond = { |obj, msg|  // what, args
			var what = msg[0], view;
			// reserved: switch tabs
			case { (view = ~tabSwitch.indexOf(what)).notNil } {
				(e {
					~tabFocusActive = false;  // suppress focusAction
					~tabs.focus(view);
					~tabFocusActive = true;
				}).defer;
			}
			{ msg[0] == \label } {
				e { ~labels[msg[1]].string = msg[2] }.defer;
			}
			// { what == \modelWasFreed } { ~free.() }
			// default: locate and update the view onscreen
			{
				view = ~views[what];
				if(view.notNil) {
					if(view[\updater].notNil) {
						{ view[\updater].value(view.view, *msg[1..]) }.defer;
					} {
						{ view.view.value = view.spec.unmap(msg[1]) }.defer;
					};
				};
			}
		};
	} => PR(\abstractTouchGUI);

	Proto {
		~windowSize = 15;
		~sendWait = 0.08;
		~sourceBP = \touch;
		~prep = {
			// why this? take advantage of address filtering in the BP
			~resp = NotificationCenter.register(BP(~sourceBP).v, '/accxyz', Environment.current, e { |msg|
				~smooth.(msg);
			});
			// ~resp = OSCFunc(e { |msg| ~smooth.(msg) }, '/accxyz');
			~movingBuf = Array.fill(~windowSize, #[0, 0, 0]);
			~sum = [0, 0, 0];
			~avg = [0, 0, 0];
			~index = 0;
			~lastSendTime = SystemClock.beats;
		};
		~freeCleanup = {
			~resp.remove;
			// ~resp.free;
		};
		~smooth = { |msg|
			var oldest = ~movingBuf.wrapAt(~index + 1);
			~sum.do { |sum, i|
				~sum[i] = sum - oldest[i] + msg[i+1];
			};
			~avg = ~sum / ~windowSize;
			~index = (~index + 1) % ~windowSize;
			~movingBuf[~index] = msg[1..];
			if((SystemClock.beats - ~lastSendTime) >= ~sendWait) {
				NotificationCenter.notify(Environment.current, '/accxyz', [~avg]);
				~lastSendTime = SystemClock.beats;
			};
		};
	} => PR(\accxyzSmoother);



	// musical action responders
	Proto {
		~oscInKey = \touch;
		~prep = { |path, specs|
			var oscin = BP(~oscInKey).v;
			~path = path.asArray;
			~specs = specs;
			~rdepth = 0;
			~resp = ~path.collect { |path|
				NotificationCenter.register(oscin, path, Environment.current, e {
					|msg, time, addr, recvPort|
					if(~rdepth < 50) {
						~rdepth = ~rdepth + 1;
						~prAction.(msg, time, addr, recvPort);
						~rdepth = ~rdepth - 1;
					} {
						Error("OSC response: Recursion limit reached").throw;
					};
				});
			};
			~userprep.(~path, specs);
			~setLabels.();
			Environment.current
		};
		~free = { |wasReassigned(false)|
			if(wasReassigned.not) {
				~setLabels.([""] /*~path.collect { |p| p.asString.split($/).last }*/);
			};
			~userfree.(wasReassigned);
			~resp.do(_.remove);
			NotificationCenter.notify(currentEnvironment, \didFree);
		};
		~setLabels = { |labels|
			var oscin = BP(~oscInKey).v;
			if(labels.isNil) { labels = ~specs[\label].asArray };
			if(labels.isString) { labels = [labels] };
			~path.do { |p, i| oscin.setLabel(p, labels.wrapAt(i)) };
		};
	} => PR(\abstrMobileResp);

	PR(\abstrMobileResp).clone {
		~userprep = { |path|
			var oscin = BP(~oscInKey).v, playing = 0;
			~udepth = 0;
			BP(~specs[\bp]).do { |bp|
				bp.addDependant(Environment.current);
				// [path, bp, bp.isPlaying].debug("play check");
				if(bp.isPlaying) { playing = 1 };
			};
			path.do { |p| oscin.setGUIValue(p, playing) };
			~prepHook.(path);
		};
		~userfree = { |wasReassigned|
			var oscin = BP(~oscInKey).v;
			BP(~specs[\bp]).do { |bp|
				bp.removeDependant(Environment.current);
			};
			if(wasReassigned.not) {
				~path.do { |p| oscin.setGUIValue(p, 0) };
			};
		};
		~prAction = { |msg|
			if(msg[1] > 0) {
				if(~specs[\once] ? false) {
					BP(~specs[\bp]).do { |bp| bp.triggerOneEvent(~specs[\quant]) };
				} {
					BP(~specs[\bp]).play(~specs[\quant]);
				};
			} {
				BP(~specs[\bp]).stop(~specs[\quant]);
			};
		};
		~update = { |obj, what|
			if(~udepth < 50) {
				~udepth = ~udepth + 1;
				case
				{ what == \free } { ~free.() }
				{ #[schedFailed, stop, couldNotPrepare, couldNotStream, oneEventPlayed].includes(what) } {
					~path.do { |p|
						BP(~oscInKey).setGUIValue(p, 0);
					}
				}
				{ what == \play } {
					~path.do { |p|
						BP(~oscInKey).setGUIValue(p, 1);
					}
				};
				~udepth = ~udepth - 1;
			} {
				Error("OSC response: updater recursion limit reached").throw;
			};
		};
	} => PR(\bptrig);

	PR(\abstrMobileResp).clone {
		~userprep = { |path|
			~gc = ~specs[\gc];
			if(~gc.isKindOf(GlobalControlBase).not) { ~gc = ~gc.value };
			~cspec = (~specs[\spec] ?? { ~gc.spec }).asSpec;
			BP(~oscInKey).setGUIValue(path[0], ~cspec.unmap(~gc.value));
			~gc.addDependant(Environment.current);
		};
		~userfree = { |wasReassigned|
			if(wasReassigned.not) {
				~path.do { |p|
					BP(~oscInKey).setGUIValue(p, 0);
				};
			};
			~gc.removeDependant(Environment.current);
		};
		~prAction = { |msg|
			~gc.set(~cspec.map(msg[1]));
		};
		~update = { |obj, what|
			switch(what.tryPerform(\at, \what))
			{ \value } {
				BP(~oscInKey).setGUIValue(~path[0], ~cspec.unmap(~gc.value));
			}
			{ \modelWasFreed } { ~free.() }
		};
	} => PR(\gcmap);

	PR(\gcmap).clone {
		~gcDidRegister = false;
		~userprep = { |path|
			~mixer = ~specs[\mixer];
			if(~mixer.isKindOf(MixerChannel).not) { ~mixer = ~mixer.value };
			~gc = ~mixer.controls[~specs[\ctl] ?? { \level }];
			if(~gc.notNil) {
				~cspec = (~specs[\spec] ?? { ~gc.spec }).asSpec;
				BP(~oscInKey).setGUIValue(path[0], ~cspec.unmap(~gc.value));
				~gc.addDependant(Environment.current);
				~gc.bus.addDependant(Environment.current);  // to sync with 'watch'... grr, bad hacks
				NotificationCenter.register(~gc, \setMixerGui, currentEnvironment, e { |mcgui|
					if(mcgui.notNil) {
						~gc.register(~specs[\ctl], mcgui);
						~gcDidRegister = true;
					} {
						~gc.register(nil, nil, 1);
						~gcDidRegister = false;
					};
				});
				if(~gc.mixerGui.notNil) {
					NotificationCenter.notify(~gc, \setMixerGui, [~gc.mixerGui]);  // stupid ugly hack
				};
			};
		};
		~userfree = { |wasReassigned|
			if(wasReassigned.not) {
				~path.do { |p|
					BP(~oscInKey).setGUIValue(p, 0);
				};
			};
			if(~gcDidRegister) { ~gc.register(nil, nil, 1) };
			~gc.bus.removeDependant(Environment.current);
			~gc.removeDependant(Environment.current);
			NotificationCenter.unregister(~gc, \setMixerGui, currentEnvironment);
		};
		~prAction = { |msg|
			var value = ~cspec.map(msg[1]);
			~gc.set(value);
			~gc.update(~gc.bus, [value]);
		};
		~update = { |obj, what|
			case
			{ what.isKindOf(Dictionary) } {
				switch(what.tryPerform(\at, \what))
				{ \value } {
					BP(~oscInKey).setGUIValue(~path[0], ~cspec.unmap(~gc.value));
				}
				{ \modelWasFreed } { ~free.() }
			}
			{ obj.isKindOf(Bus) } {
				BP(~oscInKey).setGUIValue(~path[0], ~cspec.unmap(what[0]));
			}
		};
	} => PR(\mxmap);

	PR(\abstrMobileResp).clone {
		~userprep = { |path|
			path.do { |p| BP(~oscInKey).setGUIValue(p, 0) };
		};
		~prAction = { |msg, time, addr, recvPort|
			~specs[\action].value(msg, time, addr, recvPort);
			if(~specs[\switchOff] == true) {
				{
					~path.do { |p| BP(~oscInKey).setGUIValue(p, 0) };
				}.defer(0.3);
			};
		};
	} => PR(\trigact);

	Proto {
		~prep = {
			~maps = IdentityDictionary.new;
		};
		~freeCleanup = {
			~maps.keysDo { |key| ~unmapMobile.(key) };
		};

		~mapMobile = { |type, path, specs|
			var new;
			if(PR.exists(type)) {
				new = PR(type).copy.prep(path, specs);
				~maps[path] = ~maps[path].add(new);
			};
		};

		~unmapMobile = { |path|
			~maps[path].do { |obj| obj.free };
		};
	} => PR(\mapStorage);

	BP(\osc).free;
	Proto {
		~oscKey = \touch;
		~event = ();  // dummy, to prevent bindVC from breaking

		~prep = {
			~keys = BP(~oscKey).saveKeys.collect(_.asString).select { |key| "23".includes(key[1]) };
			~faderKeys = ~keys.select { |key| key[3] == $f }.collectAs(_.asSymbol, Array).sort;
			~toggleKeys = ~keys.select { |key| key[3] == $t }.collectAs(_.asSymbol, Array).sort;
			~maps = IdentityDictionary.new;

			// drag-n-drop: the 'model' is BP(~oscKey)
			// the model doesn't know directly about me, but it sends notifications
			NotificationCenter.register(BP(~oscKey).v, \receiveDrag, ~collIndex, e(~receiveDrag));
		};
		~freeCleanup = {
			NotificationCenter.unregister(BP(~oscKey).v, \receiveDrag, ~collIndex);
			~maps.do(_.free);
		};
		~empty = { |indices|
			if(indices.isNil) { indices = 16 };
			indices.do { |i| ~bindNil.(nil, i) };
		};
		~receiveDrag = { |drag, i|
			var method = ("bind" ++ drag.bindClassName).asSymbol;
			if(method.envirGet.isFunction) {
				method.envirGet.value(drag, i.asString);  // adverb should be a string
			} {
				"Dragging % is not allowed here".format(drag).warn;
			};
			currentEnvironment
		};
		// don't do mixers this way
		~bindGenericGlobalControl = { |thing, adverb, parms|
			// prefer Tab3 for gcs
			var index = ~getIndexFromAdverb.(adverb, #[8, 0], \fader), newMap;
			~maps[~faderKeys[index]].free;
			newMap = PR(\gcmap).copy.prep(~faderKeys[index], (gc: thing, label: thing.name));
			~maps[~faderKeys[index]] = newMap;
			~setNotification.(newMap, ~faderKeys[index]);
		};
		~bindVC = { |vc, adverb, parms|
			var gcs = parms.tryPerform(\at, \gcs) ?? { vc.v.globalControlsByCreation }, num = gcs.size;
			// need custom adverb logic
			adverb = adverb.asString;
			if(adverb.every(_.isDecDigit)) {
				adverb = adverb.asInteger
			} {
				if(parms.tryPerform(\at, \over) == true) {
					adverb = 8;
				} {
					Error("Searching not implemented yet").throw;
				}
			};
			if(adverb.notNil) {
				block { |break|
					gcs.do { |gc, i|
						if(gc.allowGUI) {
							if(adverb >= ~faderKeys.size) {
								"VC(%) has controls that couldn't be assigned".format(vc.collIndex).warn;
								break.(i);
							};
							~bindGenericGlobalControl.(gc, adverb, parms);
							adverb = adverb + 1;
						};
					};
				};
			};
		};
		~bindMixerChannel = { |mixer, adverb, parms|
			var index = ~getIndexFromAdverb.(adverb, 0, \fader),
			newMap;
			// volume fader
			~maps[~faderKeys[index]].free;
			newMap = PR(\mxmap).copy.prep(~faderKeys[index],
				(mixer: mixer, ctl: \level, label: mixer.name.asString + "level"));
			~maps[~faderKeys[index]] = newMap;
			~setNotification.(newMap, ~faderKeys[index]);
			// mute button -- some hackage here
			if(parms.tryPerform(\at, \setMute) != false) {
				~maps[~toggleKeys[index]].free;
				newMap = PR(\abstrMobileResp).copy.prep(~toggleKeys[index],
					(label: "M"));
				newMap.prAction = { |msg| mixer.mute(msg[1] > 0) };
				newMap[\update] = newMap[\update].addFunc({ |obj, what, ag|
					if(what == \mixerFreed and: { ag === mixer }) {
						~free.();
						MixerChannel.removeDependant(currentEnvironment);
					};
				});
				newMap.userfree = {
					BP(~oscInKey).setGUIValue(~path[0], 0);
				};
				MixerChannel.addDependant(newMap);
				~maps[~toggleKeys[index]] = newMap;
				~setNotification.(newMap, ~toggleKeys[index]);
			};
		};
		// both mixer and play/stop
		~bindBP = { |bp, adverb, parms|
			var index = ~getIndexFromAdverb.(adverb, 0, \fader),
			mixer = bp.v.chan,
			newMap;
			if(mixer.isNil) {
				mixer = bp.event[\voicer];  // mixer as temp here
				if(mixer.notNil) { mixer = mixer.asMixer };  // mixer is really a Voicer at the start of this!
			};
			if(parms.isNil) {
				parms = ()  // setMute: false
			};
			// may be multiple mixers
			// note: 'do' handles [mixer, mixer...], and mixer, and nil
			mixer.do { |mixer, i|
				var localParms = parms;
				if(i == 0) {
					localParms = localParms.copy.put(\setMute, false);
				};
				if(index + i < ~faderKeys.size) {
					~bindMixerChannel.(mixer, index + i, localParms);
				};
			};
			~maps[~toggleKeys[index]].free;
			newMap = PR(\bptrig).copy.prep(~toggleKeys[index],
				(bp: bp.collIndex, label: bp.collIndex, once: parms[\once]));
			~maps[~toggleKeys[index]] = newMap;
			~setNotification.(newMap, ~toggleKeys[index]);
		};
		~bindNil = { |aNil, adverb|
			var index;
			tryNoTrace {
				index = ~getIndexFromAdverb.(adverb, #[], \fader);
			} { |err|
				// catch and ignore 'bad index' errors
				if(err.errorString.contains("bad index").not) { err.throw };
			};
			if(index.isInteger) {
				~maps[~toggleKeys[index]].free;
				~maps[~faderKeys[index]].free;
			} {
				"Nothing to remove, or bad index".warn
			};
		};
		~getIndexFromAdverb = { |adverb, offsetsToTry = #[0], type(\fader)|
			var coll = (type ++ "Keys").asSymbol.envirGet;
			if(coll.isNil) {
				Error("BP(%): Wrong type %".format(~collIndex.asCompileString, type.asCompileString)).throw;
			};
			adverb = adverb.asString;
			if(adverb.every(_.isDecDigit)) {
				adverb = adverb.asInteger;
			} {
				adverb = block { |break|
					offsetsToTry.asArray.do { |offset|
						(offset .. coll.size - 1).do { |i|
							if(~maps[coll[i]].isNil) { break.(i) };
						};
					};
					nil
				};
			};
			if(adverb.isNil or: { adverb.inclusivelyBetween(0, coll.size).not }) {
				Error("BP(%): No available OSC %s or bad index".format(~collIndex.asCompileString, type)).throw;
			} {
				adverb
			};
		};
		~setNotification = { |mapObj, key|
			NotificationCenter.register(mapObj, \didFree, currentEnvironment, e {
				NotificationCenter.unregister(mapObj, \didFree, currentEnvironment);
				~maps[key] = nil;
			});
		};
	} => PR(\chuckOSC);

	// not really "mobile" but helps you locate and guify controls
	Proto {
		~classes = [BP, VC];
		~bounds = Rect(800, 200, 260, 175);
		~prep = {
			~bpSimpleCtls = IdentityDictionary.new;
			~vcSimpleCtls = IdentityDictionary.new;
			~deleteTimes = Dictionary.new;
			~expanded = Dictionary.new;
			~chucking = false;
			~chuckTargetChars = "0123456789ABCDEF";
			if(~viewParent.isNil) {
				// if user puts the BP into a layout
				~view = ListView.new;
			} {
				~view = ListView(~viewParent, ~bounds)
			};
			~view.beginDragAction_(e { |view|
				if(~items.size > 0) {
					~items[~view.value].tryPerform(\at, \dragObject)
				} { nil };
			})
			.keyDownAction_(e { |view, char, mod, unicode, keycode, key|
				var item, i;
				// if you hit a mod key, the function fires and char.ascii is 0
				// don't want to clear $^ status in that case
				if(~chucking and: { char.ascii > 0 }) {
					item = ~items[view.value];
					char = char.toUpper;
					if(item.notNil and: { ~chuckTargetChars.includes(char) }) {
						i = ~chuckTargetChars.indexOf(char);
						// must chuck into the proto, not the BP
						item[\dragObject].chuck(BP(\chuckOSC).v, i);
					};
					~chucking = false;
				};
				case
				{ char == $^ } {
					~chucking = true;
				}
				{ #[8, 127].includes(char.ascii) } {
					item = ~items[view.value];
					if(item.notNil) {
						~doFree.(item);
					};
				}
				{ keycode == 65363 } {
					item = ~items[view.value];
					if(item.notNil and: { item[\object].isKindOf(BP) }) {
						~expanded[item[\id]] = true;
					};
					~updateView.();
				}
				{ keycode == 65361 } {
					i = view.value;
					item = ~items[i];
					if(item[\object].isKindOf(GenericGlobalControl)) {
						while {
							i = i - 1;
							i >= 0 and: { ~items[i][\object].isKindOf(BP).not }
						};
						if(i >= 0) {
							~view.value = i;
							item = ~items[i];
						} {
							item = nil
						};
					};
					if(item.notNil) {
						~expanded[item[\id]] = nil;
					};
					~updateView.();
				};
			})
			.onClose_(e { BP(~collIndex).free });
			~updateView.();
			if(~viewParent.isNil) { ~view.front };
			~updateView = e(~updateView);
			~putHook = e { |key, obj|
				if(obj.isKindOf(VC)) {
					// defer b/c Fact=>VC does VC new first, then putHook, *then* populate
					// so 'obj.value' is not available Right Now
					defer(e {
						~vcSimpleCtls.put(key, SimpleController(obj.value)
							.put(\addedGlobalControl, ~updateView)
							.put(\removedGlobalControl, ~updateView)
						);
					}, 0.1);
				};
				if(obj.isKindOf(BP)) {
					defer(e {
						~bpSimpleCtls.put(key, SimpleController(obj)
							.put(\play, e { defer(~updateView) })
							.put(\stop, e { |obj, what, value|
								if(value == \stopped) { defer(~updateView) };
							})
							.put(\voicer_, ~updateView)
						);
					}, 0.1);
				};
				~updateView.defer(0.05);  // updateView is already linked to this envir
			};
			~freeHook = e { |key, obj|
				var item = ~items.detect { |it| it[\object] === obj };
				if(item.notNil) {
					~deleteTimes.removeAt(item[\id]);
					~expanded.removeAt(item[\id]);
				};
				if(obj.isKindOf(VC)) {
					~vcSimpleCtls[key].remove;
					~vcSimpleCtls.removeAt(key);
				};
				if(obj.isKindOf(BP)) {
					~bpSimpleCtls[key].remove;
					~bpSimpleCtls.removeAt(key);
				};
				~updateView.defer(0.05);  // updateView is already linked to this envir
			};
			~classes.do { |class|
				class.addHook(\put, ~putHook)
				.addHook(\free, ~freeHook);
			};
			currentEnvironment
		};
		~freeCleanup = {
			~classes.do { |class|
				class.removeHook(\put, ~updateHook)
				.removeHook(\free, ~updateHook);
			};
			~bpSimpleCtls.do(_.remove);
			~vcSimpleCtls.do(_.remove);
			~view.close;
		};
		~asView = { ~view };
		~updateView = {
			// tryPerform: ~items may be nil or empty
			// also (FML) ~view.value may be nil
			var currentID, i;
			if(~items.notNil and: { ~view.value.notNil }) {
				// and even if you have an items array, you might pull nil out of it
				currentID = ~items[~view.value].tryPerform(\at, \id);
			};
			~items = ~makeList.();
			~view.items = ~items.collect(_.string);
			i = ~items.detectIndex { |item| item[\id] == currentID };
			if(i.notNil) { ~view.value = i };
			~updateColors.();
		};
		~updateColors = {
			~view.colors = ~getColors.();
		};
		~makeList = {
			// oopsy, need drags and strings
			// gc drags need to point back to the parent -- data structure
			// (uniqueID [for restoring view pos], string, dragObject, parent [only for VoicerGlobalControls])
			~makeBPList.() ++ ~makeVCList.()
		};
		~makeBPList = {
			var items = Array.new, item, event, ctls;
			// it seems to be possible to have a symbol in 'keys' whose BP doesn't actually exist
			// probably should prevent that but for now, filter such items out
			BP.keys.select { |key| BP.exists(key) }.as(Array).sort.do { |key|
				event = BP(key)[\event];
				// if you have an eventKey, you're a playable process
				// cll interface BPs do not have this
				if(event.notNil and: { event[\eventKey].notNil }) {
					item = (
						id: "BP" ++ key,
						string: "BP(%)".format(key.asCompileString),
						object: BP(key),
						dragObject: BP(key)  // mixer
					);
					items = items.add(item);
					ctls = ~getBPCtls.(BP(key), 0);  // maybe nil, but nil ++ array is ok
					if(event[\voicer].notNil) {
						ctls = ctls ++ ~getVoicerCtls.(event[\voicer], ctls.size);
					};
					if(ctls.size > 0) {
						if(~expanded[items.last[\id]] == true) {
							item[\string] = "-- " ++ item[\string];
							items = items ++ ctls;
						} {
							item[\string] = "+ " ++ item[\string];
						};
					};
				};
			};
			items
		};
		~getBPCtls = { |bp, startI|
			if(bp.v[\globalControls].notNil) {
				bp.globalControls.collect { |ctl, i|
					~itemForCtl.(ctl, bp.collIndex, startI + i)
				};
			};
		};
		~itemForCtl = { |gc, key, i|  // 'key' and 'i' are for the id field
			(
				id: "VC%:%-%".format(key, i.asPaddedString(2, "0"), gc.name),
				string: "    - % (% .. %)".format(gc.name, gc.spec.minval, gc.spec.maxval),
				object: gc,
				dragObject: gc
			)
		};
		~getVoicerCtls = { |voicer, startI|
			var items, ctls, key;
			// 'globalControlsByCreation' keeps only 'allowGUI' controls
			ctls = voicer.tryPerform(\globalControls);
			if(ctls.size > 0) {
				if(ctls.isKindOf(Dictionary)) {
					ctls = ctls.values.as(Array);
				};
				key = VC.collection.detect { |vc| vc.v === voicer }
				.tryPerform(\collIndex) ?? { "unknown" };
				ctls.sort { |a, b| a.voicerIndex < b.voicerIndex }
				.do { |gc, i|
					items = items.add(~itemForCtl.(gc, key, startI + i))
				};
			};
			items
		};
		~makeVCList = {
			var items = Array.new, ctls;
			VC.keys.select { |key| VC.exists(key) }.as(Array).sort.do { |key|
				items = items.add((
					id: "VC" ++ key,
					string: "VC(%)".format(key.asCompileString),
					object: VC(key),
					dragObject: VC(key).asMixer  // maybe nil but shouldn't be
				));
				// ctls will be shown with BPs instead
				// ctls = ~getVoicerCtls.(VC(key).v);
				// if(ctls.size > 0) { items = items ++ ctls };
			};
			items
		};
		~deleteCancelTime = 0.7;
		~doFree = { |item|
			var index;
			if(~canFree.(item)) {
				if(~deleteTimes[item[\id]].isNil) {
					~deleteTimes[item[\id]] = SystemClock.seconds;
					~updateColors.();
					AppClock.sched(~deleteCancelTime, e {
						// ~deleteTimes[item[\id]] is cleared if the user hit del a second time to cancel
						if(~deleteTimes[item[\id]].notNil) {
							index = ~view.value;
							item[\object].free;  // should delete from items too
							defer(e {
								if(index >= ~items.size) {
									~view.value = max(0, ~items.size - 1);
								} {
									~view.value = index;
								};
							}, 0.06);
						};
					});
				} {
					if(SystemClock.seconds - ~deleteTimes[item[\id]] <= ~deleteCancelTime) {
						~deleteTimes[item[\id]] = nil;
					};
				};
			} {
				"Can't delete %".format(item[\object].asString).warn;
			};
		};
		~canFree = { |item|
			var obj = item[\object];
			switch(obj.class)
			{ BP } {
				// free-able only if stopped (don't accidentally delete something that's playing)
				obj.isPlaying.not
			}
			{ VC } {
				BP.collection.every { |bp|
					var event = bp[\event];
					event.isNil or: { event[\voicer] !== obj.v }
				}
			}
			{ false }  // GenericGlobalControl, can't free this way
		};
		if(QPalette.new.color(\window).red < 0.5) {
			// dark theme
			~deletePendingColor = Color.new255(116, 46, 0);
			~playingColor = Color.new255(5, 66, 0);
			~idleColor = Color.clear;
		} {
			~deletePendingColor = Color.new255(255, 178, 78);
			~playingColor = Color.new255(140, 255, 131);
			~idleColor = Color.clear;
		};
		~getColors = {
			~items.collect { |item|
				case
				{ item[\deleteTime].notNil } { ~deletePendingColor }
				// OK for VCs because it falls back to Object's implementation --> false
				{ item[\object].isPlaying } { ~playingColor }
				{ ~idleColor };
			};
		};
	} => PR(\controlList);
} {
	AbstractChuckArray.defaultSubType = saveSubtype;
};