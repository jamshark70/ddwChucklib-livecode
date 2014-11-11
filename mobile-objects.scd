// mobile control interface objects

Proto {
	~addr = nil;  // address to match
	~pingSetsAddr = true;  // auto filter on /ping receipt
	~sendPort = 9000;
	~prep = {
		~respFunc = e { |msg, time, replyAddr, recvPort|
			if(replyAddr.matches(~addr)) {
				// if(msg[0] != '/accxyz') { msg.debug("raw") };
				~respond.(msg, time, replyAddr, recvPort)
			};
		};
		thisProcess.addOSCRecvFunc(~respFunc);
		~data = IdentityDictionary.new;  // save all incoming data by oscpath
		if(~labels.isNil) { ~labels = IdentityDictionary.new };
		~setDataKeys.();
		Environment.current
	};
	~freeCleanup = {
		thisProcess.removeOSCRecvFunc(~respFunc);
		NotificationCenter.notify(currentEnvironment, \modelWasFreed);
	};

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
		if(msg[0] == '/ping' and: { ~addr.isNil }) {
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
} => PR(\abstractTouch);

// this has GUI stuff in it -- maybe revisit
PR(\abstractTouch).clone {
	~white = Color.white;
	~yellow = Color.yellow;
	~ltYellow = ~yellow.blend(~white, 0.7);

	~aqua = Color(0, 0.4, 1.0);
	~ltAqua = ~aqua.blend(~white, 0.7);

	~purple = Color(1.0, 0.2, 1.0);
	~ltPurple = ~purple.blend(~white, 0.7);

	~green = Color(0.3, 1.0, 0.3);
	~ltGreen = ~green.blend(~white, 0.7);

	~red = Color(0.8, 0, 0);
	~ltRed = ~red.blend(~white, 0.7);

	~buttonExtent = Point(25, 25);
	~sliderWidth = 200;
	~xyExtent = Point(140, 145);
	~gap = Point(5, 5);

	~sliders = { |n = 1, prefix = "/1", oneBound(Rect(0, 0, 400, 50)), buttonExtent(Point(50, 50)), gap = 10, bcolor, bltColor, scolor, sltColor, startI = 1|
		var out = Array(n * 4),
		togStr = prefix ++ "/toggle",
		slStr = prefix ++ "/fader",
		origin = Point(gap, gap + oneBound.top);
		oneBound = oneBound.copy.top_(0);
		n.do { |i|
			out.add((togStr ++ (i+startI)).asSymbol)
			.add((
				bounds: Rect.fromPoints(origin, origin + buttonExtent),
				class: Button,
				init: { |view|
					view.states = [[" ", nil, bltColor], ["", nil, bcolor]];
				},
			))
			.add((slStr ++ (i+startI)).asSymbol)
			.add((
				bounds: Rect.fromPoints(origin + Point(buttonExtent.x + gap, 0), origin + oneBound.extent),
				class: Slider,
				init: { |view| view.knobColor_(scolor) },
				spec: [0, 1]
			));
			origin.y = origin.y + oneBound.height + gap;
		};
		out
	};

	~tabSpecs = [
		"Tab1", {
			var out = ~sliders.(1, "/1", oneBound: Rect(0, 0, ~sliderWidth, ~buttonExtent.y), buttonExtent: ~buttonExtent, gap: ~gap.x, bcolor: ~yellow, bltColor: ~ltYellow, scolor: ~yellow, sltColor: ~ltYellow),
			last = out.last;
			out = out ++ ~sliders.(2, "/1", oneBound: Rect(0, last.bounds.bottom, ~sliderWidth, ~buttonExtent.y), buttonExtent: ~buttonExtent, gap: ~gap.x, bcolor: ~aqua, bltColor: ~ltAqua, scolor: ~aqua, sltColor: ~ltAqua, startI: 2);
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
				init: e { |view| view.background_(~ltYellow).knobColor_(~yellow) },
				updater: { |view, x, y| view.setXY(x, 1.0 - y) }
			));
			out
		}.value,
		"Tab2", ~sliders.(8, "/2", oneBound: Rect(0, 0, ~sliderWidth, ~buttonExtent.y), buttonExtent: ~buttonExtent, gap: ~gap.y, bcolor: ~aqua, bltColor: ~ltAqua, scolor: ~green, sltColor: ~ltGreen),
		"Tab3", ~sliders.(8, "/3", oneBound: Rect(0, 0, ~sliderWidth, ~buttonExtent.y), buttonExtent: ~buttonExtent, gap: ~gap.y, bcolor: ~aqua, bltColor: ~ltAqua, scolor: ~red, sltColor: ~ltRed),
	];

} => PR(\mix16Touch);

Proto {
	~windowName = "TouchOSC";
	~minExtent = Point(150, 0);
	~font = Font.default.copy.size_(14);

	~prep = { |model|
		if(model.notNil) { ~model = model };
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
		if(~window.notNil and: { ~window.isClosed.not }) {
			~window.close;
		};
		~window = nil;
		~notification.remove;
		~freeNotify.remove;
	};
	~freeCleanup = { ~free.() };

	~makeWindow = {
		~maxExtent = ~calcExtent.();
		~window = Window(~windowName, ~windowBoundsFromExtent.(~maxExtent));
		~window.onClose = e {
			if(~window.notNil) { ~window.onClose = nil };
			~free.();
		};
	};
	~calcExtent = {
		var maxPt = ~minExtent;
		~tabSpecs.pairsDo { |name, viewSpecs|
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
		~tabs = TabbedView(~window, ~window.view.bounds.insetBy(2, 2), ~tabSpecs[0, 2 ..]);
		~tabSwitch = Array.fill(~tabs.views.size, { |i| ("/" ++ (i+1)).asSymbol });
		~views = IdentityDictionary.new;
		~labels = IdentityDictionary.new;
		~tabSpecs.pairsDo { |name, viewSpecs, i|
			~fillTab.(i div: 2, viewSpecs);
		};
	};

	~fillTab = { |index, specs|
		var parent = ~tabs.views[index], view;
		specs.pairsDo { |oscpath, spec|
			view = spec.copy;
			view[\view] = view[\class].new(parent, view[\bounds]);
			view[\init].value(view.view);
			view[\spec] = view[\spec].asSpec;
			view.view.action = e { |vw|
				~model.tryPerform(\viewChanged, oscpath, view[\spec].map(vw.value));
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
		~labels[oscpath] = StaticText(parent, view.bounds.setExtent(view.bounds.width, 20))
		.align_(\center)
		.font_(~font)
		.string_(label);
	};

	~respond = { |obj, msg|  // what, args
		var what = msg[0], view;
		// reserved: switch tabs
		case { (view = ~tabSwitch.indexOf(what)).notNil } {
			(e { ~tabs.focus(view) }).defer;
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
			BP(~specs[\bp]).play(~specs[\quant]);
		} {
			BP(~specs[\bp]).stop(~specs[\quant]);
		};
	};
	~update = { |obj, what|
		if(~udepth < 50) {
			~udepth = ~udepth + 1;
			case
			// { what == \free } { ~free.() }
			{ #[schedFailed, stop, couldNotPrepare, couldNotStream].includes(what) } {
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
		~gc = ~specs[\gc].value;
		~cspec = ~specs[\spec].asSpec;
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
		if(what.isKindOf(Dictionary) and: { what[\what] == \value }) {
			BP(~oscInKey).setGUIValue(~path[0], ~cspec.unmap(~gc.value));
		};
	};
} => PR(\gcmap);

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
