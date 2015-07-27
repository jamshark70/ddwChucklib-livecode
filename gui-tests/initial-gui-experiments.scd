w = Window("test", Rect(800, 200, 500, 400)).front;

u = UserView(w, w.view.bounds);
u.background = Color(0.6, 1, 0.6);

t = TextView(w, w.view.bounds.insetBy(2, 2));
t.background = Color.clear;
t.font_(Font("Inconsolata", 36));

t.bounds_(Rect(2, 2, w.view.bounds.width - 4, 200));

x = "A".bounds(t.font).width;
z = 0;

(
var gray = Color.gray(0.6);

u.drawFunc = { |view|
	var b = view.bounds, i = z;
	Pen.color_(gray);
	while { i < b.width } {
		Pen.moveTo(Point(i, 0)).lineTo(Point(i, b.height)).stroke;
		i = i + x;
	};
};
)
u.refresh;

~xSlider = EZSlider(w, Rect(2, 204, w.view.bounds.width - 4, 20),
	"width", [1, 50],
	{ |view| x = view.value; u.refresh },
	x
);

~offsetSlider = EZSlider(w, Rect(2, 226, w.view.bounds.width - 4, 20),
	"offset", [0, 50],
	{ |view| z = view.value; u.refresh },
	z
);

// can lay a userview under a transparent textview
// offset 7 works with above
// "A".bounds(t.font).width is accurate


// Maybe: one textview per prefix, layered in a StackLayout

n = 5;

(
var stack, font = Font("Inconsolata", 36);
w = Window("stack test", Rect(800, 100, 500, 400)).front;
w.layout = VLayout(
	PopUpMenu()
	.items_(Array.fill(n, { |i| "Tab" + i }))
	.action_({ |view| stack.index = view.value }),
	stack = StackLayout(
		*(d = Array.fill(n, { |i|
			TextView()
			.font_(font)
			.string_("Document" + i)
		}))
	)
);
)

d[0].enterInterpretsSelection;  // true by default

(
var stack, font = Font("Inconsolata", 36);
w = Window("stack test", Rect(800, 100, 500, 400)); //.front;
w.layout = VLayout(
	PopUpMenu()
	.items_(Array.fill(n, { |i| "Tab" + i }))
	.action_({ |view| stack.index = view.value }),
	l = stack = StackLayout();
);
stack.add(t = TextView().string_("x"));
t.setString("xyz", 0, 0);
w.front;
)

l.insert(TextView());


// insertion/deletion tracking
// what does TextView tell us about string changes?
(
var selection = [nil, nil],  // selStart, selSize
string = "xyz";

t = TextView(nil, Rect(800, 200, 500, 400)).string_(string).front;
t.keyUpAction = { |view, char, mod, unicode, keycode, key|
	var oldSel = selection, oldStr = string;
	selection = [t.selectionStart, t.selectionSize];
	string = t.string;
	[key.asCompileString, mod, keycode].debug("key");
	oldStr.debug("key old string");
	string.debug("key string");
	[oldSel, selection].flop.debug("key selection change");
};
t.mouseUpAction = {
	var oldSel = selection;
	selection = [t.selectionStart, t.selectionSize];
	[oldSel, selection].flop.debug("mouse selection change");
};
t.receiveDragHandler = {
	var oldSel = selection, oldStr = string;
	selection = [t.selectionStart, t.selectionSize];
	string = t.string;
	oldStr.debug("d'n'd old string");
	string.debug("d'n'd string");
	[oldSel, selection].flop.debug("d'n'd selection change");
};
)

(
Proto {
	~background = Color.white;
	~stringColor = Color.black;
	~markColor = Color(0.8, 0, 0);
	~font = Font("Inconsolata", 14);
	~str = "";

	~prep = { |parentView(~parentView), bounds(~bounds)|
		~parentView = parentView;
		~bounds = bounds;
		~view = TextView(parentView, bounds)
		.string_(~str)
		.background_(~background)
		.keyUpAction_(e { |... args| ~prKeyUpAction.(*args) })
		.mouseUpAction_(e { |... args| ~prMouseUpAction.(*args) })
		// .receiveDragHandler_(e { |... args| ~prReceiveDragHandler.(*args) })
		;
		// save time for platform lookup
		// this is to detect ctrl-C and ctrl-v
		// but in OSX, it's cmd-c and cmd-v
		~ctrlMod = if(thisProcess.platform.name == \osx) { 1048576 } { 262144 };
		~bookmarks = List.new;
		currentEnvironment
	};

	~addBookmark = { |pos|
		var i = ~bookmarks.detectIndex { |mark| mark >= pos };
		case
		{ i.isNil } { ~bookmarks.add(pos) }
		{ ~bookmarks[i] != pos } {
			~bookmarks.insert(i, pos);
		};
		~drawBookmarks.();
	};
	~removeBookmark = { |pos|
		~bookmarks.remove(pos);
		~drawBookmarks.();
	};

	~background_ = { |color|
		~background = color;
		currentEnvironment
	};
	~drawBookmarks = {
		~view.background_(~background).stringColor_(~stringColor);
		~bookmarks.do { |mark|
			~view.setStringColor(~markColor, mark, 1);
		};
		currentEnvironment
	};

	~setString = { |string, start, length|
		var end = start + length;
		~view.setString(string, start, length);
		~str = ~view.string;
		~updateBookmarks.(start + (length > 0).asInteger, string.size - length, ~str);
	};
	~string = { ~view.string };
	// DO NOT OVERRIDE
	~prKeyUpAction = { |view, char, mod, unicode, keycode, key|
		var newString, ascii;
		if(char.notNil) {
			~str.postcs;
			newString = view.string.postcs;
			[newString.size, ~str.size].debug("new, old sizes");
			~updateBookmarks.(~selectionStart, newString.size - ~str.size, newString);
		};
		if(newString.notNil) {
			~str = newString;
		};
		~getSelection.();
		~keyUpAction.(view, char, mod, unicode, keycode, key);
	};
	~prMouseUpAction = { |view, x, y, mod|
		~getSelection.();
		~mouseUpAction.(view, x, y, mod);
	};
	~getSelection = {
		~selectionStart = ~view.selectionStart;
		~selectionSize = ~view.selectionSize;
		[~selectionStart, ~selectionSize]
	};
	~updateBookmarks = { |start, delta = 0, string|
		var oldMark, newMark;
		if(start.notNil and: { delta != 0 }) {
			// currentEnvironment.changed(*[\contentChanged, start, delta, string].debug("updateBookmarks"));
			block { |break|
				~bookmarks.reverseDo { |mark, i|
					if(mark < start) {
						break.()
					} {
						oldMark = ~bookmarks[~bookmarks.size - i - 1];
						newMark = mark + delta;
						~bookmarks[~bookmarks.size - i - 1] = newMark;
					};
				};
			};
			~drawBookmarks.();
		};
	};
} => PR(\bookmarkTextView);

PR(\bookmarkTextView).clone {
	~addBookmark = { |start, end|
		var i = ~bookmarks.detectIndex { |mark| mark >= start };
		[i, ~bookmarks, start, end].debug;
		case
		{ i.isNil } { ~bookmarks.add(start).add(end) }
		{ ~bookmarks[i] != start } {
			if(i.even and: { end <= ~bookmarks[i+1] }) {
				~bookmarks.insert(i, end).insert(i, start);
			} {
				"Range (% .. %) overlaps with another range".format(start, end).warn;
			};
		} {
			"Range (% .. %) overlaps with another range".format(start, end).warn;
		};
		~drawBookmarks.();
	};
	~removeBookmark = { |start|
		var i = ~bookmarks.detectIndex { |mark| mark >= start };
		if(i.notNil) {
			~bookmarks.removeAt(i);
			~bookmarks.removeAt(i);
		};
		~drawBookmarks.();
	};
	~drawBookmarks = {
		~view.background_(~background).stringColor_(~stringColor);
		~visibleBookmarks.pairsDo { |start, end|
			~view.setStringColor(~markColor, start, end - start + 1);
		};
		currentEnvironment
	};

} => PR(\rangeTextView);
)

t = PR(\bookmarkTextView).copy.prep(nil, Rect(800, 200, 500, 400)); t.view.front;

t.listVars
t.bookmarks;

t.addBookmark(29);
t.removeBookmark(29);

t.addBookmark(10);

t.addBookmark(18);
t.drawBookmarks;
t.markColor = Color(0.8, 0, 0);
t.drawBookmarks;


t = PR(\rangeTextView).copy.prep(nil, Rect(800, 200, 500, 400)); t.view.front;
t.addBookmark(5, 10);

t.bookmarks;

t.addBookmark(10, 12);

t.bookmarks;

t.removeBookmark(5);

t.addBookmark(0, 5);
t.setString("insertion... ", 0, 0);

t.bookmarks;

t.addBookmark(27, 32);  // warning, good
t.addBookmark(50, 55);

/*
old from prKeyUpAction:
		case
		// 65288 = backspace, 65535 = del
		{ keycode == 65288 or: { keycode == 65535 } } {
			newString = view.string;
			~updateBookmarks.(~selectionStart, newString.size - ~string.size);
		}
		// cut/paste: assuming ctrl-shift-c etc not allowed
		{ mod == ~ctrlMod and: {
			if(char.respondsTo(\ascii)) { ascii = char.ascii };
			ascii == 3 or: { ascii == 22 }
		} } {
			newString = view.string;
			~updateBookmarks.(~selectionStart, newString.size - ~string.size);
		}
		// possibly risky: assume printable characters
		{ char.notNil and: { char.isPrint } } {
			newString = view.string;
			~updateBookmarks.(~selectionStart, newString.size - ~string.size);
		};
*/

// scrolling VLayout?
(
w = Window("scroll", Rect(800, 200, 500, 400));
v = ScrollView(w, w.view.bounds);
c = View();
l = VLayout(
	*(t = Array.fill(3, { |i|
		[StaticText().string_("Section" + i),
			TextView().minHeight_(200)]
	}).flat)
);
c.layout = l;
v.canvas = c;
w.front;
)


// redesign: page -> section -> string
// GUI is dependant of string: still 1:1, but break infinitely-recursive structure
(
// string and pos
Proto {
	~string = "";
	~pos = 0;
	~existing = false;

	~length = { ~string.size };
	~end = { ~pos + ~string.size };

	~string_ = { |newStr|
		var oldSize = ~string.size;
		~string = newStr;
		currentEnvironment.changed(\string, newStr, oldSize);
	};
	~silentString_ = { |newStr|
		~string = newStr;
		currentEnvironment
	};
	// ~update = { |obj, what, ... args|
	// 	switch(what)
	// 	{ \guiEdit } {
	// 	};
	// };
} => PR(\clGuiString);

Proto {
	~name = "";
	~prep = {
		~strings = List.new;
		currentEnvironment
	};
	~free = {
		currentEnvironment.changed(\modelWasFreed);
	};

	~addString = { |str|
		var newStr, i;
		// if(str.last != $\n) { str = str ++ $\n };
		if(str.last == $\n) {
			str = str.drop(-1);
		};
		newStr = PR(\clGuiString).copy.string_(str);
		i = ~strings.detectIndex { |item| item.string > str };
		if(i.isNil) {
			newStr.pos = ~strings.sum(_.length) + ~strings.size;
			~strings.add(newStr);
		} {
			newStr.pos = ~strings[i].pos;
			~strings.insert(i, newStr);
		};
		currentEnvironment.changed(\addString, newStr);
	};
	~removeStrings = { |match|
		var c = ~strings.size - 1;
		~strings.reverseDo { |str, i|
			if(str.string.contains(match)) {
				~strings.removeAt(c - i);
				currentEnvironment.changed(\removeString, str);
			};
		};
		currentEnvironment
	};

	~sortStrings = {
		~strings.sort { |a, b| a.pos < b.pos };
		currentEnvironment.changed(\reset, ~strings);
	};

	~contentChanged = { |start, delta, string|
		block { |break|
			~strings.reverseDo { |str, i|
				if(str.pos < start) {
					if(str.end >= start) {
						// silentString_ b/c the change is coming from the GUI
						str.silentString_(string[str.pos .. str.end - 1 + delta]);
					};
					break.();
				} {
					if(str.existing) {
						str.pos = str.pos + delta;
					} {
						str.existing = true
					};
				};
			};
		};
		currentEnvironment
	};
} => PR(\clGuiSection);

Proto {
	~minHeight = 250;
	~prep = { |model, layout, insertIndex|
		~model = model;
		~view = PR(\rangeTextView).copy.prep(nil, nil);
		~view.view.minHeight_(~minHeight);
		if(insertIndex.notNil) {
			layout.insert(~view.view, insertIndex);
		} {
			layout.add(~view.view);
		};
		model.addDependant(currentEnvironment);
		~view.addDependant(currentEnvironment);
		currentEnvironment
	};
	~free = {
		~view.removeDependant(currentEnvironment);
		~view.remove;
		~model.strings.do(_.removeDependant(currentEnvironment));
		~model.removeDependant(currentEnvironment);
		currentEnvironment
	};
	~update = { |obj, what ... args|
		switch(what)
		{ \contentChanged } {
			~model.contentChanged(*args);
		}
		{ \string } {  // obj is the \clGuiString
			~view.setString(args[0], obj.pos, args[1]);
		}
		{ \addString } {
			args = args[0];
			~view.setString(args.string ++ $\n, args.pos, 0);
			args.addDependant(currentEnvironment);  // receive changes from this \clGuiString
		}
		{ \removeString } {
			args = args[0];
			~view.setString("", args.pos, args.length);
			args.removeDependant(currentEnvironment);
		}
		{ \reset } {
			"\clGuiSectionView:reset, implement later".warn;
		}
		{ \modelWasFreed } {
			~free.();
		};
	};
} => PR(\clGuiSectionView);

Proto {
	~prep = { |layout, insertIndex|
		~view = ScrollView();
		if(insertIndex.notNil) {
			layout.insert(~view, insertIndex);
		} {
			layout.add(~view);
		};
		~layout = VLayout();
		~view.canvas_(View().layout_(~layout));
		~sections = List.new;
		currentEnvironment
	};
	~free = {
		~sections.do { |pair| pair[0].free };
	};

	~newSection = { |name|
		var i = ~sections.detectIndex { |item| item[0].name > name },
		newSect = PR(\clGuiSection).copy.name_(name).prep,
		// newGui,
		label = StaticText().string_(name),
		newGui = PR(\clGuiSectionView).copy.prep(newSect, ~layout,
			if(i.notNil) { i * 2 });

		if(i.notNil) {
			~layout.insert(label, i * 2);
			~sections.insert(i, [newSect, newGui]);
		} {
			~layout.insert(label, ~sections.size * 2);
			~sections.add([newSect, newGui]);
		};
	};
} => PR(\clGuiPage);
)

Object.dependantsDictionary.keys  // sanity check: shouldn't have lots of Protos

(
var freeFunc = { if(p.env.notNil) { p.free } };
tryNoTrace(freeFunc);

w = Window("test", Rect(800, 200, 500, 400));
w.layout = VLayout();
w.onClose = freeFunc;
w.front;

p = PR(\clGuiPage).copy.prep(w.layout);

// a = PR(\clGuiSection).copy.prep;
// g = PR(\clGuiSectionView).copy.prep(a, w.layout);
)

w.layout.insert(StaticText().string_("hello"), 0);
w.view.children

p.newSection("a");
p.listVars;
p.sections[0][1].view.listVars
p.sections[0][1].view.view.parent.parent
p.sections[0][1].view.view.bounds
p.sections[0][1].view.view.string
w.refresh;

p.newSection("b");
p.newSection("0");

a = p.sections[1][0];

a.listVars;
g.listVars;

a.addString("test");
a.addString("abc");
a.addString("xyz");

a.strings[0].string = "abcdefg";
a.strings[2].string = "abcdefg";

g.view.listVars
g.view.getSelection
g.view.view.selectionStart

a.strings[0].listVars

a.strings.collect(_.pos);

g.free;
a.dependants
a.strings[0].dependants
a.free;
