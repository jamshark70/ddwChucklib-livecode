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
		~str = "%%%".format(
			if(start > 0) { ~str[0 .. start - 1] } { "" },
			string,
			if(end <= ~str.size) { ~str[end - 1 .. ] } { "" }
		);
		~updateBookmarks.(start + (length > 0).asInteger, string.size - length);
	};
	~string = { ~view.string };
	// DO NOT OVERRIDE
	~prKeyUpAction = { |view, char, mod, unicode, keycode, key|
		var newString, ascii;
		if(char.notNil) {
			newString = view.string;
			~updateBookmarks.(~selectionStart, newString.size - ~str.size);
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
	};
	~updateBookmarks = { |start, delta = 0|
		var oldMark, newMark;
		if(start.notNil and: { delta != 0 }) {
			block { |break|
				~bookmarks.reverseDo { |mark, i|
					if(mark < start) {
						break.()
					} {
						oldMark = ~bookmarks[~bookmarks.size - i - 1];
						newMark = mark + delta;
						~bookmarks[~bookmarks.size - i - 1] = newMark;
						currentEnvironment.changed(\markMoved, oldMark, newMark);
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
		case
		{ i.isNil } { ~bookmarks.add(start).add(end) }
		{ ~bookmarks[i] != start } {
			if(i.even and: { end <= ~bookmarks[i+1] }) {
				~bookmarks.insert(i, end).insert(i, start);
			} {
				"Range (% .. %) overlaps with another range".format(start, end).warn;
			};
		};
		~drawBookmarks.();
	};
	~removeBookmark = { |start|
		var i = ~bookmarks.detectIndex { |mark| mark >= start };
		if(i.notNil) {
			~bookmarks.removeAt(i).removeAt(i);
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
t.addBookmark(25, 30);
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