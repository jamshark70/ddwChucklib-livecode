
BP(\edit).free; PR(\clEditGui).chuck(BP(\edit), nil, (bounds: Rect(800, 50, 500, 600)));
BP(\edit).pages

BP(\edit).newPage(\z);
BP(\edit).newPage(\a);

/edit(updateDefer)
/edit(updateDefer = 0.1)

// test

/k(free)

(
PR(\abstractLiveCode).chuck(BP(\k), nil, (
	defaultParm: \amp,
	parmMap: (amp: ($-: 0.5, $.: 0.1))
));

PR(\abstractLiveCode).chuck(BP(\j), nil, (
	defaultParm: \amp,
	parmMap: (amp: ($-: 0.5, $.: 0.1))
));
)

e = BP(\edit);
z = e.pages[0].sections[0][0];
z.strings.do(_.listVars);

e.pages[0].sectionForPhraseIndex(nil)
e.phraseAndIndex("main").postcs


/k = "----";  // k, j: OK; j, k: inserts one char early, messes up bookmarks
/j = " - -";

/k = "|-  -|--| -  ";
/j = "|-  -||-";
1+1

/k.main0 = "-| -|-|";

/k.main3 = "-| -|-|";
/j.main3 = " - -";

/k.main2 = "-| -|-|";

/j.main1 = "-";
/k.main1 = "--";


/k.a0 = "-|   -| -- |";

/k.d1 = "-| -|-  -| -  ";