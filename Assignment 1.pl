s(s(S)) --> sentence(S).
s(s(S,C,T)) --> sentence(S), conjunction(C), s(T).

sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase_plus(VP).

noun_phrase(np(NP)) --> complex_noun(NP).
noun_phrase(np(NP,C,T)) --> complex_noun(NP), conjunction(C), noun_phrase(T).

verb_phrase_plus(vpplus(VP)) --> verb_phrase(VP).
verb_phrase_plus(vpplus(VP,C,T)) --> verb_phrase(VP), conjunction(C), verb_phrase_plus(T).

verb_phrase(vp(VPLUS,NP)) --> verb_plus(VPLUS), noun_phrase(NP).
verb_phrase(vp(VPLUS,NP1,NP2)) --> verb_plus(VPLUS), noun_phrase(NP1), noun_phrase(NP2).
verb_phrase(vp(VPLUS,NP,PPP)) --> verb_plus(VPLUS), noun_phrase(NP), preposition_phrase_plus(PPP).
verb_phrase(vp(VPLUS,NP1,NP2,PPP)) --> verb_plus(VPLUS), noun_phrase(NP1), noun_phrase(NP2), preposition_phrase_plus(PPP).

complex_noun(cn(N)) --> noun(N).
complex_noun(cn(N,ADV)) --> noun(N), adverb_plus(ADV).
complex_noun(cn(ADJ,N)) --> adjective_plus(ADJ), noun(N).
complex_noun(cn(ADJ,N,ADV)) --> adjective_plus(ADJ), noun(N), adverb_plus(ADV).
complex_noun(cn(D,N)) --> det(D), noun(N).
complex_noun(cn(D,N,ADV)) --> det(D), noun(N), adverb_plus(ADV).
complex_noun(cn(D,ADJ,N)) --> det(D), adjective_plus(ADJ), noun(N).
complex_noun(cn(D,ADJ,N,ADV)) --> det(D), adjective_plus(ADJ), noun(N), adverb_plus(ADV).

verb_plus(vplus(V)) --> verb(V).
verb_plus(vplus(V,C,T)) --> verb(V), conjunction(C), verb_plus(T).

preposition_phrase_plus(ppplus(PP)) --> preposition_phrase(PP).
preposition_phrase_plus(ppplus(PP,T)) --> preposition_phrase(PP), preposition_phrase_plus(T).
preposition_phrase(pp(P,CN)) --> preposition(P), complex_noun(CN).
preposition_phrase(pp(P,N)) --> preposition(P), noun(N).

adjective_plus(adjplus(ADJ)) --> adjective(ADJ).
adjective_plus(adjplus(ADJ,T)) --> adjective(ADJ),adjective_plus(T).

adverb_plus(advplus(ADV)) --> adverb(ADV).
adverb_plus(advplus(ADV,T)) --> adverb(ADV),adverb_plus(T).

conjunction(conj(and)) --> [and].

noun(n(boy)) --> [boy].
noun(n(box)) --> [box].
noun(n(room)) --> [room].
noun(n(school)) --> [school].
noun(n(woman)) --> [woman].
noun(n(man)) --> [man].
noun(n(envelope)) --> [envelope].
noun(n(shed)) --> [shed].
noun(n(building)) --> [building].
noun(n(tree)) --> [tree].
noun(n(girl)) --> [girl].
noun(n(students)) --> [students].
noun(n(professors)) --> [professors].
noun(n(lecturers)) --> [lecturers].
noun(n(scientists)) --> [scientists].
noun(n(researchers)) --> [researchers].
noun(n(cat)) --> [cat].
noun(n(dog)) --> [dog].
noun(n(car)) --> [car].
noun(n(house)) --> [house].

verb(v(pushed)) --> [pushed].
verb(v(stored)) --> [stored].
verb(v(gave)) --> [gave].
verb(v(climbed)) --> [climbed].
verb(v(watched)) --> [watched].
verb(v(admired)) --> [admired].
verb(v(appreciated)) --> [appreciated].
verb(v(bought)) --> [bought].
verb(v(sold)) --> [sold].
verb(v(ran)) --> [ran].
verb(v(walked)) --> [walked].
verb(v(slept)) --> [slept].
verb(v(searched)) --> [searched].
verb(v(read)) --> [read].
verb(v(lived)) --> [lived].
verb(v(wandered)) --> [wandered].
verb(v(studied)) --> [studied].
verb(v(opened)) --> [opened].
verb(v(closed)) --> [closed].
verb(v(stopped)) --> [stopped].

adjective(adj(young)) --> [young].
adjective(adj(big)) --> [big].
adjective(adj(large)) --> [large].
adjective(adj(empty)) --> [empty].
adjective(adj(old)) --> [old].
adjective(adj(poor)) --> [poor].
adjective(adj(white)) --> [white].
adjective(adj(brilliant)) --> [brilliant].
adjective(adj(talented)) --> [talented].
adjective(adj(bright)) --> [bright].
adjective(adj(small)) --> [small].
adjective(adj(rich)) --> [rich].
adjective(adj(black)) --> [black].
adjective(adj(lovely)) --> [lovely].
adjective(adj(beautiful)) --> [beautiful].
adjective(adj(strong)) --> [strong].
adjective(adj(weak)) --> [weak].
adjective(adj(tall)) --> [tall].
adjective(adj(happy)) --> [happy].
adjective(adj(sad)) --> [sad].

adverb(adv(quickly)) --> [quickly].
adverb(adv(slowly)) --> [slowly].
adverb(adv(happily)) --> [happily].
adverb(adv(sadly)) --> [sadly].
adverb(adv(softly)) --> [softly].
adverb(adv(definitely)) --> [definitely].
adverb(adv(probably)) --> [probably].
adverb(adv(gently)) --> [gently].
adverb(adv(greedily)) --> [greedily].
adverb(adv(honestly)) --> [honestly].

preposition(p(in)) --> [in].
preposition(p(after)) --> [after].
preposition(p(behind)) --> [behind].
preposition(p(before)) --> [before].
preposition(p(with)) --> [with].
preposition(p(from)) --> [from].
preposition(p(by)) --> [by].
preposition(p(to)) --> [to].
preposition(p(on)) --> [on].
preposition(p(under)) --> [under].

det(d(the)) --> [the].
det(d(a)) --> [a].
det(d(each)) --> [each].
det(d(every)) --> [every].
det(d(some)) --> [some].
det(d(many)) --> [many].
det(d(all)) --> [all].
det(d(this)) --> [this].
det(d(that)) --> [that].
det(d(half)) --> [half].
