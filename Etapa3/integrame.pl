:- ensure_loaded('checker.pl').

test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un literal, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de literali reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un literal)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
%
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.
intrebari(_, _) :- false.
intrebari(integ(_, _, [], _), []).
intrebari(integ(_, _, [((R, C), [Text]) | Rest], _), [((R, C), Text) | Lista_intrebari]) :- intrebari(integ(_, _, Rest, _), Lista_intrebari), !.
intrebari(integ(_, _, [((R, C), [Text, Text2]) | Rest], _), [((R, C) , Text), ((R, C) , Text2) | Lista_intrebari]) :- intrebari(integ(_, _, Rest, _), Lista_intrebari), !.
intrebari(integ(_, _, [_ | Rest], _), Lista_intrebari) :- intrebari(integ(_, _, Rest, _), Lista_intrebari).


% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.
id_intrebare(_, _, _) :- false.
id_intrebare(integ(_, _, [((_, _), [(Text, _, ID)]) | Rest], _), Intrebare, Q_ID) :- Text == Intrebare, Q_ID is ID; Q_ID == ID, Intrebare = Text, id_intrebare(integ(_, _, Rest, _), Intrebare, Q_ID).
id_intrebare(integ(_, _, [((_, _), [(Text, _, ID), (Text2, _, ID2)]) | Rest], _), Intrebare, Q_ID) :- Text == Intrebare, Q_ID is ID; Q_ID == ID, Intrebare = Text;
																									  Text2 == Intrebare, Q_ID is ID2; Q_ID == ID2, Intrebare = Text2;
																									  id_intrebare(integ(_, _, Rest, _), Intrebare, Q_ID).
id_intrebare(integ(_, _, [_ | Rest], _), Intrebare, Q_ID) :- id_intrebare(integ(_, _, Rest, _), Intrebare, Q_ID).


% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvânt
% de completat; ambele sunt atomi (literali).
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).
completare(_, _, _) :- false.

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% pentru Bonus:
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), ?Intrebare, ?Lungime)
%
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).


%incr(+X, -X1) 
incr(X, X1) :- X1 is X+1.


%calculeaza_lungimea(+Integ(...), +R, +C, +Directie, -Lungime)
calculeaza_lungimea(integ(H, W, Lista, _), R, C, Directie, Lungime) :- member(((R, C), _), Lista), Lungime is 0;
																	   Dir = d, Directie == Dir, incr(C, C2), 
																	   calculeaza_lungimea(integ(H, W, Lista, _), R, C2, Directie, Lungime2), incr(Lungime2, Lungime), !;
																	   member(((R, C), _), Lista), Lungime is 0;
																	   Dir = j, Directie == Dir, incr(R, R2), 
																	   calculeaza_lungimea(integ(H, W, Lista, _), R2, C, Directie, Lungime2), incr(Lungime2, Lungime), !.
															
															
%lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
lungime_spatiu(integ(H, W, Lista, _), Intrebare, Lungime) :- intrebari(integ(_, _, Lista, _), Lista_intrebari),
															 member(((R, C), Intrebare, Directie, _), Lista_intrebari),
															 Dir = d, Directie == Dir, incr(C, C2),
															 calculeaza_lungimea(integ(H, W, Lista, _), R, C2, Directie, Lungime);
															 intrebari(integ(_, _, Lista, _), Lista_intrebari),
															 member(((R, C), Intrebare, Directie, _), Lista_intrebari),
															 Dir = j, Directie == Dir, incr(R, R2),
															 calculeaza_lungimea(integ(H, W, Lista, _), R2, C, Directie, Lungime).

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% pentru Bonus:
% intersectie(integ(+H, +W, +Lista, +Voc), ?I1, ?Poz1, ?I2, ?Poz2)
%
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
intersectie(_, _, _, _, _) :- false.
intersectie(integ(_, _, Lista, _), I1, Poz1, I2, Poz2) :- intrebari(integ(_, _, Lista, _), Lista_intrebari),
														  member(((R1, C1), I1, Directie1, _), Lista_intrebari),
														  member(((R2, C2), I2, _, _), Lista_intrebari),
														  Dir = d, Directie1 == Dir, R1 > R2, C1 < C2, 
														  Poz1 is C2 - C1 - 1, Poz2 is R1 - R2 - 1;
	
														  intrebari(integ(_, _, Lista, _), Lista_intrebari),
														  member(((R1, C1), I1, Directie1, _), Lista_intrebari),
														  member(((R2, C2), I2, _, _), Lista_intrebari),
														  Dir = j, Directie1 == Dir, R2 > R1, C1 > C2,
														  Poz1 is R2 - R1 - 1, Poz2 is C1 - C2 - 1.


% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de atomi, fiecare atom
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
solutii_posibile(_, _) :- false.
solutii_posibile(integ(H, W, Lista, Vocabular), Solutii) :- findall().

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de literali, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca literal) care este
% răspunsul la întrebare.
%
% BONUS: rezolvare nu oferă soluții duplicate - numărul de soluții ale 
% predicatului este chiar numărul de completări posibile ale integramei.
rezolvare(_, _) :- false.
