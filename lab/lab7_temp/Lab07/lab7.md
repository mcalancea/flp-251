# Laboratorul 7 – Corespondența Curry-Howard

Corespondența Curry-Howard ne arată că tipurile din lambda
calculul cu tipuri simple pot fi folosite pentru a reprezenta propoziții din logică
(propozițională, deocamdată) și faptul de a defini un termen de un astfel de tip corespunde
la o demonstrație a adevărului propoziției (în fragmentul intuiționist al logicii propoziționale).

În cadrul acestul laborator vom experimenta cu această corespondență în Haskell,
folosind atăt tipurile de date algebrice corespunzătoare constructorilor de formule din logică,
cât și codările Church corespunzatoare lor în lambda calculul cu tipuri simple.

Pentru fiecare din fișierele `src/CurryHowardTyped.hs` și `src/CurryHowardUntyped.hs` aveți definite
tipuri pentru `False`, `True`, `And`, `Or`, `Not` și `Iff`, precum și tipul predefinit `->` pentru implicație.

Va trebui să definiți căte un termen corespunzând unor reguli de deducție sau axiome / tautologii din logica propozițională. Pentru a deosebi implicația logică de tipul săgeată, vom folosi în regulile de mai jos simbolul $\supset$ pentru implicația logică.

Puteți folosi orice în afară de `undefined` sau recursie. Faptul că
reușiți să definiți un termen de tipul respectiv înseamnă că tipul este inhabited și deci rezultatul logic corespunzător este demonstrabil.

## Reguli de deducție naturală pentru logica propozițională intuiționistă

Următorul sistem de deducție este (mai mult decât) complet pentru deducția în logica propozițională intuiționistă.

Regulile sunt preluate după [Automated Theorem Proving
de Frank Pfenning](https://www.cs.cmu.edu/~fp/courses/atp/handouts/ch2-natded.pdf)

- regulă de introducere pentru `True`
  $$\frac{\checkmark}{\top}$$

- regulă de eliminare pentru `False`
  $$\frac{\bot}{\varphi}$$

- o regulă de introducere și una de eliminare pentru implicație
  $$\frac{\begin{array}{c}a\\ \vdots \\b \end{array}}{a \supset b}\hspace{3em}\frac{a \supset b\;\;\; a}{b}$$

- o regulă de introducere și două de eliminare pentru `And`
  $$\frac{a \;\;\; b}{a \wedge b} \hspace{3em} \frac{a \wedge b}{a} \hspace{3em} \frac{a \wedge b}{b}$$

- două reguli de introducere și una de eliminare pentru `Or`
  $$\frac{a}{a \vee b} \hspace{3em} \frac{b}{a \vee b} \hspace{3em} \frac{a \vee b\;\;\;a \supset c\;\;\;b \supset c}{c}$$

- o regulă de introducere și una de eliminare pentru `Not`
  $$ \frac{\begin{array}{c}a\\ \vdots \\p \end{array}}{\neg a}\hspace{3em} \frac{a \;\;\; \neg a}{p}$$

- o regulă de introducere și două de eliminare pentru `Iff`
  $$\frac{a \supset b\;\;\; b \supset a}{a \leftrightarrow b}\hspace{3em}\frac{a\leftrightarrow b\;\;\;a}{b}\hspace{3em}\frac{a\leftrightarrow b\;\;\;b}{a}$$

## O axiomatizare Hilbert pentru logica propozițională intuiționistă

Un alt sistem de deducție pentru logica propozițională intuiționistă constă din zece axiome și o singură regulă de deducție (modus ponens): https://plato.stanford.edu/entries/logic-intuitionistic/

- ax1: $a \supset b \supset a$
- ax2: $(a \supset b) \supset ((a \supset (b \supset c)) \supset (a \supset c))$
- ax3: $a \supset (b \supset (a \wedge b))$
- ax4: $(a \wedge b) \supset a$
- ax5: $(a \wedge b) \supset b$
- ax6: $a \supset (a \vee b)$
- ax7: $b \supset (a \vee b)$
- ax8: $(a \supset c) \supset ((b\supset c) \supset ((a \vee b) \supset c))$
- ax9: $(a \supset b) \supset ((a \supset \neg b) \supset \neg a)$
- ax10: $\neg a \supset (a \supset b)$
- the modus ponens rule: $$\frac{a \supset b\;\;\; a}{b}$$

## Câteva tautologii

- $p \supset (\neg p \supset \bot)$
- deMorgan rule 1: $(\neg p \wedge \neg q) \supset \neg (p \vee q)$
- deMorgan rule 2: $\neg (p \vee q) \supset (\neg p \wedge \neg q)$
- deMorgan rule 3: $(\neg p \vee \neg q) \supset \neg (p \wedge q)$
- the remaining deMorgan rule is not provable in intuitionistic logic
- Excluded middle implies double negation: $(a \vee \neg a) \supset (\neg \neg a \supset a)$
- double negation implies excluded middle: if $(\neg \neg a \supset a)$ hods for every $a$, then  $(a \vee \neg a)$ holds for every $a$.
