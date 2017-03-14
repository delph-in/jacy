# Jacy

The [Jacy][] Japanese grammar ([Siegel, Bender, & Bond, 2016][];
[Siegel & Bender, 2002][]) is a broad-coverage HPSG grammar of
Japanese. In combination with a parser (such as the [LKB][], [ACE][],
or [agree][]), it can analyze Japanese sentences, yielding derivation
trees and [MRS][] semantic representations, and also generate
sentences from semantic representations.

Input sentences are tokenized using a morphological analyzer like
[MeCab][].

## Quick Start

The [ACE][] parser/generator works on Linux and Mac machines. After
installing ACE, the following commands will let you parse and generate
with Jacy:

```bash
~$ git clone https://github.com/delph-in/jacy.git
~$ cd jacy/
~/jacy$ ace -g ace/config.tdl -G jacy.dat
~/jacy$ echo "太郎 が 次郎 に 本 を 渡し た" | ace -g jacy.dat
[...]
NOTE: parsed 1 / 1 sentences, avg 2837k, time 0.02782s
~/jacy$ echo "太郎 が 次郎 に 本 を 渡し た" | ace -g jacy.dat | ace -g jacy.dat -e
[...]
太郎 が 次郎 に 本 を 渡し た
次郎 に 太郎 が 本 を 渡し た
次郎 に 本 を 太郎 が 渡し た
[...]
NOTE: generated 1 / 9 sentences, avg 2653k, time 0.06851s
```

[Jacy]: http://moin.delph-in.net/JacyTop
[Siegel, Bender, & Bond, 2016]: https://web.stanford.edu/group/cslipublications/cslipublications/site/9781684000180.shtml
[Siegel & Bender, 2002]: http://www.aclweb.org/anthology/W/W02/W02-1210.pdf
[LKB]: http://moin.delph-in.net/LkbTop
[ACE]: http://sweaglesw.org/linguistics/ace/
[agree]: http://moin.delph-in.net/AgreeTop
[MRS]: http://moin.delph-in.net/RmrsTop
[MeCab]: http://taku910.github.io/mecab/

