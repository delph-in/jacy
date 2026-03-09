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

You can use a tokenizer such as mecab to tokenize the input:
```
echo "カタカナも漢字も大丈夫です。" | mecab -O wakati | ace -g ~/git/jacy/jacy.dat
SENT: カタカナ も 漢字 も 大丈夫 です 。 
[...]
NOTE: parsed 1 / 1 sentences, avg 1733k, time 0.00666s
```

You can use a different tokenizer, but mecab is what we used. In this configuration it will not handle unknown words, so every word has to be in the lexicon.  This is good for grammar development, but not so robust. For example, the word 平仮名 is not in the dictionary while 片仮名 and カタカナ, so it could not parse the sentence
"平仮名 も カタカナ も 漢字 も 大丈夫 です 。 ".

To be more robust, you can use use an input lattice (yy-mode) that also passes through part of speech.   The system will then handle (some) unknown words.  There is a utility to do this in the jacy repository.  Assuming you have all the dependencies installed you can go:

```
echo 'JACYは平仮名もカタカナも漢字も大丈夫です。' | utils/jpn2yy | ace -g jacy.dat -yy
SENT: (yy mode)
[...]
NOTE: parsed 1 / 1 sentences, avg 2676k, time 0.01190s
```


[Jacy]: http://moin.delph-in.net/JacyTop
[Siegel, Bender, & Bond, 2016]: https://web.stanford.edu/group/cslipublications/cslipublications/site/9781684000180.shtml
[Siegel & Bender, 2002]: http://www.aclweb.org/anthology/W/W02/W02-1210.pdf
[LKB]: http://moin.delph-in.net/LkbTop
[ACE]: http://sweaglesw.org/linguistics/ace/
[agree]: http://moin.delph-in.net/AgreeTop
[MRS]: http://moin.delph-in.net/RmrsTop
[MeCab]: http://taku910.github.io/mecab/

