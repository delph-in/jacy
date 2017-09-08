
# Change Log

## 2017-09-08

This release encapsulates many semantic fixes, updated metadata, and
efforts to get Jacy working with both ACE and the LKB.

The changes can be summarized as follows:

* harmonize predicate forms (see: http://moin.delph-in.net/PredicateRfc)
* remove redundant predicates
* fix mismatched coreferences
* help ensure ARG0s are present all all non-quantifier EPs
* normalize treatment of negation
* fix some problems causing ACE warnings
* update docs to include new canonical citation
* add entities for ShapeWorld (see https://arxiv.org/pdf/1704.04517.pdf)

## 2016-11-08

This release corresponds roughly to the state of the grammar for the
Jacy book. No change log was kept since 2007-11-28, but there were
about 250 commits during this range. Try the following command to see
the commit history in a format roughly like the old log:

```bash
git log --pretty=format:"%cd %an%n  * %s%n" --since 2007-11-29 --until 2016-11-07 --date=short
```

## Old log

The following is the original change log; it stopped in late 2007

> 2007-11-28  Francis Bond  <bond@ronf>
>
>   * harmonized dates a little - added "dofw_rel", changed NUMBER to ARG1
> 
> 2007-11-14  Francis Bond  <bond@ronf>
> 
>   * added s-end1-neg-imp-lex for 吠える　な "don't bark"
> 
> 2007-09-05  Francis Bond  <bond@nooka>
> 
>   * used :+ to redefine TAM and Messages
>   * fixed some redefinitions by merging the changes (lexical_sign-word, lex-rule)
> 
> 2007-07-12  Francis Bond  <bond@nooka>
> 
>   * added the semi.vpm to the pet settings
>   * replaced WLINK with LNK for new characterization support
> 
> 2007-05-02  Francis Bond  <bond@nooka>
> 
>   * added a patch to fix MWE lexicon indexing
> 
> 2007-05-01  Francis Bond  <bond@knut>
> 
>   * finished changing ORTH to STEM with oe's help
> 
> 
> 2007-04-20  Francis Bond  <bond@nooka>
> 
>   * changed preposition argument order to match standard practice
>     - ARG1 is external (modifiee)
>     - ARG2 is internal (complement)
>     - ARG1 on ARG2 
>     - NP on NP: the book_1 on the table_2
>     - V on NP:  I ate_1 on Monday_2
> 
>     
> 2007-04-17  Francis Bond  <bond@nooka>
> 
>   * converted to utf-8
> 
>   * added dumped lexicon
> 
>   * changed ORTH to STEM for pet <not fully working>
> 
> 2006-11-05  Francis Bond  <bond@localhost.localdomain>
> 
>   * added は and も entries for case-p-lex-postp-ga to allow the
>     case reading after postps.
> 
>   * changed  v-end_head to inherit from adj-or-adv_head.
>     This allows degree adverbs to modify past-tense adjectives.
>     
>     FIXME: now we over generate by modifying past tense normal verbs
>     
> 2006-11-02  Francis Bond  <bond@localhost.localdomain>
> 
>   * changelog: pred-adj-lex, made a sub-class of adjectives, so that
>     it can be modified by とても, made it BAR - to reduce ambiguity.
> 