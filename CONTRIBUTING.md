
# How to help

We'd be happy to accept any help in the development of Jacy. Whether
you're a first-time user or a seasoned grammar developer, you can
contribute to help make the grammar and its ecosystem better. See the
headings below for information about different ways of contributing.

## Bug reports

Submit issues here: https://github.com/delph-in/jacy/issues

Please note relevant info in your bug report, such as the grammar
version (see the `Version.lsp` file, or the git commit hash), your
platform and environment (e.g., Ubuntu 16.04, ACE 0.9.25), the
sentences that are causing problems, etc.

We welcome reports for a variety of issues:
 * trouble compiling the grammar
 * trouble parsing/generating with the grammar
 * analyses or realizations are incorrect
 * analyses or realizations are incomplete
 * questions about the grammar (use the [question][] label)
 * etc.

It's normal for a grammar to have gaps in its coverage, so its
generally unhelpful to submit issues such as "the grammar doesn't
parse this sentence", unless you have a reasonable expectation for
that sentence to parse (all the words are covered in the
lexicon, the sentence is well formed, etc.).

Also note that some questions may get a better answer on the DELPH-IN
[developers list][].

[question]: https://github.com/delph-in/jacy/labels/question
[developers list]: http://lists.delph-in.net/mailman/listinfo/developers

## Code contributions

If you want to improve the grammar itself by editing the code, work
off of the latest [develop][] branch. If you have commit access to the
main Jacy repository, consider making a new branch to contain your
changes until they are ready to be merged. If you don't have commit
access, create a private fork of Jacy and commit your work there.
When your changes are ready to be merged, [submit a pull
request](https://github.com/delph-in/jacy/compare). Finally, if you
don't have a GitHub account, feel free to generate a patch and attach
it to an issue.

[develop]: https://github.com/delph-in/jacy/tree/develop

##### Coding style

Try to follow conventional code styles, whether it's for Python,
Shell, TDL, or something else. If you don't know what the convention
is, try to follow the style of the existing code.

##### Portability

Where possible, make your code portable. The primary platform for Jacy
development is Linux, followed by Mac and then Windows.

##### Versioned assets

Don't commit binaries (e.g., compiled grammar images) or easily
reproducible files. Rather, document any dependencies or provide
scripts to reproduce the data. An exception is the [SEM-I], which
can largely be reproduced by the `etc/make-sem-i.bash` script, but
this is generally updated for release commits only.

##### Regression tests

Changes to the grammar should be tested to ensure they don't break
coverage in other areas. If up-to-date regression tests are available,
run those before committing the changes.

## Documentation

Documentation is an important part of any digital resource, but often
we don't have adequate time (read: funding) to produce comprehensive
and up-to-date documentation. Any contributions of documentation are
very welcome. If you don't have the knowledge to write the
documentation, [raise an issue][] to make the problem known.

[raise an issue]: https://github.com/delph-in/jacy/issues

## Treebanking

Competent (preferrably but not necessarily) native speakers of
Japanese with an understanding of HPSG and MRS can learn to produce
or update treebanks. Treebanks are immensely useful as they can be
used to produce better parse reranking models, for regression testing,
as gold-standard data for data-driven tasks, etc. Producing treebanks
has the additional benefit of showing where the grammar has gaps in
coverage or where it overgenerates, which can direct further
development efforts.

## Release Checklist

Core developers of Jacy should follow the following checklist when
producing a release:

##### Prepare

- [ ] merge all included feature branches into the `develop` branch
- [ ] regenerate the SEM-I (run `etc/make-sem-i.bash`)
- [ ] run regression tests on a pristine copy of the `develop` branch
- [ ] commit all affected files

##### Treebank

- [ ] treebank with the new grammar
- [ ] the above may reveal bugs to be fixed; repeat the 'prepare'
      steps if necessary, then re-treebank
- [ ] regenerate the parse reranking model
- [ ] commit all affected files

##### Document

- [ ] document major changes in `CHANGELOG.md` (this can be done as
      you go along)
- [ ] update `README.md` if necessary
- [ ] update `Version.lsp` with a [release version][]
- [ ] make the release commit

##### Publish

- [ ] push the `develop` branch
- [ ] checkout the `master` branch
- [ ] merge the `develop` branch into `master` (there shouldn't be any
      conflicts, but if there are, you'll need to resolve them, test
      again, and merge `master` back to `develop`)
- [ ] push the `master` branch

##### Finalize

- [ ] close the remaining tickets of resolved issues
- [ ] [draft a release](https://github.com/delph-in/jacy/releases/new)
- [ ] Announce on the developers list or elsewhere

##### Additional

- [ ] update [LTDB](http://moin.delph-in.net/LkbLtdb)
- [ ] update grammar versions for demos ([Demophin][], [Bottlenose][])

[release version]: http://moin.delph-in.net/GrammarVersionRfc
[Demophin]: http://chimpanzee.ling.washington.edu/demophin
[Bottlenose]: http://chimpanzee.ling.washington.edu/bottlenose/jacy/parse?input=%22%E7%8A%AC%20%E3%81%8C%20%E5%90%A0%E3%81%88%E3%82%8B%22&mrs=json
