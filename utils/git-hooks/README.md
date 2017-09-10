# Git Hooks

This directory contains hooks for Git that developers are encouraged
to use when working on the grammar.

To use the hooks, please copy them into your `.git/hooks` directory.
For example:

```
$ cp utils/git-hooks/pre-commit .git/hooks/pre-commit
```

## pre-commit

The pre-commit hook checks for things before a commit happens. It can
be used, e.g., to check if a grammar compiles or passes regression
tests. The script here only checks if Version.lsp has been updated;
if not, it prompts the user to update it and commit, commit anyway, or
abort the commit entirely.
