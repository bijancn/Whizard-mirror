# Git workflow

- general introduction to git is available [here](https://git-scm.com/book/en/v2)
- only very minor (or emergency) changes are allowed to be performed
  directly in the `master`
- the `master` as well as any branch called `testing/*` are tested
  against the full test suite
- feature branches are used for everyday work and merged with merge requests
  when the corresponding small test suite succeeds
- additional special build jobs can be added to `.gitlab-ci.yml`
- commits with `[CI SKIP]` in the header will not be build by the continuous
  integration system
- branches with names containing `xfail` won't be build either
- large merge requests can be pretested with the full test-suite by renaming
  with `git checkout -b testing/branchname; git push` before they are merged
  with the `master`

# Development

- functions/subroutines should stay below 50 lines (a.k.a. roughly a screen).
  Refactoring of violators of this rule are welcome
  - In functions/subroutines where a lot of variables have to be unpacked,
    `contained` subroutines can serve as intermediate solutions with the
    naming convention:

       function foo ()
       contains
       <<module containing foo: foo: procedures>>
       end function foo

# Style settings
## general
### spacing
- indentation of 7 spaces after continuing a function/subroutine with `&`
- indentation of 5 spaces after a `&` otherwise
- indentation of 3 spaces after `if`, `do`, `case`
- no indentation after `select case`
- 1 space in front of the brace of a function call; no space within the
  braces (`call whizard (arguments)`)
- no space in front of the brace of an array (`array(index) = 1`)
- 1 space around infix operators `=`, `+`, `-`, `*`, `/`, `//`, etc.

### other
- `intent` is the last attribute and never omitted for dummy arguments
- `use some_module, only: some_class_t` is to be preferred over
  `use some_module` as it allows the compiler to warn about imported but unused
  modules
## vim
## emacs
