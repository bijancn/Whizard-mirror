# Git and CI workflow

- For general introduction to git, have a look at the
  [git book](https://git-scm.com/book/en/v2)
- commits into `master` are by construction impossible
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
- **Any change to `.gitlab-ci.yml` has to be verified in a `testing/branchname`
  branch before it is allowed to merge into master**
- As the full test suite is only run *after* the merge, it is still possible
  (though fairly unlikely) to break the `master`. This might change in a future
  version of `gitlab` to create an unbreakable system.

# Development infrastructure

- Files ending with `.nw` are [noweb](https://www.cs.tufts.edu/~nr/noweb/)
  files. There is a cheat sheet on the syntax
  [available](https://www.cs.tufts.edu/~nr/noweb/onepage.ps).
- Our documentation chunks are written in latex (`-> .tex`) and the code chunks
  in modern Fortran (`-> .f90`).
- modules follow this structure

```
<<[[module_name.f90]]>>=
<<File header>>

module module_name

<<Use kinds>>
	use other_dependencies

<<Standard module head>>

<<Module name: public>>

<<Module name: parameters>>

<<Module name: types>>

<<Module name: interfaces>>

contains

<<Module name: procedures>>

end module module_name
@ %def module_name
```
- The `%def` are keywords for the latex documentation index

- types with type bound procedures (i.e. classes) are put in chunks

```
@
<<$Module name: public>>=
	public :: some_class_t
<<Module name: types>>=
	type :: some_class_t
		 real :: some_value
     ...
	contains
	 <<$Module name: some class: TBP>>
	end type some_class_t

@ %def some_class_t
```

  Appending `_t` is mandatory and reduces name clashes

- functions/subroutines go also in code chunks

```
@ Some documentation what the idea behind this function is
<<Module name: procedures>>=
  subroutine some_subroutine ()
    ...
  end subroutine some_subroutine

@ %def some_subroutine
```

- If it is a public function/subroutine, there is an additional chunk in
  between documentation and procedure chunk

```
<<Module name: public>>=
  public :: some_subroutine
```

- If it is a type bound function/subroutine (i.e. a method of a class), there is
  an additional chunk between documentation and procedure chunk

```
<<Module name: class name: TBP>>=
	procedure :: do_something => class_name_do_something
```

- In functions/subroutines where a lot of variables have to be unpacked in a
  straightforward dumb way, `contained` subroutines can serve as intermediate
  refactoring solutions with the naming convention:

```
<<Module name: procedures>>=
  function foo ()
  contains
  <<Module name: foo: procedures>>
  end function foo

@ %def foo
<<Module name: foo: procedures>>=
  subroutine contained_sub_routine ()
  end subroutine
```

# Testing
- New functionalities should be covered with unit tests on a small scale and
  functional tests (could also be called integration tests) on a larger scale
- For faster debugging one can only run `make` and then the functional test in
  question
- During refactorings, one should run a full `make check` or have an eye on the
  continuous integration server

## Unit tests
- Unit tests are always bundled in test suites with the naming that `test_suite`
  contains `test_suite_1`, `test_suite_2`, etc.
- For each unit test there is a reference file with the expected output as
  `share/tests/unit_tests/ref-output/test_suite_1.ref`. It is ensured that the
  test does not depend on the used precision by using numerically safe functions
  like `assert_equal` found in `numeric_utils.f90`.
- Unit tests also document how classes should be called and what to expect from
  them.
- The reference files have to registered in `share/tests/Makefile.am` to the
  according variables.
- The tests are run with shell scripts as `tests/unit_tests/test_suite.sh`
- They will be substituted and renamed to `tests/unit_tests/test_suite.run`
  in the build folder according to the rule in
  `tests/unit_tests/Makefile.am`. The test scripts also have to be
  registered in this Makefile.
- The scripts that run the unit tests consist of a simple
  `exec ./run_whizard_ut.sh --check unit_test_suite`. Unit tests may not depend
  on external dependencies.

## Functional tests
- For each functional test there is a reference file with the expected output as
  `share/tests/functional_tests/ref-output/test_name.ref`. In case the output
  differs between double and extended and/or quadruple precision, there will be
  two or three versions in `ref-output-double`, `ref-output-prec` (these are
  valid for extended as well as quadruple), `ref-output-ext` and
  `ref-output-quad`, accordingly.
- For each functional test there is a sindarin file like
  `share/tests/functional_tests/test_name.sin`.
- The reference files have to registered in `share/tests/Makefile.am` to the
  according variables. Same goes for the sindarin files.
- The tests are run with shell scripts as `tests/functional_tests/test_name.sh`
- They will be substituted and renamed to `tests/functional_tests/test_name.run`
  in the build folder according to the rule in
  `tests/functional_tests/Makefile.am`. The test scripts also have to be
  registered in this Makefile.
- In case the functional test depends on external dependencies like `OMega`,
  `LHAPDF`, etc., the test script has to check for the file
  `OCAML_FLAG`, `LHAPDF6_FLAG`, etc., which is created by the Makefile if the
  dependency is fulfilled, and skip the test by `exit`ing with `77` otherwise.

# Style settings
## spacing
- indentation of 7 spaces after continuing a function/subroutine with `&`
- indentation of 5 spaces after a `&` otherwise
- indentation of 3 spaces after `if`, `do`, `case`
- no indentation after `select case`
- 1 space in front of the brace of a function call; no space within the
  braces (`call whizard (arguments)`)
- no space in front of the brace of an array (`array(index) = 1`)
- 1 space around infix operators `=`, `+`, `-`, `*`, `/`, `//`, etc.

## programming
- Main programming style is object-oriented programming (OOP), peppered with
  some old parts written in imperative style and salted with the occasional use
  of functional programming (FP).
- `pure` functions/subroutines are preferred where possible as it makes it
  easier to reason about the code without worrying about side effects
- `elemental` is even better where appropriate as it allows to use the same
  function for a single element as on whole arrays
- functions/subroutines should stay below 50 lines (a.k.a. roughly a screen).
  Refactoring of violators of this rule are welcome
- `intent` is the last attribute and never omitted for dummy arguments
- `use some_module, only: some_class_t` is to be preferred over
  `use some_module` as it allows the compiler to warn about imported but unused
  modules and speed up compilation
- global variables should only be used for very simple problems and otherwise
  avoided
- The `save` attribute on variables in functions is handy for quick and dirty
  solutions but can always be replaced with appropriate classes

# VIM
- In case you don't do so already, plugins are easily installable with the
  following in your `.vimrc`:

```
set nocompatible

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin()

" The ultimate snippet solution
Plug 'SirVer/ultisnips'

" Faster syntax and indent for free-form Fortran
Plug 'bijancn/free-fortran.vim'

" Syntax file for sindarin
Plug 'bijancn/whizard.vim'

call plug#end()
```

 [Plug](https://github.com/junegunn/vim-plug) will then go ahead and install
 [Ultisnips](https://github.com/SirVer/ultisnips),
 [free-fortran.vim](https://github.com/bijancn/free-fortran.vim) and
 [whizard.vim](https://github.com/bijancn/whizard.vim) when you type
 `:PlugInstall<Enter>` from the Github repositories

- Ultisnips allows to use our
  [noweb snippets following the above conventions](https://raw.githubusercontent.com/bijancn/bcn_scripts/master/.vim/UltiSnips/noweb.snippets)
  including automagic substitution rules like using `class name` in the chunk,
  which gets replaced by `class_name` in the subroutine and much more.

- The `free-fortran` plugin is a faster clone of the default Fortran mode with
  less support for old Fortran editions and settings to follow our space style
  settings (see above and the `README` of `free-fortran`)

- The `whizard` plugin supports most keywords and variables of `sindarin` and
  enables VIM to do a proper highlighting (if enabled) as well as
  autocompletion (easiest usage with `Plug 'Valloric/YouCompleteMe'`)

- For fast navigation around the source code, we suggest to use `ag` (faster
  grep) together with `Plug 'mileszs/ack.vim'` and

```
if executable('ag')
  " Use ag over grep in vim as grep
  set grepprg=ag\ --nogroup\ --nocolor\ --column
  set grepformat=%f:%l:%c%m
  " Use ag over ack in ack
  let g:ackprg = 'ag --vimgrep'
endif

" Autoclose the quickfix window
let g:ack_autoclose = 1

function! FindFortranObject()
  let path = system("git rev-parse --show-toplevel")
let pattern = "'((public\|type\|function\|subroutine).* ::\|module) " . expand("<cword>") . "$'"
  execute ":Ack! " . pattern . " " . path
endfunction
function! FindAnyObject()
  let path = system("git rev-parse --show-toplevel")
  let pattern = expand("<cword>")
  execute ":Ack! " . pattern . " " . path
endfunction
nnoremap <silent> <leader>ff :call FindFortranObject()<CR>
nnoremap <silent> <leader>fa :call FindAnyObject()<CR>
```

  This allows to only press `<leader>ff` (**f**ind **f**ortran) to jump to the
  definition of most objects like types. `fa` (**f**ind **a**ll) gives a list of all
  occurences of the word under the cursor.

# emacs
- There is an emacs mode for noweb
