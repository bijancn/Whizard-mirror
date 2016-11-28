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
- starting indentation of 2 spaces in front of everything except
  `module/end module` (this will ensure that the build `.f90` has proper
  indentation)
- indentation of 7 spaces after continuing a function/subroutine with `&`
- indentation of 5 spaces after a `&` otherwise
- indentation of 3 spaces after `if`, `do`, `case`
- no indentation after `select case`
- 1 space in front of the brace of a function call; no space within the
  braces (`call whizard (arguments)`)
- no space in front of the brace of an array (`array(index) = 1`)
- 1 space around infix operators `=`, `+`, `-`, `*`, `/`, `//`, `.and.` etc.

## programming
- Main programming style is object-oriented programming (OOP), peppered with
  some old parts written in imperative style and salted with the occasional use
  of functional programming (FP)
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
- Use `>` over `.gt.`, `>=` over `.ge.`, etc.
- Inline comments are seldom necessary, use meaningful variable names instead
  (don't worry about performance. Fortran compilers remove unnecessary copies)
- In case you absolutely need inline comments, use triple exclamation marks:
  `i = 42     !!! a comment`
- Use single exclamation marks only for temporarily commenting out code. Make
  sure to remove these before you merge to master (the merge request is a good
  place to double check your changes)
- You need finalizers only when you use pointers and then you have to make sure
  you only deallocate the target once. `allocatable` arrays are deallocated by
  the Fortran compiler as soon as the object goes out of scope.
- Every class should get a `write` method to print its variables in a concise
  way. The standard layout is

```
<<My module: my class: TBP>>=
  procedure :: write => my_class_write
<<My module: procedures>>=
  subroutine my_class_write (object, unit)
    class(my_class_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)") "My class:"
    write (u, "(3x,A,ES19.12)")  "some property =", object%some_property
    ...
  end subroutine my_class_write

```

  Sticking to this interface allows to easily `call my_object%write ()` for
  debugging to STDOUT. Furthermore, it lets us incorporate
  `call my_object%write (u)` into unit tests or other output
- Always fix the format in write statements that will stay in the code like the
  ones above, i.e. never use `write (u, *)`. The `*` format is different for
  every compiler and tests will break when you use it.
- Don't put random empty lines in your subroutines. They should be concise
  enough to read them at once. If you feel like you want to separate parts of
  your subroutine with empty lines, it is a good sign to create a new
  subroutine and call that.
- Getters and setters: If you put a getter and a setter for a variable, you
  could instead just make it public. Not everything is an object. If several
  other objects need to access and set variables of your class, just make its
  properties public and treat it as a simple data structure. See also "Objects
  and Data Structures" in the "Clean Code" book by Robert C. Martin.
- Use array notation (`my_array(1, :) = 10`) over manual setting
  (`my_array(1,1) = 10`, `my_array(1,2) = 10`). This not only improves
  readability but also allows the compiler to use vectorization better. This is
  trivially obvious in such a simple example but also try writing larger
  algorithms as `elemental` functions that can be applied on the array
- Use the constants in `src/basics/constants` (e.g. `one`, `two`, `three`) over
  manual settings (`1`, `2.`, `3.0`). It will save you headaches that can come
  up when testing on higher precision
- Never compare reals directly with `==`, etc. It's not safe in general. Use the
  `nearly_equal` and `vanishes` functions in `numeric_utils` instead
- Add meaningful debug output with `msg_debug (D_MY_AREA, "important var", imp_var)`.
  Check out the code in the `diagnostics` module (you can even add colors!).
  In case the given areas don't cover the part you are working on, just add it
  to the `D_MY_AREA` constants and `d_area_of_string`/`d_area_to_string`
  functions. You (or future users) can activate the debug output with
  `whizard --debug my_area`. There is also a more verbose level
  `msg_debug2`/`whizard --debug2`. Rule of thumb is that the first level covers
  information that is written once per run and the second can contain
  event-by-event information.

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
