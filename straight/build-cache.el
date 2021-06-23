
:tanat

"28.0.50"

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("org-elpa" ("2021-06-23 19:20:54" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2021-06-23 19:20:54" nil (:type git :host github :repo "melpa/melpa" :build nil :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2021-06-23 19:20:54" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :build nil :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "el-get" ("2021-06-23 19:20:54" nil (:type git :host github :repo "dimitri/el-get" :build nil :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :flavor melpa :package "el-get" :local-repo "el-get")) "emacsmirror-mirror" ("2021-06-23 19:20:54" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :build nil :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2021-06-23 19:20:54" ("emacs") (:type git :host github :repo "raxod502/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "use-package" ("2021-06-23 19:20:54" ("emacs" "bind-key") (:type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package" :package "use-package" :local-repo "use-package")) "bind-key" ("2021-06-23 19:20:54" nil (:flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :package "bind-key" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "lsp-mode" ("2021-06-23 19:20:54" ("emacs" "dash" "f" "ht" "spinner" "markdown-mode" "lv") (:type git :flavor melpa :files (:defaults "clients/*.el" "lsp-mode-pkg.el") :host github :repo "emacs-lsp/lsp-mode" :package "lsp-mode" :local-repo "lsp-mode")) "dash" ("2021-06-23 19:20:54" ("emacs") (:type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el" :package "dash" :local-repo "dash.el")) "f" ("2021-06-23 19:20:54" ("s" "dash") (:type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el" :package "f" :local-repo "f.el")) "s" ("2021-06-23 19:20:54" nil (:type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el" :package "s" :local-repo "s.el")) "ht" ("2021-06-23 19:20:54" ("dash") (:type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el" :package "ht" :local-repo "ht.el")) "spinner" ("2021-06-23 19:20:54" ("emacs") (:type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git")) :package "spinner" :local-repo "spinner")) "markdown-mode" ("2021-06-23 19:20:54" ("emacs") (:type git :flavor melpa :host github :repo "jrblevin/markdown-mode" :package "markdown-mode" :local-repo "markdown-mode")) "lv" ("2021-06-23 19:20:54" nil (:type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra" :package "lv" :local-repo "hydra")) "lsp-ui" ("2021-06-23 19:20:54" ("emacs" "dash" "lsp-mode" "markdown-mode") (:type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el") :host github :repo "emacs-lsp/lsp-ui" :package "lsp-ui" :local-repo "lsp-ui")) "php-mode" ("2021-06-23 19:20:54" ("emacs") (:type git :flavor melpa :files (:defaults "lisp/*.el" "php-mode-pkg.el") :host github :repo "emacs-php/php-mode" :package "php-mode" :local-repo "php-mode")) "typescript-mode" ("2021-06-23 19:20:54" ("emacs") (:type git :flavor melpa :host github :repo "emacs-typescript/typescript.el" :package "typescript-mode" :local-repo "typescript.el")) "vue-mode" ("2021-06-23 19:20:54" ("mmm-mode" "vue-html-mode" "ssass-mode" "edit-indirect") (:type git :flavor melpa :host github :repo "AdamNiederer/vue-mode" :package "vue-mode" :local-repo "vue-mode")) "mmm-mode" ("2021-06-23 19:20:54" ("cl-lib") (:type git :flavor melpa :host github :repo "purcell/mmm-mode" :package "mmm-mode" :local-repo "mmm-mode")) "vue-html-mode" ("2021-06-23 19:20:54" nil (:type git :flavor melpa :host github :repo "AdamNiederer/vue-html-mode" :package "vue-html-mode" :local-repo "vue-html-mode")) "ssass-mode" ("2021-06-23 19:20:54" ("emacs") (:type git :flavor melpa :host github :repo "AdamNiederer/ssass-mode" :package "ssass-mode" :local-repo "ssass-mode")) "edit-indirect" ("2021-06-23 19:20:54" ("emacs") (:type git :flavor melpa :host github :repo "Fanael/edit-indirect" :package "edit-indirect" :local-repo "edit-indirect"))))

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight straight-x straight-autoloads) (autoload 'straight-remove-unused-repos "straight" "Remove unused repositories from the repos directory.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos.

(fn &optional FORCE)" t nil) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

(fn &optional SOURCES ACTION)" t nil) (autoload 'straight-visit-package-website "straight" "Interactively select a recipe, and visit the package's website." t nil) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil if package was actually installed, and nil
otherwise (this can only happen if NO-CLONE is non-nil).

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t nil) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t nil) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t nil) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t nil) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t nil) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded." nil nil) (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted." nil nil) (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t nil) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t nil) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t nil) (autoload 'straight-bug-report "straight" "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package '(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function '0) (autoload 'straight-dependencies "straight" "Return a list of PACKAGE's dependencies.

(fn &optional PACKAGE)" t nil) (autoload 'straight-dependents "straight" "Return a list PACKAGE's dependents.

(fn &optional PACKAGE)" t nil) (register-definition-prefixes "straight" '("straight-")) (defvar straight-x-pinned-packages nil "List of pinned packages.") (register-definition-prefixes "straight-x" '("straight-x-")) (provide 'straight-autoloads)) "bind-key" ((bind-key bind-key-autoloads) (autoload 'bind-key "bind-key" "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap variable or symbol.
For example:

  (bind-key \"M-h\" #'some-interactive-function my-mode-map)

  (bind-key \"M-h\" #'some-interactive-function 'my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time.

(fn KEY-NAME COMMAND &optional KEYMAP PREDICATE)" nil t) (autoload 'unbind-key "bind-key" "Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details.

(fn KEY-NAME &optional KEYMAP)" nil t) (autoload 'bind-key* "bind-key" "Similar to `bind-key', but overrides any mode-specific bindings.

(fn KEY-NAME COMMAND &optional PREDICATE)" nil t) (autoload 'bind-keys "bind-key" "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

(fn &rest ARGS)" nil t) (autoload 'bind-keys* "bind-key" "

(fn &rest ARGS)" nil t) (autoload 'describe-personal-keybindings "bind-key" "Display all the personal keybindings defined by `bind-key'." t nil) (register-definition-prefixes "bind-key" '("bind-key" "compare-keybindings" "get-binding-description" "override-global-m" "personal-keybindings")) (provide 'bind-key-autoloads)) "use-package" ((use-package-bind-key use-package-core use-package-autoloads use-package use-package-lint use-package-ensure use-package-delight use-package-diminish use-package-jump) (autoload 'use-package-autoload-keymap "use-package-bind-key" "Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL. It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword. It
works by binding the given key sequence to an invocation of this
function for a particular keymap. The keymap is expected to be
defined by the package. In this way, loading the package is
deferred until the prefix key sequence is pressed.

(fn KEYMAP-SYMBOL PACKAGE OVERRIDE)" nil nil) (autoload 'use-package-normalize-binder "use-package-bind-key" "

(fn NAME KEYWORD ARGS)" nil nil) (defalias 'use-package-normalize/:bind 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind* 'use-package-normalize-binder) (defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode) (defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode) (autoload 'use-package-handler/:bind "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional BIND-MACRO)" nil nil) (defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder) (autoload 'use-package-handler/:bind-keymap "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional OVERRIDE)" nil nil) (autoload 'use-package-handler/:bind-keymap* "use-package-bind-key" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (register-definition-prefixes "use-package-bind-key" '("use-package-handler/:bind*")) (autoload 'use-package "use-package-core" "Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.
:hook            Specify hook(s) to attach this package to.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic', `:hook',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:after           Delay the use-package declaration until after the named modules
                 have loaded. Once load, it will be as though the use-package
                 declaration (without `:after') had been seen at that moment.
:demand          Prevent the automatic deferred loading introduced by constructs
                 such as `:bind' (see `:defer' for the complete list).

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.
:diminish        Support for diminish.el (if installed).
:delight         Support for delight.el (if installed).
:custom          Call `custom-set' or `set-default' with each variable
                 definition without modifying the Emacs `custom-file'.
                 (compare with `custom-set-variables').
:custom-face     Call `customize-set-faces' with each face definition.
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive.

(fn NAME &rest ARGS)" nil t) (function-put 'use-package 'lisp-indent-function '1) (register-definition-prefixes "use-package-core" '("use-package-")) (autoload 'use-package-normalize/:delight "use-package-delight" "Normalize arguments to delight.

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:delight "use-package-delight" "

(fn NAME KEYWORD ARGS REST STATE)" nil nil) (register-definition-prefixes "use-package-delight" '("use-package-normalize-delight")) (autoload 'use-package-normalize/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (register-definition-prefixes "use-package-diminish" '("use-package-normalize-diminish")) (autoload 'use-package-normalize/:ensure "use-package-ensure" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:ensure "use-package-ensure" "

(fn NAME KEYWORD ENSURE REST STATE)" nil nil) (register-definition-prefixes "use-package-ensure" '("use-package-")) (autoload 'use-package-jump-to-package-form "use-package-jump" "Attempt to find and jump to the `use-package' form that loaded
PACKAGE. This will only find the form if that form actually
required PACKAGE. If PACKAGE was previously required then this
function will jump to the file that originally required PACKAGE
instead.

(fn PACKAGE)" t nil) (register-definition-prefixes "use-package-jump" '("use-package-find-require")) (autoload 'use-package-lint "use-package-lint" "Check for errors in use-package declarations.
For example, if the module's `:if' condition is met, but even
with the specified `:load-path' the module cannot be found." t nil) (register-definition-prefixes "use-package-lint" '("use-package-lint-declaration")) (provide 'use-package-autoloads)) "dash" ((dash-autoloads dash) (autoload 'dash-fontify-mode "dash" "Toggle fontification of Dash special variables.

If called interactively, toggle `Dash-Fontify mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'.

(fn &optional ARG)" t nil) (put 'global-dash-fontify-mode 'globalized-minor-mode t) (defvar global-dash-fontify-mode nil "Non-nil if Global Dash-Fontify mode is enabled.
See the `global-dash-fontify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dash-fontify-mode'.") (custom-autoload 'global-dash-fontify-mode "dash" nil) (autoload 'global-dash-fontify-mode "dash" "Toggle Dash-Fontify mode in all buffers.
With prefix ARG, enable Global Dash-Fontify mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG
is omitted or nil.

Dash-Fontify mode is enabled in all buffers where
`dash--turn-on-fontify-mode' would do it.

See `dash-fontify-mode' for more information on Dash-Fontify mode.

(fn &optional ARG)" t nil) (autoload 'dash-register-info-lookup "dash" "Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol]." t nil) (register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-juxt" "-keep" "-l" "-m" "-no" "-o" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-")) (provide 'dash-autoloads)) "s" ((s-autoloads s) (register-definition-prefixes "s" '("s-")) (provide 's-autoloads)) "f" ((f-autoloads f) (register-definition-prefixes "f" '("f-")) (provide 'f-autoloads)) "ht" ((ht ht-autoloads) (register-definition-prefixes "ht" 'nil) (provide 'ht-autoloads)) "spinner" ((spinner-autoloads spinner) (autoload 'spinner-create "spinner" "Create a spinner of the given TYPE.
The possible TYPEs are described in `spinner--type-to-frames'.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If BUFFER-LOCAL is non-nil, the spinner will be automatically
deactivated if the buffer is killed.  If BUFFER-LOCAL is a
buffer, use that instead of current buffer.

When started, in order to function properly, the spinner runs a
timer which periodically calls `force-mode-line-update' in the
curent buffer.  If BUFFER-LOCAL was set at creation time, then
`force-mode-line-update' is called in that buffer instead.  When
the spinner is stopped, the timer is deactivated.

DELAY, if given, is the number of seconds to wait after starting
the spinner before actually displaying it. It is safe to cancel
the spinner before this time, in which case it won't display at
all.

(fn &optional TYPE BUFFER-LOCAL FPS DELAY)" nil nil) (autoload 'spinner-start "spinner" "Start a mode-line spinner of given TYPE-OR-OBJECT.
If TYPE-OR-OBJECT is an object created with `make-spinner',
simply activate it.  This method is designed for minor modes, so
they can use the spinner as part of their lighter by doing:
    '(:eval (spinner-print THE-SPINNER))
To stop this spinner, call `spinner-stop' on it.

If TYPE-OR-OBJECT is anything else, a buffer-local spinner is
created with this type, and it is displayed in the
`mode-line-process' of the buffer it was created it.  Both
TYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).
To stop this spinner, call `spinner-stop' in the same buffer.

Either way, the return value is a function which can be called
anywhere to stop this spinner.  You can also call `spinner-stop'
in the same buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

DELAY, if given, is the number of seconds to wait until actually
displaying the spinner. It is safe to cancel the spinner before
this time, in which case it won't display at all.

(fn &optional TYPE-OR-OBJECT FPS DELAY)" nil nil) (register-definition-prefixes "spinner" '("spinner-")) (provide 'spinner-autoloads)) "markdown-mode" ((markdown-mode-autoloads markdown-mode) (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files.

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)) (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files.

(fn)" t nil) (autoload 'markdown-view-mode "markdown-mode" "Major mode for viewing Markdown content.

(fn)" t nil) (autoload 'gfm-view-mode "markdown-mode" "Major mode for viewing GitHub Flavored Markdown content.

(fn)" t nil) (autoload 'markdown-live-preview-mode "markdown-mode" "Toggle native previewing on save for a specific markdown file.

If called interactively, toggle `Markdown-Live-Preview mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "markdown-mode" '("defun-markdown-" "gfm-" "markdown")) (provide 'markdown-mode-autoloads)) "lv" ((lv lv-autoloads) (register-definition-prefixes "lv" '("lv-")) (provide 'lv-autoloads)) "lsp-mode" ((lsp-ido lsp-vetur lsp-fortran lsp-rf lsp-clojure lsp-css lsp-mode-autoloads lsp-protocol lsp-json lsp-svelte lsp-steep lsp lsp-headerline lsp-solargraph lsp-gdscript lsp-diagnostics lsp-completion lsp-nix lsp-ocaml lsp-pwsh lsp-xml lsp-sorbet lsp-verilog lsp-r lsp-crystal lsp-tex lsp-racket lsp-dired lsp-dhall lsp-lua lsp-purescript lsp-csharp lsp-cmake lsp-erlang lsp-yaml lsp-beancount lsp-fsharp lsp-haxe lsp-iedit lsp-sqls lsp-rust lsp-zig lsp-modeline lsp-elixir lsp-go lsp-bash lsp-angular lsp-terraform lsp-elm lsp-markdown lsp-groovy lsp-ada lsp-pyls lsp-pylsp lsp-vimscript lsp-lens lsp-html lsp-perl lsp-vhdl lsp-semantic-tokens lsp-hack lsp-eslint lsp-php lsp-actionscript lsp-vala lsp-prolog lsp-icons lsp-mode lsp-javascript lsp-kotlin lsp-d lsp-dockerfile lsp-clangd lsp-nim) (register-definition-prefixes "lsp-actionscript" '("lsp-actionscript-")) (register-definition-prefixes "lsp-ada" '("lsp-ada-")) (register-definition-prefixes "lsp-angular" '("lsp-client")) (register-definition-prefixes "lsp-bash" '("lsp-bash-")) (register-definition-prefixes "lsp-beancount" '("lsp-beancount-")) (autoload 'lsp-cpp-flycheck-clang-tidy-error-explainer "lsp-clangd" "Explain a clang-tidy ERROR by scraping documentation from llvm.org.

(fn ERROR)" nil nil) (register-definition-prefixes "lsp-clangd" '("lsp-c")) (register-definition-prefixes "lsp-clojure" '("lsp-clojure-")) (define-obsolete-variable-alias 'lsp-prefer-capf 'lsp-completion-provider "lsp-mode 7.0.1") (define-obsolete-variable-alias 'lsp-enable-completion-at-point 'lsp-completion-enable "lsp-mode 7.0.1") (autoload 'lsp-completion-at-point "lsp-completion" "Get lsp completions." nil nil) (autoload 'lsp-completion--enable "lsp-completion" "Enable LSP completion support." nil nil) (autoload 'lsp-completion-mode "lsp-completion" "Toggle LSP completion support.

If called interactively, toggle `Lsp-Completion mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (add-hook 'lsp-configure-hook (lambda nil (when (and lsp-auto-configure lsp-completion-enable) (lsp-completion--enable)))) (register-definition-prefixes "lsp-completion" '("lsp-")) (register-definition-prefixes "lsp-crystal" '("lsp-clients-crystal-executable")) (register-definition-prefixes "lsp-csharp" '("lsp-csharp-")) (register-definition-prefixes "lsp-css" '("lsp-css-")) (define-obsolete-variable-alias 'lsp-diagnostic-package 'lsp-diagnostics-provider "lsp-mode 7.0.1") (define-obsolete-variable-alias 'lsp-flycheck-default-level 'lsp-diagnostics-flycheck-default-level "lsp-mode 7.0.1") (autoload 'lsp-diagnostics-lsp-checker-if-needed "lsp-diagnostics" nil nil nil) (autoload 'lsp-diagnostics--enable "lsp-diagnostics" "Enable LSP checker support." nil nil) (autoload 'lsp-diagnostics-mode "lsp-diagnostics" "Toggle LSP diagnostics integration.

If called interactively, toggle `Lsp-Diagnostics mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (add-hook 'lsp-configure-hook (lambda nil (when lsp-auto-configure (lsp-diagnostics--enable)))) (register-definition-prefixes "lsp-diagnostics" '("lsp-diagnostics-")) (defvar lsp-dired-mode nil "Non-nil if Lsp-Dired mode is enabled.
See the `lsp-dired-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-dired-mode'.") (custom-autoload 'lsp-dired-mode "lsp-dired" nil) (autoload 'lsp-dired-mode "lsp-dired" "Display `lsp-mode' icons for each file in a dired buffer.

If called interactively, toggle `Lsp-Dired mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "lsp-dired" '("lsp-dired-")) (register-definition-prefixes "lsp-dockerfile" '("lsp-dockerfile-language-server-command")) (register-definition-prefixes "lsp-elixir" '("lsp-elixir-")) (register-definition-prefixes "lsp-elm" '("lsp-")) (register-definition-prefixes "lsp-erlang" '("lsp-erlang-server-")) (register-definition-prefixes "lsp-eslint" '("lsp-")) (register-definition-prefixes "lsp-fortran" '("lsp-clients-")) (autoload 'lsp-fsharp--workspace-load "lsp-fsharp" "Load all of the provided PROJECTS.

(fn PROJECTS)" nil nil) (register-definition-prefixes "lsp-fsharp" '("lsp-fsharp-")) (register-definition-prefixes "lsp-gdscript" '("lsp-gdscript-")) (register-definition-prefixes "lsp-go" '("lsp-go-")) (register-definition-prefixes "lsp-groovy" '("lsp-groovy-")) (register-definition-prefixes "lsp-hack" '("lsp-clients-hack-command")) (register-definition-prefixes "lsp-haxe" '("lsp-")) (autoload 'lsp-headerline-breadcrumb-mode "lsp-headerline" "Toggle breadcrumb on headerline.

If called interactively, toggle `Lsp-Headerline-Breadcrumb mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'lsp-breadcrumb-go-to-symbol "lsp-headerline" "Go to the symbol on breadcrumb at SYMBOL-POSITION.

(fn SYMBOL-POSITION)" t nil) (autoload 'lsp-breadcrumb-narrow-to-symbol "lsp-headerline" "Narrow to the symbol range on breadcrumb at SYMBOL-POSITION.

(fn SYMBOL-POSITION)" t nil) (register-definition-prefixes "lsp-headerline" '("lsp-headerline-")) (register-definition-prefixes "lsp-html" '("lsp-html-")) (register-definition-prefixes "lsp-icons" '("lsp-")) (autoload 'lsp-ido-workspace-symbol "lsp-ido" "`ido' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point.

(fn ARG)" t nil) (register-definition-prefixes "lsp-ido" '("lsp-ido-")) (autoload 'lsp-iedit-highlights "lsp-iedit" "Start an `iedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil) (autoload 'lsp-evil-multiedit-highlights "lsp-iedit" "Start an `evil-multiedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil) (register-definition-prefixes "lsp-iedit" '("lsp-iedit--on-ranges")) (register-definition-prefixes "lsp-javascript" '("lsp-")) (register-definition-prefixes "lsp-json" '("lsp-")) (register-definition-prefixes "lsp-kotlin" '("lsp-")) (autoload 'lsp-lens--enable "lsp-lens" "Enable lens mode." nil nil) (autoload 'lsp-lens-show "lsp-lens" "Display lenses in the buffer." t nil) (autoload 'lsp-lens-hide "lsp-lens" "Delete all lenses." t nil) (autoload 'lsp-lens-mode "lsp-lens" "Toggle code-lens overlays.

If called interactively, toggle `Lsp-Lens mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'lsp-avy-lens "lsp-lens" "Click lsp lens using `avy' package." t nil) (register-definition-prefixes "lsp-lens" '("lsp-lens-")) (register-definition-prefixes "lsp-lua" '("lsp-")) (register-definition-prefixes "lsp-markdown" '("lsp-markdown-")) (put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp) (put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i)))) (autoload 'lsp-load-vscode-workspace "lsp-mode" "Load vscode workspace from FILE

(fn FILE)" t nil) (autoload 'lsp-save-vscode-workspace "lsp-mode" "Save vscode workspace to FILE

(fn FILE)" t nil) (autoload 'lsp "lsp-mode" "Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start.

(fn &optional ARG)" t nil) (autoload 'lsp-deferred "lsp-mode" "Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs." nil nil) (register-definition-prefixes "lsp-mode" '("lsp-" "make-lsp-client" "when-lsp-workspace" "with-lsp-workspace")) (define-obsolete-variable-alias 'lsp-diagnostics-modeline-scope 'lsp-modeline-diagnostics-scope "lsp-mode 7.0.1") (autoload 'lsp-modeline-code-actions-mode "lsp-modeline" "Toggle code actions on modeline.

If called interactively, toggle `Lsp-Modeline-Code-Actions mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'lsp-diagnostics-modeline-mode 'lsp-modeline-diagnostics-mode "lsp-mode 7.0.1") (autoload 'lsp-modeline-diagnostics-mode "lsp-modeline" "Toggle diagnostics modeline.

If called interactively, toggle `Lsp-Modeline-Diagnostics mode'.
If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (autoload 'lsp-modeline-workspace-status-mode "lsp-modeline" "Toggle workspace status on modeline.

If called interactively, toggle `Lsp-Modeline-Workspace-Status
mode'.  If the prefix argument is positive, enable the mode, and
if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "lsp-modeline" '("lsp-")) (register-definition-prefixes "lsp-nix" '("lsp-nix-server-path")) (register-definition-prefixes "lsp-ocaml" '("lsp-ocaml-l")) (register-definition-prefixes "lsp-perl" '("lsp-perl-")) (register-definition-prefixes "lsp-php" '("lsp-")) (register-definition-prefixes "lsp-prolog" '("lsp-prolog-server-command")) (register-definition-prefixes "lsp-protocol" '("dash-expand:&RangeToPoint" "lsp")) (register-definition-prefixes "lsp-purescript" '("lsp-purescript-")) (register-definition-prefixes "lsp-pwsh" '("lsp-pwsh-")) (register-definition-prefixes "lsp-pyls" '("lsp-")) (register-definition-prefixes "lsp-pylsp" '("lsp-")) (register-definition-prefixes "lsp-r" '("lsp-clients-r-server-command")) (register-definition-prefixes "lsp-racket" '("lsp-racket-lang")) (register-definition-prefixes "lsp-rf" '("expand-start-command" "lsp-rf-language-server-" "parse-rf-language-server-")) (register-definition-prefixes "lsp-rust" '("lsp-")) (autoload 'lsp--semantic-tokens-initialize-buffer "lsp-semantic-tokens" "Initialize the buffer for semantic tokens.
IS-RANGE-PROVIDER is non-nil when server supports range requests.

(fn IS-RANGE-PROVIDER)" nil nil) (autoload 'lsp--semantic-tokens-initialize-workspace "lsp-semantic-tokens" "Initialize semantic tokens for WORKSPACE.

(fn WORKSPACE)" nil nil) (autoload 'lsp-semantic-tokens--warn-about-deprecated-setting "lsp-semantic-tokens" "Warn about deprecated semantic highlighting variable." nil nil) (autoload 'lsp-semantic-tokens--enable "lsp-semantic-tokens" "Enable semantic tokens mode." nil nil) (autoload 'lsp-semantic-tokens-mode "lsp-semantic-tokens" "Toggle semantic-tokens support.

If called interactively, toggle `Lsp-Semantic-Tokens mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "lsp-semantic-tokens" '("lsp-")) (register-definition-prefixes "lsp-solargraph" '("lsp-solargraph-")) (register-definition-prefixes "lsp-sorbet" '("lsp-sorbet-")) (register-definition-prefixes "lsp-sqls" '("lsp-sql")) (register-definition-prefixes "lsp-steep" '("lsp-steep-")) (register-definition-prefixes "lsp-svelte" '("lsp-svelte-plugin-")) (register-definition-prefixes "lsp-terraform" '("lsp-terraform-")) (register-definition-prefixes "lsp-tex" '("lsp-")) (register-definition-prefixes "lsp-vala" '("lsp-clients-vala-ls-executable")) (register-definition-prefixes "lsp-verilog" '("lsp-clients-")) (register-definition-prefixes "lsp-vetur" '("lsp-")) (register-definition-prefixes "lsp-vhdl" '("ghdl-ls-bin-name" "hdl-checker-bin-name" "lsp-vhdl-" "vhdl-")) (register-definition-prefixes "lsp-vimscript" '("lsp-clients-vim-")) (register-definition-prefixes "lsp-xml" '("lsp-xml-")) (register-definition-prefixes "lsp-yaml" '("lsp-yaml-")) (register-definition-prefixes "lsp-zig" '("lsp-zig-zls-executable")) (provide 'lsp-mode-autoloads)) "lsp-ui" ((lsp-ui-imenu lsp-ui-sideline lsp-ui-doc lsp-ui-flycheck lsp-ui lsp-ui-peek lsp-ui-util lsp-ui-autoloads) (autoload 'lsp-ui-mode "lsp-ui" "Toggle language server UI mode on or off.
\342\200\230lsp-ui-mode\342\200\231 is a minor mode that contains a series of useful UI
integrations for \342\200\230lsp-mode\342\200\231.  With a prefix argument ARG, enable
language server UI mode if ARG is positive, and disable it
otherwise.  If called from Lisp, enable the mode if ARG is
omitted or nil, and toggle it if ARG is \342\200\230toggle\342\200\231.

(fn &optional ARG)" t nil) (register-definition-prefixes "lsp-ui" '("lsp-ui-")) (register-definition-prefixes "lsp-ui-doc" '("lsp-ui-doc-")) (register-definition-prefixes "lsp-ui-flycheck" '("lsp-ui-flycheck-")) (register-definition-prefixes "lsp-ui-imenu" '("lsp-ui-imenu")) (register-definition-prefixes "lsp-ui-peek" '("lsp-")) (register-definition-prefixes "lsp-ui-sideline" '("lsp-ui-sideline")) (register-definition-prefixes "lsp-ui-util" '("lsp-ui-util-")) (provide 'lsp-ui-autoloads)) "php-mode" ((php-align php-local-manual php-face php-project php php-mode-debug php-mode-autoloads php-mode) (let ((loads (get 'php 'custom-loads))) (if (member '"php" loads) nil (put 'php 'custom-loads (cons '"php" loads)))) (autoload 'php-mode-maybe "php" "Select PHP mode or other major mode." t nil) (autoload 'php-current-class "php" "Insert current class name if cursor in class context." t nil) (autoload 'php-current-namespace "php" "Insert current namespace if cursor in namespace context." t nil) (autoload 'php-copyit-fqsen "php" "Copy/kill class/method FQSEN." t nil) (autoload 'php-run-builtin-web-server "php" "Run PHP Built-in web server.

`ROUTER-OR-DIR': Path to router PHP script or Document root.
`HOSTNAME': Hostname or IP address of Built-in web server.
`PORT': Port number of Built-in web server.
`DOCUMENT-ROOT': Path to Document root.

When `DOCUMENT-ROOT' is NIL, the document root is obtained from `ROUTER-OR-DIR'.

(fn ROUTER-OR-DIR HOSTNAME PORT &optional DOCUMENT-ROOT)" t nil) (autoload 'php-find-system-php-ini-file "php" "Find php.ini FILE by `php --ini'.

(fn &optional FILE)" t nil) (register-definition-prefixes "php" '("php-")) (autoload 'php-align-setup "php-align" "Setup alignment configuration for PHP code." nil nil) (autoload 'php-align-mode "php-align" "Alignment lines for PHP script.

If called interactively, toggle `Php-Align mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t nil) (register-definition-prefixes "php-align" '("php-align-")) (let ((loads (get 'php-faces 'custom-loads))) (if (member '"php-face" loads) nil (put 'php-faces 'custom-loads (cons '"php-face" loads)))) (register-definition-prefixes "php-face" '("php-annotations-annotation-face")) (autoload 'php-local-manual-search "php-local-manual" "Search the local PHP documentation (i.e. in `php-manual-path') for
the word at point.  The function returns t if the requested documentation
exists, and nil otherwise.

With a prefix argument, prompt (with completion) for a word to search for.

(fn WORD)" t nil) (define-obsolete-function-alias 'php-search-local-documentation #'php-local-manual-search "2.0.0") (register-definition-prefixes "php-local-manual" '("php-local-manual-")) (let ((loads (get 'php-mode 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php-mode 'custom-loads (cons '"php-mode" loads)))) (add-to-list 'interpreter-mode-alist (cons "php\\(?:-?[34578]\\(?:\\.[0-9]+\\)*\\)?" 'php-mode)) (define-obsolete-variable-alias 'php-available-project-root-files 'php-project-available-root-files "1.19.0") (autoload 'php-mode "php-mode" "Major mode for editing PHP code.

\\{php-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-mode)) (add-to-list 'auto-mode-alist '("\\.\\(?:php\\.inc\\|stub\\)\\'" . php-mode)) (add-to-list 'auto-mode-alist '("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-mode-maybe)) (register-definition-prefixes "php-mode" '("php-")) (register-definition-prefixes "php-mode-debug" '("php-mode-debug")) (defvar-local php-project-root 'auto "Method of searching for the top level directory.

`auto' (default)
      Try to search file in order of `php-project-available-root-files'.

SYMBOL
      Key of `php-project-available-root-files'.

STRING
      A file/directory name of top level marker.
      If the string is an actual directory path, it is set as the absolute path
      of the root directory, not the marker.") (put 'php-project-root 'safe-local-variable #'(lambda (v) (or (stringp v) (assq v php-project-available-root-files)))) (defvar-local php-project-etags-file nil) (put 'php-project-etags-file 'safe-local-variable #'(lambda (v) (or (functionp v) (eq v t) (php-project--eval-bootstrap-scripts v)))) (defvar-local php-project-bootstrap-scripts nil "List of path to bootstrap php script file.

The ideal bootstrap file is silent, it only includes dependent files,
defines constants, and sets the class loaders.") (put 'php-project-bootstrap-scripts 'safe-local-variable #'php-project--eval-bootstrap-scripts) (defvar-local php-project-php-executable nil "Path to php executable file.") (put 'php-project-php-executable 'safe-local-variable #'(lambda (v) (and (stringp v) (file-executable-p v)))) (defvar-local php-project-phan-executable nil "Path to phan executable file.") (put 'php-project-phan-executable 'safe-local-variable #'php-project--eval-bootstrap-scripts) (defvar-local php-project-coding-style nil "Symbol value of the coding style of the project that PHP major mode refers to.

Typically it is `pear', `drupal', `wordpress', `symfony2' and `psr2'.") (put 'php-project-coding-style 'safe-local-variable #'symbolp) (defvar-local php-project-align-lines t "If T, automatically turn on `php-align-mode' by `php-align-setup'.") (put 'php-project-align-lines 'safe-local-variable #'booleanp) (defvar-local php-project-php-file-as-template 'auto "
`auto' (default)
      Automatically switch to mode for template when HTML tag detected in file.

`t'
      Switch all PHP files in that directory to mode for HTML template.

`nil'
      Any .php  in that directory is just a PHP script.

((PATTERN . SYMBOL))
      Alist of file name pattern regular expressions and the above symbol pairs.
      PATTERN is regexp pattern.
") (put 'php-project-php-file-as-template 'safe-local-variable #'php-project--validate-php-file-as-template) (defvar-local php-project-repl nil "Function name or path to REPL (interactive shell) script.") (put 'php-project-repl 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v)))) (defvar-local php-project-unit-test nil "Function name or path to unit test script.") (put 'php-project-unit-test 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v)))) (defvar-local php-project-deploy nil "Function name or path to deploy script.") (put 'php-project-deploy 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v)))) (defvar-local php-project-build nil "Function name or path to build script.") (put 'php-project-build 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v)))) (defvar-local php-project-server-start nil "Function name or path to server-start script.") (put 'php-project-server-start 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v)))) (autoload 'php-project-get-bootstrap-scripts "php-project" "Return list of bootstrap script." nil nil) (autoload 'php-project-get-root-dir "php-project" "Return path to current PHP project." nil nil) (register-definition-prefixes "php-project" '("php-project-")) (provide 'php-mode-autoloads)) "typescript-mode" ((typescript-mode-autoloads typescript-mode typescript-mode-test-utilities) (put 'typescript-indent-level 'safe-local-variable #'integerp) (autoload 'typescript-mode "typescript-mode" "Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

(fn)" t nil) (eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}"))) (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)) (register-definition-prefixes "typescript-mode" '("typescript-")) (register-definition-prefixes "typescript-mode-test-utilities" '("font-lock-test" "get-face-at" "test-with-")) (provide 'typescript-mode-autoloads)) "mmm-mode" ((mmm-mason mmm-cmds mmm-region mmm-compat mmm-vars mmm-noweb mmm-cweb mmm-auto mmm-univ mmm-mode-autoloads mmm-sample mmm-mode mmm-rpm mmm-erb mmm-class mmm-defaults mmm-myghty mmm-utils) (register-definition-prefixes "mmm-auto" '("mmm-")) (register-definition-prefixes "mmm-class" '("mmm-")) (register-definition-prefixes "mmm-cmds" '("mmm-")) (register-definition-prefixes "mmm-compat" '("mmm-" "skeleton-positions")) (register-definition-prefixes "mmm-cweb" '("mmm-cweb-")) (autoload 'html-erb-mode "mmm-erb" "

(fn)" t nil) (autoload 'nxml-web-mode "mmm-erb" "

(fn)" t nil) (register-definition-prefixes "mmm-erb" '("html-erb-after-syntax-propertize" "mmm-erb-")) (register-definition-prefixes "mmm-mason" '("mmm-mason-")) (register-definition-prefixes "mmm-mode" '("mmm-")) (register-definition-prefixes "mmm-myghty" '("mmm-myghty-")) (register-definition-prefixes "mmm-noweb" '("mmm-")) (register-definition-prefixes "mmm-region" '("mmm-")) (register-definition-prefixes "mmm-rpm" '("mmm-rpm-sh-")) (register-definition-prefixes "mmm-sample" '("mmm-")) (register-definition-prefixes "mmm-univ" '("mmm-univ-get-mode")) (register-definition-prefixes "mmm-utils" '("mmm-")) (autoload 'mmm-add-classes "mmm-vars" "Add the submode classes CLASSES to `mmm-classes-alist'.

(fn CLASSES)" nil nil) (register-definition-prefixes "mmm-vars" '("mmm-")) (provide 'mmm-mode-autoloads)) "vue-html-mode" ((vue-html-mode vue-html-mode-autoloads) (autoload 'vue-html-mode "vue-html-mode" "Major mode for Vue.js templates.

(fn)" t nil) (register-definition-prefixes "vue-html-mode" '("vue-html-")) (provide 'vue-html-mode-autoloads)) "ssass-mode" ((ssass-mode-autoloads ssass-mode) (autoload 'ssass-mode "ssass-mode" "Major mode for Sass

(fn)" t nil) (register-definition-prefixes "ssass-mode" '("ssass-")) (provide 'ssass-mode-autoloads)) "edit-indirect" ((edit-indirect-autoloads edit-indirect) (autoload 'edit-indirect-region "edit-indirect" "Edit the region BEG..END in a separate buffer.
The region is copied, without text properties, to a separate
buffer, called edit-indirect buffer, and
`edit-indirect-guess-mode-function' is called to set the major
mode.
When done, exit with `edit-indirect-commit', which will remove the
original region and replace it with the edited version; or with
`edit-indirect-abort', which will drop the modifications.

This differs from `clone-indirect-buffer' with narrowing in that
the text properties are not shared, so the parent buffer major mode
and the edit-indirect buffer major mode will not be able to tread
on each other's toes by setting up potentially conflicting text
properties, which happens surprisingly often when the font-lock
mode is used.

Edit-indirect buffers use the `edit-indirect-mode-map' keymap.

If there's already an edit-indirect buffer for BEG..END, use that.
If there's already an edit-indirect buffer active overlapping any
portion of BEG..END, an `edit-indirect-overlapping' error is
signaled.

When DISPLAY-BUFFER is non-nil or when called interactively,
display the edit-indirect buffer in some window and select it.

In any case, return the edit-indirect buffer.

(fn BEG END &optional DISPLAY-BUFFER)" t nil) (register-definition-prefixes "edit-indirect" '("edit-indirect-")) (provide 'edit-indirect-autoloads)) "vue-mode" ((vue-mode vue-mode-autoloads) (autoload 'vue-mode-edit-all-indirect "vue-mode" "Open all subsections with `edit-indirect-mode' in seperate windows.
If KEEP-WINDOWS is set, do not delete other windows and keep the root window
open.

(fn &optional KEEP-WINDOWS)" t nil) (autoload 'vue-mode "vue-mode" "

(fn)" t nil) (setq mmm-global-mode 'maybe) (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode)) (register-definition-prefixes "vue-mode" '("vue-")) (provide 'vue-mode-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 9 "melpa" nil "gnu-elpa-mirror" nil "el-get" nil "emacsmirror-mirror" nil "straight" nil "use-package" nil "bind-key" nil "lsp-mode" nil "dash" nil "f" nil "s" nil "ht" nil "spinner" nil "markdown-mode" nil "lv" nil "lsp-ui" nil "php-mode" nil "typescript-mode" nil "vue-mode" nil "mmm-mode" nil "cl-lib" nil "vue-html-mode" nil "ssass-mode" nil "edit-indirect" nil)) melpa #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "el-get" (el-get :type git :flavor melpa :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :host github :repo "dimitri/el-get") "emacsmirror-mirror" nil "straight" nil "use-package" (use-package :type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package") "bind-key" (bind-key :type git :flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :host github :repo "jwiegley/use-package") "lsp-mode" (lsp-mode :type git :flavor melpa :files (:defaults "clients/*.el" "lsp-mode-pkg.el") :host github :repo "emacs-lsp/lsp-mode") "dash" (dash :type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el") "f" (f :type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el") "s" (s :type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el") "ht" (ht :type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el") "spinner" nil "markdown-mode" (markdown-mode :type git :flavor melpa :host github :repo "jrblevin/markdown-mode") "lv" (lv :type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra") "lsp-ui" (lsp-ui :type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el") :host github :repo "emacs-lsp/lsp-ui") "php-mode" (php-mode :type git :flavor melpa :files (:defaults "lisp/*.el" "php-mode-pkg.el") :host github :repo "emacs-php/php-mode") "typescript-mode" (typescript-mode :type git :flavor melpa :host github :repo "emacs-typescript/typescript.el") "vue-mode" (vue-mode :type git :flavor melpa :host github :repo "AdamNiederer/vue-mode") "mmm-mode" (mmm-mode :type git :flavor melpa :host github :repo "purcell/mmm-mode") "cl-lib" nil "vue-html-mode" (vue-html-mode :type git :flavor melpa :host github :repo "AdamNiederer/vue-html-mode") "ssass-mode" (ssass-mode :type git :flavor melpa :host github :repo "AdamNiederer/ssass-mode") "edit-indirect" (edit-indirect :type git :flavor melpa :host github :repo "Fanael/edit-indirect"))) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "emacsmirror-mirror" nil "straight" nil "spinner" (spinner :type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git"))) "cl-lib" nil)) el-get #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 1 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "cl-lib" nil))))

("org-elpa" "melpa" "gnu-elpa-mirror" "el-get" "emacsmirror-mirror" "straight" "emacs" "use-package" "bind-key" "lsp-mode" "dash" "f" "s" "ht" "spinner" "markdown-mode" "lv" "lsp-ui" "typescript-mode" "php-mode" "vue-mode" "mmm-mode" "cl-lib" "vue-html-mode" "ssass-mode" "edit-indirect")

t
