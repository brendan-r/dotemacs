# Preferences

- Write documentation and example documents in Org format (`.org`) rather than Markdown. Keep this file in Markdown.

- Use one physical line per prose paragraph or list item in Markdown and Org files; do not hard-wrap prose. Preserve appropriate formatting in code blocks.

- Prefer built-in Emacs features and existing packages over substantial custom Emacs Lisp. Before implementing custom behavior, inspect the relevant documentation and source code of existing dependencies and promising alternative libraries. Small glue functions are fine when they provide the simplest maintainable solution.

- Put substantial new functionality in a focused module under `lisp/`, with its package declaration and configuration in `config.org`. Use `br/` for miscellaneous personal functions. For dedicated modules, use a consistent module prefix for functions and variables, such as `br-python-` in `br-python.el`.

- Look for existing solutions in [Prelude](https://github.com/bbatsov/prelude), [Doom Emacs](https://github.com/doomemacs/core), and [Spacemacs](https://github.com/syl20bnr/spacemacs). Use their package combinations and configuration patterns as references, checking whether they depend on distribution-specific helpers or assumptions before adapting them.

# Context

- Source of truth: edit `config.org`; `config.el` is generated and should not be edited directly. `init.el`, `early-init.el`, and local Lisp modules are separately maintained sources.

- Package conventions: use Elpaca through existing `use-package` declarations. External packages normally use `:ensure t`; built-in and local libraries use `:ensure nil`.

- Config should work on Emacs 30+, on the latest MacOS on a Macbook Pro, and Debian Trixie on ThinkPad
