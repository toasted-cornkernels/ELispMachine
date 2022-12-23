# (E)Lisp-Machine

[Lisp Machine](https://en.wikipedia.org/wiki/Lisp_machine) was a computer designed to be a Lisp heaven. All of its application and systems software were written in Lisp and was fully customizable, on-the-fly. Its hardware was specifically designed to carry out Lisp code efficiently, with CAR and CDR baked into the processors' instruction set.

Now that non-Lisp machines became more than good enough to execute Lisp code fast, Lisp machines, together with the full customizability they offered became a thing of the past. However, there is a free software that still embodies that hacker's spirit of freedom: Emacs. With sufficient knowledge of ELisp and the boldness to dive into source codes when necessary, one can ride any type of system if those systems support Emacs. And Emacs is one of the most ported software to date; it's almost a dark power to manipulate any type of system one may encounter in his/her hacker journey.

## ELisp-Machine

While I'm too young to have used a full Lisp Machine (like, from LMI or Symbolics), I'm about to reach 3 full years of using Emacs, starting from my own humble `init.el`. However, as an undergraduate who was also interning part-time in a graduate school lab, I lacked the time to hack my init script with too many things to do. So briefly after I jumped to [Spacemacs](https://www.spacemacs.org/).

While Spacemacs provided the power of fully customized Emacs with minimum effort on the user's side, adding my own features and replacing Spacemacs' default package slowly but steadily became a hassle. While one *could* do that, since Emacs is fully introspectable, the call chain of triggering a command would go down deep through Spacemacs' own layer of abstractions, with functions, macros, and variables.

Therefore comes my set of config, the ELisp-Machine (sorry, I don't have a better name yet). This config aims to:

- [Clone](http://www.petecorey.com/blog/2019/07/01/building-my-own-spacemacs/) [Spacemacs'](https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/) [behavior](https://www.youtube.com/watch?v=6INMXmsCCC8) [completely](https://gist.github.com/yaodong/532e5b31781724ea2566503edcc498c3) (so that it's transparent to the user), while
- Maintaining relatively fast startup time (< 6s), and
- Avoid unnecessary visual elements like a fancy modeline or all-the-icons.

Spacemacs is outstandingly good at maintaining [prefix trees](https://en.wikipedia.org/wiki/Trie) on every major mode as well as a global one that makes keybindings memorable and very easy to press, so I clone their `packages.el` in every layer imaginable as well as the core configs. I do it in a very vanilla way: not by using some esoteric custom functions or macros to do something (e.g. defining a local keybinding), but by using built-in functionalities from `use-package`, `general.el` and `straight.el`. The result is a Spacemacs-like config that is both lean and idiomatic.

While this is a work in progress as for now, it's my daily driver. Enjoy!
