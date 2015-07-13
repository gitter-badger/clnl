# clnl

This is an experiment at creating an alternate implementation of NetLogo mainly as an exercise for me so that I come to understand NetLogo better.  It is true for me that I only truly understand something when I can explain it to someone else without repetition.  In code, this manifests as expressing a program in another language.

See the [wiki](https://github.com/frankduncan/clnl/wiki) for more information at this time.

# Building

If you'd like to build it, you're going to need a few things:

* An implementation of sbcl with threads enabled
* The following common lisp libraries (included in deps/tarpit folder)
  * cl-opengl
  * alexandria
  * babel
  * cffi
  * ironclad
  * mt19937
  * nibbles
  * trivial-features
  * style-checker
* rlwrap

# Running

bin/nl has been added for convenience to run the netlogo instance.  Currently it bombs if you try to do anything to expressive with it.  A good test is

```
crt 10
ask turtles [ fd 1 ]
show random 5
```

If you'd like to run using your own sbcl instance, you can attach the clnl.asd file wherever you link asd files, and then use:

```lisp
(asdf:load-system :clnl)
(sb-thread:make-thread #'clnl-interface:run)
(clnl:run)
```

# Testing

If you'd like to see the tests go on your system, there's a number of utilities to look at:

* bin/test.lisp
* bin/diagnose-test
* bin/diagnose-view-test
* bin/test-mode
