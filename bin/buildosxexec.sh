#!/bin/bash

# This sciprt builds a version of sbcl pre loaded with libraries that can run on osx.
# You can use ./osxsbcl --script bin/run.lisp to run it

mkdir -p tmp/sbcl
mkdir -p tmp/sbcl-binary

cwd=$PWD
( cd tmp &&
  tar jxf ../deps/tarpit/sbcl-1.2.11-x86-64-darwin-binary.tar.bz2 &&
  tar jxf ../deps/tarpit/sbcl-1.2.13-source.tar.bz2 &&
  cd sbcl-1.2.11-x86-64-darwin/ &&
  SBCL_HOME="" INSTALL_ROOT=$cwd/tmp/sbcl-binary/ bash install.sh &&
  cd ../sbcl-1.2.13/ &&
  PATH="$cwd/tmp/sbcl-binary/bin/:$PATH" SBCL_HOME="$cwd/tmp/sbcl-binary/lib/sbcl/" bash make.sh --with-sb-thread &&
  SBCL_HOME="" INSTALL_ROOT=$cwd/tmp/sbcl/ bash install.sh )

mkdir -p tmp/deps/

( cd tmp/deps &&
  tar zxf ../../deps/tarpit/3b-cl-opengl-993d627.tar.gz &&
  tar zxf ../../deps/tarpit/alexandria-b1c6ee0.tar.gz &&
  tar zxf ../../deps/tarpit/babel_0.5.0.tar.gz &&
  tar zxf ../../deps/tarpit/cffi_0.15.0.tar.gz &&
  tar zxf ../../deps/tarpit/cl-ppcre.tar.gz &&
  tar zxf ../../deps/tarpit/ironclad.tar.gz &&
  tar zxf ../../deps/tarpit/mt19937-latest.tar.gz &&
  tar zxf ../../deps/tarpit/nibbles-v0.12.tar.gz &&
  tar zxf ../../deps/tarpit/trivial-features_0.8.tar.gz &&
  tar zxf ../../deps/tarpit/cl-charms-9bb94ef.tar.gz # &&
  ln -s 3b-cl-opengl-993d627/cl-glut.asd . &&
  ln -s 3b-cl-opengl-993d627/cl-opengl.asd . &&
  ln -s alexandria-b1c6ee0/alexandria.asd . &&
  ln -s babel_0.5.0/babel-streams.asd . &&
  ln -s babel_0.5.0/babel.asd . &&
  ln -s cffi_0.15.0/cffi-examples.asd . &&
  ln -s cffi_0.15.0/cffi-grovel.asd . &&
  ln -s cffi_0.15.0/cffi-libffi.asd . &&
  ln -s cffi_0.15.0/cffi-uffi-compat.asd . &&
  ln -s cffi_0.15.0/cffi.asd . &&
  ln -s cl-charms/cl-charms.asd . &&
  ln -s cl-ppcre-2.0.10/cl-ppcre.asd . &&
  ln -s mt19937-1.1.1/mt19937.asd . &&
  ln -s nibbles-0.12/nibbles.asd . &&
  ln -s trivial-features_0.8/trivial-features.asd . &&
  ln -s ../../src/main/clnl.asd .
)

SBCL_HOME="" tmp/sbcl/bin/sbcl --core tmp/sbcl/lib/sbcl/sbcl.core --no-sysinit --no-userinit \
  --eval "(require 'asdf)" \
  --eval '(setf asdf:*central-registry* (list #p"tmp/deps/"))' \
  --eval "(asdf:load-system :clnl)" \
  --eval "(asdf:clear-output-translations)" \
  --eval '(sb-ext:save-lisp-and-die "osxsbcl" :executable t :toplevel (function clnl:run))' \

chmod +x osxsbcl

rm -rf tmp
