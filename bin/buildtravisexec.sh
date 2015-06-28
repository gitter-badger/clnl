#!/bin/bash

mkdir -p tmp/sbcl

cwd=$PWD
( cd tmp &&
  tar jxf ../deps/tarpit/sbcl-1.2.6-x86-64-linux-binary.tar.bz2 &&
  cd sbcl-1.2.6-x86-64-linux/ &&
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
  ln -s cl-ppcre-2.0.10/cl-ppcre.asd . &&
  ln -s ironclad_0.33.0/ironclad.asd . &&
  ln -s mt19937-1.1.1/mt19937.asd . &&
  ln -s nibbles-0.12/nibbles.asd . &&
  ln -s 3b-cl-opengl-993d627/cl-glut.asd . &&
  ln -s frank/.sbcl/site/3b-cl-opengl-993d627/cl-opengl.asd . &&
  ln -s alexandria-b1c6ee0/alexandria.asd . &&
  ln -s babel_0.5.0/babel-streams.asd . &&
  ln -s babel_0.5.0/babel.asd . &&
  ln -s cffi_0.15.0/cffi-examples.asd . &&
  ln -s cffi_0.15.0/cffi.asd . &&
  ln -s cffi_0.15.0/cffi-libffi.asd . &&
  ln -s cffi_0.15.0/cffi-grovel.asd . &&
  ln -s cffi_0.15.0/cffi-uffi-compat.asd . &&
  ln -s trivial-features_0.8/trivial-features.asd .
)


SBCL_HOME="" tmp/sbcl/bin/sbcl --core tmp/sbcl/lib/sbcl/sbcl.core \
  --eval "(require 'asdf)" \
  --eval '(setf asdf:*central-registry* (list #p"tmp/deps/"))' \
  --eval "(asdf:load-system :cl-ppcre)" \
  --eval "(asdf:load-system :mt19937)" \
  --eval "(asdf:load-system :ironclad)" \
  --eval "(asdf:load-system :cl-opengl)" \
  --eval "(asdf:load-system :cl-glut)" \
  --eval "(asdf:clear-output-translations)" \
  --eval '(sb-ext:save-lisp-and-die "deps/travissbcl" :executable t)' \

chmod +x deps/travissbcl

rm -rf tmp
