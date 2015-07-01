#!/bin/bash

mkdir -p tmp/sbcl

cwd=$PWD
( cd tmp &&
  tar jxf ../deps/tarpit/sbcl-1.2.6-x86-64-linux-binary.tar.bz2 &&
  cd sbcl-1.2.6-x86-64-linux/ &&
  INSTALL_ROOT=$cwd/tmp/sbcl/ bash install.sh )

mkdir -p tmp/deps/

( cd tmp/deps &&
  tar zxf ../../deps/tarpit/cl-ppcre.tar.gz &&
  tar zxf ../../deps/tarpit/ironclad.tar.gz &&
  tar zxf ../../deps/tarpit/mt19937-latest.tar.gz &&
  tar zxf ../../deps/tarpit/nibbles-v0.12.tar.gz &&
  ln -s cl-ppcre-2.0.10/cl-ppcre.asd . &&
  ln -s ironclad_0.33.0/ironclad.asd . &&
  ln -s mt19937-1.1.1/mt19937.asd . &&
  ln -s nibbles-0.12/nibbles.asd .
)


tmp/sbcl/bin/sbcl --core tmp/sbcl/lib/sbcl/sbcl.core \
  --eval "(require 'asdf)" \
  --eval '(setf asdf:*central-registry* (list #p"tmp/deps/"))' \
  --eval "(asdf:load-system :cl-ppcre)" \
  --eval "(asdf:load-system :mt19937)" \
  --eval "(asdf:load-system :ironclad)" \
  --eval "(asdf:clear-output-translations)" \
  --eval '(sb-ext:save-lisp-and-die "deps/travissbcl" :executable t)' \

chmod +x deps/travissbcl

rm -rf tmp
