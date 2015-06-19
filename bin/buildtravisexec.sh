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
  tar zxf ../../deps/tarpit/cl-charms-9bb94ef.tar.gz &&
  tar zxf ../../deps/tarpit/style-checker_0.1.tar.gz &&
  tar zxf ../../deps/tarpit/docgen_0.1.tar.gz &&
  ln -s cl-ppcre-2.0.10/cl-ppcre.asd . &&
  ln -s ironclad_0.33.0/ironclad.asd . &&
  ln -s mt19937-1.1.1/mt19937.asd . &&
  ln -s nibbles-0.12/nibbles.asd . &&
  ln -s 3b-cl-opengl-993d627/cl-glut.asd . &&
  ln -s 3b-cl-opengl-993d627/cl-opengl.asd . &&
  ln -s alexandria-b1c6ee0/alexandria.asd . &&
  ln -s babel_0.5.0/babel-streams.asd . &&
  ln -s babel_0.5.0/babel.asd . &&
  ln -s cffi_0.15.0/cffi-examples.asd . &&
  ln -s cffi_0.15.0/cffi.asd . &&
  ln -s cffi_0.15.0/cffi-libffi.asd . &&
  ln -s cffi_0.15.0/cffi-grovel.asd . &&
  ln -s cffi_0.15.0/cffi-uffi-compat.asd . &&
  ln -s trivial-features_0.8/trivial-features.asd . &&
  ln -s cl-charms/cl-charms.asd . &&
  ln -s style-checker_0.1/style-checker.asd . &&
  ln -s docgen_0.1/docgen.asd .
)


SBCL_HOME="" tmp/sbcl/bin/sbcl --core tmp/sbcl/lib/sbcl/sbcl.core \
  --eval "(require 'asdf)" \
  --eval '(setf asdf:*central-registry* (list #p"tmp/deps/"))' \
  --eval "(asdf:load-system :cl-ppcre)" \
  --eval "(asdf:load-system :mt19937)" \
  --eval "(asdf:load-system :ironclad)" \
  --eval "(asdf:load-system :cl-opengl)" \
  --eval "(asdf:load-system :cl-glut)" \
  --eval "(asdf:load-system :cl-charms)" \
  --eval "(asdf:load-system :style-checker)" \
  --eval "(asdf:load-system :docgen)" \
  --eval "(asdf:clear-output-translations)" \
  --eval '(sb-ext:save-lisp-and-die "travissbcl" :executable t)' \

chmod +x travissbcl
travisname=travissbcl-$(git rev-parse --short HEAD)
mv travissbcl $travisname

echo "You should upload via the command: scp $travisname nami:/opt/travis/sbcls/clnl/"
echo "You should also set travisname in .travis.yml to $travisname"

rm -rf tmp
