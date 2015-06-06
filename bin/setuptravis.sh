#/bin/sh

mkdir -p $HOME/sbcl
mkdir -p tmp
( cd tmp &&
  tar jxf ../deps/tarpit/sbcl-1.2.6-x86-64-linux-binary.tar.bz2 &&
  cd sbcl-1.2.6-x86-64-linux/ &&
  INSTALL_ROOT=$HOME/sbcl bash install.sh )
( cd tmp &&
  tar zxf ../deps/tarpit/cl-ppcre.tar.gz &&
  tar zxf ../deps/tarpit/ironclad.tar.gz &&
  tar zxf ../deps/tarpit/mt19937-latest.tar.gz &&
  tar zxf ../deps/tarpit/nibbles-v0.12.tar.gz &&
  cd ../deps &&
  ln -s ../tmp/cl-ppcre-2.0.10/cl-ppcre.asd . &&
  ln -s ../tmp/ironclad_0.33.0/ironclad.asd . &&
  ln -s ../tmp/mt19937-1.1.1/mt19937.asd . &&
  ln -s ../tmp/nibbles-0.12/nibbles.asd .
)

ls -l deps
