#!/bin/bash

bin/buildosxexec.sh && \
mkdir tmp && \
cp -ap dist/osx/CLNL.app tmp && \
mv osxsbcl tmp/CLNL.app/Contents/MacOS/ && \
cd tmp && \
hdiutil create ../CLNL.dmg -srcfolder CLNL.app/ && \
cd ..
rm -rf tmp
