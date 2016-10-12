#!/bin/bash
echo "Redefine delta name"
ghc -isrc -cpp -DSAFETY_LEVEL -DDONT_DEFINE_DELTA -DDELTADIR='"./data/"' -DDELTARPM='"ecall-delta-1.0-1.drpm"' --make src/Main.hs
