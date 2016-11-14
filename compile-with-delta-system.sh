#!/bin/bash
echo "Redefine delta name"
ghc -o VCG -isrc -cpp -DSAFETY_LEVEL -DDONT_DEFINE_DELTA -DDELTADIR='"/root/"' -DDELTARPM='"ecall-delta-1.0-1.drpm"' -DSYSTEM --make src/Main.hs
