#!/bin/bash
target="./usr/local/bin/ecall_i686"; 
for symbol in $(nm -D $target | grep "U " | cut -b12-); 
do for library in $(ldd $target | cut -d ' ' -f3- | cut -d' ' -f1); 
   do if [ "$library" != "not" ]; then 
      for lib_symbol in $(nm -D $library | grep "T " | cut -b12-); 
      do if [ $symbol == $lib_symbol ]; then echo -n ""; fi ; 
      done;
      else echo "missing $symbol"; 
      fi; 
   done; 
done;
