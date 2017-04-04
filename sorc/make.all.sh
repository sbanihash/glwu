#!/bin/bash
set -x

module use ../modulefiles

module load build_glwu.module

dirs=`ls -d *.fd`
codes=`echo $dirs | sed 's/\.fd/ /g'`

for i in  $codes
do
	cd ${i}.fd
        mv make.log make.log.bak
	make clean > make.log 2>&1
        module list >> make.log 2>&1
	make >> make.log 2>&1
        mv $i ../../exec
        cd ../
done

