#!/bin/bash
###############################################################################
#                                                                             #
# Compiles all codes, moves executables to exec and cleans up                 #
#                                                                             #
#                                                                 Dec, 2016   #
#                                                                             #
###############################################################################
#

# 0. Install large external fix files
./get_externals.sh

# 1. Build ice field processor

module purge
module use ../modulefiles
module load build_glwu.module

dirs=`ls -d inpaint*.fd`
codes=`echo $dirs | sed 's/\.fd/ /g'`

mkdir ../exec

for i in  $codes
do
	cd ${i}.fd
        mv make.log make.log.bak
	make clean > make.log 2>&1
        module list >> make.log 2>&1
	make >> make.log 2>&1
        mv $i ../../exec
        make clean
        cd ../
done

# 2. Build WW3 model codes

set -x
module purge
module use ../modulefiles
module load build_wavewatch3.modules

# 2.1 Preparations: seek source codes to be compiled

  fcodes=`ls -d *wave*.fd | sed 's/\.fd//g'`
  ccodes=`ls -d *wave*.Cd | sed 's/\.Cd//g'`

  echo " FORTRAN codes found: "$fcodes
  echo " C codes found: "$ccodes

  outfile=`pwd`/make_codes.out
  rm -f ${outfile}

# 2.2 Create executables

  for code in $fcodes $ccodes
  do
    echo ' '
    echo " Making ${code} "
    echo " Making ${code} " >> ${outfile}
    cd ${code}.?d
    make >> ${outfile} 2>> ${outfile}
    echo " Moving ${code} to exec" >> ${outfile}
    if [ -f ${code} ]
    then
      echo " Code ${code} created successfully, moving to exec "
      mv ${code} ../../exec/
    else
      echo " FATAL ERROR making ${code} "
    fi
    echo ' '
    echo " Cleaning up ${code} directory" >> ${outfile}
    make clean
    echo ' ' >> ${outfile}
    cd ..
  done


