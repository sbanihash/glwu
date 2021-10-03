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

source ../versions/build.ver

outfile=`pwd`/make.all.out
rm -f ${outfile}

module purge
#module use ../modulefiles
#module load build_glwu.module
source ../modulefiles/build_glwu.module

dirs=`ls -d inpaint*.fd`
codes=`echo $dirs | sed 's/\.fd/ /g'`

mkdir ../exec

for i in  $codes
do
	cd ${i}.fd
	make clean > ${outfile} 2>> ${outfile}
        module list >> ${outfile} 2>> ${outfile}
	make >> ${outfile} 2>> ${outfile}
        mv $i ../../exec
        make clean
        cd ../
done

# 2. Build WW3 model codes

set -x
module purge
#module use ../modulefiles
#module load build_wavewatch3.modules
source ../modulefiles/build_wavewatch3.modules

# 2.1 Preparations: seek source codes to be compiled

  fcodes=`ls -d *wave*.fd | sed 's/\.fd//g'`

  echo " FORTRAN codes found: "$fcodes

# 2.2 Create executables

  module list >> ${outfile} 2>> ${outfile}

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


