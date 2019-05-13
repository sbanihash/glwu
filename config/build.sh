#!/bin/bash
## ---------------------------------------------------------------------------#
# build.sh: Script to clone the WW3 code from a Git  repository and runs the  #
#           program "CompileScript" to generate source codes and makefiles,   #
#           converting WW3 from a development branch or Master repository     # 
#           into independent code  directories.  The code is cloned in the    #
#           "WW3/" directory and inside this directory source code and        #
#           executables are created                                           #
#                                                                             #
#           SEE AT THE BOTTOM FOR SPECIFIC TASKS FOR THE                      #
#           DIFFERENT OPERATIONAL SYSTEMS                                     #
#                                                                             #
# Script name:  build.sh                                                      #
# Authors:      Roberto.Padilla@noaa.gov                                      #
# Created:      2019/05/07                                                    #
# Last Modif:   2019/05/12                                                    #
#                                                                             #
#                                                                             #
# Run:  ./build.sh {[-clone] [-compile]}                                      #
#                   -clone :   Only clone the WW3 code                        #
#                   -compile:  Only compile the code, do not clone it         #
#                                                                             #
#                   if non of the arguments are provided then the code will   #
#                   be cloned and compiled, for compilation only the code     #
#                   musty be already in "WW3/"                                #
#                                                                             #
#                                                                             #
# ----------------------------------------------------------------------------# 
set +xa
#
# -----SET THE PROPER VARIABLES NAMES FOR YOU SYSTEM---
#
#Set the repository and the branch names to be check out
gitRepos=ssh://git@github.com/ajhenrique/WW3.git
branchName=HF_ounf_fixedfile
#branchName=master
#
# Set your script for compilation, this must in the same directory as this script (build.sh) 
CompileScript=make_ww3_to_nco
#
# SET the directory where WW3 code will reside
WW3Dir=WW3XXX     # This is the default name


# 1. Checking the input arguments
if [ $# = 0 ] || [ $# = 2 ]
then
  clone=1
  compile=1
  echo ""
  echo " **** Cloning and Compiling WW3 code ****"
else [ $# = 1 ]
  arg=$1
  if [[ $arg == "-clone" ]]; then
    clone=1
    compile=0
    echo ""
    echo " **** Only cloning WW3 code... NOT Compilation****"
  elif [[ $arg == "-compile" ]]; then
    clone=0
    compile=1
    echo ""
    echo " **** NOT Cloning... Only compiling WW3 code ****"
  else
    echo " Something is wrong with your input arguments"
    echo " You must provide: [-clone] or [-compile] or "
    echo " both arguments or not arguments at all "
    echo " Example,  built.sh -clone -compile "
    echo " or        built.sh -compile"
    echo " or with no arguments, in this case -clone and -compile is assumed"
    echo " You provided built.sh $1 $2"
    echo ""
    echo " +++++ Program aborting +++++ "
    echo ""
    exit 1
  fi
fi
sleep 3
 
configDir=`pwd`
#Change dir where the source code and executables are going to reside
cd ../ 

#2.  Cloning WW3 code
if [ $clone = 1 ]
then
  echo "CLONING THE CODE"

  # Remove any previous WW3 version
  rm -rf $WW3Dir 
  #=================================================================
  # The directory for WaveWatch is WW3/  by default
   git clone -b $branchName $gitRepos  $WW3Dir
  #git clone --recursive gerrit:EMC_ww3
  #
  #=================================================================
  cd $WW3Dir
  git checkout $branchName  2>&1

  #git symbolic-ref HEAD | sed -e "s/^refs\/heads\///"
  export WW3VER=`grep CHARACTER ./model/ftn/w3initmd.ftn | grep PARAMETER | grep WWVER | awk '{print $6}' | sed 's/'\''//g'`
  echo " WW3 version is: $WW3VER"
  #Delete all directories except the model/
  rm -rf guide  LICENSE.md  manual README.md  regtests  smc_docs .git
  #Delete all hidden files from git under this directory.
  find . -name ".git*" -type f -delete
fi
#
# 3. Compiling the code
if [ $compile = 1 ]
then
  echo "COMPILING THE CODE"; sleep 1
  cd $configDir

  #Check if WW3 directory is not emty
  if [ "$(ls -A ../$WW3Dir)" ]; then
     echo "WW3 code exist and it will be compiled"
     cd ../$WW3Dir
     #sleep 3
     #git checkout $branchName
     export WW3VER=`grep CHARACTER ./model/ftn/w3initmd.ftn | grep PARAMETER | grep WWVER | awk '{print $6}' | sed 's/'\''//g'`
     echo " WW3 version is: $WW3VER"
    #Always run quietly when setup and compilation is done automatically
    sed -i '/WCOSS/d' model/bin/w3_setenv
    sed '/basename $0/ a  quiet=1  # Run quitley in WCOSS'  model/bin/w3_setenv  > model/bin/w3_setenv.temp
    mv -f model/bin/w3_setenv.temp  model/bin/w3_setenv
    chmod +x model/bin/w3_setenv
    # Make the code and executables for NCO
    cp ${configDir}/$CompileScript .
    chmod +x $CompileScript
    ./$CompileScript
    echo " WW3 Version number: $WW3VER "
    echo " "
  else
    echo "$WW3Dir IS EMPTY OR DOES NOT EXIST"
    echo "You must clone WW3 code"
    echo "RUN: build.sh -clone -compile"
    exit 1
  fi
fi

# ================EXCLUSIVELY FOR GLWU SYSTEM===============
#Update the WW3 version in the wave_glwu.ver file
sed -e 's/VersionNumber/'$WW3VER'/g' ../versions/wave_glwu.temp  > ../versions/wave_glwu.ver
