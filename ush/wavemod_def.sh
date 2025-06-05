#!/bin/sh
###############################################################################
#                                                                             #
# This script creates the mod_def file for the grid.                          #
#                                                                             #
# Remarks :                                                                   #
# - Shell script variables controling time, directories etc. are set in the   #
#   mother script.                                                            #
# - This script runs in the work directory designated in the mother script.   #
# - Script is run in a sub directory that is then removed at the end          #
# - See section 0.c for variables that need to be set.                        #
#                                                                             #
# Script parameters are:                                                      #
#  grdID=$1      - Grid identity                                              #
#                                                                             #
#                                                            April 08, 2011   #
# Last update : 02-29-2012                                                    #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  # set execution trace prompt.  ${0##*/} adds the script's basename
  PS4=" \${SECONDS} ${0##*/} L\${LINENO} + "
  set -x

  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  cd $DATA
  postmsg   "Generating mod_def file"

  mkdir -p moddef_${1}
  cd moddef_${1}

  grdID=$1

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!     Generate moddef file       |'
  echo '+--------------------------------+'
  echo "   Grid            : $1"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 0.b Check if grid set

  if [ "$#" -lt '1' ]
  then
    set +x
    echo ' '
    echo '**************************************************'
    echo '*** Grid not identifife in wavemod_def.sh ***'
    echo '**************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg   "GRID IN wavemod_def.sh NOT SET"
    exit 1
  else
    grdID=$1
  fi

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$grdID" ] || [ -z "$EXECglwu" ] || [ -z "$glwu_ver" ]
  then
    set +x
    echo ' '
    echo '*********************************************************'
    echo '*** EXPORTED VARIABLES IN wavemod_def.sh NOT SET ***'
    echo '*********************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg   "EXPORTED VARIABLES IN wavemod_def.sh NOT SET"
    exit 2
  fi

# --------------------------------------------------------------------------- #
# 2.  Create mod_def file 

  set +x
  echo ' '
  echo '   Creating mod_def file ...'
  echo "   Executing $EXECglwu/multiwavegrid"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x
 
  rm -f ww3_grid.inp 
  ln -sf ../$grdID.inp ww3_grid.inp
  if [ -f ../mesh.${grdID} ]
  then
    ln -fs ../mesh.$grdID .
  fi

  $EXECglwu/multiwavegrid
  err=$?

  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '******************************************** '
    echo '*** FATAL ERROR : ERROR IN multiwavegrid *** '
    echo '******************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg   "FATAL ERROR : ERROR IN multiwavegrid"
    exit 3
  fi
 
  if [ -f mod_def.ww3 ]
  then
    cp mod_def.ww3 $FIXglwu/wave_${modID}_${grdID}.moddef.${glwu_ver}
    mv mod_def.ww3 ../mod_def.$grdID
  else
    set +x
    echo ' '
    echo '******************************************** '
    echo '*** FATAL ERROR : MOD DEF FILE NOT FOUND *** '
    echo '******************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg   "FATAL ERROR : Mod def File creation FAILED"
    exit 4
  fi

# --------------------------------------------------------------------------- #
# 3.  Clean up

  cd ../.
  rm -rf moddef_$grdID

  set +x
  echo ' '
  echo 'End of multiwavemod_def.sh at'
  date

# End of multiwavemod_def.sh ------------------------------------------------- #
