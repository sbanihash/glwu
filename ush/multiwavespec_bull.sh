#!/bin/sh
###############################################################################
#                                                                             #
# This script generates the tabular bulletins for a single output points      #
# of  WAVEATCH MWW3 implemnatation. It is run as a child script by a          # 
# postprocessor.                                                              #
#                                                                             #
# Remarks :                                                                   #
# - The necessary files are retrieved by the mother script.                   #
# - Shell script variables controling time, directories etc. are set in the   #
#   mother script.                                                            #
# - This script runs in the work directory designated in the mother script.   #
#   Under this directory it geneates a work directory bull_$loc which is      #
#   removed if this script exits normally.                                    #
# - See section 0.c for variables that need to be set.                        #
# - This script replaces multiwavebull.sh and marks transition of generating  #
#   bulletins from the point output post processor instead of a separate      #
#   partitioning code                                                         #
#                                                                             #
# - For use with GLW and GLWN system, modID has been replaced with runID      #
#                                                                             #
# Script parameters are:                                                      #
#  grdID=$1      - Grid identity                                              #
#  ymdh=$2       - Date and time of data being processed                      #
#                                                                             #
#                                                                Jan  2011    #
# Last update : 06-18-2012                                                    #
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

  rm -rf bull_$1
  mkdir bull_$1
  err=$?
  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '************************************************************************************ '
    echo '*** FATAL ERROR : ERROR IN multiwavespec_bull (COULD NOT CREATE TEMP DIRECTORY) *** '
    echo '************************************************************************************ '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    ../postmsg "$jlogfile" "FATAL ERROR : ERROR IN multiwavespec_bull (Could not create temp directory)"
    exit 1
  fi

  cd bull_$1
  
  ymdh=$2
  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!     Make spectral bulletin     |'
  echo '+--------------------------------+'
  echo "   Run   ID        : $runID"
  [[ "$LOUD" = YES ]] && set -x

# 0.b Check if buoy location set

  if [ "$#" -lt '2' ]
  then
    set +x
    echo ' '
    echo '****************************************************'
    echo '*** LOCATION ID IN multiwavespec_bull.sh NOT SET ***'
    echo '****************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    ../postmsg "$jlogfile" "Error LOCATION ID IN multiwavespec_bull.sh NOT SET"
    exit 1
  else
    buoy=$1
    point=`grep -a $buoy ../buoy_log.ww3 | awk '{ print $1 }'`
    set +x
    echo "   Location ID/#   : $buoy (${point})"
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    if [ -z "$point" ]
    then
      set +x
      echo '***********************************************************'
      echo '*** LOCATION ID IN multiwavespec_bull.sh NOT RECOGNIZED ***'
      echo '***********************************************************'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      ../postmsg "$jlogfile" "Error LOCATION ID IN multiwavespec_bull.sh NOT RECOGNIZED"
      exit 2
    fi
  fi

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$YMDH" ] || [ -z "$date" ] || [ -z "$cycle" ] || \
     [ -z "$dtbull" ] || [ -z "$EXECcode" ] || [ -z "$runID" ] || \
     [ -z "$ymdh" ] || [ -z "${STA_DIR}" ]
  then
    set +x
    echo ' '
    echo '***********************************************************'
    echo '*** EXPORTED VARIABLES IN multiwavespec_bull.sh NOT SET ***'
    echo '***********************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    ../postmsg "$jlogfile" "Error EXPORTED VARIABLES IN multiwavespec_bull.sh NOT SET"
    exit 3
  fi

# 0.d Starting time for output

  tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
  truntime="`echo $YMDH | cut -c1-8` `echo $YMDH | cut -c9-10`0000"

  set +x
  echo "   Output starts at $tstart."
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 0.e sync important files

  $UTILROOT/exec/fsync_file ${DATA}/mod_def.${pntgrd}
  $UTILROOT/exec/fsync_file ${DATA}/out_pnt.ww3
  $UTILROOT/exec/fsync_file ${DATA}/multiwavespec_bull.inp.tmpl

# 0.f Links to mother directory

  ln -s ../mod_def.${pntgrd} mod_def.ww3
  ln -s ../out_pnt.ww3 .

# --------------------------------------------------------------------------- #
# 2.  Generate spectral data file
# 2.a Input file for postprocessor

  set +x
  echo "   Generate input file for multiwavespec"
  [[ "$LOUD" = YES ]] && set -x

  sed -e "s/TIME/$tstart/g" \
      -e "s/DT/$dtbull/g" \
      -e "s/POINT/$point/g" \
      -e "s/REFT/$truntime/g" \
                               ../multiwavespec_bull.inp.tmpl > multiwavespec.inp

# 2.b Run the postprocessor

  set +x
  echo "   Executing $EXECcode/multiwavespec"
  [[ "$LOUD" = YES ]] && set -x

  $EXECcode/multiwavespec
  err=$?

  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '******************************************** '
    echo '*** FATAL ERROR : ERROR IN multiwavespec *** '
    echo '******************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    ../postmsg "$jlogfile" "FATAL ERROR : ERROR IN multiwavespec."
    exit 4
  fi

  if [ -f $buoy.bull ] && [ -f $buoy.cbull ] && [ -f $buoy.csv ]
  then
    mv $buoy.bull  ${STA_DIR}/bull/$runID.$buoy.bull
    mv $buoy.cbull ${STA_DIR}/cbull/$runID.$buoy.cbull
    mv $buoy.csv   ${STA_DIR}/csbull/$runID.$buoy.csbull
  else
    set +x
    echo ' '
    echo '********************************************* '
    echo '*** FATAL ERROR : BULLETIN FILE NOT FOUND *** '
    echo '********************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    ../postmsg "$jlogfile" "FATAL ERROR : BULLETIN FILE NOT FOUND"
    exit 7
  fi

  for ext in bull cbull csbull
  do
    if [ ! -f "${STA_DIR}/${ext}/$runID.$buoy.$ext" ]
    then
      set +x
      echo ' '
      echo'*******************************************************' 
      echo '*** FATAL ERROR : BULLETIN FILE NOT MOVED PROPERLY ***'
      echo'*******************************************************' 
      echo ' '
      echo " file $runID.$buoy.$ext not found !!"
      [[ "$LOUD" = YES ]] && set -x
      ../postmsg "$jlogfile" "FATAL ERROR : BULLETIN FILE $runID.$buoy.$ext NOT FOUND"
      exit 8
    fi
  done

# --------------------------------------------------------------------------- #
# 3 Clean up
 
  rm -f multiwavespec.inp
  rm -f mod_def.ww3 out_pnt.ww3
  cd ..
  rm -rf bull_$buoy

  set +x
  echo ' '
  echo 'End of multiwavespec_bull.sh at'
  date

# End of multiwavespec_bull.sh----------------------------------------------- #
