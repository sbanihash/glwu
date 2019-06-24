#!/bin/sh
###############################################################################
#                                                                             #
# This script generates ASCII data files with the wave spectral data at full  #
# resolution for a given output point of WAVEWATCH (MWW3) implementation      #
# or parallel. The location ID and position is passed as a shel script        # 
# parameter.                                                                  #
#                                                                             #
# Remarks :                                                                   #
# - The necessary files are retrieved by the mother script.                   #
# - Shell script variables controling time, directories etc. are set in the   #
#   mother script.                                                            #
# - This script runs in the work directory designated in the mother script.   #
#   Under this directory it geneates a work directory spec_$loc which is      #
#   removed if this script exits normally.                                    #
# - See section 0.c for variables that need to be set.                        #
#                                                                             #
# - For use with GLW and GLWN system, modID has been replaced with runID      #
#                                                                             #
# Script parameters are:                                                      #
#  grdID=$1      - Grid identity                                              #
#  ymdh=$2       - Date and time of data being processed                      #
#                                                                             #
#                                                            March 12, 2007   #
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

  rm -rf spec_$1
  mkdir spec_$1
  export err=$?
  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '****************************************************************************** '
    echo '*** FATAL ERROR : ERROR IN multiwavespec (COULD NOT CREATE TEMP DIRECTORY) *** '
    echo '****************************************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN multiwavespec (Could not create temp directory)"
    exit 1
  fi

  cd spec_$1

  ymdh=$2
  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!       Make spectral file       |'
  echo '+--------------------------------+'
  echo "   Model ID        : $runID"
  [[ "$LOUD" = YES ]] && set -x

# 0.b Check if buoy location set

  if [ "$#" -lt '1' ]
  then
    set +x
    echo ' '
    echo '***********************************************'
    echo '*** LOCATION ID IN multiwavespec2.sh NOT SET ***'
    echo '***********************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "LOCATION ID IN multiwavespec2.sh NOT SET"
    exit 1
  else
    buoy=$1
    point=`grep -a $buoy ../buoy_log.ww3 | awk '{ print $1 }'`
    set +x
    echo "              Location ID/#   : $buoy (${point})"
    echo "   Spectral output start time : $ymdh "
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    if [ -z "$point" ]
    then
      set +x
      echo '******************************************************'
      echo '*** LOCATION ID IN multiwavespec2.sh NOT RECOGNIZED ***'
      echo '******************************************************'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      postmsg "$jlogfile" "LOCATION ID IN multiwavespec2.sh NOT RECOGNIZED"
      exit 2
    fi
  fi


# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$YMDH" ] || [ -z "$dtspec" ] || [ -z "$EXECcode" ] || \
     [ -z "$runID" ] || [ -z "${STA_DIR}" ]
  then
    set +x
    echo ' '
    echo '******************************************************'
    echo '*** EXPORTED VARIABLES IN multiwavespec2.sh NOT SET ***'
    echo '******************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "EXPORTED VARIABLES IN multiwavespec2.sh NOT SET"
    exit 3
  fi

# 0.d Starting time for output

  tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"

  set +x
  echo "   Output starts at $tstart."
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 0.e sync important files

  $UTILROOT/exec/fsync_file ${DATA}/mod_def.points
  $UTILROOT/exec/fsync_file ${DATA}/out_pnt.ww3
  $UTILROOT/exec/fsync_file ${DATA}/multiwavespec.inp.tmpl

# 0.f Links to mother directory

  ln -s ../mod_def.ww3 .
  ln -s ../out_pnt.ww3 .

# --------------------------------------------------------------------------- #
# 2.  Generate spectral data file
# 2.a Input file for postprocessor

  set +x
  echo "   Generate input file for multiwavespec."
  [[ "$LOUD" = YES ]] && set -x

  sed -e "s/TIME/$tstart/g" \
      -e "s/DT/$dtspec/g" \
      -e "s/POINT/$point/g" \
      -e "s/ITYPE/1/g" \
      -e "s/FORMAT/F/g" \
                               ../multiwavespec.inp.tmpl > multiwavespec.inp

# 2.b Run the postprocessor

  set +x
  echo "   Executing $EXECcode/multiwavespec"
  [[ "$LOUD" = YES ]] && set -x

  #XXX Production utilities
  . prep_step
  startmsg

  $EXECcode/multiwavespec
  export err=$?

  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '******************************************** '
    echo '*** FATAL ERROR : ERROR IN multiwavespec *** '
    echo '******************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN multiwavespec"
    exit 4
  fi

# --------------------------------------------------------------------------- #
# 3.  Clean up
# 3.a Move data to directory for station ascii files

  outfile=ww3.`echo $tstart | cut -c3-8``echo $tstart | cut -c10-11`.spc

  if [ -f $outfile ]
  then
    mv $outfile  ${STA_DIR}/spec/$runID.$buoy.spec
  else
    set +x
    echo ' '
    echo '***************************************************************** '
    echo '*** FATAL ERROR : OUTPUT DATA FILE FOR BOUY $bouy NOT FOUND *** '
    echo '***************************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : OUTPUT DATA FILE FOR BOUY $bouy NOT FOUND"
    exit 5
  fi

# 3.b Clean up the rest

  cd ..
  rm -rf spec_$buoy

  set +x
  echo ' '
  echo 'End of multiwavespec2.sh at'
  date

# End of multiwavespec2.sh ---------------------------------------------------- #
