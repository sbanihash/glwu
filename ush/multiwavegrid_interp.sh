#!/bin/ksh
###############################################################################
#                                                                             #
# This script generates the interpolated data for the older grids             #
# in the MWW3 forecast model                                                  #
# It is run as a child scipt interactively by the postprocessor.              #
#                                                                             #
# Remarks :                                                                   #
# - The necessary files are retrieved by the mother script.                   #
# - This script generates it own sub-directory 'grint_*'.                     # 
# - See section 0.b for variables that need to be set.                        # 
# - The script is designed to generate interpolated files for a single step   #
#                                                                             #
# Script parameters are:                                                      #
#  grdID=$1      - Grid identity                                              #
#  ymdh=$2       - Date and time of data being processed                      #
#  dt=$3         - Time interval of data being processed                      #
#  nst=$4        - number of output times                                     #
#                                                                             #
#                                                             July 10, 2009   #
# Last update : 02-29-2012                                                    #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  # set execution trace prompt.  ${0##*/} adds the script's basename
  PS4=" \${SECONDS} ${0##*/} L\${LINENO} + "
  echo ' '
  echo "Start of multiwavegrid_interp.sh at"
  date
  set -x

  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  cd $DATA

  grdID=$1  
  ymdh=$2
  dt=$3
  nst=$4
  postmsg "$jlogfile" "Making GRID Interpolation Files for $grdID."
  rm -rf grint_${grdID}_${ymdh}
  mkdir grint_${grdID}_${ymdh}
  err=$?
  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '************************************************************************************* '
    echo '*** FATAL ERROR : ERROR IN multiwavegrid_interp (COULD NOT CREATE TEMP DIRECTORY) *** '
    echo '************************************************************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN multiwavegrid_interp (Could not create temp directory)"
    exit 1
  fi

  cd grint_${grdID}_${ymdh}

# 0.b Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make GRID files        |'
  echo '+--------------------------------+'
  echo " Model Run ID       : $runID"
  [[ "$LOUD" = YES ]] && set -x

  if [ -z "$YMDH" ] || [ -z "$cycle" ] || [ -z "$EXECcode" ] || \
     [ -z "$COMOUT" ] || [ -z "$runID" ] || [ -z "$SENDCOM" ] || \
     [ -z "$SENDDBN" ] || [ -z "$grids" ]
  then
    set +x
    echo ' '
    echo '***************************************************'
    echo '*** EXPORTED VARIABLES IN postprocessor NOT SET ***'
    echo '***************************************************'
    echo ' '
    echo "$YMDH $cycle $EXECcode $COMOUT $runID $SENDCOM $SENDDBN $grids"
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "EXPORTED VARIABLES IN postprocessor NOT SET"
    exit 1
  fi

# 0.c Links to files

  rm -f ../out_grd.$grdID
  
  if [ ! -f ../${grdID}_interp.inp.tmpl ]; then
    cp $FIXwave/${grdID}_interp.inp.tmpl ../.
  fi
  if [ -f "$FIXwave/grint_weights.${grdID}" ]
  then
    echo ' '
    echo "Copying grint weight file grint_weights.${grdID}"
    cp $FIXwave/grint_weights.${grdID} ./WHTGRIDINT.bin 
  fi
  ln -sf ../${grdID}_interp.inp.tmpl . 

  for ID in $grids $isgrids $itgrids # Include all igrids for cross-breeding
  do
    ln -sf ../out_grd.$ID .
  done

  for ID in $grids $grdID $itgrids $isgrids # Include all igrids for cross-breeding 
  do
    ln -sf ../mod_def.$ID .
  done

# --------------------------------------------------------------------------- #
# 1.  Generate GRID file with all data
# 1.a Generate Input file

  time="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"

  sed -e "s/TIME/$time/g" \
      -e "s/DT/$dt/g" \
      -e "s/NSTEPS/$nst/g" ${grdID}_interp.inp.tmpl > multiwavegrid_interp.inp

# 1.b Run interpolation code

  set +x
  echo "   Run multiwavegrid_interp"
  echo "   Executing $EXECcode/multiwavegrid_interp"
  [[ "$LOUD" = YES ]] && set -x

## If there is a precomputed weights file, copy from FIX
#if [ -f $FIXwave/precomp_weights.bin.$grdID ]
#then
#   cp -f $FIXwave/precomp_weights.bin.$grdID ./precomp_weights.bin
#fi
  startmsg
  $EXECcode/multiwavegrid_interp
  export err=$?; err_chk

  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '*************************************************** '
    echo '*** FATAL ERROR : ERROR IN multiwavegrid_interp *** '
    echo '*************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN multiwavegrid_interp"
    exit 3
  fi

# 1.b Clean up

#  rm -f grid_interp.inp
#  rm -f mod_def.*

# 1.c Save in /com

  if [ "$SENDCOM" = 'YES' ]
  then
    set +x
    echo "   Saving GRID file as $COMOUT/$runID.$grdID.$cycle.outgrd"
    [[ "$LOUD" = YES ]] && set -x
#    cp out_grd.$grdID $COMOUT/$runID.$grdID.$cycle.outgrd

  fi 

# --------------------------------------------------------------------------- #
# 2.  Clean up the directory

  set +x
  echo "   Removing work directory after success."
  [[ "$LOUD" = YES ]] && set -x

  cd ..
  mv -f grint_${grdID}_${ymdh} grint_${grdID}_${ymdh}.done

  set +x
  echo ' '
  echo "End of multiwavegrid_interp.sh at"
  date

# End of multiwavegrid_interp.sh -------------------------------------------- #
