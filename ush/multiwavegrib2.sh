#!/bin/sh
###############################################################################
#                                                                             #
# This script generates the GRIB2 file for the MWW3 forecast model            #
# It is run as a child scipt interactively by the postprocessor.              #
#                                                                             #
# Remarks :                                                                   #
# - The necessary files are retrieved by the mother script.                   #
# - This script generates it own sub-directory 'grib_*'.                      # 
# - See section 0.b for variables that need to be set.                        # 
#                                                                             #
# Script parameters are:                                                      #
#  grdID=$1      - Grid identity                                              #
#  dtgrib=$2     - Time interval for grib output                              #
#  ngrib=$3      - Number of grid output slices                               #
#  GRIDNR=$4     - WMO grid number (255 if not defined)                       #
#  MODNR=$5      - WMO model number                                           #
#  GTMPLN=$6     - GRIB2 template                                             #
#  gribflags=$7  - WW3 output flags for grib2 output processor                #
#                                                                             #
#                                                                July, 2007   #
# Last update : 07-06-2007                                                    #
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

  grdID=$1  
  rm -rf grib_$grdID
  mkdir grib_$grdID
  cd grib_$grdID

# 0.b Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  dtgrib=$2
  ngrib=$3
  GRIDNR=$4
  MODNR=$5
  GTMPLN=$6 

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make GRIB files        |'
  echo '+--------------------------------+'
  echo "   Model ID         : $modID"
  echo "   Run   ID         : $runID"
  [[ "$LOUD" = YES ]] && set -x

  if [ -z "$YMDH" ] || [ -z "$cycle" ] || [ -z "$EXECglwu" ] || \
     [ -z "$COMOUT" ] || [ -z "$runID" ] || [ -z "$SENDCOM" ] || \
     [ -z "$dtgrib" ] || [ -z "$ngrib" ] || \
     [ -z "$GRIDNR" ] || [ -z "$MODNR" ] || [ -z "$SENDDBN" ]
  then
    set +x
    echo ' '
    echo '***************************************************'
    echo '*** EXPORTED VARIABLES IN postprocessor NOT SET ***'
    echo '***************************************************'
    echo ' '
    postmsg "$jlogfile" "EXPORTED VARIABLES IN postprocessor NOT SET"
    exit 1
    [[ "$LOUD" = YES ]] && set -x
  fi

# 0.c Starting time for output

  ymdh=$YMDH
  tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"

  set +x
  echo "   Starting time    : $tstart"
  echo "   Time step        : $dtgrib"
  echo "   Number of times  : $ngrib"
  echo "   GRIB field flags : $gribflags"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 0.d Links to working directory

  ln -s ../mod_def.$grdID mod_def.ww3
  if [ "$grdID" == "grlc_2p5km_sr" ]
  then
    ln -s ../out_grd.grlc_2p5km  out_grd.ww3
  else
    ln -s ../out_grd.$grdID  out_grd.ww3 
  fi
# --------------------------------------------------------------------------- #
# 1.  Generate GRIB file with all data
# 1.a Generate input file for multiwavegrib2
#     Template copied in mother script ...

  set +x
  echo "   Generate input file for multiwavegrib2"
  [[ "$LOUD" = YES ]] && set -x

  sed -e "s/TIME/$tstart/g" \
      -e "s/DT/$dtgrib/g" \
      -e "s/NT/$ngrib/g" \
      -e "s/GRIDNR/$GRIDNR/g" \
      -e "s/MODNR/$MODNR/g" \
      -e "s/GTMPLN/$GTMPLN/g" \
      -e "s/FLAGS/$gribflags/g" \
                               ../multiwavegrib2.inp.tmpl > multiwavegrib2.inp

# 1.b Run GRIB packing program

  set +x
  echo "   Run multiwavegrib2"
  [[ "$LOUD" = YES ]] && set -x

  ln -sf ../$runID.$grdID.$cycle.grib2 gribfile
  $EXECglwu/multiwavegrib2
  err=$?

  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '********************************************* '
    echo '*** FATAL ERROR : ERROR IN multiwavegrib2 *** '
    echo '********************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN multiwavegrib2"
    exit 3
  fi

# 1.c Clean up

  rm -f multiwavegrib2.inp
  rm -f mod_def.ww3
  rm -f out_grd.ww3

# 1.d Create grib2 index file

  $GRB2INDEX gribfile gribfile_indx
  err=$?
  echo "err from grb2index = $err"

# Copy files to $COMOUT
  echo "   Saving GRIB file as $COMOUT/$runID.$grdID.$cycle.grib2"
  cp gribfile $COMOUT/$runID.$grdID.$cycle.grib2
  echo "   Creating wgrib index of $COMOUT/$runID.$grdID.$cycle.grib2"
  $WGRIB2 -s ../$runID.$grdID.$cycle.grib2 > $COMOUT/$runID.$grdID.$cycle.grib2.idx

  if [ "$SENDDBN" = 'YES' ]
  then
    set +x
    echo "   Alerting GRIB file as $COMOUT/$runID.$grdID.$cycle.grib2"
    echo "   Alerting GRIB index file as $COMOUT/$runID.$grdID.$cycle.grib2.idx"
    set -x
    [[ "$LOUD" = YES ]] && set -x
    $DBNROOT/bin/dbn_alert MODEL WAVE_GRIB_GB2 $job $COMOUT/$runID.$grdID.$cycle.grib2
    $DBNROOT/bin/dbn_alert MODEL WAVE_GRIB_GB2_WIDX $job $COMOUT/$runID.$grdID.$cycle.grib2.idx
  fi
 
# --------------------------------------------------------------------------- #
# 3.  Clean up the directory

  set +x
  echo "   Removing work directory after success."
  [[ "$LOUD" = YES ]] && set -x

  cd ..
  rm -rf grib_$grdID

  set +x
  echo ' '
  echo "End of multiwavegrib2.sh at"
  date

# End of multiwavegrib2.sh -------------------------------------------------- #
