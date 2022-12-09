#!/bin/sh
###############################################################################
#                                                                             #
# This script generates a netcdf  file for the MWW3 forecast model            #
# It is run as a child scipt interactively by the postprocessor.              #
#                                                                             #
# Remarks :                                                                   #
# - The necessary files are retrieved by the mother script.                   #
# - This script generates it own sub-directory 'nc_*'.                      # 
# - See section 0.b for variables that need to be set.                        # 
#                                                                             #
# Script parameters are:                                                      #
#                                                                             #
#                                                                Jan,  2017   #
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
  rm -rf nc_$grdID
  mkdir nc_$grdID
  cd nc_$grdID
  dtnc=$2
  ncflags=$3

# 0.b Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make NCDF files        |'
  echo '+--------------------------------+'
  echo "   Model ID         : $modID"
  echo "   Run   ID         : $runID"
  [[ "$LOUD" = YES ]] && set -x

  if [ -z "$YMDH" ] || [ -z "$cycle" ] || [ -z "$EXECglwu" ] || \
     [ -z "$COMOUT" ] || [ -z "$runID" ] || [ -z "$SENDCOM" ] || \
     [ -z "$ncflags" ] || [ -z "$SENDDBN" ]
  then
    set +x
    echo ' '
    echo '***************************************************'
    echo '*** EXPORTED VARIABLES IN postprocessor NOT SET ***'
    echo '***************************************************'
    echo ' '
    postmsg   "EXPORTED VARIABLES IN postprocessor NOT SET"
    exit 1
    [[ "$LOUD" = YES ]] && set -x
  fi

# 0.c Starting time for output

  ymdh=$YMDH
  tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"

  set +x
  echo "   Starting time    : $tstart"
  echo "   Time step        : $dtnc"
  echo "   Number of times  : $nnc"
  echo "   NCDF field flags : $ncflags"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 0.d Links to working directory

  ln -s ../mod_def.$grdID mod_def.ww3
  ln -s ../out_grd.$grdID  out_grd.ww3 

# --------------------------------------------------------------------------- #
# 1.  Generate NCDF file with all data
# 1.a Generate input file for multiwavenc
#     Template copied in mother script ...

  set +x
  echo "   Generate input file for multiwavenc"
  [[ "$LOUD" = YES ]] && set -x

  sed -e "s/TIME/$tstart/g" \
      -e "s/DT/$dtnc/g" \
      -e "s/FLAGS/$ncflags/g" \
                               ../multiwavefldn.inp.tmpl > ww3_ounf.inp

# 1.b Run NCDF packing program

  set +x
  echo "   Run multiwavenc"
  [[ "$LOUD" = YES ]] && set -x

  $EXECglwu/multiwavefldn
  err=$?
 #  err='0'

  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '********************************************* '
    echo '*** FATAL ERROR : ERROR IN multiwavenc *** '
    echo '********************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg   "FATAL ERROR : ERROR IN multiwavenc"
    exit 3
  fi

# 1.c Clean up

  rm -f multiwavenc.inp
  rm -f mod_def.ww3
  rm -f out_grd.ww3

# Copy files up a tree level for ex-script handling of copies and alerts
    cp ww3.*.nc ../$runID.$grdID.$cycle.nc

# --------------------------------------------------------------------------- #
# 3.  Clean up the directory

  set +x
  echo "   Removing work directory after success."
  [[ "$LOUD" = YES ]] && set -x

  cd ..
  rm -rf nc_$grdID

  set +x
  echo ' '
  echo "End of multiwavenc.sh at"
  date

# End of multiwavenc.sh -------------------------------------------------- #
