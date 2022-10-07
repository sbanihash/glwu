#!/bin/sh
###############################################################################
#                                                                             #
# This script converts hrrr files into wind.ww3 format files for WW3          #
#                                                                             #
# This version extracts forcing fields information from the hrrr grib2 file.  #
#                                                                             #
# Remarks :                                                                   #
# - This script runs in the work directory designated in the mother script in #
#   which it generates its own sub-directory 'hrrr_yyyymmddhh'. If all's well #
#   the directory is renamed at the end of the script.                        #
# - The time group yyyymmddhh is the first parameter passed to the script.    #
# - See section 0.c for variables that need to be set.                        #
#                                                                             #
# Script parameters are:                                                      #
#  $1            - DateTime of data being processed                           #
#  count=$2      - File count                                                 #
#  hdr=$3        - Header defining if ww3 binary will have a header           #
#                                                                             #
#                                                                May,  2016   #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  echo ' Start of wavehrrr_glwu.sh at: '
  date

  cd $DATA

  set -x
  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  rm -rf hrrr_$1
  mkdir hrrr_$1
  cd hrrr_$1

  count=$2
  hdr=$3

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!     Find and copy hrrr files    |'
  echo '+--------------------------------+'
  [[ "$LOUD" = YES ]] && set -x

# 0.b Check if time set

  if [ "$#" -lt '1' ]
  then
    set +x
    echo ' '
    echo '***************************************'
    echo '*** TIME IN wavehrrr_glwu.sh NOT SET ***'
    echo '***************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" " TIME IN wavehrrr_glwu.sh NOT SET"
    exit 1
  else
    ymdh=$1
    set +x
    echo "   Time            : $ymdh"
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

# 0.c The tested variables should be exported by the calling script.

  if [ -z "$DCOMIN" ] || [ -z "$EXECglwu" ]
  then
    set +x
    echo ' '
    echo '**********************************'
    echo '*** EXPORTED VARIABLES NOT SET ***'
    echo '**********************************'
    echo ' '
    postmsg "$jlogfile" " EXPORTED VARIABLES NOT SET."
    exit 1
    [[ "$LOUD" = YES ]] && set -x
  fi

# --------------------------------------------------------------------------- #
# 1. The hrrr file has been selected by the mother script

# --------------------------------------------------------------------------- #
# 1b. Convert grib2 hrrr to regular hrrr, compute UGRD/VGRD, interpolate to 
#     regular grid and create netcdf file. (Interpolation is needed because 
#     the WW3 netcdf preprocessor only works with regular grids)


# Interpolate to regular grid 
 $WGRIB2 ${hrrr_file} -match ":vt=${ymdh}" -match "UGRD:10 m above ground:|VGRD:10 m above ground:" -new_grid_winds earth -new_grid_vectors UGRD:VGRD  -new_grid latlon 286.5:200:0.009 43.5:250:0.009  gluv_hrrr.grib2 > grib_hrrr_interp.out 2>&1

 # $WGRIB2 gluv.grb2 -new_grid_winds earth -new_grid_vectors UGRD:VGRD -new_grid latlon 267.28:1376:0.0125 41.1675:910:.009 gluv.grib2 > grib_interp.out 2>&1 286.5:200:0.009 43.5:250:0.009


# Final store on NetCDF file
  $WGRIB2 gluv_hrrr.grib2 -netcdf gluv_hrrr.nc > grib2nc_hrrr.out 2>&1

  if [ -f gluv_hrrr.nc ]
  then
    hrrrOK='yes'
  else
    hrrrOK='no'
  fi
 
# --------------------------------------------------------------------------- #
# 2. run waveprep 

  if [ "$hrrrOK" = 'yes' ]
  then

# 2.a Prepare the input file

    sed -e "s/HDR/$hdr/g" ../multiwaveprnc.hrrr_glwu.tmpl > ww3_prnc.inp

# 2.b Execute code

    for grdID in $grids
    do 

      cp ../mod_def.$grdID mod_def.ww3
      $EXECglwu/multiwaveprnc > multiwaveprnc.out.$ymdh 
      err=$?

      if [ "$err" != '0' ]
      then
        set +x
        echo ' '
        echo '*****************************************'
        echo '*** FATAL ERROR IN EXECUTING WAVEPREP ***'
        echo '*****************************************'
        echo ' '
        postmsg "$jlogfile" "ERROR IN EXECUTING WAVEPREP FOR $ymdh."
        [[ "$LOUD" = YES ]] && set -x
        hrrrOK='no'
      else
        if [ ! -f wind.ww3 ]
        then
          set +x
          echo ' '
          echo '****************************************'
          echo '*** FATAL ERROR : wind.ww3 NOT FOUND ***'
          echo '****************************************'
          echo ' '
          postmsg "$jlogfile" "ERROR IN EXECUTING WAVEPREP FOR $ymdh."
          [[ "$LOUD" = YES ]] && set -x
          hrrr_ok='no'
        else
          set +x
          echo ' '
          echo " Run succesful moving wind file ..."
          [[ "$LOUD" = YES ]] && set -x
          cp -f wind.ww3 ../wind.$grdID.$ymdh
          cp -f wdata.dat ../wdata.$grdID.$ymdh
          cp -f wmask.dat ../wmask.dat
          cp -f ll.dat ../ll.dat
        fi
      fi

    done
  
  else
    set +x
    echo ' '
    echo '**************************************************'
    echo '*** FATAL ERROR : COULD NOT GENERATE wind file ***'
    echo '**************************************************'
    echo ' '
    postmsg "$jlogfile" "COULD NOT EXTRACT WIND FOR $ymdh."
    [[ "$LOUD" = YES ]] && set -x
    hrrrOK='no'
  fi

# --------------------------------------------------------------------------- #
# 3.  Clean up the directory

  if [ "$hrrrOK" = 'yes' ]
  then
    set +x
    echo ' '
    echo "   Removing work directory after success."
    [[ "$LOUD" = YES ]] && set -x

    cd ..

# rm -rf hrrr_$ymdh
  mv hrrr_$ymdh hrrr_${ymdh}_done

  fi

  set +x
  echo ' '
  echo 'End of wavehrrr_glwu.sh at'
  date

# End of wavehrrr_glwu.sh ---------------------------------------------------- #
