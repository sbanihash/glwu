#!/bin/sh
###############################################################################
#                                                                             #
# This script creates an ice file based on climatology, IMS mask and NIC data #
#                                                                             #
# Remarks :                                                                   #
# - This script runs in the work directory designated in the mother script in #
#   which it generates its own sub-directory 'ice_yyyymmddhh'. If all is well #
#   the directory is removed at the end of the script.                        #
#                                                                             #
# Script parameters are:                                                      #
#  ymdh $1       - Date and time of data being processed                      #
#                                                                             #
#                                                                 Oct, 2014   #
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  set -x
  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  
  cd $DATA

   rm -rf eice_$1
   mkdir eice_$1
   cd eice_$1

  set +x
  echo ' '
  echo '+----------------------------------+'
  echo '!   Run codes to generate ice file |'
  echo '+----------------------------------+'
  [[ "$LOUD" = YES ]] && set -x

# 0.b Check if time set

  if [ "$#" -lt '1' ]
  then
    set +x
    echo ' '
    echo '*******************************************'
    echo '*** TIME IN waveice_lc.sh NOT SET ***'
    echo '*******************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" " TIME IN waveice_glw.sh NOT SET"
    exit 1
  else
    ymdh=$1
    set +x
    echo "   Time            : $ymdh"
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

# 0.c The tested variables should be exported by the calling script.

  if [ -z "$DCOMIN" ]
  then
    set +x
    echo ' '
    echo '**********************************'
    echo '*** EXPORTED VARIABLES NOT SET ***'
    echo '**********************************'
    echo ' '
    postmsg "$jlogfile" " EXPORTED VARIABLES NOT SET."
    exit 2
    [[ "$LOUD" = YES ]] && set -x
  fi




 #Set variables for processing and cross-referencing
  export PDYCE=`echo $ymdh | cut -c1-8`
  export YEAR=`echo $ymdh | cut -c1-4`
  export MONTH=`echo $ymdh | cut -c5-6`
  export DAY=`echo $ymdh | cut -c7-8`
  export MONTHNAME=`date -d ${YEAR}-${MONTH}-${DAY} '+%b'`
  export PDYtag=`echo $ymdh | cut -c1-8`
  export CYCtag=`echo $ymdh | cut -c9-10`
  
  fcsth=`${NHOUR} $ymdh $YMDH_ICE`

    if [ "$RetroRun" = "YES" ]
    then
      export dcominice=${DCOMIN}
    else
      export dcominice=/lfs/h1/ops/dev/dcom
    fi

# Initial NIC ice concentration file test for now
   nicice=${dcominice}/${PDYCE}/wtxtbul/OpenWater.nc

# Set search windows for older ice files, and search cutoff
  ndays=0
  ndaylim=10

# Set find parameter
  foundOK='no'

# Set parameters for first and last day of ice season
  ice_season_end=531     # May 31
  ice_season_start=1101  # November 01


# Set paramter for checking if date within limits for ice periods
  stag=`echo $PDYCE | cut -c5-8`

  while [ ${ndays} -le ${ndaylim} ] && [ "$foundOK" = 'no' ]
  do

    if [ $stag -gt $ice_season_end ] && [ $stag -lt $ice_season_start ] ; then
      set +x
      echo " "
      echo "  Date outside ice window, setting ice fields to zero"
      echo " "
      [[ "$LOUD" = YES ]] && set -x
      break
    else

# Start searching for NIC file
      set +x
      echo " "
      echo " Starting search for NIC ice concentrations file "
      echo " "
      [[ "$LOUD" = YES ]] && set -x

      if [ -f ${nicice} ]
      then

        set +x
        echo " "
        echo " NIC ice file found in ${dcominice}: ${nicice}" 
        echo " "
        [[ "$LOUD" = YES ]] && set -x

        foundOK='yes'
        #copy file here
        cp ${nicice} .
# Convert nicice netcdf file into txt file for ww3_prep use

   $NCDUMP -v IceCoverage_SFC OpenWater.nc | sed -e '1,/data:/d' -e '$d' | sed 's/,//g' | sed -e 's/IceCoverage_SFC =//' > LC.ice

# (ice will be tagged with the run start time, independent of its actual time)

   file=LC.ice
   cat ${file} | sed 's/ /\n/g' | sed '/^\s*$/d' | awk '!/\;/' >  tmp.ice
   cat tmp.ice  | awk  -v m=100 '{print $1 /m}' > LC_BVT_ICE.newice

# Create final ice file for intake in ww3_prep
cat > ../LC_BVT_ICE.${ymdh} << EOF
${PDYtag} ${CYCtag}0000
EOF
        cat LC_BVT_ICE.newice >> ../LC_BVT_ICE.${ymdh}
        cp ../LC_BVT_ICE.${ymdh} ../LC_BVT_ICE.newice

      else

# Continue search back in time
        ndays=`expr ${ndays} + 1`
        set +x
        echo " "
        echo " Moving back ${ndays} days to search for ice file " 
        echo " "
        [[ "$LOUD" = YES ]] && set -x
        PDYCE=`${NDATE} -24 ${PDYCE}00 | cut -c1-8`
        stag=`echo $PDYCE | cut -c5-8`
        nicice=${dcominice}/${PDYCE}/wtxtbul/OpenWater.nc

      fi

# Write file to whatused
      if [ "${foundOK}" = "yes" ]
      then
        echo "$ymdh LC_BVT_ICE_${YEAR}_${MONTHNAME}_${DAY}" >> ../whatglwice_lc
      fi

    fi
  done

# Use zero ice file if NIC ice data has not been found
   if [ "${foundOK}" = "no" ]
   then

    set +x
       echo " "
     echo " WARNING: NIC ice file not found, using zero ice all over "
       echo " "
    [[ "$LOUD" = YES ]] && set -x

    cp ${FIXglwu}/LC_BVT_ICE.zeros ./

# Create final ice file for intake in ww3_prep
cat > ../LC_BVT_ICE.${ymdh} << EOF
${PDYtag} ${CYCtag}0000
EOF

     cat LC_BVT_ICE.zeros >> ../LC_BVT_ICE.${ymdh}
     cp ../LC_BVT_ICE.${ymdh} ../LC_BVT_ICE.newice
     echo "$ymdh LC_BVT_ICE_${YEAR}_${MONTHNAME}_${DAY}" >> ../whatglwice_lc

   fi

#---------------------------------------------------------------------#

  if [ ! ../LC_BVT_ICE.${ymdh} ]
  then
    set +x
    echo ' '
    echo '********************************************'
    echo '*** FATAL ERROR IN ICE CONCENTRATION FILE **'
    echo '********************************************'
    echo ' '
    postmsg "$jlogfile" "ERROR IN COPYING SPECTRAL FILE FOR $ymdh."
    [[ "$LOUD" = YES ]] && set -x
    exit 4
  else
# Write out identifier for error checking in exwaveprep
    set +x
    echo "   File for ${ymdh} : LC_BVT_ICE.${ymdh}"
    [[ "$LOUD" = YES ]] && set -x
  fi

# --------------------------------------------------------------------------- #
# 4.  Clean up the directory

  set +x
  echo ' '
  echo "   Removing work directory after success."
  [[ "$LOUD" = YES ]] && set -x

  cd ..

  mv -f eice_$ymdh deice_$ymdh

  set +x
  echo ' '
  echo 'End of waveice_lc.sh at'
  date

# End of waveice_lc.sh ------------------------------------------------ #
