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
    echo '*** TIME IN waveice_glw.sh NOT SET ***'
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

# Set variables for processing and cross-referencing
# (ice will be tagged with the run start time, independent of its actual time)
  export PDYCE=`echo $ymdh | cut -c1-8`
  export PDYtag=`echo $ymdh | cut -c1-8`
  export CYCtag=`echo $ymdh | cut -c9-10`

  fcsth=`${NHOUR} $ymdh $YMDH_ICE`
#======================================================================
# Initial NIC ice concentration file
#  if [ "${IceResol}" = "LOW" ]
#  then
#     nicice=${DCOMIN}/${PDYCE}/wgrbbul/T_OEBA88_C_KNWC_${PDYCE}120000.gr1
#  elif [ "${IceResol}" = "HIGH" ]
#  then
#     #START NEW LINES
#     NOSCRUB=/gpfs/hps3/emc/marine/noscrub/Roberto.Padilla/NICNewIce/${PDYCE}/wgrbbul
#     Prefix=T_OEBA88_C_KNWC
#     cp ${NOSCRUB}/NIC_LKS_2018_Jan_05.zip ${NOSCRUB}/${Prefix}_${PDYCE}120000.zip
#     nicice=${NOSCRUB}/${Prefix}_${PDYCE}120000.zip
#  fi
  set -xa
  echo "DCOMIN/PDYCE in waveice.glw.sh: ${DCOMIN}/${PDYCE}"
  if [ "${IceResol}" = "LOW" ]
  then
     nicice=${DCOMIN}/${PDYCE}/wgrbbul/T_OEBA88_C_KNWC_${PDYCE}120000.gr1
  elif [ "${IceResol}" = "HIGH" ] &&  [ "$RetroRun" = "YES" ]
  then
     #START NEW LINES
     IceDir=${DCOMIN}/${PDYCE}/wgrbbul/
     Prefix=T_OEBA88_C_KNWC
     #while read line; do     IceFile="$line"        ; done << (ls ${IceDir}/NIC*)
     for file in "${IceDir}"/NIC*zip;do  nicice=$file; echo "$nicice"; done

  elif [ "${IceResol}" = "HIGH" ] &&  [ "$RetroRun" = "NOT" ]
  then
     #START NEW LINES
     IceDir=${DCOMIN}/${PDYCE}/wgrbbul/
     Prefix=T_OEBA88_C_KNWC
     for file in "${IceDir}"/NIC*zip;do  nicice=$file; echo "$nicice"; done

  fi

#======================================================================






# Set search windows for older ice files, and search cutoff
  ndays=0
  ndaylim=5

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
        echo " NIC ice file found in ${DCOMIN}: ${nicice}" 
        echo " "
        [[ "$LOUD" = YES ]] && set -x

        foundOK='yes'

        # START XXX RPH CHANGES ====================================
        echo "ICE RESOLUTION : ${IceResol}"
        if [ "${IceResol}" = "LOW" ]
        then
           cp ${nicice} ./nicice.gr1
           #Convert grib1 to grib2
           $CNVGRIB -g12 nicice.gr1 T_OEBA88_C_KNWC.grb2
           # Extract to text
           $WGRIB2 T_OEBA88_C_KNWC.grb2 -text T_OEBA88_C_KNWC.ice
           # Use inpaint to extend ice concentrations over ice-file land mask
           # (Conservative approach: no averaging to fill model-data sea gaps)
           cp ${FIXwave}/T_OEBA88_C_KNWC.mask ./
        elif [ "${IceResol}" = "HIGH" ]
        then
           cp ${nicice} ./${Prefix}_${PDYCE}120000.zip
           nicice=${Prefix}_${PDYCE}120000.zip
           cp ${nicice} ./nicice.zip
           # unzip the ice file
           unzip nicice.zip
           # Extract the third column (we have: lon, lat, iceconc, lakeNumber 
           #$WGRIB2 T_OEBA88_C_KNWC.grb2 -text T_OEBA88_C_KNWC.ice
           icefile=$(find NIC*.txt)
           #Removing the header
           sed -i '1,7d' $icefile
           #deleting a file if exist
           if [ -f T_OEBA88_C_KNWC.ice ]
           then
             rm T_OEBA88_C_KNWC.ice
           fi
           #Making the new file
           echo '2554 1823' > T_OEBA88_C_KNWC.ice
           #Extracting only the third column, the ice concentartion, and send this to ice_conc.new
           sed 's/[\t ][\t ]*/ /g' < $icefile | cut -d',' -f3 >> T_OEBA88_C_KNWC.ice
           #
           # (Conservative approach: no averaging to fill model-data sea gaps)
           cp -f ${FIXwave}/GLWU_NIC.mask ./T_OEBA88_C_KNWC.mask
         else
           echo '********************************************'
           echo '*** FATAL ERROR IN ICE RESOLUTION CHOICE **'
           echo '**  IceResol must be : LOW or HIGH         **'
           echo '********************************************'
           echo ' '
           postmsg "$jlogfile" "ERROR IN ICE RESOLUTION CHOICE"
           [[ "$LOUD" = YES ]] && set -x
           exit 
        fi 
#END XXX RPH CHANGES =============================

        $EXECwave/inpaint_nic_glwu #1> inpaint.out 2>&1

        if [ ! -f T_OEBA88_C_KNWC.newice ]
        then 
          echo ' '
          echo '***************************************'
          echo '*** ERROR RUNNNING inpaint_nic_glwu ***'
          echo '***************************************'
          echo ' '
          postmsg "$jlogfile" " INPAINT FAILED."
          exit 3
        fi 

# Create final ice file for intake in ww3_prep
cat > ../T_OEBA88_C_KNWC.${ymdh} << EOF
${PDYtag} ${CYCtag}0000
EOF

        cat T_OEBA88_C_KNWC.newice >> ../T_OEBA88_C_KNWC.${ymdh}
        cp ../T_OEBA88_C_KNWC.${ymdh} ../T_OEBA88_C_KNWC.newice

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
        ### Replace by if block nicice=${DCOMIN}/${PDYCE}/wgrbbul/T_OEBA88_C_KNWC_${PDYCE}120000.gr1

## NEW for High or Low Ice resolution and retrospectives or not
        if [ "${IceResol}" = "LOW" ]
        then
           nicice=${DCOMIN}/${PDYCE}/wgrbbul/T_OEBA88_C_KNWC_${PDYCE}120000.gr1
        elif [ "${IceResol}" = "HIGH" ] &&  [ "$RetroRun" = "YES" ]
        then
           IceDir=${DCOMIN}/${PDYCE}/wgrbbul/
           Prefix=T_OEBA88_C_KNWC
           for file in "${IceDir}"/NIC*zip;do  nicice=$file; echo "$nicice"; done
        elif [ "${IceResol}" = "HIGH" ] &&  [ "$RetroRun" = "NOT" ]
        then
           #START NEW LINES
           IceDir=${DCOMIN}/${PDYCE}/wgrbbul/
           Prefix=T_OEBA88_C_KNWC
           for file in "${IceDir}"/NIC*zip;do  nicice=$file; echo "$nicice"; done        fi
         fi

# Write file to whatused
      if [ "${foundOK}" = "yes" ]
      then
        echo "$ymdh T_OEBA88_C_KNWC_${PDYCE}120000.gr1" >> ../whatglwice
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

    cp ${FIXwave}/T_OEBA88_C_KNWC.zeros ./

# Create final ice file for intake in ww3_prep
cat > ../T_OEBA88_C_KNWC.${ymdh} << EOF
${PDYtag} ${CYCtag}0000
EOF

     cat T_OEBA88_C_KNWC.zeros >> ../T_OEBA88_C_KNWC.${ymdh}
     cp ../T_OEBA88_C_KNWC.${ymdh} ../T_OEBA88_C_KNWC.newice

     echo "$ymdh T_OEBA88_C_KNWC_${PDYCE}120000.gr1" >> ../whatglwice

   fi

#---------------------------------------------------------------------#

  if [ ! ../T_OEBA88_C_KNWC.${ymdh} ]
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
    echo "   File for ${ymdh} : T_OEBA88_C_KNWC.${ymdh}"
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
  echo 'End of waveice_glw.sh at'
  date

# End of waveice_glw.sh ------------------------------------------------ #
