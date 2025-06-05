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
    postmsg   " TIME IN waveice_glw.sh NOT SET"
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
    postmsg   " EXPORTED VARIABLES NOT SET."
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

# Initial NIC ice concentration file
   fnice="NIC_LKS_${YEAR}_${MONTHNAME}_${DAY}"
   nicice=${DCOMIN}/nic_lks/NIC_LKS_${YEAR}_${MONTHNAME}_${DAY}.zip

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
        

# Convert nicice file into txt file for inpaint use

        unzip ${nicice}
#  deleting the 5 first lines in text file with header info and empty line    
        cat ${fnice}.txt | sed '/^\s*$/d' | awk '!/Great/ && !/data/ && !/US/ && !/coo/ && !/val/ && !/file/' > testnic.txt
#extracting ice concentration from file, and adding LON-LAT dimensions to header
        awk -F ', ' '{print $3}' testnic.txt > NIC_LKS_${PDYtag}.icec
	cat NIC_LKS_${PDYtag}.icec |sed 's/-1/1000/g' | awk -v m=100 '{print $1/m}' > NIC_LKS_${PDYtag}_tmp.ice
        { echo -e "2554 1823"; cat NIC_LKS_${PDYtag}_tmp.ice; } > T_OEBA88_C_KNWC.ice

## (Conservative approach: no averaging to fill model-data sea gaps)
        cp ${FIXglwu}/T_OEBA88_C_KNWC.mask ./
        $EXECglwu/inpaint_nic_glwu #1> inpaint.out 2>&1

        if [ ! -f T_OEBA88_C_KNWC.newice ]
        then 
          echo ' '
          echo '***************************************'
          echo '*** ERROR RUNNNING inpaint_nic_glwu ***'
          echo '***************************************'
          echo ' '
          postmsg   " INPAINT FAILED."
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
	export YEAR=`echo $PDYCE | cut -c1-4`
	export MONTH=`echo $PDYCE | cut -c5-6`
	export DAY=`echo $PDYCE | cut -c7-8`
	export MONTHNAME=`date -d ${YEAR}-${MONTH}-${DAY} '+%b'`
        fnice="NIC_LKS_${YEAR}_${MONTHNAME}_${DAY}"
        nicice=${DCOMIN}/nic_lks/NIC_LKS_${YEAR}_${MONTHNAME}_${DAY}.zip

      fi

# Write file to whatused
      if [ "${foundOK}" = "yes" ]
      then
        echo "$ymdh NIC_LKS_${YEAR}_${MONTHNAME}_${DAY}" >> ../whatglwice
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

    cp ${FIXglwu}/T_OEBA88_C_KNWC.zeros ./

# Create final ice file for intake in ww3_prep
cat > ../T_OEBA88_C_KNWC.${ymdh} << EOF
${PDYtag} ${CYCtag}0000
EOF

     cat T_OEBA88_C_KNWC.zeros >> ../T_OEBA88_C_KNWC.${ymdh}
     cp ../T_OEBA88_C_KNWC.${ymdh} ../T_OEBA88_C_KNWC.newice
     echo "$ymdh zero ice" >> ../whatglwice
     #echo "$ymdh T_OEBA88_C_KNWC_${PDYCE}120000.gr1" >> ../whatglwice

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
    postmsg   "ERROR IN COPYING SPECTRAL FILE FOR $ymdh."
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
