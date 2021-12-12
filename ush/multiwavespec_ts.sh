#!/bin/sh
###############################################################################
#                                                                             #
# This script generates ASCII data files with mean parameter tables at point  #
# output of WAVEWATCH (MWW3) implementation.                                  #
# The location ID and position is passed as a shel script parameter.          #
#                                                                             #
# Remarks :                                                                   #
# - The necessary files are retrieved by the mother script.                   #
# - Shell script variables controling time, directories etc. are set in the   #
#   mother script.                                                            #
# - This script runs in the work directory designated in the mother script.   #
#   Under this directory it geneates a work directory ts_$loc which is        #
#   removed if this script exits normally.                                    #
# - See section 0.c for variables that need to be set.                        #
#                                                                             #
# - For use with GLW and GLWN system, modID has been replaced with runID      #
#                                                                             #
# Script parameters are:                                                      #
#  grdID=$1      - Grid identity                                              #
#  ymdh=$2       - Date and time of data being processed                      #
#                                                                             #
#                                                            July  12, 2016   #
# Last update : 07-18-2016                                                    #
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

  rm -rf ts_$1
  mkdir ts_$1
  err=$?
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

  cd ts_$1

  ymdh=$2
  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!       Make mean param file     |'
  echo '+--------------------------------+'
  echo "   Model ID        : $runID"
  [[ "$LOUD" = YES ]] && set -x

# 0.b Check if buoy location set

  if [ "$#" -lt '1' ]
  then
    set +x
    echo ' '
    echo '***********************************************'
    echo '*** LOCATION ID IN multiwavespec_ts.sh NOT SET ***'
    echo '***********************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "LOCATION ID IN multiwavespec_ts.sh NOT SET"
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
      echo '*** LOCATION ID IN multiwavespec_ts.sh NOT RECOGNIZED ***'
      echo '******************************************************'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      postmsg "$jlogfile" "LOCATION ID IN multiwavespec_ts.sh NOT RECOGNIZED"
      exit 2
    fi
  fi


# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$YMDH" ] || [ -z "$dtts" ] || [ -z "$EXECglwu" ] || \
     [ -z "$runID" ] || [ -z "${STA_DIR}" ]
  then
    set +x
    echo ' '
    echo '******************************************************'
    echo '*** EXPORTED VARIABLES IN multiwavespec_ts.sh NOT SET ***'
    echo '******************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "EXPORTED VARIABLES IN multiwavespec_ts.sh NOT SET"
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
  $UTILROOT/exec/fsync_file ${DATA}/multiwavespec_ts.inp.tmpl

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
      -e "s/DT/$dtts/g" \
      -e "s/POINT/$point/g" \
      -e "s/ITYPE/2/g" \
      -e "s/FORMAT/F/g" \
                               ../multiwavespec_ts.inp.tmpl > multiwavespec.inp

# 2.b Run the postprocessor

  set +x
  echo "   Executing $EXECglwu/multiwavespec"
  [[ "$LOUD" = YES ]] && set -x

  $EXECglwu/multiwavespec
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
    postmsg "$jlogfile" "FATAL ERROR : ERROR IN multiwavespec"
    exit 4
  fi

# --------------------------------------------------------------------------- #
# 3.  Clean up
# 3.a Move data to directory for station ascii files

  outfile=tab51.ww3

# Create input file for Rip Current program (emulated SWAN output file)
    bfile=../buoy.loc
    file5m=${runID}.${PDY}_${cyc}00_${buoy}.5m_contour
    echo '%' > ${file5m}
    echo '%' >> ${file5m}
    echo "% Run:$PDY$cyc  Table:5mcont            WW3 version:4.18.0" >> ${file5m}
    echo '%' >> ${file5m}
    echo '%   Time     Xp    Yp    Hsig    TPsmoo  Dir     Dspr  X-Vel   Y-Vel   Watlev  X-Windv  Y-Windv' >> ${file5m}
    echo '%   [ ]    [degr] [degr] [m]     [sec]   [degr]  [deg] [m/s]   [m/s]   [m]     [m/s]    [m/s]' >> ${file5m}
    echo '%' >> ${file5m}

# Get coordinates for each point 
      pnt_coord=(`grep $buoy $bfile | awk '{printf "%.4f %.4f", $1,$2}' | sed 's/'\''//g'`)
# Extract data from WW3 mean parameter table
      sed '1,3d' ${outfile} | sed 's/^\(.\{11\}\) \(.\{1\}\).\{0\}\(.*\)/\10\2\3/' > tabdum
#      awk '{print $1"."$2$3$4,'${pnt_coord[0]}','${pnt_coord[1]}',$5,$7,$8,$9,"0.00 0.00 0.00 0.00 0.00"}' tabdum >> ${file5m}
      awk '{printf "%8i.%02i%02i%02i %.4f %.4f %.4f %.4f %.4f %.4f 0.00 0.00 0.00 0.00 0.00\n", $1,$2,$3,$4,'${pnt_coord[0]}','${pnt_coord[1]}',$5,$7,$8,$9}' tabdum >> ${file5m}
  if [ -f $outfile ]
  then
    mv $outfile ${STA_DIR}/ts/$runID.$buoy.ts
    mv $file5m ${STA_DIR}/ripin
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
  rm -rf ts_$buoy

  set +x
  echo ' '
  echo 'End of multiwavespec_ts.sh at'
  date

# End of multiwavespec_ts.sh ---------------------------------------------------- #
