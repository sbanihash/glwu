#!/bin/sh
###############################################################################
#                                                                             #
# This script tars the sectral or bulletin files into a single file and       #
# puts it into /com. This is a separate script to enable it to be run in      #
# parallel using poe. It also tars the spectral and bulletin files of the     #
# old grids that are generated for backward compatibility                     #
#                                                                             #
# Remarks :                                                                   #
# - Shell script variables controling time, directories etc. are set in the   #
#   mother script.                                                            #
# - This script runs in the work directory designated in the mother script.   #
#   Under this directory it geneates a work directory TAR_$type_$ID which is  #
#   removed if this script exits normally.                                    #
# - See section 0.c for variables that need to be set.                        #
#                                                                             #
#                                                            March 13, 2007   #
# Last update : 02-29-2012                                                    #
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
  postmsg   "Making TAR FILE"

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make tar file          |'
  echo '+--------------------------------+'
  echo "   ID              : $1"
  echo "   Type            : $2"
  echo "   Number of files : $3"
  [[ "$LOUD" = YES ]] && set -x


# 0.b Check if type set

  if [ "$#" -lt '3' ]
  then
    set +x
    echo ' '
    echo '********************************************'
    echo '*** VARIABLES IN multiwavetar.sh NOT SET ***'
    echo '********************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg   "TYPE IN multiwavetar.sh NOT SET"
    exit 1
  else
    ID=$1
    type=$2
    nb=$3
  fi

  rm -rf TAR_${type}_$ID 
  mkdir  TAR_${type}_$ID
# this directory is used only for error capturing

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$cycle" ] || [ -z "$COMOUT" ] || [ -z "$modID" ] ||  \
     [ -z "$SENDCOM" ] || [ -z "$SENDDBN" ] || [ -z "${STA_DIR}" ]
  then
    set +x
    echo ' '
    echo '*****************************************************'
    echo '*** EXPORTED VARIABLES IN multiwavetar.sh NOT SET ***'
    echo '*****************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg   "EXPORTED VARIABLES IN multiwavetar.sh NOT SET"
    exit 2
  fi

  cd ${STA_DIR}/${type}

# --------------------------------------------------------------------------- #
# 2.  Generate tar file (spectral files are compressed)

  set +x
  echo ' '
  echo '   Making tar file ...'

  count=0
  countMAX=5
  tardone='no'

  if [ "${type}" = "ripin" ]
  then
    ftag=5m_contour
  else
    ftag=${type}
  fi

  while [ "$count" -lt "$countMAX" ] && [ "$tardone" = 'no' ]
  do
    
    [[ "$LOUD" = YES ]] && set -v
    nf=`ls $ID.*.$ftag | wc -l | awk '{ print $1 }'`
    if [ "$nf" = "$nb" ]
    then 
      tar -cf $ID.$cycle.${type}_tar ./$ID.*.$ftag
      exit=$?
      set +v; [[ "$LOUD" = YES ]] && set -x

      if  [ "$exit" != '0' ]
      then
        set +x
        echo ' '
        echo '***************************************** '
        echo '*** FATAL ERROR : TAR CREATION FAILED *** '
        echo '***************************************** '
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        postmsg   "FATAL ERROR : TAR CREATION FAILED"
        exit 3
      fi
      
      if [ -f "$ID.$cycle.${type}_tar" ]
      then
        tardone='yes'
      fi
    else
      set +x
      echo ' All files not found for tar. Sleeping 10 seconds and trying again ..'
      [[ "$LOUD" = YES ]] && set -x
      sleep 10
      count=`expr $count + 1`
    fi

  done

  if [ "$tardone" = 'no' ]
  then
    set +x
    echo ' '
    echo '***************************************** '
    echo '*** FATAL ERROR : TAR CREATION FAILED *** '
    echo '***************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg   "FATAL ERROR : TAR CREATION FAILED"
    exit 3
  fi

  if [ "$type" = "spec" ] || [ "$type" = "ts" ]
  then
    if [ -s $ID.$cycle.${type}_tar ]
    then
      file_name=$ID.$cycle.${type}_tar.gz
      /usr/bin/gzip -c $ID.$cycle.${type}_tar > ${file_name}
      exit=$?

      if  [ "$exit" != '0' ]
      then
        set +x
        echo ' '
        echo '***************************************************** '
        echo '*** FATAL ERROR : SPECTRAL TAR COMPRESSION FAILED *** '
        echo '***************************************************** '
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        postmsg   "FATAL ERROR : SPECTRAL TAR COMPRESSION FAILED"
        exit 4
      fi
    fi
  else
    file_name=$ID.$cycle.${type}_tar
  fi

# --------------------------------------------------------------------------- #
# 3.  Copy one tree level up for ex-script handling of copy to com and alerts

  cp ${file_name} ${DATA}/.

# --------------------------------------------------------------------------- #
# 4.  Final clean up

  cd $DATA
  rm -rf TAR_${type}_$ID
  set +x; [[ "$LOUD" = YES ]] && set -v
  rm -f  $ID.*.$ftag
  set +v

  echo ' '
  echo 'End of multiwavetar.sh at'
  date

# End of multiwavetar.sh ----------------------------------------------------- #
