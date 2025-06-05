#!/bin/sh
###############################################################################
#                                                                             #
# This script is the product generator ("graphics job")  for the multi-grid   #
# GLWU wave model.                                                            #
#                                                                             #
# Remarks :                                                                   #
# - Supplemental error output is witten to the wave.log file.                 #
#                                                                             #
#                                                                             #
# Origination  : 07/01/2008                                                   #
# Last update  : 11/03/2022       adding Champlain header                     # 
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  set -x 

  cd $DATA

  seton='-xa'
  setoff='+xa'
  set $seton
  postmsg   "HAS BEGUN on `hostname`"

  msg="Starting GLWU PRODUCTS SCRIPT"
  postmsg   "$msg"

  if [ ${cyc} -eq 1 ] || [ ${cyc} -eq 7 ] || [ ${cyc} -eq 13 ] || [ ${cyc} -eq 19 ]
  then
    grids='grlc_2p5km_lc grlc_2p5km_lc_sr'
  else
    grids='grlc_2p5km_lc_sr'
  fi
  awipsgrib='yes'
  awipsbull='no'

# 0.b Date and time stuff

  export date=$PDY
  export YMDH=${PDY}${cyc}

  set $setoff
  echo ' '
  echo '                         ****************************'
  echo '                         *** GLWU PRODUCTS SCRIPT ***'
  echo '                         ****************************'
  echo "                                       $date $cycle"
  echo ' '
  echo "Starting at : `date`"
  echo ' '
  echo "   AWIPS grib fields : $awipsgrib"
  echo "   AWIPS bulletins   : $awipsbull"
  echo "   Wave  Grids       : $grids"
  echo ' '
  set $seton

# --------------------------------------------------------------------------- #
# 1.  Get necessary files

  set $setoff
  echo ' '
  echo 'Preparing input files :'
  echo '-----------------------'
  set $seton

# 1.a Grib file (AWIPS and FAX charts)

  for grdID in $grids
  do
    if [ ! -f gribfile.$grdID ]
    then
      echo "   Copying $modID.$grdID.$cycle.grib2 from $COMIN"
      cp $COMIN/$modID.$grdID.$cycle.grib2 gribfile.$grdID
    fi

    if [ -f gribfile.$grdID ]
    then
      echo "   gribfile.$grdID exists."
    else
      msg="ABNORMAL EXIT: NO GRIB FILE FOR GRID $grdID"
      postmsg   "$msg"
      set $setoff
      echo ' '
      echo '**************************** '
      echo '*** ERROR : NO GRIB FILE *** '
      echo '**************************** '
      echo ' '
      echo "$modID $grdID pgen $date $cycle : GRIB file missing." >> $wavelog
      echo $msg
      set $seton
      awipsgrib='no'
      err=1;export err;err_chk
    fi

  done

# 1.b Bulletin files ( AWIPS )

  echo "   Copying bulletins from $COMIN"
  cp $COMIN/${modID}_lc.$cycle.cbull_tar cbull.tar

  if [ ! -f cbull.tar ]
  then
    msg="ABNORMAL EXIT: NO BULLETIN TAR FILE"
    postmsg   "$msg"
    set $setoff
    echo ' '
    echo '************************************ '
    echo '*** ERROR : NO BULLETIN TAR FILE *** '
    echo '************************************ '
    echo ' '
    echo "$modID pgen $date $cycle : bulletin tar missing." >> $wavelog
    echo $msg
    set $seton
    awipsbull='no'
    err=2;export err;err_chk
  fi

  echo "   Untarring bulletins ..."
  tar -xf cbull.tar
  OK=$?

  if [ "$OK" = '0' ]
  then
    echo "      Unpacking successfull ..."
    rm -f cbull.tar
  else
    msg="ABNORMAL EXIT: ERROR IN BULLETIN UNTAR"
    postmsg   "$msg"
    set $setoff
    echo ' '
    echo '****************************************** '
    echo '*** ERROR : ERROR IN BULLETIN TAR FILE *** '
    echo '****************************************** '
    echo ' '
    echo "$modID pgen $date $cycle : bulletin untar error." >> $wavelog
    echo $msg
    set $seton
    awipsbull='no'
    err=3;export err;err_chk
  fi

# 1.c Output locations from bulletin files

  Nb=0
  for fld in `ls -1 *.cbull`
  do
    buoy=`echo $fld | cut -d. -f2`
    if [ "$Nb" -eq '0' ]
    then
      bulls=$buoy
    else
      bulls=`echo $bulls $buoy`
    fi
    Nb=`expr $Nb + 1`
  done


# 1.d Input template files

# Unified extension as now there is only one run type
   ext='glwu'

   if [ "$ext" = 'null' ]
   then
     msg="ABNORMAL EXIT: WIND INFORMATION NOT RECOGNIZED" 
     postmsg   "$msg" 
     set $setoff 
     echo ' ' 
     echo '*********************************************** '
     echo '*** ERROR : WIND INFORMATION NOT RECOGNIZED *** '
     echo '*********************************************** ' 
     echo ' ' 
     echo "$modID pgen $date $cycle : Extension $ext not known " >> $wavelog 
     echo $msg 
     set $seton  
     err=7;export err;err_chk 
   fi

  for grdID in $grids
  do
       
    if [ -f $PARMglwu/grib2_awips_${ext}.$grdID ]
    then
      cp $PARMglwu/grib2_awips_${ext}.$grdID awipsgrb.$grdID
    fi

    if [ -f awipsgrb.$grdID ]
    then
      echo "   awipsgrb.$grdID copied."
    else
      msg="ABNORMAL EXIT: NO AWIPS GRIB HEADER DATA FILE FOR GRID $grdID"
      postmsg   "$msg"
      set $setoff
      echo ' '
      echo '*************************************** '
      echo '*** ERROR : NO AWIPS GRIB DATA FILE *** '
      echo '*************************************** '
      echo ' '
      echo "$modID $grdID pgen $date $cycle : GRIB template file missing." >> $wavelog
      echo $msg
      set $seton
      err=7;export err;err_chk
    fi
 
  done

  if [ "$awipsbull" = 'yes' ]
  then 
    if [ -f $PARMglwu/bull_awp$modID ]
    then
      cp $PARMglwu/bull_awp$modID awipsbull.data
    fi
  
    if [ -f awipsbull.data ]
    then
      echo "   awipsbull.data copied."
    else
      msg="ABNORMAL EXIT: NO AWIPS BULLETIN HEADER DATA FILE"
      postmsg   "$msg"
      set $setoff
      echo ' '
      echo '******************************************* '
      echo '*** ERROR : NO AWIPS BULLETIN DATA FILE *** '
      echo '******************************************* '
      echo ' '
      echo "$modID pgen $date $cycle : Bulletin header data file  missing." >> $wavelog
      echo $msg
      set $seton
      err=7;export err;err_chk
    fi

  fi

# 1.d Data summary

  set $setoff
  echo ' '
  echo '   All data files accounted for.'
  echo ' '
  echo "                 AWIPSGRIB? : $awipsgrib"
  echo "                 AWIPSBULL? : $awipsbull"
  echo "   Number of bulletin files :   $Nb"
  echo '   --------------------------'
  echo ' '
  set $seton

# --------------------------------------------------------------------------- #
# 2.  AWIPS product generation
# 2.a AWIPS GRIB file with headers

  if [ "$awipsgrib" = 'yes' ]
  then

    for grdID in $grids
    do

      set $setoff
      echo ' '
      echo 'AWIPS headers to GRIB file ...'
      echo '------------------------------'
      set $seton

# 2.a.1 Set up for tocgrib2

      set $setoff
      echo "   Do set up for tocgrib2."
      set $seton

      AWIPSGRB=awipsgrib.$grdID

# 2.a.2 Make GRIB index

      set $setoff
      echo "   Make GRIB index for tocgrib2."
      set $seton


      $GRB2INDEX gribfile.$grdID gribindex.$grdID
      OK=$?

      if [ "$OK" != '0' ]
      then
        msg="ABNORMAL EXIT: ERROR IN grb2index MWW3 for grid $grdID"
        postmsg   "$msg"
        set $setoff
        echo ' '
        echo '******************************************** '
        echo '*** FATAL ERROR : ERROR IN grb2index MWW3 *** '
        echo '******************************************** '
        echo ' '
        echo "$modID $grdID pgen $date $cycle : error in grbindex." >> $wavelog
        echo $msg
        set $seton
        err=20;export err;err_chk
      fi

# 2.a.3 Run AWIPS GRIB packing program tocgrib2

      set $setoff
      echo "   Run tocgrib2"
      set $seton

      export pgm=tocgrib2
      export pgmout=tocgrib2.out
      . prep_step

      export FORT11="gribfile.$grdID"
      export FORT31="gribindex.$grdID"
      export FORT51="$AWIPSGRB.$grdID"

      $TOCGRIB2 < awipsgrb.$grdID parm='KWBJ' > tocgrib2.out 2>&1
      OK=$?

      if [ "$OK" != '0' ]
      then
        cat tocgrib2.out
        msg="ABNORMAL EXIT: ERROR IN tocgrib2"
        postmsg   "$msg"
        set $setoff
        echo ' '
        echo '*************************************** '
        echo '*** FATAL ERROR : ERROR IN tocgrib2 *** '
        echo '*************************************** '
        echo ' '
        echo "$modID pgen $date $cycle : error in tocgrib2." >> $wavelog
        echo $msg
        set $seton
        err=21;export err;err_chk
      fi

# 2.a.4 Get the AWIPS grib bulletin out ...
 
      set $setoff
      echo "   Get awips GRIB bulletins out ..."
      set $seton

      if [ "$SENDCOM" = 'YES' ]
      then
        echo "      Saving $AWIPSGRB.$grdID as grib2.$cycle.awipsww3_${grdID}"
        echo "          in COMOUTwmo"
        cp $AWIPSGRB.$grdID $COMOUTwmo/grib2.$cycle.awipsww3_${grdID}
      fi

      if [ "$SENDDBN_NTC" = 'YES' ]
      then
        echo "      Sending $AWIPSGRB.$grdID to DBNET."
        $SIPHONROOT/bin/dbn_alert GRIB_LOW $NET $job $COMOUTwmo/grib2.$cycle.awipsww3_${grdID}
      fi

      rm -f $AWIPSGRB.$grdID tocgrib2.out

    done

  fi

# 2.b AWIPS bulletins for output points

  if [ "$awipsbull" = 'yes' ]
  then
      
    set $setoff
    echo ' '
    echo 'AWIPS bulletins ...'
    echo '-------------------'
    echo '   Sourcing data file with header info ...'
    set $seton

# 2.b.1 Set up environment variables

    . ${DATA}/awipsbull.data

# 2.b.2 Generate list of bulletins to process

    set $setoff
    echo '   Generating buoy list ...'
    set $seton
  
    bulls=`sed -e 's/export b//g' -e 's/=/ /' awipsbull.data | awk '{ print $1}'`
  
# 2.b.3 Looping over buoys running formbul
  
    set $setoff
    echo '   Looping over buoys ...'
    set $seton
  
    for bull in $bulls
    do
      bname="b$bull"
      fname="${modID}_lc.$bull.cbull"
      oname="awipsbull.$bull.$job"
      headr=`grep "b${bull}=" awipsbull.data | sed 's/=/ /g' |  awk '{ print $3}'`
  
      set $setoff
      echo "      Processing $bull ($headr $oname) ..."
      set $seton
  
      if [ -z "$headr" ] || [ ! -s $fname ]
      then
        msg="ABNORMAL EXIT: MISSING BULLETING INFO"
        postmsg   "$msg"
        set $setoff
        echo ' '
        echo '******************************************** '
        echo '*** FATAL ERROR : MISSING BULLETING INFO *** '
        echo '******************************************** '
        echo ' '
        echo "$modID pgen $date $cycle : Missing bulletin data." >> $wavelog
        echo $msg
        set $seton
        err=22;export err;err_chk
      fi
  
# test version
#     $HOMEglwu/scripts/formbul.pl -d $headr -f $fname -j $job -m $modID \
#          -p $COMOUTwmo -s NO -o $oname > formbul.out 2>&1

# operational version
      formbul.pl -d $headr -f $fname -j $job -m $modID \
           -p $COMOUTwmo -s NO -o $oname > formbul.out 2>&1

      OK=$?

      if [ "$OK" != '0' ]
      then
        cat formbul.out
        msg="ABNORMAL EXIT: ERROR IN formbul"
        postmsg   "$msg"
        set $setoff
        echo ' '
        echo '************************************** '
        echo '*** FATAL ERROR : ERROR IN formbul *** '
        echo '************************************** '
        echo ' '
        echo "$modID pgen $date $cycle : error in formbul." >> $wavelog
        echo $msg
        set $seton
        err=23;export err;err_chk
      fi
        
      #XXW cat $oname >> awipsbull.$job
      cat $oname >> glwu_lc.t${cyc}z.awipsbull
      rm -f formbul.out $oname
    done

  if [ $SENDCOM = "YES" ] ; then
    #XXW cp  awipsbull.$job $COMOUTwmo/awipsbull.$job
    cp  glwu_lc.t${cyc}z.awipsbull $COMOUTwmo/.
    if [ $SENDDBN = "YES" ] ; then
       #XXW make_ntc_bull.pl  WMOBH NONE KWBJ NONE $DATA/awipsbull.$job $COMOUTwmo/awipsbull.$job
       make_ntc_bull.pl  WMOBH NONE KWBJ NONE $DATA/glwu_lc.t${cyc}z.awipsbull $COMOUTwmo/glwu_lc.t${cyc}z.awipsbull
    fi
  fi

  fi

# --------------------------------------------------------------------------- #
# 5.  Clean up

  rm -f gribfile gribindex.* awipsgrb.* awipsbull.data
  rm -f $modID.*.cbull

# --------------------------------------------------------------------------- #
# 6.  Ending output

  set $setoff
  echo ' '
  echo ' '
  echo "Ending at : `date`"
  echo ' '
  echo '                *** End of GLWU product generation ***'
  echo ' '

  msg="$job completed normally"
  postmsg   "$msg"

# End of GLWU product generation script -------------------------------------- #
