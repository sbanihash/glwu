#!/bin/bash
###############################################################################
#                                                                             #
# This script is the postprocessor for the multi scale GLW wave model. It    #
# sets some shell script variables for export to child scripts and copies     #
# some generally used files to the work directory. After this the actual      #
# postprocessing is performed by the following child scripts :                #
#                                                                             #
#  multiwavegrib2.sh        : generates GRIB2 files.                          #
#  multiwavespec.sh         : generates spectral data files for output        #
#                             locations.                                      #
#  multiwavespec_ts.sh      : generates mean parameter tables for output      #
#                             locations.                                      #
#  multiwavebull.sh         : generates bulletins for output locations.       #
#  multiwavetar.sh          : tars the spectral and bulletin multiple files   #
#                                                                             #
# Remarks :                                                                   #
# - The above scripts are (mostly) run under poe in parallel.                 #
#   Each script runs in its own directory created in DATA. If all is well     #
#   this directory disappears. If this directory is still there after poe     #
#   has finished, an error has occured Only then the output of the process    #
#   is copied to the output file. Otherwise, the output is deleted.           #
# - For non-fatal errors output is witten to the wave.log file.               #
#                                                                             #
# Origination                                                   Mar 2007      #
# Last CCS update                                               Mar 2011      #
# Transition to WCOSS                                          Dec,  2012     # 
# Transition to Cray                                           Jul,  2016     # 
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  set -x
  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  cd $DATA

  postmsg "$jlogfile" "HAS BEGUN on `hostname`"

  msg="Starting GLW POSTPROCESSOR SCRIPT for $runID"
  postmsg "$jlogfile" "$msg"

  set +x
  echo ' '
  echo '                     *********************************'
  echo '                     *** GLW POSTPROCESSOR SCRIPT ***'
  echo '                     *********************************'
  echo ' '
  echo "Starting at : `date`"
  echo '-------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# 0.b Date and time stuff

  export date=$PDY
  export YMDH=${PDY}${cyc}

# 0.c Defining model grids

  export pntgrd="glwu"
  export grids='glwu'
  export isgrids='grlr_500m' # Compute 500m as source grid for other interpolations
  export itgrids='grlc_2p5km grlr' # Target grids for distributions 
  export ggrids='grlc_2p5km grlc_2p5km_sr' # grids that require grib output
  export addggrids='grlr_500m' # additional grids that require grib output
  export ngrids='glwu' # Grids with netcdf output

# 0.c.1 Define a temporary directory for storing ascii point output files
#       and flush it

  export STA_DIR=$DATA/station_ascii_files
  if [ -d $STA_DIR ]
  then 
    rm -rf ${STA_DIR}
  fi
  mkdir -p ${STA_DIR}
  mkdir -p ${STA_DIR}/spec
  mkdir -p ${STA_DIR}/bull
  mkdir -p ${STA_DIR}/cbull
  mkdir -p ${STA_DIR}/csbull
  mkdir -p ${STA_DIR}/ts
  mkdir -p ${STA_DIR}/ripin

  set +x
  echo ' '
  echo 'Grid information  :'
  echo '-------------------'
  echo "   wave grids    : $grids"
  echo "   output points : ${modID}_$pntgrd"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x


# 0.d Wind source  
# --------------------------------------------------------------------------- #
 if [ "${runID}" = "glwn" ]
  then
    set +x
    wndID=`head -n 1 $COMIN/what_glwn_used.t${cyc}z`
    echo "   wind source   : $wndID"
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

  field_OK='yes'
  point_OK='yes'
   grib_OK='yes'
   ncdf_OK='yes'
   spec_OK='yes'
   bull_OK='yes'
     ts_OK='yes'
  ripin_OK='yes'
export grint_OK='yes'
#
  exit_code=0

  set +x
  echo ' '
  echo 'Preparing input files :'
  echo '-----------------------'
  [[ "$LOUD" = YES ]] && set -x

# 1.a Copy output files

# 1.a.1 Set up the ${mpicmd} command 

#  ifile=1
  nfile=`echo ${LSB_HOSTS} | wc -w | awk '{ print $1}'`

  if [ "$nfile" -gt '1' ]
  then
    cmdtype="mpirun.lsf cfp"
  else
    nfile=1
    cmdtype='sh'
#    nskip='-'
#    nper='-'
  fi

  set +x
  echo ' '
  echo "   Setting up first command file for copying data files."
  echo "   Set up command file structure (type = $cmdtype)."
  echo "      Number of command files                         : $nfile"
  [[ "$LOUD" = YES ]] && set -x

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

  for grdID in $grids 
  do
  
    if [ ! -f out_grd.$grdID ]
    then
      set +x
      echo "   Copying $runID.$grdID.$cycle.outgrd from $COMIN to out_grd.$grdID"
      [[ "$LOUD" = YES ]] && set -x

      echo  "cp $COMIN/$runID.$grdID.$cycle.outgrd out_grd.$grdID"  >> cmdfile

    fi 

  done

# 1.a.2 Execute the ${mpicmd} command

  set +x
  echo ' '
  echo "   Executing the copy command file at : `date`"
  echo '   ------------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$nfile" -gt '1' ]
  then
    mpiexec -np 120 --cpu-bind verbose,core cfp cmdfile
    exit=$?
  else
    nfile=1
    ./cmdfile
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '********************************************'
    echo '*** POE FAILURE DURING RAW DATA COPYING ***'
    echo '********************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

# 1.a.3 Error checks

  for grdID in $grids
  do
    if [ ! -f out_grd.$grdID ]
    then
      set +x
      echo ' '
      echo '**************************************** '
      echo '*** ERROR : NO RAW FIELD OUTPUT FILE *** '
      echo '**************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $grdID $date $cycle : field output missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO RAW FIELD OUTPUT FILE"
      exit_code=1
      field_OK='no'
      grib_OK='no'
    else
      set +x
      echo "File out_grd.$grdID found. Syncing to all nodes ..."
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC out_grd.$grdID
    fi
  done

# 1.b Model definition files

  for grdID in $grids $itgrids $isgrids $ggrids $ngrids $addggrids
  do
    if [ -f "$FIXglwu/wave_${modID}_${grdID}.moddef.${glwu_ver}" ]
    then
      set +x
      echo " Mod def file for $grdID found in $FIXglwu. copying ...."
      [[ "$LOUD" = YES ]] && set -x

      cp $FIXglwu/wave_${modID}_${grdID}.moddef.${glwu_ver} mod_def.$grdID

    else
      set +x
      echo " Mod def file for $grdID not found in $FIXglwu. Setting up to generate ..."
      [[ "$LOUD" = YES ]] && set -x

      if [ -f $FIXglwu/wave_$grdID.inp ]
      then
        cp $FIXglwu/wave_$grdID.inp $grdID.inp
      fi

      if [ -f $grdID.inp ]
      then
        set +x
        echo "   $grdID.inp copied ($FIXglwu/wave_$grdID.inp)."
        [[ "$LOUD" = YES ]] && set -x
      else
        msg="ABNORMAL EXIT: NO INP FILE FOR MODEL DEFINITION FILE"
        postmsg "$jlogfile" "$msg"
        set +x
        echo ' '
        echo '*********************************************************** '
        echo '*** FATAL ERROR : NO INP FILE FOR MODEL DEFINITION FILE *** '
        echo '*********************************************************** '
        echo "                                grdID = $grdID"
        echo ' '
        echo $msg
        [[ "$LOUD" = YES ]] && set -x
        echo "$runID post $date $cycle : $grdID.inp missing." >> $wavelog
        exit_code=2
      fi

      set +x
      echo "   Generating mod_def file for $grdID ... "
      [[ "$LOUD" = YES ]] && set -x

      $USHglwu/wavemod_def.sh $grdID > $grdID.out

    fi

    if [ -f mod_def.$grdID ]
    then
      set +x
      echo " mod_def.$grdID created. Syncing to all nodes ... "
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC mod_def.$grdID
    else
      msg="ABNORMAL EXIT: NO MODEL DEFINITION FILE"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '********************************************** '
      echo '*** FATAL ERROR : NO MODEL DEFINITION FILE *** '
      echo '********************************************** '
      echo "                                grdID = $grdID"
      echo ' '
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : mod_def.$grdID missing." >> $wavelog
      sed "s/^/$grdID.out : /g"  $grdID.out
      exit_code=3
      grib_OK='no'
      mv -f $grdID.out $grdID.err
    fi
  done

# 1.c Raw point data file and moddef file

  if [ -f "$FIXglwu/wave_${modID}_${pntgrd}.moddef.${glwu_ver}" ]
  then
    set +x
    echo " Mod def file for $pntgrd found in $FIXglwu. copying ...."
    [[ "$LOUD" = YES ]] && set -x
    cp $FIXglwu/wave_${modID}_${pntgrd}.moddef.${glwu_ver} mod_def.$pntgrd
  else
    set +x
    echo " Mod def file for $pntgrd not found in $FIXglwu. Setting up to generate ..."
    [[ "$LOUD" = YES ]] && set -x
    if [  -f $FIXglwu/wave_${pntgrd}.inp ]
    then 
       cp $FIXglwu/wave_$pntgrd.inp $pntgrd.inp
    fi 

    if [  -f $pntgrd.inp ]
    then
      set +x
      echo "   $pntgrd.inp copied ($FIXglwu/wave_$pntgrd.inp)."
      [[ "$LOUD" = YES ]] && set -x
    else
      msg="ABNORMAL EXIT: NO INP FILE FOR MODEL DEFINITION FILE"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '*********************************************************** '
      echo '*** FATAL ERROR : NO INP FILE FOR MODEL DEFINITION FILE *** '
      echo '*********************************************************** '
      echo "                                grdID = $pntgrd"
      echo ' '
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : $pntgrd.inp missing." >> $wavelog
      exit_code=10
    fi

    set +x
    echo "   Generating mod_def file for $pntgrd ... "
    [[ "$LOUD" = YES ]] && set -x

    $USHglwu/wavemod_def.sh $pntgrd > $pntgrd.out

  fi

  if [ -f mod_def.$pntgrd ]
  then
    set +x
    echo " mod_def.$pntgrd created. Syncing to all nodes ... "
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC mod_def.$pntgrd
  else
    msg="ABNORMAL EXIT: NO MODEL DEFINITION FILE"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '********************************************** '
    echo '*** FATAL ERROR : NO MODEL DEFINITION FILE *** '
    echo '********************************************** '
    echo "                                grdID = $pntgrd"
    echo ' '
    echo $msg
    [[ "$LOUD" = YES ]] && set -x
    echo "$runID post $date $cycle : mod_def.$pntgrd missing." >> $wavelog
    sed "s/^/$pntgrd.out : /g"  $pntgrd.out
    mv -f $pntgrd.out $pntgrd.err
    exit_code=11
    point_OK='no'
    spec_OK='no'
    bull_OK='no'
    Ospec_OK='no'
    Obull_ok='no'
  fi

  if [ ! -f out_pnt.ww3 ]
  then
    set +x
    echo "   Copying out_pnt.ww3 from $COMIN"
    [[ "$LOUD" = YES ]] && set -x
    cp $COMIN/$runID.${pntgrd}.$cycle.outpnt out_pnt.ww3
  fi

  if [ -f out_pnt.ww3 ]
  then
    set +x
    echo "   out_pnt.ww3 exists. Syncing to all nodes ..."
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC out_pnt.ww3
  else
    set +x
    echo ' '
    echo '**************************************** '
    echo '*** ERROR : NO RAW POINT OUTPUT FILE *** '
    echo '**************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$runID post $date $cycle : point output missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR NO RAW POINT OUTPUT FILE"
    exit_code=12
    point_OK='no'
    spec_OK='no'
    bull_OK='no'
  fi

# 1.d Output locations file

  rm -f buoy.loc

  if [ -f $FIXglwu/wave_$modID.buoys ]
  then
    cp $FIXglwu/wave_$modID.buoys buoy.loc.temp
    sed -n '/^\$.*/!p' buoy.loc.temp > buoy.loc
    rm -f buoy.loc.temp
  fi

  if [ -f buoy.loc ]
  then
    set +x
    echo "   buoy.loc copied and processed ($FIXglwu/wave_$modID.buoys)."
    [[ "$LOUD" = YES ]] && set -x
  else
    set +x
    echo ' '
    echo '************************************* '
    echo '*** ERROR : NO BUOY LOCATION FILE *** '
    echo '************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$runID post $date $cycle : buoy location file missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR : NO BUOY LOCATION FILE"
    exit_code=13
    point_OK='no'
    spec_OK='no'
    bull_OK='no'
  fi

# 1.e Input template files

  if [ "$grib_OK" = 'yes' ]
  then
    if [ -f $FIXglwu/multiwavegrib2.inp.tmpl ]
    then
      cp $FIXglwu/multiwavegrib2.inp.tmpl multiwavegrib2.inp.tmpl
    fi

    if [ -f multiwavegrib2.inp.tmpl ]
    then
      set +x
      echo "   multiwavegrib2.inp.tmpl copied. Syncing to all nodes ..."
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC multiwavegrib2.inp.tmpl
    else
      set +x
      echo ' '
      echo '*********************************************** '
      echo '*** ERROR : NO TEMPLATE FOR GRIB INPUT FILE *** '
      echo '*********************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : GRIB2 template file missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR GRIB2 INPUT FILE"
      exit_code=16
      grib_OK='no'
    fi
  fi

  if [ "$ncdf_OK" = 'yes' ]
  then
    if [ -f $FIXglwu/multiwavefldn.inp.tmpl ]
    then
      cp $FIXglwu/multiwavefldn.inp.tmpl multiwavefldn.inp.tmpl
    fi

    if [ -f multiwavefldn.inp.tmpl ]
    then
      set +x
      echo "   multiwavefldn.inp.tmpl copied. Syncing to all nodes ..."
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC multiwavefldn.inp.tmpl
    else
      set +x
      echo ' '
      echo '*********************************************** '
      echo '*** ERROR : NO TEMPLATE FOR GRIB INPUT FILE *** '
      echo '*********************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : GRIB2 template file missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR GRIB2 INPUT FILE"
      exit_code=16
      grib_OK='no'
    fi
  fi

  if [ -f $FIXglwu/multiwavespec.inp.tmpl ]
  then
    cp $FIXglwu/multiwavespec.inp.tmpl multiwavespec.inp.tmpl
  fi

  if [ -f multiwavespec.inp.tmpl ]
  then
    set +x
    echo "   multiwavespec.inp.tmpl copied. Syncing to all grids ..."
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC multiwavespec.inp.tmpl
  else
    set +x
    echo ' '
    echo '*********************************************** '
    echo '*** ERROR : NO TEMPLATE FOR SPEC INPUT FILE *** '
    echo '*********************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$runID post $date $cycle : specra template file missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR SPEC INPUT FILE"
    exit_code=18
    spec_OK='no'
    bull_OK='no'
    Ospec_OK='no'
    Obull_OK='no'
  fi

  if [ -f $FIXglwu/multiwavespec_ts.inp.tmpl ]
  then
    cp $FIXglwu/multiwavespec_ts.inp.tmpl multiwavespec_ts.inp.tmpl
  fi

 if [ -f multiwavespec_ts.inp.tmpl ]
  then
    set +x
    echo "   multiwavespec_ts.inp.tmpl copied. Syncing to all grids ..."
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC multiwavespec_ts.inp.tmpl
  else
    set +x
    echo ' '
    echo '*********************************************** '
    echo '*** ERROR : NO TEMPLATE FOR TIME SERIES INPUT FILE *** '
    echo '*********************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$runID post $date $cycle : time series template file missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR TIME SERIES INPUT FILE"
    exit_code=18
    ts_OK='no'
    ripin_OK='no'
  fi

  if [ -f $FIXglwu/multiwavespec_bull.inp.tmpl ]
  then
    cp $FIXglwu/multiwavespec_bull.inp.tmpl multiwavespec_bull.inp.tmpl
  fi

  if [ -f multiwavespec_bull.inp.tmpl ]
  then
    set +x
    echo "   multiwavespec_bull.inp.tmpl copied. Syncing to all nodes ..."
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC multiwavespec_bull.inp.tmpl
  else
    set +x
    echo ' '
    echo '*************************************************** '
    echo '*** ERROR : NO TEMPLATE FOR BULLETIN INPUT FILE *** '
    echo '*************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$runID post $date $cycle : bulletin template file missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR BULLETIN INPUT FILE"
    exit_code=19
    bull_OK='no'
    Obull_OK='no'
  fi

# 1.f Getting buoy information for points

  if [ "$spec_OK" = 'yes' ] || [ "$bull_OK" = 'yes' ]
  then
    ymdh=`$NDATE -9 $YMDH`                                     # CHECK THIS NDATE: -9 applies for Multi_1, not GLW
    tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
    dtspec=3600.            # default time step (not used here)
    sed -e "s/TIME/$tstart/g" \
        -e "s/DT/$dtspec/g" \
        -e "s/POINT/1/g" \
        -e "s/ITYPE/0/g" \
        -e "s/FORMAT/F/g" \
                               multiwavespec.inp.tmpl > ww3_outp.inp
   
    ln -s mod_def.$pntgrd mod_def.ww3
    $EXECglwu/multiwavespec > buoy_tmp.loc 
    err=$?

    if [ "$err" != '0' ]
    then
      pgm=wave_post
      msg="ABNORMAL EXIT: ERROR IN multiwavespec"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '******************************************** '
      echo '*** FATAL ERROR : ERROR IN multiwavespec *** '
      echo '******************************************** '
      echo ' '
      echo "$runID post $date $cycle : buoy log file failed to be created." >> $wavelog
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      exit_code=19
      spec_OK='no'
      bull_OK='no'
      Ospec_OK='no'
      Obull_OK='no'
    fi

    sed -n '11,/^$/p' buoy_tmp.loc > buoy_tmp2.loc
    sed    '$d' buoy_tmp2.loc > buoy_tmp3.loc

    buoys=`awk '{ print $1 }' buoy_tmp3.loc`
    Nb=`wc buoy_tmp3.loc | awk '{ print $1 }'`
    rm buoy_tmp.loc buoy_tmp2.loc buoy_tmp3.loc

    if [ -f buoy_log.ww3 ]
    then
      set +x
      echo 'Buoy log file created. Syncing to all nodes ...'
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC buoy_log.ww3
    else
      set +x
      echo ' '
      echo '**************************************** '
      echo '*** ERROR : NO BUOY LOG FILE CREATED *** '
      echo '**************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : buoy log file missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO BUOY LOG FILE GENERATED FOR SPEC AND BULLETIN FILES"
      exit_code=19
      spec_OK='no'
      bull_OK='no'
    fi

  fi

# 1.g Interpolated grids: generate data files

# Set up cfp process
  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

  if [ "$grint_OK" = 'yes' ]
  then
    for igrdID in $isgrids
    do
      ymdh_int=`$NDATE -9 $YMDH`
      dt_int=3600.
      n_int=9999
      $USHglwu/multiwavegrid_interp.sh $igrdID $ymdh_int $dt_int $n_int > grint_$igrdID.out
    done

    for igrdID in $itgrids
    do
      ymdh_int=`$NDATE -9 $YMDH`
      dt_int=3600.
      n_int=9999
      echo "$USHglwu/multiwavegrid_interp.sh $igrdID $ymdh_int $dt_int $n_int > grint_$igrdID.out 2>&1" >> cmdfile
    done

  fi

# 1.g.2 Execute the ${mpicmd} command

  set +x
  echo ' '
  echo "   Executing interpolation of grids at : `date`"
  echo '   ------------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$nfile" -gt '1' ]
  then
    # ${mpicmd} cmdfile
    mpiexec -np 120 --cpu-bind verbose,core cfp cmdfile
    exit=$?
  else
    nfile=1
#    ./cmdfile.1
    ./cmdfile
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '********************************************'
    echo '*** POE FAILURE DURING INTERPOLATED GRID DATA MAKING ***'
    echo '********************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

# 1.g.b Grid interpolation error check

  if [ "$grint_OK" = 'yes' ]
  then
    for igrdID in $igrids
    do
      if [ -d grint_${igrdID}_${ymdh_int} ]
      then
        set +x
        echo "      Error in grid interpolation for $igrdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "FATAL ERROR in GRID interpolation for $igrdID."
        err=1 ; export err ; err_chk
        mv -f grint_$igrdID.out grint_$igrdID.err
      else
        set +x
        echo "      GRID interpolation successful for $igrdID."
        [[ "$LOUD" = YES ]] && set -x
        touch $COMOUT/$igrdID.t${cyc}z.grintdone
      fi

    done
  fi

# 1.h Data summary

  set +x
  echo ' '
  echo "   Input files read and processed at : `date`"
  echo ' ' 
  echo '   Data summary : '
  echo '   ---------------------------------------------'
  echo "      Sufficient data for GRIB files            : $grib_OK"
  echo "      Sufficient data for spectral files        : $spec_OK ($Nb points)"
  echo "      Sufficient data for bulletins             : $bull_OK ($Nb points)"
  echo "      Sufficient data for interpolated grids    : $grint_OK"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x


# --------------------------------------------------------------------------- #
# 2.  Make second command file(s) (GRID interpolation and GRIB generation)
# 2.a Command file set-up
#     The command file points to $nfile files named cmdfile.$ifile.
#     The actual work is distributed over these files. The skip parameter
#     is used for load balancing. GRIB packing takes more time than making
#     spectral data files or bulletins.

  set +x
  echo '   Making second command file (GRID Interpolation, GRIB, SPEC and BULLETINS) '
  echo "   Set up command file structure (type = $cmdtype)."
  echo "      Number of command files                : $nfile"
  [[ "$LOUD" = YES ]] && set -x

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile
  set +x; [[ "$LOUD" = YES ]] && set -x

# 2.b GRIB files

# GRIB field time step -- dtgrib
# Number of GRIB fields -- ngrib
# Assigned NCEP number for grid -- GRIDNR
# Assigned NCEP number for model -- MODNR

  if [ "$grib_OK" = 'yes' ]
  then
    for grdID in $ggrids
    do
      if [ ${cyc} -eq 1 ] || [ ${cyc} -eq 7 ] || [ ${cyc} -eq 13 ] || [ ${cyc} -eq 19 ]
      then
        case $grdID in
          grl) GRIDNR=176  ; MODNR=131  ; dtgrib=3600. ; ngrib=150 ; GTMPLN=0 ;;
          grlr) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=150 ; GTMPLN=0 ;;
          grlr_500m) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=150 ; GTMPLN=0 ;;
          grlc_2p5km) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=150 ; GTMPLN=30 ;;
          grlc_2p5km_sr) GRIDNR=255  ; MODNR=133  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=30 ;;
          grlc_1p0km) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=30 ;;
        esac
      else
        case $grdID in
          grl) GRIDNR=176  ; MODNR=131  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=0 ;;
          grlr) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=0 ;;
          grlr_500m) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=0 ;;
          grlc_2p5km) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=30 ;;
          grlc_2p5km_sr) GRIDNR=255  ; MODNR=133  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=30 ;;
          grlc_1p0km) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=30 ;;
        esac
      fi
      echo "RUNID: $runID "
      echo THIS IS IT '$grdID $dtgrib $ngrib $GRIDNR $MODNR $GTMPLN'
      echo $grdID $dtgrib $ngrib $GRIDNR $MODNR $GTMPLN
# Restrict running grib2 for long-range files only for appropriate cycles
      grib2_GO='yes'
      if [ "$grdID" = "grlc_2p5km" ]
      then
        grib2_GO='no'
        if [ "${cyc}" = "01" ] || [ "${cyc}" = "07" ]  || [ "${cyc}" = "13" ] || [ "${cyc}" = "19" ]
        then
          grib2_GO='yes'
        fi
      fi
      if [ "$grib2_GO" = "yes" ]
      then 
        echo "$USHglwu/multiwavegrib2.sh $grdID $dtgrib $ngrib $GRIDNR $MODNR $GTMPLN > grib_$grdID.out 2>&1"               >> cmdfile
      fi
    done
  fi

# 2.c.2 Create netcdf gridded file

  if [ "$ncdf_OK" = 'yes' ]
  then
    dtnc=1800.
    ncdfFL=\''WND HS FP DP PHS PTP PDIR CHA'\'
    for grdID in $ngrids
    do
      echo "$USHglwu/wave_fldn.sh $grdID $dtnc $ncdfFL > ncdf_$grdID.out 2>&1"               >> cmdfile
    done
  fi

# 2.d Spectral data files and time series

  if [ "$spec_OK" = 'yes' ]
  then
    export dtspec=3600.   # time step for spectra
    ymdh=`$NDATE -9 $YMDH` # start time for spectra output

    for buoy in $buoys
    do
      echo "$USHglwu/multiwavespec2.sh $buoy $ymdh > spec_$buoy.out 2>&1" >> cmdfile
    done
  fi

  if [ "$ts_OK" = 'yes' ]
  then
    export dtts=3600.   # time step for Time Series
    ymdh=`$NDATE -9 $YMDH` # start time for TS output

    for buoy in $buoys
    do
      echo "$USHglwu/multiwavespec_ts.sh $buoy $ymdh > ts_$buoy.out 2>&1" >> cmdfile
    done
  fi

#retro run ounp


 for grdID in $grids
 do	 
  if [ "$RetroRun" = "YES" ]
  then
    export dtspec=3600.   # time step for spectra
    ymdh=`$NDATE -9 $YMDH` # start time for spectra output
    echo "$USHglwu/multiwavespnc.sh $grdID $ymdh > spnc.out 2>&1" >> cmdfile
  fi
 done




# 2.e Bulletins

#  set +x; [ "$LOUD" = YES -a "$bull_OK" = 'yes' ] && set -v
  if [ "$bull_OK" = 'yes' ]
  then
    export dtbull=3600.    # time step for bulletins
    ymdh=`$NDATE -9 $YMDH` # start time for bulletin output
   
    for buoy in $buoys
    do
      echo "$USHglwu/multiwavespec_bull.sh $buoy $ymdh > bull_$buoy.out 2>&1" >> cmdfile
    done
  fi
#  set +v; [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 3   Execute second command file
# 3.a Execution

  set +x
  echo "   Executing second command file at : `date`"
  echo '   ----------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$nfile" -gt '1' ]
  then
    # ${mpicmd} cmdfile
    mpiexec -np 120 --cpu-bind verbose,core cfp cmdfile
    exit=$?
  else
#    ./cmdfile.1
    ./cmdfile
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '***********************************************************'
    echo '*** POE FAILURE DURING GRIB AND POINT OUTPUT GENERATION ***'
    echo '***********************************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

# --------------------------------------------------------------------------- #
# 3.  Check for errors

  set +x
  echo ' '
  echo '   Checking for errors (error output concatenated below).'
  [[ "$LOUD" = YES ]] && set -x

# 3.a GRIB file

  if [ "$grib_OK" = 'yes' ]
  then
    for grdID in $ggrids
    do
      if [ -d grib_$grdID ]
      then
        set +x
        echo "      Error in GRIB encoding for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRIB encoding for $grdID."
        mv -f grib_$grdID.out grib_$grdID.err
      else
        set +x
        echo "      GRIB encoding successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
      fi
    done
  fi

# 3.b netcdf file
  if [ "$ncdf_OK" = 'yes' ]
  then
    for grdID in $ngrids
    do
      if [ -d nc_$grdID ]
      then
        set +x
        echo "      Error in NCDF encoding for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in NCDF encoding for $grdID."
        mv -f ncdf_$grdID.out ncdf_$grdID.err
      else
        set +x
        echo "      NCDF encoding successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
      fi
    done
  fi


# 4.b Grid interpolation section

  if [ "$grint_OK" = 'yes' ]
  then
    for igrdID in $igrids
    do

      if [ -d grib_$igrdID ]
      then
        set +x
        echo "      Error in GRIB encoding for $igrdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRIB encoding for $igrdID."
        mv -f grib_$igrdID.out grib_$igrdID.err
      else
        set +x
        echo "      GRIB encoding successful for $igrdID."
        [[ "$LOUD" = YES ]] && set -x
        touch $COMOUT/$igrdID.t${cyc}z.gribdone
      fi

    done
  fi

# 4.c Spectral data files and bulletins

  bullstring='Bulletins not generated'
  specstring='Spectra not generated'
  tsstring='Time series not generated'

#  set +x; [[ "$LOUD" = YES ]] && set -v
  for buoy in $buoys
  do

    if [ "$spec_OK" = 'yes' ]
    then
      if [ -d spec_$buoy ]
      then
        specstring='Error in spectra.'
        postmsg "$jlogfile" "NON-FATAL ERROR in spectra."
        mv -f spec_$buoy.out spec_$buoy.err
      else
        specstring='Spectra OK.'
      fi
    fi

    if [ "$ts_OK" = 'yes' ]
    then
      if [ -d ts_$buoy ]
      then
        tsstring='Error in time series.'
        postmsg "$jlogfile" "NON-FATAL ERROR in Time Series."
        mv -f ts_$buoy.out ts_$buoy.err
      else
        tsstring='Time Series OK.'
      fi
    fi

    if [ "$bull_OK" = 'yes' ]
    then
      if [ -d bull_$buoy ]
      then
        bullstring='Error in bulletins.'
        postmsg "$jlogfile" "NON-FATAL ERROR in bulletins."
        mv -f bull_$buoy.out bull_$buoy.err
      else
        bullstring='Bulletins OK.'
      fi
    fi

    set +x
    echo "      $buoy : $specstring  $tsstring $bullstring"
    [[ "$LOUD" = YES ]] && set -x

  done
#  set +v; [[ "$LOUD" = YES ]] && set -x

  if ls *.err  1> /dev/null 2>&1
  then
    for grdID in $grids
    do 
      if [ -f grib_$grdID.err ]
      then
        set +x
        echo ' '
        echo '**************************************'
        echo '*** ERROR OUTPUT multiwavegrib2.sh ***'
        echo '**************************************'
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        echo "$runID post $date $cycle : error in GRIB." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in multiwavegrib2.sh"
        exit_code=22
        sed "s/^/grib_$grdID.err : /g"  grib_$grdID.err
      fi

    done

    if ls spec_*.err  1> /dev/null 2>&1
    then
      set +x
      echo ' '
      echo '*************************************'
      echo '*** ERROR OUTPUT multiwavespec.sh ***'
      echo '*************************************'
      echo '            Possibly in multiple calls'
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : error in spectra." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in multiwavespec.sh, possibly in multiple calls."
      exit_code=24
      for file in spec_*.err
      do
        echo ' '
        sed "s/^/$file : /g" $file
      done
    fi

#  set +x; [[ "$LOUD" = YES ]] && set -v
    if ls ts_*.err  1> /dev/null 2>&1
    then
      set +x
      echo ' '
      echo '*************************************'
      echo '*** ERROR OUTPUT multiwavespec_ts.sh ***'
      echo '*************************************'
      echo '            Possibly in multiple calls'
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : error in time series." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in multiwavespec_ts.sh, possibly in multiple calls."
      exit_code=24
      for file in ts_*.err
      do
        echo ' '
        sed "s/^/$file : /g" $file
      done
     fi
#  set +v; [[ "$LOUD" = YES ]] && set -x

#  set +x; [[ "$LOUD" = YES ]] && set -v
    if ls bull_*.err  1> /dev/null 2>&1
    then
      set +x
      echo ' '
      echo '******************************************'
      echo '*** ERROR OUTPUT multiwavespec_bull.sh ***'
      echo '******************************************'
      echo '            Possibly in multiple calls'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : error in bulletins." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in multiwavebull.sh, possibly in multiple calls."
      exit_code=25
      for file in bull_*.err
      do
        echo ' '
        sed "s/^/$file : /g" $file
      done
      rm -f bull_*.err
    fi
  fi
#  set +v; [[ "$LOUD" = YES ]] && set -x

#  set +x; [[ "$LOUD" = YES ]] && set -v
  rm -f cmdfile*
#  set +v; [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 7.  Make fourth command file


#  nper=8
#  nskip=16

  set +x
  echo ' '
  echo '   Making fourth command file for taring all point output files.'
  echo "   Number of tasks set per node     :  $nper"
  [[ "$LOUD" = YES ]] && set -x

#  ifile=1

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile


# 7.a Spectral data files

  if [ "$spec_OK" = 'yes' ]
  then
    echo "$USHglwu/multiwavetar.sh $runID spec $Nb > ${runID}_spec_tar.out 2>&1 "   >> cmdfile
  fi

  if [ "$ts_OK" = 'yes' ]
  then
    echo "$USHglwu/multiwavetar.sh $runID ts $Nb > ${runID}_ts_tar.out 2>&1 "   >> cmdfile
  fi

  if [ "$ripin_OK" = 'yes' ]
  then
# Create transect file for rip current model
    Nrip=$Nb
    cd $STA_DIR/ripin
    trans_loc='IDSPRip IDNPRip SILBRip WHIBRip'
    for tloc in $trans_loc
    do
      cat ${runID}.${PDY}_${cyc}00_${tloc}???.5m_contour | sed '/%/!d' | sed '1,7!d' > head
      cat ${runID}.${PDY}_${cyc}00_${tloc}???.5m_contour | sed '/%/d' | sort | uniq > tfdum
      cat head tfdum > ${runID}.${PDY}_${cyc}00_${tloc}.5m_contour
      Nrip=`expr $Nrip + 1`
    done
    cd $DATA
    echo "$USHglwu/multiwavetar.sh $runID ripin $Nrip > ${runID}_ripin_tar.out 2>&1 "   >> cmdfile
  fi

# 7.b Bulletins

  if [ "$bull_OK" = 'yes' ]
  then
    echo "$USHglwu/multiwavetar.sh $runID bull $Nb > ${runID}_bull_tar.out 2>&1 "   >> cmdfile
  fi

# 7.c Compressed bulletins

  if [ "$bull_OK" = 'yes' ]
  then
     echo "$USHglwu/multiwavetar.sh $runID cbull $Nb > ${runID}_cbull_tar.out 2>&1 " >> cmdfile
  fi

# 7.d CSV bulletins

  if [ "$bull_OK" = 'yes' ]
  then
    echo "$USHglwu/multiwavetar.sh $runID csbull $Nb > ${runID}_csbull_tar.out 2>&1 " >> cmdfile
  fi

# --------------------------------------------------------------------------- #
# 8.  Execute fourth command file

  set +x
  echo "   Executing tar command file at : `date`"
  echo '   -------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$nfile" -gt '1' ]
  then
    # ${mpicmd} cmdfile
    mpiexec -np 120 --cpu-bind verbose,core cfp cmdfile
    exit=$?
  else
#    ./cmdfile.1
    ./cmdfile
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '**************************************'
    echo '*** POE FAILURE DURING TAR PROCESS ***'
    echo '**************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

#  set +x; [[ "$LOUD" = YES ]] && set -v
  rm -f cmdfile*
#  set +v; [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 9.  Check for errors

  set +x
  echo ' '
  echo '   Checking for errors (error output concatenated below).'
  [[ "$LOUD" = YES ]] && set -x

#  9.a Spectral and time series tar file

  if [ "$spec_OK" = 'yes' ]
  then
    if [ -d TAR_spec_$runID ]
    then
      set +x
      echo "      Error in $runID spectral tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : error in spectral tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $runID spectral tar file."
      mv -f ${runID}_spec_tar.out ${runID}_spec_tar.err
    else
      set +x
      echo "      $runID Spectral tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi
  fi

  if [ "$ts_OK" = 'yes' ]
  then
    if [ -d TAR_ts_$runID ]
    then
      set +x
      echo "      Error in $runID time series tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : error in time series tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $runID time series tar file."
      mv -f ${runID}_ts_tar.out ${runID}_ts_tar.err
    else
      set +x
      echo "      $runID Time series tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi
  fi

  if [ "$ripin_OK" = 'yes' ]
  then
    if [ -d TAR_ripin_$runID ]
    then
      set +x
      echo "      Error in $runID rip input tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : error in rip input tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $runID rip input tar file."
      mv -f ${runID}_ripin_tar.out ${runID}_ripin_tar.err
    else
      set +x
      echo "      $runID Rip input tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi
  fi

#  9.b Bulletin tar files

  if [ "$bull_OK" = 'yes' ]
  then
    if [ -d TAR_bull_$runID ]
    then
      set +x
      echo "      Error in $runID bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : error in bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $runID bulletin tar file."
      mv -f ${runID}_bull_tar.out ${runID}_bull_tar.err
    else
      set +x
      echo "      $runID Bulletin tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi

    if [ -d TAR_cbull_$runID ]
    then
      set +x
      echo "      Error in $runID compressed bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : error in compressed bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $runID compressed bulletin tar file."
      mv -f ${runID}_cbull_tar.out ${runID}_cbull_tar.err
    else
      set +x
      echo "      $runID compressed bulletin tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi

    if [ -d TAR_csbull_$runID ]
    then
      set +x
      echo "      Error in $runID csv bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID post $date $cycle : error in csv bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $runID csv bulletin tar file."
      mv -f ${runID}_csbull_tar.out ${runID}_csbull_tar.err
    else
      set +x
      echo "      $runID csv bulletin tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi
  fi

#  9.e Error outputs

  if ls *.err  1> /dev/null 2>&1
  then
    set +x
    echo ' '
    echo '*********************'
    echo '*** ERROR OUTPUTS ***'
    echo '*********************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    exit_code=31
    for file in *.err
    do
      echo ' '
      sed "s/^/$file : /g" $file
    done
  fi


# --------------------------------------------------------------------------- #
# 10. Save in /com and send off alerts


  if [ "$SENDCOM" = 'YES' ]
  then
    if [ "${grib_OK}" = 'yes' ]
    then
# Save native grid files
      grdID=grlc_2p5km 
      set +x
      cp -f out_grd.$grdID $COMOUT/$runID.$grdID.$cycle.outgrd

# Save netcdf file
      grdID=glwu
      cp -f $runID.$grdID.$cycle.nc $COMOUT/$runID.$grdID.$cycle.nc

      if [ $SENDDBN = YES ]; then
        $SIPHONROOT/bin/dbn_alert MODEL WAVE_NC $job $COMOUT/$runID.$grdID.$cycle.nc 
      fi
# Save 4km regular outgrd for web plotting
      grdID=grlr
      cp -f out_grd.$grdID $COMOUT/$runID.$grdID.$cycle.outgrd

# Save 500m regular grid files
      grdID=grlr_500m

# Save 2.5km lambert sr grid files
      grdID=grlc_2p5km_sr

# grib2 files are DBN-alerted within multiwavegrib2.sh
    fi 
    
    echo ' '
    echo "   Moving tar file ${file_name} to $COMOUT ..."
    [[ "$LOUD" = YES ]] && set -x
   
    for type in spec ripin ts bull cbull csbull
    do

      file_name=$runID.$cycle.${type}_tar
      if [ "$type" = 'spec' ] || [ "$type" = 'ts' ]
      then
        file_name=${file_name}.gz
      fi

      cp ${file_name} $COMOUT/.
      exit=$?

      if  [ "$exit" != '0' ]
      then
        set +x
        echo ' '
        echo '************************************* '
        echo '*** FATAL ERROR : TAR COPY FAILED *** '
        echo '************************************* '
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "FATAL ERROR : TAR COPY FAILED"
        err=2 ; export err; err_chk
      fi

      if [ "$SENDDBN" = 'YES' ]
      then
        set +x
        echo ' '
        echo "   Alerting TAR file as $COMOUT/${file_name}"
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        # add optional tag to dbn_alert subtyp (to distinguish from standard prod alerts)
        $SIPHONROOT/bin/dbn_alert MODEL OMBWAVE $job $COMOUT/${file_name}
      fi
    done
  fi

  ecflow_client --event release_pgen
# --------------------------------------------------------------------------- #
# 11. Additional grib2 files
  if [ "$grib_OK" = 'yes' ]
  then
    for grdID in $addggrids
    do
      case $grdID in
        grl) GRIDNR=176  ; MODNR=131  ; dtgrib=3600. ; ngrib=148 ; GTMPLN=0 ;;
        grlr) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=148 ; GTMPLN=0 ;;
        grlr_500m) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=0 ;;
        grlc_2p5km) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=148 ; GTMPLN=30 ;;
        grlc_2p5km_sr) GRIDNR=255  ; MODNR=133  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=30 ;;
        grlc_1p0km) GRIDNR=255  ; MODNR=131  ; dtgrib=3600. ; ngrib=49 ; GTMPLN=30 ;;
      esac
      echo "RUNID: $runID "
      echo THIS IS IT '$grdID $dtgrib $ngrib $GRIDNR $MODNR $GTMPLN'
      echo $grdID $dtgrib $ngrib $GRIDNR $MODNR $GTMPLN
      $USHglwu/multiwavegrib2.sh $grdID $dtgrib $ngrib $GRIDNR $MODNR $GTMPLN > grib_$grdID.out 2>&1
    done
  fi
# --------------------------------------------------------------------------- #
# 12.  CLean up

#  set +x; [[ "$LOUD" = YES ]] && set -v
  rm -f *.tmpl
  for ID in $runID
  do
    rm -f $ID.*.spec
    rm -f $ID.*.bull
    rm -f $ID.*.cbull
    rm -f $ID.*.csbull
  done
#  set +v; [[ "$LOUD" = YES ]] && set -x

#  chmod 664 $COMOUT/$runID.$cycle.*

# --------------------------------------------------------------------------- #
# 11.  Ending output

  set +x
  echo ' '
  echo "Ending at : `date`"
  echo '-----------'
  echo ' '
  echo '                     *** End of GLW postprocessor ***'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$exit_code" -ne '0' ]
  then
     msg="ABNORMAL EXIT: Problem in GLW POST"
     postmsg "$jlogfile" "$msg"
     echo $msg
     err=$exit_code ; export err ; err_chk
  else
     touch $COMOUT/$runID.$cycle.postdone
  fi

  msg="$job completed normally"
  postmsg "$jlogfile" "$msg"

# End of GLW prostprocessor script ---------------------------------------- #
