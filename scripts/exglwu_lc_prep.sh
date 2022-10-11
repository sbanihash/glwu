#!/bin/bash
###############################################################################
#                                                                             #
# This script is the preprocessor for the GRL multi-grid wave model that runs #
# using the single grid of the stand alone NWW# suite. It sets some shell     #
# script variables for export to child scripts and copies some generally used #
# files to the work directory. After this the actual preprocessing is         #
# performed by the following child scripts :                                  #
#                                                                             #
#   multiwavemod_def.sh   :  generate mod_def.ww3 files when needed           #
#   waveice_lc.sh        :  preprocess ice fields                            #
#   wavehrrr_glwu.sh      :  find and copy hrrr wind files (GLWU)             #
#   wavegfs_glwu.sh       :  find and copy gfs wind files  (GLWU)                                                                          #
# Also used is the utililty script                                            #
#                                                                             #
#  multiwavestart.sh   : get time of most recent restart file.                #
#                                                                             #
# Remarks :                                                                   #
#                                                                             #
# Origination:                                                   July, 2007   #
# Environment Equivalence Compliance:                            June, 2012   #
# Transition to WCOSS                                            Dec,  2012   #
# Upgrades to new curvilinear grid, ice and NAM winds            Oct,  2014   #
# Adding Lake Champlain wind data HRRR-GFS			 Jul,  2022   #
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

  msg="HAS BEGUN on `hostname`"
  postmsg "$jlogfile" "$msg"
    msg="Starting GLWU PREPROCESSOR SCRIPT"
  postmsg "$jlogfile" "$msg"

  set +x
  echo ' '
  echo '                      ********************************'
  echo '                      *** GLWU PREPROCESSOR SCRIPT ***'
  echo '                      ********************************'
  echo ' '
  echo "Starting at : `date`"
  [[ "$LOUD" = YES ]] && set -x

  rm -f ice_lc.warning

  export lsth_ice=0 # Using only one ice data time slice
  ttime13=36

  wndTAG_1="hrrr_glwu"
  wndTAG_2="gfs_glwu"

  export buoy='glwu_lc'
  export grids='glwu_lc'
  export igrids=

# 0.b Date and time stuff
#     The ending time of the run always is the $lsth hour forecast. The starting
#     time depends on availablility of restart files, and is obtained with
#     multiwavestart.sh
#
#     Make sure nback is set identically in the forecast script !!!
#     nback is the number of cycles (12-hour cycles) to look back.

  export date=$PDY
  export YMDH=${PDY}${cyc}
  export nback=0
  $USHglwu/wavestart_glwu_lc.sh
  ymdh_beg=`head wavestart.out | awk '{ print $1 }'`
  rm -f wavestart.out
  time_beg="`echo $ymdh_beg | cut -c1-8` `echo $ymdh_beg | cut -c9-10`0000"    

# Ice file generated for glw and glwu independently, needs ice timing variables set to NAM always
  export YMDH_ICE="${YMDH}"
  ymdh_beg_ice=$ymdh_beg
  ymdh_newtres_ice=`$NDATE ${ttime13} $YMDH_ICE`  
  time_beg_ice="`echo $ymdh_beg_ice | cut -c1-8` `echo $ymdh_beg_ice | cut -c9-10`0000"    

  if  [ "$RetroRun" = "YES" ]
  then
    export COMINwind_HRRR=/lfs/h2/emc/couple/noscrub/saeideh.banihashemi/Retro/HRRR
    export COMINwind_GFS=/lfs/h2/emc/couple/noscrub/saeideh.banihashemi/Retro/GFS
  else    
    export COMINwind_HRRR=/lfs/h1/ops/prod/com/hrrr/v4.1
    export COMINwind_GFS=/lfs/h1/ops/prod/com/gfs/v16.2
  fi

  ymdh=$ymdh_beg
  hrrr_ok='yes'

  set +x
  echo ' '
  echo '+-----------------------------------------------+'
  echo '|   Looking for relevant available HRRR files   |'
  echo '+-----------------------------------------------+'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  inc=0
  foundOK='no'
  lookback=12 # hours to look back for wind files
  
  while [ "$inc" -le "$lookback" ] && [ "$foundOK" = 'no' ]
  do
   hrrr_files=

# Find first forecast hour that has 18 and 48 hours of forecast

    ymdhb=`$NDATE -$inc $ymdh`
    ymdb=`echo $ymdhb | cut -c1-8`
    extb=`echo $ymdhb | cut -c9-10`
    
    hrrrfile18=$COMINwind_HRRR/hrrr.${ymdb}/conus/hrrr.t${extb}z.wrfprsf18.grib2
    hrrrfile48=$COMINwind_HRRR/hrrr.${ymdb}/conus/hrrr.t${extb}z.wrfprsf48.grib2
    
    if [ ! -f $hrrrfile18 ] 
    then
      set +x
      echo " Could not find HRRR files for ${hrrr_file} (Looking further)"
      [[ "$LOUD" = YES ]] && set -x
    else
      # 18 hour forecast is available	    
      export hrrr_file="${DATA}/hrrr_${extb}.grib2"
      time_hrrr18="`$WGRIB2 -vt $hrrrfile18 | sed -e 's/:/ /g' -e 's/vt=/ /g' | awk '{print $3}' | head -1`"
      hrrr18_files=`find  $COMINwind_HRRR/hrrr.${ymdb}/conus/hrrr.t${extb}z.wrfprsf??.grib2`
      
      mkdir ./hrrr_tmp
      cd ./hrrr_tmp

      cp ${hrrr18_files} ./.
    # Set UGRD and VGRD tags
      files=`ls hrrr.t${extb}z.wrfprsf??.grib2`

    # Save all files for HRRR 18 hour forecast 
      ntimes=
        for file in $files
        do
         time_tag="`$WGRIB2 -vt $file | sed -e 's/:/ /g' -e 's/vt=/ /g' | awk '{print $3}' | head -1`"
         ntimes="$ntimes $time_tag"
         $WGRIB2 -match "UGRD:10 m above ground" $file -grib hrrr_UGRD_${time_tag}.grb2 > gen_file.out 2>&1
         $WGRIB2 -match "VGRD:10 m above ground" $file -grib hrrr_VGRD_${time_tag}.grb2 > gen_file.out 2>&1
        done

      if [ ! -f $hrrrfile48 ] 
      then
        set +x
	echo " Could not find 48 hour HRRR forecast  (Looking further)"
        [[ "$LOUD" = YES ]] && set -x
   
        while [ ! -f $hrrrfile48 ]
       	do 
	  inc=`expr $inc + 1`
          ymdhb2=`$NDATE -$inc $ymdh`
          ymdb2=`echo $ymdhb2 | cut -c1-8`
          extb2=`echo $ymdhb2 | cut -c9-10`
          hrrrfile48=$COMINwind_HRRR/hrrr.${ymdb2}/conus/hrrr.t${extb2}z.wrfprsf48.grib2
	done
	time_hrrr48="`$WGRIB2 -vt $hrrrfile48 | sed -e 's/:/ /g' -e 's/vt=/ /g' | awk '{print $3}' | head -1`"
      else
      #both 18 and 48 hours are in the same directory
        extb2=$extb
	ymbd2=$ymdb
	time_hrrr48=${time_hrrr18}
      fi
      #  copy 48 hour forecasts
        
      hrrr48_files=`find  $COMINwind_HRRR/hrrr.${ymdb2}/conus/hrrr.t${extb2}z.wrfprsf??.grib2`

      cp ${hrrr48_files} ./.

      # Set UGRD and VGRD tags
      files=`ls hrrr.t${extb2}z.wrfprsf??.grib2`

      # Save all files for HRRR 48 hour forecast
      ntimes=
      for file in $files
      do
        time_tag="`$WGRIB2 -vt $file | sed -e 's/:/ /g' -e 's/vt=/ /g' | awk '{print $3}' | head -1`"
        #ntimes="$ntimes $time_tag"
        if [ ${time_tag} -gt  ${time_hrrr18} ]
	then
          $WGRIB2 -match "UGRD:10 m above ground" $file -grib hrrr_UGRD_${time_tag}.grb2 > gen_file.out 2>&1
          $WGRIB2 -match "VGRD:10 m above ground" $file -grib hrrr_VGRD_${time_tag}.grb2 > gen_file.out 2>&1
        fi
      done
  

      set +x
      echo " Found HRRR files --> ${hrrr_files}"
      [[ "$LOUD" = YES ]] && set -x

      #  echo " Reading latest hrrr files in selected time range: $tlatest"
      cat hrrr_UGRD_*.grb2 > hrrr_UGRD_t${extb}z.grb2
      cat hrrr_VGRD_*.grb2 > hrrr_VGRD_t${extb}z.grb2
      cat hrrr_UGRD_t${extb}z.grb2 hrrr_VGRD_t${extb}z.grb2 > hrrr_t${extb}z.grb2
      vtimei=`$WGRIB2 hrrr_t${extb}z.grb2 -var -vt | head -1 | sed 's/:vt=/ /g' | awk '{print $2}'`
      if [ ${vtimei} -le ${ymdh_beg} ]
      then
        cp hrrr_t${extb}z.grb2 ${hrrr_file}
        echo "HRRR file used: hrrr_t${extb}z.grb2 " > $DATA/what_${runID_lc}_used_hrrr.t${cyc}z
      fi


# Create final HRRR file with found consistent UGRD and VGRD data
      if [ -f ${hrrr_file} ]
      then

        cd ${DATA}
        rm -rf ./hrrr_tmp

# Get first time slice of hrrr file
        vtimei=`$WGRIB2 ${hrrr_file} -var -vt | head -1 | sed 's/:vt=/ /g' | awk '{print $2}'`
        if [ ${vtimei} -le ${ymdh_beg} ]
        then
# Get inventory of time stamps in hrrr file
          vtimes_ugrd=`$WGRIB2 ${hrrr_file} -match UGRD -vt | sed 's/:vt=/ /g' | awk '{print $2}' | sort | uniq`
          vtimes_vgrd=`$WGRIB2 ${hrrr_file} -match VGRD -vt | sed 's/:vt=/ /g' | awk '{print $2}' | sort | uniq`
          nvt1=`echo $vtimes_ugrd | wc -w`  
          nvt2=`echo $vtimes_vgrd | wc -w`  
          if  [ ${nvt1} -eq ${nvt2} ]
          then
            vtimes_hrrr=$vtimes_ugrd
            foundOK='yes'
          else
            echo " HRRR file has incongruent numbers of UGRD and VGRD data (looking further)"
          fi
        else
          echo " HRRR file does not extend back to starting time (Looking further)"
          foundOK='no'
        fi
      fi
    fi
    inc=`expr $inc + 1`
  done

  if [ "$foundOK" = 'no' ]
  then
    msg="FATAL ERROR: COULD NOT FIND RELEVANT HRRR FILES"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '*****************************************************'
    echo '*** FATAL ERROR: DID NOT FIND RELEVANT HRRR FILES ***'
    echo '*****************************************************'
    echo ' '
    echo $msg
    [[ "$LOUD" = YES ]] && set -x
    echo "$runID prep $date $cycle : $grdID.inp missing." >> $wavelog
    err=1;export err;err_chk
  else
    export hrrr_file
    
    if [ ${cyc} -eq 1 ] || [ ${cyc} -eq 7 ] || [ ${cyc} -eq 13 ] || [ ${cyc} -eq 19 ]
    then
      lsth=149
    else
      lsth=48
    fi
    wndID_1='HRRR'
  fi

# ---------------------------------------------------------------------------- #
# Get GFS wind 

  ymdh=$ymdh_beg

  gfs_ok='yes'
   
  set +x
  echo ' '
  echo '+-----------------------------------------------+'
  echo '|   Looking for relevant available GFS files   |'
  echo '+-----------------------------------------------+'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  inc=0
  foundOK='no'
  lookback=12 # hours to look back for wind files

  while [ "$inc" -le "$lookback" ] && [ "$foundOK" = 'no' ]
  do
    gfs_files=

  # Find latest cycle that has GFS wind data

    ymdhb=`$NDATE -$inc $ymdh`
    ymdb=`echo $ymdhb | cut -c1-8`
    extb=`echo $ymdhb | cut -c9-10`
    
    if [ ${cyc} -eq 1 ] || [ ${cyc} -eq 7 ] || [ ${cyc} -eq 13 ] || [ ${cyc} -eq 19 ] # glwu long cycle
    then
      flen=144
      gfsfile=$COMINwind_GFS/gfs.${ymdb}/${extb}/atmos/gfs.t${extb}z.pgrb2.0p25.f156
    else
      flen=48
      gfsfile=$COMINwind_GFS/gfs.${ymdb}/${extb}/atmos/gfs.t${extb}z.pgrb2.0p25.f060
    fi
    time_gfs_end=`$NDATE $flen $YMDH`
    
    if [ ${extb} -eq 0 ] || [ ${extb} -eq 6 ] || [ ${extb} -eq 12 ] || [ ${extb} -eq 18 ] # gfs cycles
    then
      if [ ! -f $gfsfile ]
      then
        set +x
        echo " Could not find GFS files for ${gfs_file} (Looking further)"
        [[ "$LOUD" = YES ]] && set -x
      else
      # gfs forecast is available
        export gfs_file="${DATA}/gfs_${extb}.grib2"

        time_gfs="`$WGRIB2 -vt $gfsfile | sed -e 's/:/ /g' -e 's/vt=/ /g' | awk '{print $3}' | head -1`"
        
	if [ ${cyc} -eq 1 ] || [ ${cyc} -eq 7 ] || [ ${cyc} -eq 13 ] || [ ${cyc} -eq 19 ] # if long cycle we need 144 hours
	then
	   gfs_files=`find  $COMINwind_GFS/gfs.${ymdb}/${extb}/atmos/gfs.t${extb}z.pgrb2.0p25.f[0-1]??`
   	else   # if short cycle, we need 48 hours
	   gfs_files=`find  $COMINwind_GFS/gfs.${ymdb}/${extb}/atmos/gfs.t${extb}z.pgrb2.0p25.f[0][4-6]?`
	fi
	
	mkdir ./gfs_tmp
	cd ./gfs_tmp

	ntimes= 

	for file in $gfs_files
        do
          time_tag="`$WGRIB2 -vt $file | sed -e 's/:/ /g' -e 's/vt=/ /g' | awk '{print $3}' | head -1`"
          #ntimes="$ntimes $time_tag"
	  if [ ${time_tag} -gt  ${time_hrrr48} ] && [ ${time_tag} -le ${time_gfs_end} ]
	  then
            $WGRIB2 -match "UGRD:10 m above ground" $file -grib gfs_UGRD_${time_tag}.grb2 > gfs_gen_file.out 2>&1
            $WGRIB2 -match "VGRD:10 m above ground" $file -grib gfs_VGRD_${time_tag}.grb2 > gfs_gen_file.out 2>&1
	    #cp ${file} ./.
          fi
        done 

	set +x
	echo " Found GFS files --> ${gfs_files}"
	[[ "$LOUD" = YES ]] && set -x
	#  echo " Reading latest gfs files in selected time range"

        cat gfs_UGRD_*.grb2 > gfs_UGRD_t${extb}z.grb2
	cat gfs_VGRD_*.grb2 > gfs_VGRD_t${extb}z.grb2
	cat gfs_UGRD_t${extb}z.grb2 gfs_VGRD_t${extb}z.grb2 > gfs_t${extb}z.grb2

	vtimei=`$WGRIB2 gfs_t${extb}z.grb2 -var -vt | head -1 | sed 's/:vt=/ /g' | awk '{print $2}'`
	cp gfs_t${extb}z.grb2 ${gfs_file}
	 echo  "GFS file used: gfs_t${extb}z.grb2 " > ${DATA}/what_${runID_lc}_used_gfs.t${cyc}z
	#echo "GFS file used: gfs_t${extb}z.grb2 " > $DATA/what_${runID}_used.t${cyc}z
  
# create final HRRR file with found consistent UGRD and VGRD data
        if [ -f ${gfs_file} ]
        then
          cd ${DATA}
          rm -rf ./gfs_tmp

   # Get inventory of time stamps in gfs file
          vtimes_ugrd=`$WGRIB2 ${gfs_file} -match UGRD -vt | sed 's/:vt=/ /g' | awk '{print $2}' | sort | uniq`
          vtimes_vgrd=`$WGRIB2 ${gfs_file} -match VGRD -vt | sed 's/:vt=/ /g' | awk '{print $2}' | sort | uniq`
          nvt1=`echo $vtimes_ugrd | wc -w`
          nvt2=`echo $vtimes_vgrd | wc -w`
          
	  if  [ ${nvt1} -eq ${nvt2} ]
          then
            vtimes_gfs=$vtimes_ugrd
            foundOK='yes'
          else
            echo " GFS file has incongruent numbers of UGRD and VGRD data (looking further)"
	  fi
        else
          echo " GFS file does not extend back to starting time (Looking further)"
	  foundOK='no'
        fi
      fi
    fi
    inc=`expr $inc + 1`
  done

  if [ "$foundOK" = 'no' ]
  then
    msg="FATAL ERROR: COULD NOT FIND RELEVANT GFS FILES"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '*****************************************************'
    echo '*** FATAL ERROR: DID NOT FIND RELEVANT GFS FILES ***'
    echo '*****************************************************'
    echo ' '
    echo $msg
    [[ "$LOUD" = YES ]] && set -x
    echo "$runID prep $date $cycle : $grdID.inp missing." >> $wavelog
    err=1;export err;err_chk
  else
    export gfs_file
  wndID_2='GFS'
  fi
# --------------------------------------------------------------------------------------------------------- #



  ymdh_end=`$NDATE $lsth $YMDH`
  time_end="`echo $ymdh_end | cut -c1-8` `echo $ymdh_end | cut -c9-10`0000"


  ymdh_end_ice=`$NDATE $lsth_ice $ymdh_beg_ice`
  time_end_ice="`echo $ymdh_end_ice | cut -c1-8` `echo $ymdh_end_ice | cut -c9-10`0000"

  set +x 
  echo ' ' 
  echo 'Times in wave model format :' 
  echo '----------------------------' 
  echo "   date / cycle  : $date $cycle"
  echo "   starting time : $time_beg"
  echo "   ending time   : $time_end"
  echo ' '
 
# 0.c Command file set-up
#     The command file points to $nfile files named cmdfile.$ifile.
#     The actual work is distributed over these files.

  [[ "$LOUD" = YES ]] && set -x
  nfile=`echo $LSB_HOSTS | wc -w | awk '{ print $1}'`

  if [ "$nfile" -gt '1' ]
  then
    cmdtype="aprun"
  else
    cmdtype='sh'
    nfile=1
  fi

  set +x
  echo ' '
  echo '   Making command file(s)'
  echo "   Set up command file structure (type = $cmdtype)."
  echo "      Number of command files  : $nfile"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts


  set +x 
  echo 'Preparing input files :'
  echo '-----------------------'
  [[ "$LOUD" = YES ]] && set -x

# 1.a  Model definition file

  nmoddef=0

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

  for grdID in $grids $igrids #$buoy
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
      if [ -f $FIXglwu/wave_$grdID.inp ] || [ -f $FIXglwu/mesh.$grdID ]
      then
        cp $FIXglwu/wave_$grdID.inp $grdID.inp
        cp $FIXglwu/mesh.$grdID mesh.$grdID
      fi

      if [ -f $grdID.inp ] && [ "$grdID" = 'points' ]
      then
        set +x
        echo ' '
        echo "   $grdID.inp copied ($FIXglwu/wave_$grdID.inp)."
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
      elif [ -f $grdID.inp ] && [ -f mesh.$grdID ]
      then
        set +x
        echo ' '
        echo "   $grdID.inp copied ($FIXglwu/wave_$grdID.inp)."
        echo ' '
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
        echo "$runID prep $date $cycle : $grdID.inp missing." >> $wavelog
        err=2;export err;err_chk
      fi

      echo "$USHglwu/wavemod_def.sh $grdID > $grdID.out 2>&1" >> cmdfile

      nmoddef=`expr $nmoddef + 1`

    fi

  done




# 1.a.1 Execute $mpicmd (if needed)

  if [ "$nmoddef" -gt '0' ]
  then

    set +x
    echo ' '
    echo " Generating $nmoddef mod def files"
    echo ' '
    [[ "$LOUD" = YES ]] && set -x

    if [ "$nfile" -gt '1' ]
    then
      mpiexec -np 5 --cpu-bind verbose,core cfp cmdfile
      exit=$?
    else
      ./cmdfile.1
      ./cmdfile
      exit=$?
    fi

  fi

# 1.a.2 File check

  for grdID in $grids $igrids $buoy
  do
    if [ -f mod_def.$grdID ]
    then
      set +x
      echo ' '
      echo " mod_def.$grdID succesfully created/copied "
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
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
      sed "s/^/$grdID.out : /g"  $grdID.out
      echo "$runID prep $date $cycle : mod_def.$grdID missing." >> $wavelog
      err=3;export err;err_chk
    fi
  done

# Remove cmdfile after creating moddef (if the case)
  if ls cmdfile* &> /dev/null; then
    rm -f cmdfile*
  fi

# 1.b.1 Wind preprocessor template file for HRRR

  if [ -f $FIXglwu/multiwaveprnc.${wndTAG_1}.tmpl ]
  then
    cp $FIXglwu/multiwaveprnc.${wndTAG_1}.tmpl .
  fi

  if [ -f multiwaveprnc.${wndTAG_1}.tmpl ]
  then
    echo "   multiwaveprnc."${wndTAG_1}".tmpl copied ($FIXglwu/multiwaveprnc."${wndTAG_1}".tmpl)."
  else
    msg="ABNORMAL EXIT: NO FILE multiwaveprnc."${wndTAG_1}".tmpl"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '************************************** '
    echo '*** FATAL ERROR : NO TEMPLATE FILE *** '
    echo '************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo $msg
    echo "$runID prep $date $cycle : template file missing." >> $wavelog       
    err=4;export err;err_chk
  fi



# 1.b.2 wind preprocessor template file for HRRR

  if [ -f $FIXglwu/multiwaveprnc.${wndTAG_2}.tmpl ]
  then
    cp $FIXglwu/multiwaveprnc.${wndTAG_2}.tmpl .
  fi

  if [ -f multiwaveprnc.${wndTAG_2}.tmpl ]
  then
    echo "   multiwaveprnc."${wndTAG_2}".tmpl copied ($FIXglwu/multiwaveprnc."${wndTAG_1}".tmpl)."
  else
    msg="ABNORMAL EXIT: NO FILE multiwaveprnc."${wndTAG_2}".tmpl"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '************************************** '
    echo '*** FATAL ERROR : NO TEMPLATE FILE *** '
    echo '************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo $msg
    echo "$runID prep $date $cycle : template file missing." >> $wavelog
    err=4;export err;err_chk
  fi

# This section was exclusive to GLW, but will keep for both GLW and GLWU, in case
# future late runs have other ice types etc, so will generate new ice at every glw
# and glwu runs.

# 1.c Ice preprocessor template file
  if [ -f $FIXglwu/multiwaveprep.ice_lc.tmpl ]
  then
    cp $FIXglwu/multiwaveprep.ice_lc.tmpl ./multiwaveprep.ice.tmpl
  fi

  if [ -f multiwaveprep.ice.tmpl ]
  then
    echo "   multiwaveprep.ice.tmpl copied ($FIXglwu/multiwaveprep.ice_lc.tmpl)."
  else
    msg="ABNORMAL EXIT: NO FILE multiwaveprep.ice.tmpl"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '************************************** '
    echo '*** FATAL ERROR : NO TEMPLATE FILE *** '
    echo '************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo $msg
    echo "$runID prep $date $cycle : template file missing." >> $wavelog
    err=5;export err;err_chk
  fi


# 1.e Copy the ice template files
#
# This is an approach using directly the BVT ICE file format
#
  for ftype in mask0 ll  
  do
 
    file=$FIXglwu/LC_BVT_ICE.${ftype}

    if [ -f $file ]
    then
      cp $file ./
    fi

    if [ -f ${file} ]
    then
      set +x 
      echo "   LC_BVT_ICE.${ftype} copied."
      [[ "$LOUD" = YES ]] && set -x
    else
      set +x
      echo ' '
      echo '************************************** '
      echo "*** ERROR : NO ICE FIELD ${ftype} FILE *** "
      echo '************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      postmsg "$jlogfile" "FATAL ERROR - NO ICE FIELD ${ftype} FILE"
      err=7;export err;err_chk
    fi

  done


# --------------------------------------------------------------------------- #
# 2.  Make command file(s) (ice, SST, coefficient files)i and/or (HRRR/GFS winds)

# 2.a Command file set-up
#     The command file points to $nfile files named cmdfile.$ifile.            
#     The actual work is distributed over these files.

  set +x
  echo '   Making command file(s)'
  echo "   Set up command file structure (type = $cmdtype)."
  echo "      Number of command files  : $nfile"
  [[ "$LOUD" = YES ]] && set -x

  touch cmdfile
  chmod 744 cmdfile

  ifile=1

# 2.b Ice preprocessing 
  echo "Ice preprocessing"
  ymdh=$ymdh_beg_ice

  while [ "$ymdh" -le "$ymdh_end_ice" ]
  do
  echo "$USHglwu/waveice_lc.sh $ymdh > ice_$ymdh.err 2>&1"     >> cmdfile
    tinc_ice=1
    if [ $ymdh -ge $ymdh_newtres_ice -o $ymdh -lt $YMDH_ICE ]
    then
      tinc_ice=3
    fi
    ymdh=`$NDATE $tinc_ice $ymdh`
  done

# 2.c hrrr winds
  ymdh=$ymdh_beg
  hdr='T'

  for ymdh in $vtimes_hrrr
  do
    if [ "$ymdh" -le "$ymdh_end" ]
    then
      echo "$USHglwu/wavehrrr_glwu_inc.sh $ymdh 1 $hdr > hrrr_$ymdh.err 2>&1"     >> cmdfile
# Remove header from subsequent binary WW3 wind files so they can be catted  
      hdr='F'
    else
      break
    fi
  done



# 2.d gfs winds
  #ymdh=$ymdh_beg
  #hdr='F'

  for ymdh in $vtimes_gfs
  do
    if [ "$ymdh" -le "$ymdh_end" ]
    then
      echo "$USHglwu/wavegfs_glwu_inc.sh $ymdh 1 $hdr > gfs_$ymdh.err 2>&1"     >> cmdfile
# Remove header from subsequent binary WW3 wind files so they can be catted
      hdr='F'
    else
      break
    fi
  done

  
  
  # --------------------------------------------------------------------------- #
# 3   Execute command file
# 3.a Execution

  set +x
  echo "   Executing command file."
  [[ "$LOUD" = YES ]] && set -x

  if [ "$nfile" -gt '1' ]
  then
    mpiexec -np 120 --cpu-bind verbose,core cfp cmdfile
    exit=$?
  else
    ./cmdfile
    exit=$?
  fi

# 3.b Error trap on ${mpicmd} or shell

  if [ "$exit" != '0' ]
  then
    msg="ABNORMAL EXIT: ERROR IN $cmdtype"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '*********************************************** '
    echo "*** FATAL ERROR : ERROR IN aprun OR sh *** "
    echo '*********************************************** '
    echo ' '
    echo ' Probing for errors in cmdfile outputs '
    [[ "$LOUD" = YES ]] && set -x
    cmderr=`grep 'FATAL ERROR' *.err`
    if [ -z "$cmderr" ]
    then
       echo ' '
       echo ' CMDFILE WITH NO OUTPUT, UNKOWN ERROR HAS OCCURRED'
    else
       echo ' '
       echo ' CMDFILE EXITED WITH FOLLOWING ERRORS: '
       grep 'FATAL ERROR' *.err
    fi
    echo "$runID prep $date $cycle : error in $cmdtype." >> $wavelog          
    echo $msg
    err=8;export err;err_chk
  fi

# --------------------------------------------------------------------------- #
# 4.  ICE fields: Check for errors and pack onto input file

  set +x
  echo ' '
  echo '   Checking for errors.'
  [[ "$LOUD" = YES ]] && set -x

# 4.a Check for error in Ice field
#     We will go on if the number of errors in files is less
#     than err_max

  err_max=1
  set +x
  echo '      Sources of NIC-ice files :'
  [[ "$LOUD" = YES ]] && set -x

  ymdh=$ymdh_beg_ice
  nr_err=0

  while [ "$ymdh" -le "$ymdh_end_ice" ]
  do
    if [ -d eice_${ymdh} ]
    then
      postmsg "$jlogfile" "    File for $ymdh : error in waveice_lc.sh"
      set +x
      echo "         File for $ymdh : error in waveice_lc.sh"
      [[ "$LOUD" = YES ]] && set -x       
      nr_err=`expr $nr_err + 1`
      rm -f eice.$ymdh
    else
      ticfile=`grep 'File for' ice_${ymdh}.err`
      if [ -z "$nicfile" ]
      then
        set +x
        echo "         File for $ymdh : cannot identify source"
        nr_err=`expr $nr_err + 1`
        [[ "$LOUD" = YES ]] && set -x
        rm -f eice.$ymdh
      else
        if [ ! -f LC_BVT_ICE.$ymdh ]
        then
        set +x
          echo "         File for $ymdh : file not found"
        [[ "$LOUD" = YES ]] && set -x
          nr_err=`expr $nr_err + 1`
        else
        set +x
          echo "      $nicfile"
          mv -f ice_${ymdh}.err ice_${ymdh}.out
        [[ "$LOUD" = YES ]] && set -x
        fi
      fi
    fi
    tinc=1
    if [ $ymdh -ge $ymdh_newtres_ice -o $ymdh -lt $YMDH_ICE ]
    then
      tinc=3
    fi
     ymdh=`$NDATE $tinc $ymdh`
  done

  if ls ice_*.err  1> /dev/null 2>&1
  then
    set +x
    echo ' '
    echo '****************************************'
    echo '*** ERROR OUTPUT waveice_lc.sh ***'
    echo '****************************************'
    echo '            Possibly in multiple calls'
    echo "$runID prep $date $cycle : error in ice   files." >> $wavelog
    [[ "$LOUD" = YES ]] && set -x
    for file in ice_*.err
    do
      set +x
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      sed "s/^/$file : /g" $file
    done
     # rm -f ice_*.out
    postmsg "$jlogfile" "NON-FATAL ERROR in waveice_lc.sh, possibly in multiple calls."
  fi
  if [ "$nr_err" -gt "$err_max" ]
  then
    msg="ABNORMAL EXIT: ERROR(S) IN NAM-ICE FILES"
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '*********************************************** '
    echo '*** FATAL ERROR : ERROR(S) IN NAM-ICE FILES *** '
    echo '*********************************************** '
    echo $nr_err
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo $msg
    echo "$runID prep $date $cycle : fatal error in NIC   files." >> $wavelog
    err=9;export err;err_chk
  fi
  rm -f cmdfile*

# 4.b  Run waveprep for ice field
  set +x
  echo ' '
  echo '   Running ice field through preprocessor.'
  [[ "$LOUD" = YES ]] && set -x
  sed "s/GRIDLAYOUT/$GRID/g" multiwaveprep.ice.tmpl > ww3_prep.inp
  rm -f multiwaveprep.ice.tmpl

# Here using igrids (the regular grid for ice grlr)
  for grdID in $grids
  do

    cp mod_def.$grdID mod_def.ww3
    $EXECglwu/multiwaveprep > multiwaveprep.out
    err=$?

    if [ "$err" != '0' ]
    then
      msg="ABNORMAL EXIT: ERROR IN multiwaveprep"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '******************************************** '
      echo '*** FATAL ERROR : ERROR IN multiwaveprep *** '
      echo '******************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID $grdID prep $date $cycle : error in multiwaveprep." >> $wavelog
      err=12;export err;err_chk
    fi

    if [ ! -f ice.ww3 ]
    then
      msg="ABNORMAL EXIT: FILE ice.ww3 MISSING"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      cat multiwaveprep.out
      echo ' '
      echo '****************************************'
      echo '*** FATAL ERROR : ice.ww3 NOT FOUND ***'
      echo '****************************************'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$runID $grdID prep $date $cycle : ice.ww3 missing." >> $wavelog
      err=13;export err;err_chk
    else
      set +x
      echo "      Ice file succesfully created for $ymdh"
      [[ "$LOUD" = YES ]] && set -x
      cp ice.ww3 ice.$grdID
    fi
  done
  rm -f eice.*
  rm -f ice.new
  rm -f ww3_prep.inp
# --------------------------------------------------------------------------- #
# 5.  HRRR-GFS fields: Check for errors and pack onto input file

# 5.a.1 HRRR Error checking made at extraction level, check if files are there
#     and move to final file name

  ymdh=$ymdh_beg

  first='y'

  for ymdh in $vtimes_hrrr
  do
    if [ "$ymdh" -le "$ymdh_end" ]
    then
      if [ -d hrrr_${ymdh} ]
      then
        set +x
        echo "         Wind data could not be found for $ymdh"
        echo "         Error output from wavehrrr_glwu.sh:"
        cat hrrr_$ymdh.err
        hrrr_ok='no'
        [[ "$LOUD" = YES ]] && set -x
      else
        set +x
        echo "      Wind data succesfully extracted for $ymdh"
        [[ "$LOUD" = YES ]] && set -x
          if [ "$first" = 'y' ]
          then
            for grdID in $grids
            do
              mv wind.$grdID.$ymdh wind.$grdID
            done
            first='n'
          else
            for grdID in $grids
            do
              cat wind.$grdID.$ymdh >> wind.$grdID
            done
          fi
        mv -f hrrr_$ymdh.err hrrr_$ymdh.out
      fi
    else
      break
    fi
  done
  
# end of if loop from 5.a.1 

# 5.a.2 GFS Error checking made at extraction level, check if files are there
#     and move to final file name

  ymdh=$ymdh_beg

  for ymdh in $vtimes_gfs
  do
    if [ "$ymdh" -le "$ymdh_end" ]
    then
      if [ -d gfs_${ymdh} ]
      then
        set +x
        echo "         Wind data could not be found for $ymdh"
        echo "         Error output from wavegfs_glwu.sh:"
        cat gfs_$ymdh.err
        gfs_ok='no'
        [[ "$LOUD" = YES ]] && set -x
      else
        set +x
        echo "      Wind data succesfully extracted for $ymdh"
        [[  "$LOUD" = YES ]] && set -x
        if [ "$first" = 'y' ]
        then
          for grdID in $grids
          do
            mv wind.$grdID.$ymdh wind.$grdID
          done
	  first='n'
        else
          for grdID in $grids
	  do
            cat wind.$grdID.$ymdh >> wind.$grdID
          done
        fi
      mv -f gfs_$ymdh.err gfs_$ymdh.out
    fi
  else
    break
  fi
  done

# end of if loop from 5.a.2



# 5.b Error captcha

  if [ "$hrrr_ok" = 'no' ]
  then
    msg="ERROR IN EXTRACTING DATA FROM HRRR GRIDS"
    postmsg "$jlogfile" "$msg"
    echo ' ' > warning
    echo '***********************************************************' >> warning
    echo '*** WARNING !! ERROR IN EXTRACTING DATA FROM HRRR GRIDS ***' >> warning
    echo "***    Missing HRRR winds for LC run $PDY t${cyc}z    ***" >> warning
    echo '***********************************************************' >> warning
    echo ' ' >> warning
    echo "$runID prep $date $cycle : Error converting winds." >> $wavelog
    set +x
    cat warning
    [[ "$LOUD" = YES ]] && set -x
    err=14;export err;./err_chk
  fi

  if [ "$gfs_ok" = 'no' ]
  then
    msg="ERROR IN EXTRACTING DATA FROM GFS GRIDS"
    postmsg "$jlogfile" "$msg"
    echo ' ' > warning
    echo '***********************************************************' >> warning
    echo '*** WARNING !! ERROR IN EXTRACTING DATA FROM GFS GRIDS ***' >> warning
    echo "***    Missing GFS winds for LC run $PDY t${cyc}z    ***" >> warning
    echo '***********************************************************' >> warning
    echo ' ' >> warning
    echo "$runID prep $date $cycle : Error converting winds." >> $wavelog
    set +x
    cat warning
    [[ "$LOUD" = YES ]] && set -x
     err=14;export err;./err_chk
  fi

# --------------------------------------------------------------------------- #
# 6.  Output to /com

  if [ "$SENDCOM" = 'YES' ]
  then
    for grdID in $grids 
    do
     for input in wind ice
     do
      set +x
      echo ' '
      if [ -f ${input}.$grdID ]
      then
        echo "   Saving ${input}.$grdID as $COMOUT/$runID.$grdID.$cycle.${input}"        
        [[ "$LOUD" = YES ]] && set -x
        cp ${input}.$grdID $COMOUT/$runID.$grdID.$cycle.${input}
        chmod 664 $COMOUT/$runID.$grdID.$cycle.${input}
      fi
     done
    done

    [[ "$LOUD" = YES ]] && set -x

# Copy the ice file log to com
    cp whatglwice_lc  $COMOUT/whatglwice_lc.$cycle
    if [ -f ice_lc.warning ] ; then echo "$runID prep $date $cycle : missing ice info" >> $wavelog ; fi

    if [ -f ice_lc.warning ] && [ "${runID}" == "glwu" ] ; then
# prepare ice email	
	echo ' ' > email.ice
	echo '******************************************************' >> email.ice
	echo '***   WARNING !! COULD NOT FIND GLWU/LC ICE INFO     ' >> email.ice
	echo '***        (SETTING ICE COVERAGE TO ZEROS)            ' >> email.ice
	echo '******************************************************' >> email.ice
	echo ' ' >> email.ice
   
        mail.py -s "Missing GLWU_LC ice info for $PDY t${cyc}z" email.ice
    fi
      sort -b what_${runID_lc}_used_hrrr.$cycle > $COMOUT/what_${runID_lc}_used_hrrr.$cycle
      sort -b what_${runID_lc}_used_gfs.$cycle > $COMOUT/what_${runID_lc}_used_gfs.$cycle
      #$SIPHONROOT/bin/dbn_alert MODEL OMBWAVE $job $COMOUT/what_${runID_lc}_used.$cycle
  fi






# 0--------------------------------------------------------------------------- #
# 7.  Ending output

  set +x
  echo ' '
  echo "Ending at : `date`"
  echo ' '
  echo '                     *** End of GLWU preprocessor ***'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  msg="$job completed normally"
  postmsg "$jlogfile" "$msg"

# End of GLW preprocessor script ------------------------------------------- #
