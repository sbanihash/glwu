
############################################
### JWAVE_GLWU_MIC_PREP: Great Lakes Wave Sys ###
###      LSF card for development        ###
############################################
#
# LSF options
#
##BSUB -M 2700
#BSUB -M 100
#BSUB -P GLW-T2O
#BSUB -J GET_NDF_HPSS
#BSUB -extsched 'CRAYLINUX[]' -R '1*{select[craylinux && !vnode]} +120*{select[craylinux && vnode]span[ptile=24] cu[type=cabinet]}'
#BSUB -oo /gpfs/hps3/emc/marine/noscrub/Roberto.Padilla/GLWUInput/GET_NDF_HPSS.out
#BSUB -eo /gpfs/hps3/emc/marine/noscrub/Roberto.Padilla/GLWUInput/GET_NDF_HPSS.out 
#BSUB -W 01:00
#BSUB -q "dev_transfer"
#BSUB -cwd /gpfs/hps3/emc/marine/noscrub/Roberto.Padilla/ops/lsf_scripts/wave_glwu.v1.0.0/util

module load hpss

set -xa
HPSSTAR=`pwd`/hpsstar
cd /gpfs/hps3/emc/marine/noscrub/Roberto.Padilla/GLWUInput


#YY=$1
#MMS=$2
#DDS=$3



export PDY=20171101    # Initial Time
#   - - - OR from today - - - 
#export PDY=$(date +%Y%m%d)  # From Today
#
PDYend=20171102         # END Time
PDY_epoch=$(date -d "${PDY}" +%s)           # Initial date epoch 
secDay=86400


until [ "$PDY" == "$PDYend" ]              #USE THIS  OR
do
   PDY=${YY}${MM}${DD}
   mkdir -p ${PDY}/wgrbbul
   cd ${PDY}/wgrbbul
   HPSSDIR=/NCEPPROD/hpssprod/runhistory/rh${YY}/${YY}${MM}/${PDY}
   echo $HPSSDIR
   TARFILE=dcom_us007003_${PDY}.tar 
   echo $TARFILE
   TAKEHOME=`$HPSSTAR inv ${HPSSDIR}/${TARFILE} | grep ndfd | grep 2p5 | awk '{print $7}'`
   echo $TAKEHOME
   echo "$HPSSTAR get ${HPSSDIR}/${TARFILE} ${TAKEHOME}"
   $HPSSTAR get ${HPSSDIR}/${TARFILE} ${TAKEHOME}
   cd /gpfs/hps3/emc/marine/noscrub/Roberto.Padilla/GLWUInput

   PDY_epoch=$(( $PDY_epoch + 86400 ))       # add 24 hours
   PDY=$(date -d @${PDY_epoch} +"%Y%m%d")    # 
done

#if [ $YY = 2014 ]
#then
#  MMS='06 07 08 09 10 11 12'
#else
#  MMS='01 02 03 04 05 06 07'
#fi

#YYS='2017 2018'
#DDS='01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31'

#for YY in $YYS
#do
#   if [ "$YY" -eq '2017' ]
#   then
#      MMS='11 12'
#   else
#      MMS='01 02 03 04'
#   fi
#   for MM in $MMS
#   do
#      for DD in ${DDS} 
#      do
#         PDY=${YY}${MM}${DD}
#         mkdir -p ${PDY}
#         cd ${PDY}
#         HPSSDIR=/NCEPPROD/hpssprod/runhistory/rh${YY}/${YY}${MM}/${PDY}
#         TARFILE=dcom_us007003_${PDY}.tar
#         TAKEHOME=`$HPSSTAR inv ${HPSSDIR}/${TARFILE} | grep ndfd | grep 2p5 | awk '{print $7}'`
#         echo "$HPSSTAR get ${HPSSDIR}/${TARFILE} ${TAKEHOME}"
#         $HPSSTAR get ${HPSSDIR}/${TARFILE} ${TAKEHOME}
#         cd ..
#      done
#   done
#done

