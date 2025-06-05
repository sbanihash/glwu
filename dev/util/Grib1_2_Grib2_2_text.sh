#!/bin/bash
#Roberto.Padilla

module load grib_util/1.0.3
set +xa
#OutputDir=/gpfs/hps3/emc/marine/noscrub/Roberto.Padilla/GLWUInput/LOWResolution/ICE
export PDY=20180415  # Initial Time
WorkDir=/gpfs/hps3/emc/marine/noscrub/Roberto.Padilla/GLWUInput/${PDY}/wgrbbul
cd $Workdir

nicice=$(find T*.gr1)
cp ${nicice} ./nicice.gr1
# Convert grib1 to grib2
$CNVGRIB -g12 nicice.gr1 T_OEBA88_C_KNWC.grb2
# Extract to text
$WGRIB2 T_OEBA88_C_KNWC.grb2 -text LowResIce_${PDY}.ice

