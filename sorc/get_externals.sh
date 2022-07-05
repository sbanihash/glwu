#!/bin/bash
# --------------------------------------------------------------------------- #
#                                                                             #
# Copy external fix files that are too large to store in repository           #
#                                                                             #
# Last Changed : 07-05-2022                                        July 2022  #
# --------------------------------------------------------------------------- #

echo 'Fetching externals...'
cp -p /lfs/h2/emc/couple/noscrub/andre.vanderwesthuysen/git/fv3gfs/fix/fix_glwu/mesh.glwu ../fix/
cp -p /lfs/h2/emc/couple/noscrub/andre.vanderwesthuysen/git/fv3gfs/fix/fix_glwu/grint_weights.grlc_2p5km ../fix/
cp -p /lfs/h2/emc/couple/noscrub/andre.vanderwesthuysen/git/fv3gfs/fix/fix_glwu/grint_weights.grlr ../fix/
cp -p /lfs/h2/emc/couple/noscrub/andre.vanderwesthuysen/git/fv3gfs/fix/fix_glwu/grint_weights.grlr_500m ../fix/
