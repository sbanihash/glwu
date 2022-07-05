#!/bin/bash
# --------------------------------------------------------------------------- #
#                                                                             #
# Copy external fix files that are too large to store in repository           #
#                                                                             #
# Last Changed : 09-27-2021                                   September 2021  #
# --------------------------------------------------------------------------- #

echo 'Fetching externals...'
cp -p /lfs/h2/emc/couple/noscrub/andre.vanderwesthuysen/git/fv3gfs/fix/fix_glwu/mesh.glwu ../fix/
cp -p /lfs/h2/emc/couple/noscrub/andre.vanderwesthuysen/git/fv3gfs/fix/fix_glwu/grint_weights.grlc_2p5km ../fix/
cp -p /lfs/h2/emc/couple/noscrub/andre.vanderwesthuysen/git/fv3gfs/fix/fix_glwu/grint_weights.grlr ../fix/
cp -p /lfs/h2/emc/couple/noscrub/andre.vanderwesthuysen/git/fv3gfs/fix/fix_glwu/grint_weights.grlr_500m ../fix/
