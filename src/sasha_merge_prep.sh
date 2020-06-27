#!/bin/bash
# Move files to different directories to match ETH Zurich
# directory structure
mkdir -f Etalon_dataInputs Obsolete Documentation
git mv README* Documentation
git mv etalon_* Etalon_data
git mv *\.in\.* Inputs
git mv prsgrd*.F Obsolete/
git mv Obsolete/prsgrd32AC1.F .
git mv  ascii.F cpp_clean.F get_psource.F get_ssh.F lmd_kpp_old.F lmd_swfrac_old.F lsb_string.F mpc_alt_double.F mpi_exchange4.F mpi_exchange8.F pacific.in plotter.F smol_adv.F step2D_FB_VECTOR.F step2d_FB.F step2d_LF_AM3.F test_robust_diurnal.F Obsolete
git rm Make.depend
