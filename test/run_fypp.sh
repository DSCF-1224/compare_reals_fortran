#!/bin/sh -e
fypp -F                      compare_reals_fortran_support.fypp compare_reals_fortran_support.f90
fypp -F -DTARGET_INTERFACE=1 test_common.fypp                   test_eq_transfer.f90
fypp -F -DTARGET_INTERFACE=2 test_common.fypp                   test_is_contained_by_next_out.f90
fypp -F -DTARGET_INTERFACE=3 test_common.fypp                   test_le_and_ge.f90
