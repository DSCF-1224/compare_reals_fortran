#!/bin/sh -e
fypp -F                      test/compare_reals_fortran_support.fypp test/compare_reals_fortran_support.f90
fypp -F -DTARGET_INTERFACE=0 test/test_common.fypp                   test/test_intrinsic.f90
rm -rf build
fpm test --flag "-Wall -Werror -pedantic -std=f2008 -ffree-line-length-none"

rm test/test_intrinsic.f90
fypp -F -DTARGET_INTERFACE=1 test/test_common.fypp test/test_eq_transfer.f90
fypp -F -DTARGET_INTERFACE=2 test/test_common.fypp test/test_is_contained_by_next_out.f90
fypp -F -DTARGET_INTERFACE=3 test/test_common.fypp test/test_le_and_ge.f90
rm -rf build
fpm test --flag "-Wall -Wextra -Werror -pedantic -std=f2008 -ffree-line-length-none"
