#!/bin/sh -e
cd test
fypp -F                      compare_reals_fortran_support.fypp compare_reals_fortran_support.f90
fypp -F -DTARGET_INTERFACE=0 test_common.fypp                   test_intrinsic.f90

cd ..
rm -rf build
fpm test --flag "-Wall -Werror -pedantic -std=f2008 -ffree-line-length-none"

cd test
rm test_intrinsic.f90
fypp -F -DTARGET_INTERFACE=1 test_common.fypp eq_transfer.f90
cd ..
rm -rf build
fpm test --flag "-Wall -Wextra -Werror -pedantic -std=f2008 -ffree-line-length-none"

cd test
rm eq_transfer.f90
fypp -F -DTARGET_INTERFACE=2 test_common.fypp test_is_contained_by_next_out.f90
cd ..
rm -rf build
fpm test --flag "-Wall -Wextra -Werror -pedantic -std=f2008 -ffree-line-length-none"

cd test
rm test_is_contained_by_next_out.f90
fypp -F -DTARGET_INTERFACE=3 test_common.fypp test_le_and_ge.f90
cd ..
rm -rf build
fpm test --flag "-Wall -Wextra -Werror -pedantic -std=f2008 -ffree-line-length-none"
