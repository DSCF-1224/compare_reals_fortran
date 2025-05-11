#!/bin/sh -e
fypp -F compare_reals_fortran.fypp                          compare_reals_fortran.f90
fypp -F compare_reals_fortran_eq_transfer.fypp              compare_reals_fortran_eq_transfer.f90
fypp -F compare_reals_fortran_is_contained_by_next_out.fypp compare_reals_fortran_is_contained_by_next_out.f90
fypp -F compare_reals_fortran_le_and_ge.fypp                compare_reals_fortran_le_and_ge.f90
