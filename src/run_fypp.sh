#!/bin/sh -e
fypp -F compare_reals_fortran.fypp           compare_reals_fortran.f90
fypp -F compare_reals_fortran_le_and_ge.fypp compare_reals_fortran_le_and_ge.f90
