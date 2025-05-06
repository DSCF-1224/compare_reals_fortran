#!/bin/sh -e
fypp -F compare_reals_fortran_support.fypp compare_reals_fortran_support.f90
fypp -F test_le_and_ge.fypp                test_le_and_ge.f90
