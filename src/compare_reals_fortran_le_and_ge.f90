submodule (compare_reals_fortran) compare_reals_fortran_le_and_ge

    implicit none


    contains


    module procedure le_and_ge_real32

        is_equal = (x .le. y) .and. (x .ge. y)

    end procedure le_and_ge_real32


    module procedure le_and_ge_real64

        is_equal = (x .le. y) .and. (x .ge. y)

    end procedure le_and_ge_real64


    module procedure le_and_ge_real128

        is_equal = (x .le. y) .and. (x .ge. y)

    end procedure le_and_ge_real128


end submodule compare_reals_fortran_le_and_ge
