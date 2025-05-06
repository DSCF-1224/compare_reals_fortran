module compare_reals_fortran

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128


    implicit none


    private
    public  :: le_and_ge


    interface le_and_ge
        !! version: experimental
        !!
        !! return `(x .le. y) .and. (x .ge. y)`

        module pure elemental logical function le_and_ge_real32(x, y) result(is_equal)

            real(real32), intent(in) :: x, y

        end function le_and_ge_real32


        module pure elemental logical function le_and_ge_real64(x, y) result(is_equal)

            real(real64), intent(in) :: x, y

        end function le_and_ge_real64


        module pure elemental logical function le_and_ge_real128(x, y) result(is_equal)

            real(real128), intent(in) :: x, y

        end function le_and_ge_real128

    end interface le_and_ge

end module compare_reals_fortran
