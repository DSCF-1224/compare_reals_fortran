module compare_reals_fortran

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: ieee_class_fortran, only: is_ieee_either_zero


    implicit none


    private
    public  :: eq_transfer
    public  :: le_and_ge


    interface eq_transfer
        !! version: experimental
        !! return `all( transfer( x, 1, storage_size(x)/storage_size(1) ) .eq. transfer( y, 1, storage_size(x)/storage_size(1) ) )`

        module pure elemental logical function eq_transfer_real32(x, y) result(is_equal)

            real(real32), intent(in) :: x, y

        end function eq_transfer_real32


        module pure elemental logical function eq_transfer_real64(x, y) result(is_equal)

            real(real64), intent(in) :: x, y

        end function eq_transfer_real64


        module pure elemental logical function eq_transfer_real128(x, y) result(is_equal)

            real(real128), intent(in) :: x, y

        end function eq_transfer_real128

    end interface eq_transfer


    interface le_and_ge
        !! version: experimental
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
