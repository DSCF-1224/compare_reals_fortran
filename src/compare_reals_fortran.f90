module compare_reals_fortran

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_negative_inf
    use, intrinsic :: ieee_arithmetic, only: ieee_next_after
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf
    use, intrinsic :: ieee_arithmetic, only: ieee_value

    use, non_intrinsic :: ieee_class_fortran, only: is_ieee_either_zero
    use, non_intrinsic :: ieee_class_fortran, only: is_ieee_negative_inf
    use, non_intrinsic :: ieee_class_fortran, only: is_ieee_positive_inf


    implicit none


    private
    public  :: eq_transfer
    public  :: is_contained_by_next_out
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


    interface is_contained_by_next_out
        !! version: experimental
        !! return `(ieee_next_down(x) .lt. y) .and. (y .lt. ieee_next_up(x))`

        module pure elemental logical function is_contained_by_next_out_real32(x, y) result(is_equal)

            real(real32), intent(in) :: x, y

        end function is_contained_by_next_out_real32


        module pure elemental logical function is_contained_by_next_out_real64(x, y) result(is_equal)

            real(real64), intent(in) :: x, y

        end function is_contained_by_next_out_real64


        module pure elemental logical function is_contained_by_next_out_real128(x, y) result(is_equal)

            real(real128), intent(in) :: x, y

        end function is_contained_by_next_out_real128

    end interface is_contained_by_next_out


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
