submodule (compare_reals_fortran) compare_reals_fortran_eq_transfer

    implicit none


    contains


    module procedure eq_transfer_real32

        if ( ieee_unordered(x,y) ) then
            is_equal = .false.
            return
        end if

        is_equal = is_ieee_either_zero(x) .and. is_ieee_either_zero(y)

        if (is_equal) return

        associate( &!
        &   storage_size_int  => storage_size(1) , &!
        &   storage_size_real => storage_size(x)   &!
        )

            associate( array_size => storage_size_real / storage_size_int )

                associate( &!
                &   x_transferred => transfer( source = x, mold = 1, size = array_size ) , &!
                &   y_transferred => transfer( source = y, mold = 1, size = array_size )   &!
                )

                    is_equal = all( x_transferred(:) .eq. y_transferred(:) )

                end associate

            end associate

        end associate

    end procedure eq_transfer_real32


    module procedure eq_transfer_real64

        if ( ieee_unordered(x,y) ) then
            is_equal = .false.
            return
        end if

        is_equal = is_ieee_either_zero(x) .and. is_ieee_either_zero(y)

        if (is_equal) return

        associate( &!
        &   storage_size_int  => storage_size(1) , &!
        &   storage_size_real => storage_size(x)   &!
        )

            associate( array_size => storage_size_real / storage_size_int )

                associate( &!
                &   x_transferred => transfer( source = x, mold = 1, size = array_size ) , &!
                &   y_transferred => transfer( source = y, mold = 1, size = array_size )   &!
                )

                    is_equal = all( x_transferred(:) .eq. y_transferred(:) )

                end associate

            end associate

        end associate

    end procedure eq_transfer_real64


    module procedure eq_transfer_real128

        if ( ieee_unordered(x,y) ) then
            is_equal = .false.
            return
        end if

        is_equal = is_ieee_either_zero(x) .and. is_ieee_either_zero(y)

        if (is_equal) return

        associate( &!
        &   storage_size_int  => storage_size(1) , &!
        &   storage_size_real => storage_size(x)   &!
        )

            associate( array_size => storage_size_real / storage_size_int )

                associate( &!
                &   x_transferred => transfer( source = x, mold = 1, size = array_size ) , &!
                &   y_transferred => transfer( source = y, mold = 1, size = array_size )   &!
                )

                    is_equal = all( x_transferred(:) .eq. y_transferred(:) )

                end associate

            end associate

        end associate

    end procedure eq_transfer_real128


end submodule compare_reals_fortran_eq_transfer
