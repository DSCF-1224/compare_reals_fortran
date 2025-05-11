submodule (compare_reals_fortran) compare_reals_fortran_is_contained_by_next_out

    implicit none


    contains


    module procedure is_contained_by_next_out_real32

        if      ( is_ieee_negative_inf(x) ) then ; is_equal = is_ieee_negative_inf(y)
        else if ( is_ieee_positive_inf(x) ) then ; is_equal = is_ieee_positive_inf(y)
        else

            associate( &!
                ieee_negative_inf_x => ieee_value( x = x, class = ieee_negative_inf ) , &!
                ieee_positive_inf_x => ieee_value( x = x, class = ieee_positive_inf )   &!
            )

                associate( &!
                    ieee_next_down_x => ieee_next_after( x = x, y = ieee_negative_inf_x ) , &!
                    ieee_next_up_x   => ieee_next_after( x = x, y = ieee_positive_inf_x )   &!
                )

                    is_equal =       ( ieee_next_down_x .lt. y              ) &!
                    &          .and. ( y                .lt. ieee_next_up_x )

                end associate

            end associate

        end if

    end procedure is_contained_by_next_out_real32


    module procedure is_contained_by_next_out_real64

        if      ( is_ieee_negative_inf(x) ) then ; is_equal = is_ieee_negative_inf(y)
        else if ( is_ieee_positive_inf(x) ) then ; is_equal = is_ieee_positive_inf(y)
        else

            associate( &!
                ieee_negative_inf_x => ieee_value( x = x, class = ieee_negative_inf ) , &!
                ieee_positive_inf_x => ieee_value( x = x, class = ieee_positive_inf )   &!
            )

                associate( &!
                    ieee_next_down_x => ieee_next_after( x = x, y = ieee_negative_inf_x ) , &!
                    ieee_next_up_x   => ieee_next_after( x = x, y = ieee_positive_inf_x )   &!
                )

                    is_equal =       ( ieee_next_down_x .lt. y              ) &!
                    &          .and. ( y                .lt. ieee_next_up_x )

                end associate

            end associate

        end if

    end procedure is_contained_by_next_out_real64


    module procedure is_contained_by_next_out_real128

        if      ( is_ieee_negative_inf(x) ) then ; is_equal = is_ieee_negative_inf(y)
        else if ( is_ieee_positive_inf(x) ) then ; is_equal = is_ieee_positive_inf(y)
        else

            associate( &!
                ieee_negative_inf_x => ieee_value( x = x, class = ieee_negative_inf ) , &!
                ieee_positive_inf_x => ieee_value( x = x, class = ieee_positive_inf )   &!
            )

                associate( &!
                    ieee_next_down_x => ieee_next_after( x = x, y = ieee_negative_inf_x ) , &!
                    ieee_next_up_x   => ieee_next_after( x = x, y = ieee_positive_inf_x )   &!
                )

                    is_equal =       ( ieee_next_down_x .lt. y              ) &!
                    &          .and. ( y                .lt. ieee_next_up_x )

                end associate

            end associate

        end if

    end procedure is_contained_by_next_out_real128


end submodule compare_reals_fortran_is_contained_by_next_out
