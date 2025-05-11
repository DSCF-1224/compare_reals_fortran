program test_le_and_ge

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: compare_reals_fortran, only: le_and_ge

    use, non_intrinsic :: compare_reals_fortran_support
    use, non_intrinsic :: ieee_class_fortran


    implicit none


    call test_real32
    call test_real64
    call test_real128


    contains


    subroutine test_real32

        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_inf(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.2"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.3"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.4"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.5"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.6"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.7"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.8"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.9"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.10"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.11"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.12"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.13"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.14"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.15"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.16"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.17"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.18"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.19"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.20"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.21"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.22"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.23"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.24"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_huge(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.25"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.26"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.27"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.28"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.29"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.30"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.31"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.32"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.33"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.34"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.35"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.36"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.37"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.38"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.39"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.40"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.41"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.42"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.43"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.44"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.45"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.46"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.47"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.48"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.49"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.50"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.51"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.52"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.53"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.54"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.55"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.56"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.57"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.58"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.59"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.60"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.61"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.62"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.63"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.64"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.65"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.66"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.67"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.68"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.69"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.70"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.71"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.72"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.73"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.74"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.75"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.76"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.77"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.78"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.79"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.80"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.81"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.82"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.83"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.84"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.85"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.86"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.87"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.88"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.89"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.90"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.91"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.92"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.93"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.94"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.95"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.96"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_one(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.97"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.98"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.99"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.100"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.101"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.102"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.103"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.104"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.105"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.106"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.107"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.108"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.109"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.110"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.111"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.112"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.113"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.114"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.115"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.116"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.117"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.118"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.119"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.120"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.121"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.122"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.123"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.124"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.125"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.126"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.127"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.128"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.129"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.130"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.131"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.132"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.133"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.134"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.135"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.136"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.137"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.138"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.139"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.140"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.141"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.142"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.143"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.144"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.145"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.146"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.147"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.148"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.149"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.150"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.151"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.152"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.153"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.154"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.155"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.156"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.157"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.158"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.159"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.160"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.161"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.162"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.163"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.164"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.165"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.166"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.167"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.168"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.169"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.170"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.171"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.172"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.173"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.174"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.175"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.176"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.177"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.178"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.179"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.180"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.181"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.182"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.183"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.184"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.185"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.186"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.187"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.188"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.189"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.190"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.191"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.192"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.193"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.194"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.195"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.196"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.197"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.198"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.199"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.200"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.201"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.202"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.203"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.204"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.205"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.206"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.207"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.208"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.209"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.210"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.211"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.212"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.213"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.214"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.215"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.216"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.217"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.218"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.219"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.220"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.221"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.222"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.223"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.224"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.225"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.226"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.227"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.228"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.229"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.230"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.231"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.232"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.233"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.234"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.235"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.236"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.237"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.238"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.239"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.240"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.241"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.242"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.243"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.244"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.245"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.246"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.247"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.248"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.249"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.250"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.251"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.252"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.253"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.254"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.255"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.256"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.257"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.258"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.259"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.260"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.261"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.262"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.263"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.264"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.265"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.266"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.267"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.268"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.269"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.270"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.271"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.272"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.273"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.274"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.275"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.276"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.277"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.278"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.279"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.280"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.281"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.282"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.283"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.284"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.285"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.286"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.287"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.288"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.289"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.290"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.291"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.292"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.293"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.294"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.295"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.296"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.297"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.298"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.299"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.300"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.301"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.302"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.303"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.304"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.305"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.306"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.307"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.308"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.309"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.310"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.311"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.312"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.313"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.314"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.315"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.316"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.317"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.318"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.319"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.320"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.321"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.322"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.323"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.324"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.325"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.326"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.327"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.328"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.329"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.330"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.331"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.332"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.333"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.334"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.335"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.336"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.337"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.338"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.339"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.340"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.341"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.342"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.343"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.344"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.345"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.346"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.347"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.348"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.349"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.350"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.351"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.352"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.353"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.354"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.355"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.356"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.357"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.358"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.359"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.360"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.361"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.362"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.363"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.364"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.365"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.366"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.367"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.368"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.369"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.370"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.371"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.372"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.373"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.374"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.375"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.376"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.377"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.378"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.379"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.380"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.381"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.382"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.383"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.384"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.385"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.386"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.387"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.388"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.389"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.390"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.391"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.392"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.393"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.394"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.395"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.396"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.397"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.398"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.399"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.400"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.401"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.402"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.403"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.404"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.405"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.406"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.407"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.408"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.409"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.410"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.411"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.412"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.413"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.414"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.415"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.416"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.417"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.418"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.419"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.420"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.421"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.422"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.423"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.424"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.425"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.426"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.427"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.428"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.429"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.430"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.431"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.432"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_epsilon(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.433"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.434"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.435"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.436"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.437"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.438"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.439"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.440"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.441"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.442"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.443"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.444"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.445"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.446"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.447"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.448"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.449"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.450"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.451"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.452"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.453"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.454"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.455"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.456"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_one(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.457"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.458"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.459"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.460"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.461"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.462"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.463"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.464"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.465"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.466"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.467"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.468"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.469"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.470"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.471"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.472"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.473"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.474"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.475"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.476"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.477"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.478"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.479"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.480"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.481"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.482"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.483"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.484"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.485"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.486"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.487"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.488"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.489"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.490"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.491"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.492"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.493"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.494"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.495"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.496"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.497"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.498"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.499"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.500"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.501"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.502"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.503"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.504"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_huge(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.505"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.506"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.507"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.508"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.509"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.510"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.511"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.512"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.513"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.514"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.515"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.516"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.517"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.518"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.519"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.520"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.521"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.522"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.523"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.524"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.525"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.526"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.527"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.528"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_inf(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.529"
            end if

        end block

    end subroutine test_real32



    subroutine test_real64

        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_inf(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.530"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.531"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.532"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.533"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.534"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.535"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.536"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.537"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.538"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.539"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.540"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.541"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.542"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.543"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.544"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.545"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.546"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.547"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.548"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.549"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.550"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.551"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.552"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.553"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_huge(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.554"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.555"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.556"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.557"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.558"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.559"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.560"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.561"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.562"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.563"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.564"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.565"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.566"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.567"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.568"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.569"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.570"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.571"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.572"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.573"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.574"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.575"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.576"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.577"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.578"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.579"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.580"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.581"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.582"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.583"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.584"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.585"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.586"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.587"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.588"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.589"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.590"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.591"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.592"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.593"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.594"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.595"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.596"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.597"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.598"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.599"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.600"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.601"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.602"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.603"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.604"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.605"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.606"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.607"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.608"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.609"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.610"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.611"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.612"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.613"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.614"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.615"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.616"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.617"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.618"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.619"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.620"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.621"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.622"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.623"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.624"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.625"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_one(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.626"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.627"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.628"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.629"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.630"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.631"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.632"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.633"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.634"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.635"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.636"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.637"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.638"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.639"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.640"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.641"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.642"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.643"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.644"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.645"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.646"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.647"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.648"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.649"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.650"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.651"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.652"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.653"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.654"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.655"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.656"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.657"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.658"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.659"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.660"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.661"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.662"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.663"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.664"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.665"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.666"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.667"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.668"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.669"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.670"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.671"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.672"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.673"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.674"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.675"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.676"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.677"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.678"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.679"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.680"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.681"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.682"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.683"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.684"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.685"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.686"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.687"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.688"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.689"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.690"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.691"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.692"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.693"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.694"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.695"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.696"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.697"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.698"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.699"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.700"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.701"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.702"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.703"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.704"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.705"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.706"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.707"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.708"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.709"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.710"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.711"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.712"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.713"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.714"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.715"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.716"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.717"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.718"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.719"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.720"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.721"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.722"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.723"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.724"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.725"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.726"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.727"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.728"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.729"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.730"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.731"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.732"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.733"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.734"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.735"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.736"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.737"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.738"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.739"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.740"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.741"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.742"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.743"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.744"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.745"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.746"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.747"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.748"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.749"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.750"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.751"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.752"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.753"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.754"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.755"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.756"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.757"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.758"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.759"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.760"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.761"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.762"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.763"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.764"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.765"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.766"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.767"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.768"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.769"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.770"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.771"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.772"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.773"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.774"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.775"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.776"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.777"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.778"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.779"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.780"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.781"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.782"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.783"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.784"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.785"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.786"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.787"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.788"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.789"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.790"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.791"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.792"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.793"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.794"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.795"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.796"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.797"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.798"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.799"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.800"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.801"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.802"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.803"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.804"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.805"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.806"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.807"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.808"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.809"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.810"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.811"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.812"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.813"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.814"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.815"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.816"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.817"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.818"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.819"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.820"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.821"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.822"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.823"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.824"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.825"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.826"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.827"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.828"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.829"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.830"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.831"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.832"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.833"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.834"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.835"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.836"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.837"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.838"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.839"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.840"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.841"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.842"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.843"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.844"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.845"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.846"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.847"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.848"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.849"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.850"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.851"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.852"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.853"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.854"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.855"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.856"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.857"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.858"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.859"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.860"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.861"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.862"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.863"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.864"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.865"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.866"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.867"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.868"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.869"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.870"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.871"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.872"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.873"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.874"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.875"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.876"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.877"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.878"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.879"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.880"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.881"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.882"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.883"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.884"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.885"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.886"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.887"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.888"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.889"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.890"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.891"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.892"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.893"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.894"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.895"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.896"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.897"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.898"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.899"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.900"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.901"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.902"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.903"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.904"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.905"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.906"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.907"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.908"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.909"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.910"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.911"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.912"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.913"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.914"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.915"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.916"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.917"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.918"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.919"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.920"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.921"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.922"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.923"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.924"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.925"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.926"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.927"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.928"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.929"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.930"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.931"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.932"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.933"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.934"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.935"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.936"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.937"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.938"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.939"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.940"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.941"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.942"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.943"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.944"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.945"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.946"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.947"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.948"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.949"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.950"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.951"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.952"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.953"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.954"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.955"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.956"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.957"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.958"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.959"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.960"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.961"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_epsilon(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.962"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.963"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.964"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.965"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.966"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.967"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.968"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.969"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.970"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.971"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.972"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.973"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.974"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.975"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.976"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.977"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.978"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.979"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.980"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.981"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.982"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.983"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.984"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.985"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_one(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.986"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.987"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.988"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.989"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.990"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.991"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.992"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.993"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.994"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.995"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.996"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.997"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.998"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.999"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1000"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1001"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1002"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1003"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1004"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1005"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1006"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1007"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1008"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1009"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1010"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1011"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1012"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1013"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1014"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1015"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1016"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1017"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1018"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1019"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1020"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1021"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1022"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1023"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1024"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1025"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1026"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1027"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1028"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1029"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1030"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1031"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1032"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1033"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_huge(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1034"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1035"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1036"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1037"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1038"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1039"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1040"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1041"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1042"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1043"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1044"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1045"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1046"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1047"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1048"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1049"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1050"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1051"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1052"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1053"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1054"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1055"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1056"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1057"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_inf(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1058"
            end if

        end block

    end subroutine test_real64



    subroutine test_real128

        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_inf(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1059"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1060"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1061"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1062"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1063"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1064"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1065"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1066"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1067"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1068"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1069"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1070"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1071"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1072"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1073"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1074"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1075"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1076"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1077"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1078"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1079"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1080"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1081"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1082"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_huge(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1083"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1084"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1085"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1086"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1087"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1088"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1089"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1090"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1091"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1092"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1093"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1094"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1095"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1096"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1097"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1098"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1099"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1100"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1101"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1102"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1103"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1104"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1105"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1106"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1107"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1108"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1109"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1110"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1111"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1112"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1113"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1114"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1115"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1116"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1117"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1118"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1119"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1120"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1121"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1122"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1123"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1124"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1125"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1126"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1127"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1128"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1129"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1130"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1131"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1132"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1133"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1134"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1135"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1136"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1137"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1138"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1139"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1140"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1141"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1142"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1143"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1144"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1145"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1146"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1147"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1148"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1149"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1150"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1151"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1152"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1153"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1154"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_one(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1155"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1156"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1157"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1158"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1159"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1160"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1161"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1162"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1163"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1164"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1165"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1166"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1167"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1168"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1169"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1170"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1171"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1172"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1173"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1174"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1175"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1176"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1177"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1178"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1179"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1180"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1181"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1182"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1183"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1184"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1185"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1186"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1187"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1188"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1189"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1190"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1191"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1192"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1193"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1194"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1195"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1196"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1197"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1198"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1199"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1200"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1201"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1202"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1203"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1204"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1205"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1206"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1207"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1208"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1209"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1210"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1211"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1212"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1213"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1214"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1215"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1216"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1217"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1218"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1219"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1220"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1221"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1222"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1223"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1224"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1225"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1226"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1227"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1228"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1229"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1230"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1231"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1232"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1233"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1234"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1235"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1236"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1237"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1238"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1239"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1240"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1241"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1242"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1243"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1244"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1245"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1246"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1247"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1248"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1249"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1250"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1251"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1252"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1253"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1254"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1255"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1256"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1257"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1258"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1259"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1260"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1261"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1262"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1263"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1264"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1265"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1266"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1267"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1268"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1269"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1270"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1271"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1272"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1273"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1274"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1275"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1276"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1277"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1278"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1279"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1280"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1281"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1282"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1283"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1284"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1285"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1286"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1287"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1288"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1289"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1290"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1291"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1292"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1293"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1294"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1295"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1296"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1297"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1298"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1299"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1300"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1301"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1302"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1303"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1304"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1305"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1306"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1307"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1308"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1309"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1310"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1311"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1312"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1313"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1314"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1315"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1316"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1317"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1318"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1319"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1320"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1321"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1322"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1323"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1324"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1325"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1326"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1327"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1328"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1329"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1330"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1331"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1332"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1333"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1334"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1335"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1336"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1337"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1338"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1339"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1340"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1341"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1342"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1343"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1344"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1345"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1346"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1347"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1348"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1349"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1350"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1351"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1352"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1353"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1354"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1355"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1356"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1357"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1358"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1359"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1360"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1361"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1362"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1363"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1364"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1365"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1366"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1367"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1368"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1369"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1370"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1371"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1372"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1373"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1374"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1375"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1376"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1377"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1378"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1379"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1380"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1381"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1382"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1383"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1384"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1385"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1386"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1387"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1388"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1389"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1390"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1391"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1392"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1393"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1394"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1395"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1396"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1397"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1398"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1399"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1400"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1401"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1402"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1403"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1404"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1405"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1406"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1407"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1408"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1409"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1410"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1411"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1412"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1413"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1414"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1415"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1416"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1417"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1418"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1419"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1420"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1421"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1422"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1423"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1424"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1425"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1426"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1427"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1428"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1429"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1430"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1431"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1432"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1433"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1434"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1435"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1436"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1437"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1438"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1439"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1440"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1441"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1442"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1443"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1444"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1445"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1446"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1447"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1448"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1449"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1450"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1451"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1452"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1453"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1454"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1455"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1456"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1457"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1458"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1459"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1460"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1461"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1462"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1463"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1464"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1465"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1466"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1467"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1468"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1469"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1470"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1471"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1472"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1473"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1474"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1475"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1476"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1477"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1478"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1479"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1480"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1481"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1482"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1483"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1484"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1485"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1486"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1487"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1488"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1489"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1490"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_epsilon(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1491"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1492"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1493"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1494"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1495"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1496"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1497"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1498"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1499"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1500"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1501"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1502"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1503"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1504"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1505"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1506"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1507"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1508"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1509"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1510"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1511"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1512"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1513"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1514"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_one(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1515"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1516"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1517"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1518"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1519"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1520"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1521"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1522"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1523"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1524"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1525"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1526"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1527"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1528"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1529"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1530"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1531"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1532"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1533"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1534"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1535"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1536"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1537"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1538"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge_next_down(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1539"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1540"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1541"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1542"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1543"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1544"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1545"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1546"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1547"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1548"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1549"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1550"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1551"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1552"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1553"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1554"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1555"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1556"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1557"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1558"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1559"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1560"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1561"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1562"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_huge(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1563"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1564"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1565"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1566"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1567"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1568"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1569"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1570"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1571"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1572"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1573"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1574"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1575"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1576"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1577"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1578"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1579"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1580"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1581"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1582"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1583"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1584"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1585"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1586"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_inf(y)

            if ( .not. le_and_ge(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1587"
            end if

        end block

    end subroutine test_real128

end program test_le_and_ge
