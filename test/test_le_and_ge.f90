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

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.2"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.3"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.4"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.5"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.6"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.7"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.8"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.9"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.10"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.11"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.12"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.13"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.14"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.15"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.16"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.17"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.18"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.19"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.20"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.21"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.22"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.23"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.24"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_huge(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.25"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.26"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.27"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.28"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.29"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.30"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.31"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.32"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.33"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.34"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.35"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.36"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.37"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.38"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.39"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.40"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.41"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.42"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.43"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.44"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.45"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.46"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.47"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.48"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.49"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.50"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.51"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.52"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.53"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.54"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.55"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.56"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.57"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.58"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.59"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.60"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.61"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.62"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.63"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.64"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.65"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.66"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.67"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.68"

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.69"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.70"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.71"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.72"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.73"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.74"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.75"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.76"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.77"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.78"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.79"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.80"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.81"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.82"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.83"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.84"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.85"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.86"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.87"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.88"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.89"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.90"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.91"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.92"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.93"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.94"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.95"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.96"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_one(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.97"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.98"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.99"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.100"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.101"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.102"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.103"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.104"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.105"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.106"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.107"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.108"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.109"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.110"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.111"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.112"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.113"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.114"

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.115"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.116"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.117"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.118"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.119"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.120"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.121"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.122"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.123"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.124"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.125"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.126"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.127"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.128"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.129"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.130"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.131"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.132"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.133"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.134"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.135"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.136"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.137"

        end block


        block

            real(real32) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.138"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.139"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.140"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.141"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.142"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.143"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.144"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.145"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.146"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.147"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.148"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.149"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.150"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.151"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.152"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.153"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.154"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.155"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.156"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.157"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.158"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.159"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.160"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.161"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.162"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.163"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.164"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.165"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.166"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.167"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.168"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.169"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.170"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.171"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.172"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.173"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.174"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.175"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.176"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.177"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.178"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.179"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.180"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.181"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.182"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.183"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.184"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.185"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.186"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.187"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.188"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.189"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.190"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.191"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.192"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.193"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.194"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.195"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.196"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.197"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.198"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.199"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.200"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.201"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.202"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.203"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.204"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.205"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.206"

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.207"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.208"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.209"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.210"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.211"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.212"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.213"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.214"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.215"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.216"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.217"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.218"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.219"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.220"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.221"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.222"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.223"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.224"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.225"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.226"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.227"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.228"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.229"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.230"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.231"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.232"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.233"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.234"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.235"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.236"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.237"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.238"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.239"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.240"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.241"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.242"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.243"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.244"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.245"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.246"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.247"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.248"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.249"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.250"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.251"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.252"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.253"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.254"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.255"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.256"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.257"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.258"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.259"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.260"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.261"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.262"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.263"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.264"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.265"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.266"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.267"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.268"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.269"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.270"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.271"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.272"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.273"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.274"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.275"

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.276"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.277"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.278"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.279"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.280"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.281"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.282"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.283"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.284"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.285"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.286"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.287"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.288"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.289"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.290"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.291"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.292"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.293"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.294"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.295"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.296"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.297"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.298"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.299"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.300"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.301"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.302"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.303"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.304"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.305"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.306"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.307"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.308"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.309"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.310"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.311"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.312"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.313"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.314"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.315"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.316"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.317"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.318"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.319"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.320"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.321"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.322"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.323"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.324"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.325"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.326"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.327"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.328"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.329"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.330"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.331"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.332"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.333"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.334"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.335"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.336"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.337"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.338"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.339"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.340"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.341"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.342"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.343"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.344"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.345"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.346"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.347"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.348"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.349"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.350"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.351"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.352"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.353"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.354"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.355"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.356"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.357"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.358"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.359"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.360"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.361"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.362"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.363"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.364"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.365"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.366"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.367"

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.368"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.369"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.370"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.371"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.372"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.373"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.374"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.375"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.376"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.377"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.378"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.379"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.380"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.381"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.382"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.383"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.384"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.385"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.386"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.387"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.388"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.389"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.390"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.391"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.392"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.393"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.394"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.395"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.396"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.397"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.398"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.399"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.400"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.401"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.402"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.403"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.404"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.405"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.406"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.407"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.408"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.409"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.410"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.411"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.412"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.413"

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.414"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.415"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.416"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.417"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.418"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.419"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.420"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.421"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.422"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.423"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.424"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.425"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.426"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.427"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.428"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.429"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.430"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.431"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.432"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_epsilon(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.433"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.434"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.435"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.436"

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.437"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.438"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.439"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.440"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.441"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.442"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.443"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.444"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.445"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.446"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.447"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.448"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.449"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.450"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.451"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.452"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.453"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.454"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.455"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.456"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_one(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.457"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.458"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.459"

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.460"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.461"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.462"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.463"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.464"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.465"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.466"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.467"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.468"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.469"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.470"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.471"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.472"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.473"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.474"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.475"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.476"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.477"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.478"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.479"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.480"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.481"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.482"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.483"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.484"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.485"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.486"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.487"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.488"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.489"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.490"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.491"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.492"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.493"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.494"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.495"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.496"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.497"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.498"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.499"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.500"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.501"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.502"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.503"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.504"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_huge(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.505"

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.506"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.507"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.508"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.509"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.510"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.511"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.512"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.513"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.514"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.515"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.516"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.517"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.518"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.519"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.520"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.521"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.522"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.523"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.524"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.525"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.526"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.527"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.528"

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_inf(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.529"

        end block

    end subroutine test_real32



    subroutine test_real64

        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_inf(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.530"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.531"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.532"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.533"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.534"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.535"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.536"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.537"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.538"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.539"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.540"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.541"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.542"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.543"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.544"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.545"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.546"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.547"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.548"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.549"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.550"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.551"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.552"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.553"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_huge(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.554"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.555"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.556"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.557"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.558"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.559"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.560"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.561"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.562"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.563"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.564"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.565"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.566"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.567"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.568"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.569"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.570"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.571"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.572"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.573"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.574"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.575"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.576"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.577"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.578"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.579"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.580"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.581"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.582"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.583"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.584"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.585"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.586"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.587"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.588"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.589"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.590"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.591"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.592"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.593"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.594"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.595"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.596"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.597"

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.598"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.599"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.600"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.601"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.602"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.603"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.604"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.605"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.606"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.607"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.608"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.609"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.610"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.611"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.612"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.613"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.614"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.615"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.616"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.617"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.618"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.619"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.620"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.621"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.622"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.623"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.624"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.625"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_one(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.626"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.627"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.628"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.629"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.630"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.631"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.632"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.633"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.634"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.635"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.636"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.637"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.638"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.639"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.640"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.641"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.642"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.643"

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.644"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.645"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.646"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.647"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.648"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.649"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.650"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.651"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.652"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.653"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.654"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.655"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.656"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.657"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.658"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.659"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.660"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.661"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.662"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.663"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.664"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.665"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.666"

        end block


        block

            real(real64) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.667"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.668"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.669"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.670"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.671"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.672"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.673"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.674"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.675"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.676"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.677"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.678"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.679"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.680"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.681"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.682"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.683"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.684"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.685"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.686"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.687"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.688"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.689"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.690"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.691"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.692"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.693"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.694"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.695"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.696"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.697"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.698"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.699"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.700"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.701"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.702"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.703"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.704"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.705"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.706"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.707"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.708"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.709"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.710"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.711"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.712"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.713"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.714"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.715"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.716"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.717"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.718"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.719"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.720"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.721"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.722"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.723"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.724"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.725"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.726"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.727"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.728"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.729"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.730"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.731"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.732"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.733"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.734"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.735"

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.736"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.737"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.738"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.739"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.740"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.741"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.742"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.743"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.744"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.745"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.746"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.747"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.748"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.749"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.750"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.751"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.752"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.753"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.754"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.755"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.756"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.757"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.758"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.759"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.760"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.761"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.762"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.763"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.764"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.765"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.766"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.767"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.768"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.769"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.770"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.771"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.772"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.773"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.774"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.775"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.776"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.777"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.778"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.779"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.780"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.781"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.782"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.783"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.784"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.785"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.786"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.787"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.788"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.789"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.790"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.791"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.792"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.793"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.794"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.795"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.796"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.797"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.798"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.799"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.800"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.801"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.802"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.803"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.804"

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.805"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.806"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.807"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.808"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.809"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.810"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.811"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.812"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.813"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.814"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.815"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.816"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.817"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.818"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.819"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.820"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.821"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.822"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.823"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.824"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.825"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.826"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.827"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.828"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.829"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.830"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.831"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.832"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.833"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.834"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.835"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.836"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.837"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.838"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.839"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.840"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.841"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.842"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.843"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.844"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.845"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.846"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.847"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.848"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.849"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.850"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.851"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.852"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.853"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.854"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.855"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.856"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.857"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.858"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.859"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.860"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.861"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.862"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.863"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.864"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.865"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.866"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.867"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.868"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.869"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.870"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.871"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.872"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.873"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.874"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.875"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.876"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.877"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.878"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.879"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.880"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.881"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.882"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.883"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.884"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.885"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.886"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.887"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.888"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.889"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.890"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.891"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.892"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.893"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.894"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.895"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.896"

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.897"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.898"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.899"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.900"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.901"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.902"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.903"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.904"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.905"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.906"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.907"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.908"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.909"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.910"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.911"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.912"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.913"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.914"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.915"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.916"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.917"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.918"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.919"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.920"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.921"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.922"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.923"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.924"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.925"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.926"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.927"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.928"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.929"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.930"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.931"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.932"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.933"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.934"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.935"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.936"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.937"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.938"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.939"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.940"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.941"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.942"

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.943"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.944"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.945"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.946"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.947"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.948"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.949"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.950"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.951"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.952"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.953"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.954"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.955"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.956"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.957"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.958"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.959"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.960"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.961"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_epsilon(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.962"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.963"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.964"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.965"

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.966"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.967"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.968"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.969"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.970"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.971"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.972"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.973"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.974"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.975"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.976"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.977"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.978"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.979"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.980"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.981"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.982"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.983"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.984"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.985"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_one(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.986"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.987"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.988"

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.989"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.990"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.991"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.992"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.993"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.994"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.995"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.996"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.997"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.998"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.999"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1000"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1001"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1002"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1003"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1004"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1005"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1006"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1007"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1008"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1009"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1010"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1011"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1012"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1013"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1014"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1015"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1016"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1017"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1018"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1019"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1020"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1021"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1022"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1023"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1024"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1025"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1026"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1027"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1028"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1029"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1030"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1031"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1032"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1033"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_huge(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1034"

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1035"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1036"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1037"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1038"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1039"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1040"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1041"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1042"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1043"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1044"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1045"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1046"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1047"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1048"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1049"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1050"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1051"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1052"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1053"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1054"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1055"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1056"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1057"

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_inf(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1058"

        end block

    end subroutine test_real64



    subroutine test_real128

        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_inf(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1059"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1060"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1061"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1062"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1063"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1064"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1065"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1066"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1067"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1068"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1069"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1070"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1071"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1072"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1073"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1074"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1075"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1076"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1077"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1078"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1079"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1080"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1081"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1082"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_huge(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1083"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1084"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1085"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1086"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1087"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1088"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1089"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1090"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1091"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1092"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1093"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1094"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1095"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1096"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1097"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1098"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1099"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1100"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1101"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1102"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1103"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1104"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1105"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1106"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1107"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1108"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1109"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1110"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1111"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1112"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1113"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1114"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1115"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1116"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1117"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1118"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1119"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1120"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1121"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1122"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1123"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1124"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1125"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1126"

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1127"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1128"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1129"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1130"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1131"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1132"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1133"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1134"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1135"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1136"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1137"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1138"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1139"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1140"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1141"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1142"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1143"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1144"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1145"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1146"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1147"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1148"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1149"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1150"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1151"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1152"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1153"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1154"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_one(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1155"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1156"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1157"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1158"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1159"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1160"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1161"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1162"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1163"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1164"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1165"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1166"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1167"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1168"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1169"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1170"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1171"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1172"

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1173"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1174"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1175"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1176"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1177"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1178"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_one_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1179"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1180"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1181"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1182"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1183"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1184"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1185"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1186"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1187"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1188"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1189"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1190"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1191"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1192"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1193"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1194"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1195"

        end block


        block

            real(real128) :: x, y

            call set_negative_one_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1196"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1197"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1198"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1199"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1200"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1201"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1202"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1203"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1204"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1205"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1206"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1207"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1208"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1209"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1210"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1211"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1212"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1213"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1214"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1215"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1216"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1217"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1218"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1219"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1220"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1221"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1222"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1223"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1224"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1225"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1226"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1227"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1228"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1229"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1230"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1231"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1232"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1233"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1234"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1235"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1236"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1237"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1238"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1239"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1240"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1241"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1242"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1243"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1244"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1245"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1246"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1247"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1248"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1249"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1250"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1251"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1252"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1253"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1254"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1255"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1256"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1257"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1258"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1259"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1260"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1261"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1262"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1263"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1264"

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1265"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1266"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1267"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1268"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1269"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1270"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1271"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1272"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1273"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1274"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1275"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1276"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1277"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1278"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1279"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1280"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1281"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1282"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1283"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1284"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1285"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1286"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1287"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1288"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1289"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1290"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1291"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1292"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1293"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1294"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1295"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1296"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1297"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1298"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1299"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1300"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1301"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1302"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1303"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1304"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1305"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1306"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1307"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1308"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1309"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1310"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1311"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1312"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1313"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1314"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1315"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1316"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1317"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1318"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1319"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1320"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1321"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1322"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1323"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1324"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1325"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1326"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1327"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1328"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1329"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1330"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1331"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1332"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1333"

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1334"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1335"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1336"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1337"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1338"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1339"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1340"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1341"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1342"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1343"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1344"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1345"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1346"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1347"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1348"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1349"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1350"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1351"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1352"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1353"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1354"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1355"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1356"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1357"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1358"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1359"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1360"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1361"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1362"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1363"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1364"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1365"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1366"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1367"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1368"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1369"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1370"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1371"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1372"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1373"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1374"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1375"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1376"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1377"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1378"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1379"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1380"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1381"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1382"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1383"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1384"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1385"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1386"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1387"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1388"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1389"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1390"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1391"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1392"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1393"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1394"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny_next_up(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1395"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1396"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1397"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1398"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1399"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1400"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1401"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1402"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny_next_up(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1403"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1404"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1405"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1406"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1407"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1408"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1409"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1410"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1411"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1412"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1413"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1414"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1415"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1416"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1417"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1418"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1419"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1420"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1421"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1422"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1423"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1424"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1425"

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1426"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1427"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1428"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1429"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1430"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1431"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1432"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1433"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1434"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1435"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1436"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1437"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1438"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1439"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1440"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1441"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1442"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1443"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1444"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1445"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1446"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1447"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1448"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1449"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1450"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1451"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1452"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1453"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1454"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1455"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1456"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1457"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1458"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1459"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1460"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1461"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1462"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1463"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1464"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1465"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1466"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1467"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1468"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1469"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1470"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1471"

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1472"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1473"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1474"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1475"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1476"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1477"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1478"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1479"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1480"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1481"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1482"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1483"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1484"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1485"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1486"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1487"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1488"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1489"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1490"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_epsilon(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1491"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1492"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1493"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1494"

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1495"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1496"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1497"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1498"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1499"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1500"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1501"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1502"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1503"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1504"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1505"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1506"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1507"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1508"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1509"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1510"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1511"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1512"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1513"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1514"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_one(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1515"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1516"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1517"

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1518"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1519"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1520"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1521"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1522"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1523"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1524"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1525"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1526"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1527"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1528"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1529"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1530"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1531"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1532"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1533"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1534"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1535"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1536"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1537"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1538"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge_next_down(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1539"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1540"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1541"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1542"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1543"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1544"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1545"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1546"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1547"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1548"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1549"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1550"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1551"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1552"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1553"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1554"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1555"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1556"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1557"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1558"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1559"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1560"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1561"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1562"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_huge(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1563"

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1564"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_inf(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1565"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1566"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1567"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1568"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1569"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1570"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1571"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1572"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1573"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1574"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1575"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1576"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1577"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1578"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny_next_up(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1579"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1580"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_zero(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1581"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1582"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_epsilon(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1583"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_one(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1584"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge_next_down(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1585"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge(y)

            if ( le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_le_and_ge.fypp No.1586"

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_inf(y)

            if ( .not. le_and_ge(x,y) ) error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_le_and_ge.fypp No.1587"

        end block

    end subroutine test_real128

end program test_le_and_ge
