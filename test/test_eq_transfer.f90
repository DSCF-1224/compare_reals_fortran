program test_eq_transfer

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: compare_reals_fortran, only: eq_transfer

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

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.1"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.2"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.3"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.4"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.5"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.6"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.7"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.8"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.9"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.10"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.11"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.12"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.13"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.14"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.15"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_huge(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.16"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.17"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.18"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.19"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.20"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.21"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.22"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.23"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.24"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.25"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.26"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.27"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.28"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.29"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.30"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge_next_up(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.31"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.32"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.33"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.34"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.35"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.36"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.37"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.38"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.39"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.40"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.41"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.42"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.43"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.44"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.45"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_one(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.46"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.47"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.48"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.49"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.50"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.51"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.52"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.53"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.54"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.55"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.56"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.57"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.58"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.59"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.60"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.61"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.62"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.63"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.64"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.65"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.66"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.67"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.68"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.69"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.70"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.71"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.72"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.73"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.74"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.75"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.76"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.77"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.78"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.79"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.80"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.81"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.82"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.83"
            end if

        end block


        block

            real(real32) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.84"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.85"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.86"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.87"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.88"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.89"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.90"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.91"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.92"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.93"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.94"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.95"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.96"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.97"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.98"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.99"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.100"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.101"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.102"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.103"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.104"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.105"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.106"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.107"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.108"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.109"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.110"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.111"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.112"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.113"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.114"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.115"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.116"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.117"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.118"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.119"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.120"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.121"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.122"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.123"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.124"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.125"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.126"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.127"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.128"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.129"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.130"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.131"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.132"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.133"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.134"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.135"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_epsilon(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.136"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.137"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.138"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.139"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.140"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.141"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.142"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.143"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.144"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.145"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.146"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.147"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.148"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.149"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.150"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_one(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.151"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.152"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.153"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.154"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.155"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.156"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.157"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.158"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.159"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.160"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.161"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.162"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.163"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.164"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.165"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge_next_down(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.166"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.167"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.168"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.169"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.170"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.171"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.172"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.173"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.174"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.175"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.176"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.177"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.178"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.179"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.180"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_positive_huge(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.181"
            end if

        end block


        block

            real(real32) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.182"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.183"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.184"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.185"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.186"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.187"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.188"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.189"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.190"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.191"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.192"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.193"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.194"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.195"
            end if

        end block


        block

            real(real32) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_inf(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.196"
            end if

        end block

    end subroutine test_real32



    subroutine test_real64

        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_inf(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.197"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.198"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.199"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.200"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.201"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.202"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.203"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.204"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.205"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.206"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.207"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.208"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.209"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.210"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.211"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_huge(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.212"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.213"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.214"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.215"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.216"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.217"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.218"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.219"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.220"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.221"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.222"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.223"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.224"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.225"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.226"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge_next_up(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.227"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.228"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.229"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.230"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.231"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.232"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.233"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.234"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.235"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.236"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.237"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.238"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.239"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.240"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.241"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_one(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.242"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.243"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.244"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.245"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.246"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.247"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.248"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.249"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.250"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.251"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.252"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.253"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.254"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.255"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.256"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.257"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.258"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.259"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.260"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.261"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.262"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.263"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.264"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.265"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.266"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.267"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.268"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.269"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.270"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.271"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.272"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.273"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.274"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.275"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.276"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.277"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.278"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.279"
            end if

        end block


        block

            real(real64) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.280"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.281"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.282"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.283"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.284"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.285"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.286"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.287"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.288"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.289"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.290"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.291"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.292"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.293"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.294"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.295"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.296"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.297"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.298"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.299"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.300"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.301"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.302"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.303"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.304"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.305"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.306"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.307"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.308"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.309"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.310"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.311"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.312"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.313"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.314"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.315"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.316"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.317"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.318"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.319"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.320"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.321"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.322"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.323"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.324"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.325"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.326"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.327"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.328"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.329"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.330"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.331"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_epsilon(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.332"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.333"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.334"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.335"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.336"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.337"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.338"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.339"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.340"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.341"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.342"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.343"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.344"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.345"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.346"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_one(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.347"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.348"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.349"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.350"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.351"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.352"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.353"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.354"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.355"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.356"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.357"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.358"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.359"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.360"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.361"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge_next_down(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.362"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.363"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.364"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.365"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.366"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.367"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.368"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.369"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.370"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.371"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.372"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.373"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.374"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.375"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.376"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_positive_huge(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.377"
            end if

        end block


        block

            real(real64) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.378"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.379"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.380"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.381"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.382"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.383"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.384"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.385"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.386"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.387"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.388"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.389"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.390"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.391"
            end if

        end block


        block

            real(real64) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_inf(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.392"
            end if

        end block

    end subroutine test_real64



    subroutine test_real128

        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_inf(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.393"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.394"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.395"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.396"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.397"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.398"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.399"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.400"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.401"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.402"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.403"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.404"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.405"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_inf(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.406"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.407"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_huge(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.408"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.409"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.410"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.411"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.412"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.413"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.414"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.415"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.416"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.417"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.418"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.419"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.420"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.421"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.422"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_huge_next_up(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.423"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.424"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.425"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.426"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.427"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.428"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.429"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.430"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.431"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.432"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.433"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_huge_next_up(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.434"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.435"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.436"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.437"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_one(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.438"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.439"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.440"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.441"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.442"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.443"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.444"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.445"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.446"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.447"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_one(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.448"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.449"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.450"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.451"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.452"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_epsilon(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.453"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.454"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.455"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.456"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.457"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.458"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.459"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.460"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.461"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.462"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.463"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.464"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.465"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.466"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.467"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_negative_tiny(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.468"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.469"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.470"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.471"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.472"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.473"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.474"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.475"
            end if

        end block


        block

            real(real128) :: x, y

            call set_negative_tiny(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.476"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.477"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.478"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.479"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.480"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.481"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.482"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.483"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.484"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.485"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.486"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.487"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.488"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.489"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_negative_zero(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.490"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.491"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.492"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.493"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.494"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.495"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.496"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_negative_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.497"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_zero(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.498"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.499"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.500"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.501"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.502"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.503"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_zero(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.504"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.505"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.506"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.507"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.508"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.509"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.510"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.511"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.512"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_tiny(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.513"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.514"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.515"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.516"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.517"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_tiny(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.518"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.519"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.520"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.521"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.522"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.523"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.524"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.525"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.526"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.527"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_epsilon(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.528"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.529"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.530"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.531"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_epsilon(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.532"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.533"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.534"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.535"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.536"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.537"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.538"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.539"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.540"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.541"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.542"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_one(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.543"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.544"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.545"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_one(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.546"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.547"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.548"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.549"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.550"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.551"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.552"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.553"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.554"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.555"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.556"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.557"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge_next_down(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.558"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.559"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge_next_down(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.560"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.561"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.562"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.563"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.564"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.565"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.566"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.567"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.568"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.569"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.570"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.571"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.572"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_positive_huge(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.573"
            end if

        end block


        block

            real(real128) :: x, y

            call set_positive_huge(x)
            call set_ieee_positive_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.574"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_inf(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.575"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.576"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_huge_next_up(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.577"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.578"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.579"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_negative_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.580"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_negative_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.581"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_zero(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.582"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_tiny(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.583"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_epsilon(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.584"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_one(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.585"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge_next_down(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.586"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_positive_huge(y)

            if ( eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .false. / RESULT: .true. @test_eq_transfer.fypp No.587"
            end if

        end block


        block

            real(real128) :: x, y

            call set_ieee_positive_inf(x)
            call set_ieee_positive_inf(y)

            if ( .not. eq_transfer(x,y) ) then
                error stop "> NG! ; REQUIRED: .true. / RESULT: .false. @test_eq_transfer.fypp No.588"
            end if

        end block

    end subroutine test_real128

end program test_eq_transfer
