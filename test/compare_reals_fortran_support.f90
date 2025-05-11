module compare_reals_fortran_support

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_negative_inf
    use, intrinsic :: ieee_arithmetic, only: ieee_next_after
    use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf
    use, intrinsic :: ieee_arithmetic, only: ieee_signaling_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_value


    implicit none


    private
    public  :: set_negative_huge
    public  :: set_negative_huge_next_up
    public  :: set_negative_one_next_down
    public  :: set_negative_one
    public  :: set_negative_one_next_up
    public  :: set_negative_epsilon_next_down
    public  :: set_negative_epsilon
    public  :: set_negative_epsilon_next_up
    public  :: set_negative_tiny
    public  :: set_positive_tiny
    public  :: set_positive_epsilon_next_down
    public  :: set_positive_epsilon
    public  :: set_positive_epsilon_next_up
    public  :: set_positive_one_next_down
    public  :: set_positive_one
    public  :: set_positive_one_next_up
    public  :: set_positive_huge_next_down
    public  :: set_positive_huge


    interface set_negative_huge
        module procedure :: set_negative_huge_real32
        module procedure :: set_negative_huge_real64
        module procedure :: set_negative_huge_real128
    end interface set_negative_huge


    interface set_negative_huge_next_up
        module procedure :: set_negative_huge_next_up_real32
        module procedure :: set_negative_huge_next_up_real64
        module procedure :: set_negative_huge_next_up_real128
    end interface set_negative_huge_next_up


    interface set_negative_one_next_down
        module procedure :: set_negative_one_next_down_real32
        module procedure :: set_negative_one_next_down_real64
        module procedure :: set_negative_one_next_down_real128
    end interface set_negative_one_next_down


    interface set_negative_one
        module procedure :: set_negative_one_real32
        module procedure :: set_negative_one_real64
        module procedure :: set_negative_one_real128
    end interface set_negative_one


    interface set_negative_one_next_up
        module procedure :: set_negative_one_next_up_real32
        module procedure :: set_negative_one_next_up_real64
        module procedure :: set_negative_one_next_up_real128
    end interface set_negative_one_next_up


    interface set_negative_epsilon_next_down
        module procedure :: set_negative_epsilon_next_down_real32
        module procedure :: set_negative_epsilon_next_down_real64
        module procedure :: set_negative_epsilon_next_down_real128
    end interface set_negative_epsilon_next_down


    interface set_negative_epsilon
        module procedure :: set_negative_epsilon_real32
        module procedure :: set_negative_epsilon_real64
        module procedure :: set_negative_epsilon_real128
    end interface set_negative_epsilon


    interface set_negative_epsilon_next_up
        module procedure :: set_negative_epsilon_next_up_real32
        module procedure :: set_negative_epsilon_next_up_real64
        module procedure :: set_negative_epsilon_next_up_real128
    end interface set_negative_epsilon_next_up


    interface set_negative_tiny
        module procedure :: set_negative_tiny_real32
        module procedure :: set_negative_tiny_real64
        module procedure :: set_negative_tiny_real128
    end interface set_negative_tiny


    interface set_positive_tiny
        module procedure :: set_positive_tiny_real32
        module procedure :: set_positive_tiny_real64
        module procedure :: set_positive_tiny_real128
    end interface set_positive_tiny


    interface set_positive_epsilon_next_down
        module procedure :: set_positive_epsilon_next_down_real32
        module procedure :: set_positive_epsilon_next_down_real64
        module procedure :: set_positive_epsilon_next_down_real128
    end interface set_positive_epsilon_next_down


    interface set_positive_epsilon
        module procedure :: set_positive_epsilon_real32
        module procedure :: set_positive_epsilon_real64
        module procedure :: set_positive_epsilon_real128
    end interface set_positive_epsilon


    interface set_positive_epsilon_next_up
        module procedure :: set_positive_epsilon_next_up_real32
        module procedure :: set_positive_epsilon_next_up_real64
        module procedure :: set_positive_epsilon_next_up_real128
    end interface set_positive_epsilon_next_up


    interface set_positive_one_next_down
        module procedure :: set_positive_one_next_down_real32
        module procedure :: set_positive_one_next_down_real64
        module procedure :: set_positive_one_next_down_real128
    end interface set_positive_one_next_down


    interface set_positive_one
        module procedure :: set_positive_one_real32
        module procedure :: set_positive_one_real64
        module procedure :: set_positive_one_real128
    end interface set_positive_one


    interface set_positive_one_next_up
        module procedure :: set_positive_one_next_up_real32
        module procedure :: set_positive_one_next_up_real64
        module procedure :: set_positive_one_next_up_real128
    end interface set_positive_one_next_up


    interface set_positive_huge_next_down
        module procedure :: set_positive_huge_next_down_real32
        module procedure :: set_positive_huge_next_down_real64
        module procedure :: set_positive_huge_next_down_real128
    end interface set_positive_huge_next_down


    interface set_positive_huge
        module procedure :: set_positive_huge_real32
        module procedure :: set_positive_huge_real64
        module procedure :: set_positive_huge_real128
    end interface set_positive_huge


    contains


    pure elemental subroutine set_negative_huge_real32(x)

        real(real32), intent(inout) :: x

        x = - huge(x)

    end subroutine set_negative_huge_real32


    pure elemental subroutine set_negative_huge_real64(x)

        real(real64), intent(inout) :: x

        x = - huge(x)

    end subroutine set_negative_huge_real64


    pure elemental subroutine set_negative_huge_real128(x)

        real(real128), intent(inout) :: x

        x = - huge(x)

    end subroutine set_negative_huge_real128



    pure elemental subroutine set_negative_huge_next_up_real32(x)

        real(real32), intent(inout) :: x

        call set_negative_huge(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_negative_huge_next_up_real32


    pure elemental subroutine set_negative_huge_next_up_real64(x)

        real(real64), intent(inout) :: x

        call set_negative_huge(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_negative_huge_next_up_real64


    pure elemental subroutine set_negative_huge_next_up_real128(x)

        real(real128), intent(inout) :: x

        call set_negative_huge(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_negative_huge_next_up_real128



    pure elemental subroutine set_negative_one_next_down_real32(x)

        real(real32), intent(inout) :: x

        call set_negative_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_negative_one_next_down_real32


    pure elemental subroutine set_negative_one_next_down_real64(x)

        real(real64), intent(inout) :: x

        call set_negative_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_negative_one_next_down_real64


    pure elemental subroutine set_negative_one_next_down_real128(x)

        real(real128), intent(inout) :: x

        call set_negative_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_negative_one_next_down_real128



    pure elemental subroutine set_negative_one_real32(x)

        real(real32), intent(inout) :: x

        x = -1.0_real32

    end subroutine set_negative_one_real32


    pure elemental subroutine set_negative_one_real64(x)

        real(real64), intent(inout) :: x

        x = -1.0_real64

    end subroutine set_negative_one_real64


    pure elemental subroutine set_negative_one_real128(x)

        real(real128), intent(inout) :: x

        x = -1.0_real128

    end subroutine set_negative_one_real128



    pure elemental subroutine set_negative_one_next_up_real32(x)

        real(real32), intent(inout) :: x

        call set_negative_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_negative_one_next_up_real32


    pure elemental subroutine set_negative_one_next_up_real64(x)

        real(real64), intent(inout) :: x

        call set_negative_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_negative_one_next_up_real64


    pure elemental subroutine set_negative_one_next_up_real128(x)

        real(real128), intent(inout) :: x

        call set_negative_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_negative_one_next_up_real128



    pure elemental subroutine set_negative_epsilon_next_down_real32(x)

        real(real32), intent(inout) :: x

        call set_negative_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_negative_epsilon_next_down_real32


    pure elemental subroutine set_negative_epsilon_next_down_real64(x)

        real(real64), intent(inout) :: x

        call set_negative_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_negative_epsilon_next_down_real64


    pure elemental subroutine set_negative_epsilon_next_down_real128(x)

        real(real128), intent(inout) :: x

        call set_negative_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_negative_epsilon_next_down_real128



    pure elemental subroutine set_negative_epsilon_real32(x)

        real(real32), intent(inout) :: x

        x = - epsilon(x)

    end subroutine set_negative_epsilon_real32


    pure elemental subroutine set_negative_epsilon_real64(x)

        real(real64), intent(inout) :: x

        x = - epsilon(x)

    end subroutine set_negative_epsilon_real64


    pure elemental subroutine set_negative_epsilon_real128(x)

        real(real128), intent(inout) :: x

        x = - epsilon(x)

    end subroutine set_negative_epsilon_real128



    pure elemental subroutine set_negative_epsilon_next_up_real32(x)

        real(real32), intent(inout) :: x

        call set_negative_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_negative_epsilon_next_up_real32


    pure elemental subroutine set_negative_epsilon_next_up_real64(x)

        real(real64), intent(inout) :: x

        call set_negative_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_negative_epsilon_next_up_real64


    pure elemental subroutine set_negative_epsilon_next_up_real128(x)

        real(real128), intent(inout) :: x

        call set_negative_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_negative_epsilon_next_up_real128



    pure elemental subroutine set_negative_tiny_real32(x)

        real(real32), intent(inout) :: x

        x = - tiny(x)

    end subroutine set_negative_tiny_real32


    pure elemental subroutine set_negative_tiny_real64(x)

        real(real64), intent(inout) :: x

        x = - tiny(x)

    end subroutine set_negative_tiny_real64


    pure elemental subroutine set_negative_tiny_real128(x)

        real(real128), intent(inout) :: x

        x = - tiny(x)

    end subroutine set_negative_tiny_real128



    pure elemental subroutine set_positive_tiny_real32(x)

        real(real32), intent(inout) :: x

        x = tiny(x)

    end subroutine set_positive_tiny_real32


    pure elemental subroutine set_positive_tiny_real64(x)

        real(real64), intent(inout) :: x

        x = tiny(x)

    end subroutine set_positive_tiny_real64


    pure elemental subroutine set_positive_tiny_real128(x)

        real(real128), intent(inout) :: x

        x = tiny(x)

    end subroutine set_positive_tiny_real128



    pure elemental subroutine set_positive_epsilon_next_down_real32(x)

        real(real32), intent(inout) :: x

        call set_positive_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_positive_epsilon_next_down_real32


    pure elemental subroutine set_positive_epsilon_next_down_real64(x)

        real(real64), intent(inout) :: x

        call set_positive_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_positive_epsilon_next_down_real64


    pure elemental subroutine set_positive_epsilon_next_down_real128(x)

        real(real128), intent(inout) :: x

        call set_positive_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_positive_epsilon_next_down_real128



    pure elemental subroutine set_positive_epsilon_real32(x)

        real(real32), intent(inout) :: x

        x = epsilon(x)

    end subroutine set_positive_epsilon_real32


    pure elemental subroutine set_positive_epsilon_real64(x)

        real(real64), intent(inout) :: x

        x = epsilon(x)

    end subroutine set_positive_epsilon_real64


    pure elemental subroutine set_positive_epsilon_real128(x)

        real(real128), intent(inout) :: x

        x = epsilon(x)

    end subroutine set_positive_epsilon_real128



    pure elemental subroutine set_positive_epsilon_next_up_real32(x)

        real(real32), intent(inout) :: x

        call set_positive_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_positive_epsilon_next_up_real32


    pure elemental subroutine set_positive_epsilon_next_up_real64(x)

        real(real64), intent(inout) :: x

        call set_positive_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_positive_epsilon_next_up_real64


    pure elemental subroutine set_positive_epsilon_next_up_real128(x)

        real(real128), intent(inout) :: x

        call set_positive_epsilon(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_positive_epsilon_next_up_real128



    pure elemental subroutine set_positive_one_next_down_real32(x)

        real(real32), intent(inout) :: x

        call set_positive_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_positive_one_next_down_real32


    pure elemental subroutine set_positive_one_next_down_real64(x)

        real(real64), intent(inout) :: x

        call set_positive_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_positive_one_next_down_real64


    pure elemental subroutine set_positive_one_next_down_real128(x)

        real(real128), intent(inout) :: x

        call set_positive_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_positive_one_next_down_real128



    pure elemental subroutine set_positive_one_real32(x)

        real(real32), intent(inout) :: x

        x = 1.0_real32

    end subroutine set_positive_one_real32


    pure elemental subroutine set_positive_one_real64(x)

        real(real64), intent(inout) :: x

        x = 1.0_real64

    end subroutine set_positive_one_real64


    pure elemental subroutine set_positive_one_real128(x)

        real(real128), intent(inout) :: x

        x = 1.0_real128

    end subroutine set_positive_one_real128



    pure elemental subroutine set_positive_one_next_up_real32(x)

        real(real32), intent(inout) :: x

        call set_positive_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_positive_one_next_up_real32


    pure elemental subroutine set_positive_one_next_up_real64(x)

        real(real64), intent(inout) :: x

        call set_positive_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_positive_one_next_up_real64


    pure elemental subroutine set_positive_one_next_up_real128(x)

        real(real128), intent(inout) :: x

        call set_positive_one(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_positive_inf ) )

    end subroutine set_positive_one_next_up_real128



    pure elemental subroutine set_positive_huge_next_down_real32(x)

        real(real32), intent(inout) :: x

        call set_positive_huge(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_positive_huge_next_down_real32


    pure elemental subroutine set_positive_huge_next_down_real64(x)

        real(real64), intent(inout) :: x

        call set_positive_huge(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_positive_huge_next_down_real64


    pure elemental subroutine set_positive_huge_next_down_real128(x)

        real(real128), intent(inout) :: x

        call set_positive_huge(x)
        x = ieee_next_after( x = x, y = ieee_value( x = x, class = ieee_negative_inf ) )

    end subroutine set_positive_huge_next_down_real128



    pure elemental subroutine set_positive_huge_real32(x)

        real(real32), intent(inout) :: x

        x = huge(x)

    end subroutine set_positive_huge_real32


    pure elemental subroutine set_positive_huge_real64(x)

        real(real64), intent(inout) :: x

        x = huge(x)

    end subroutine set_positive_huge_real64


    pure elemental subroutine set_positive_huge_real128(x)

        real(real128), intent(inout) :: x

        x = huge(x)

    end subroutine set_positive_huge_real128


end module compare_reals_fortran_support
