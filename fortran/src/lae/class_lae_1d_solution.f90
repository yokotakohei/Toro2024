!> Module defining analytical solution of 1D linear advection equation.
module class_lae_1d_solution
    use, intrinsic :: iso_fortran_env
    use abstract_initial_condition
    implicit none

    private
    public :: t_lae_1d_solution

    !> Solution of 1D linear advection equation (LAE)
    type :: t_lae_1d_solution
        private
        !> Initial condition
        class(t_abstract_initial_condition), allocatable :: initial_condition

        !> Wave speed
        real(real64) :: wave_speed

        contains

        procedure, public :: init
        procedure, public :: evaluate
    end type

    contains

    !> Initializes the solver with given initial condition and wave speed.
    subroutine init(this, initial_condition, wave_speed)
        class(t_lae_1d_solution), intent(inout) :: this
        !> Initial condition
        class(t_abstract_initial_condition), intent(in) :: initial_condition
        !> Wave speed
        real(real64), intent(in) :: wave_speed
        
        if (allocated(this%initial_condition)) deallocate(this%initial_condition)
        allocate(this%initial_condition, source=initial_condition)
        this%wave_speed = wave_speed
    end subroutine


    !> Returns solution at given time and position.
    function evaluate(this, t, x) result(f)
        class(t_lae_1d_solution), intent(in) :: this
        !> Time coordinate
        real(real64), intent(in) :: t
        !> Spatial coordinate
        real(real64), intent(in) :: x
        !> Solution value at (t, x)
        real(real64) :: f

        ! f(t, x) = h(x - ct) where c is wave_speed
        f = this%initial_condition%evaluate(x - this%wave_speed * t)
    end function
end module
