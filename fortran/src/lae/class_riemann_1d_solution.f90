!> Module defining Riemann problem solution for 1D LAE.
module class_riemann_1d_solution
    use, intrinsic :: iso_fortran_env
    use abstract_initial_condition
    use class_lae_1d_solution
    implicit none

    private
    public :: t_riemann_1d_solution

    !> Riemann problem initial condition
    type, extends(t_abstract_initial_condition) :: t_riemann_initial_condition
        private
        !> Left state value
        real(real64) :: ql

        !> Right state value
        real(real64) :: qr

        contains

        procedure :: evaluate => evaluate_riemann
    end type


    !> Solver for 1D Riemann problem
    type :: t_riemann_1d_solution
        private
        !> LAE solver
        type(t_lae_1d_solution) :: solver

        !> Initial condition for Riemann problem
        type(t_riemann_initial_condition) :: ic

        contains

        procedure, public :: initialize
        procedure, public :: evaluate
    end type

    contains

    !> Initializes Riemann problem solver.
    subroutine initialize(this, wave_speed, ql, qr)
        class(t_riemann_1d_solution), intent(inout) :: this
        !> Wave speed
        real(real64), intent(in) :: wave_speed
        !> Left state value
        real(real64), intent(in) :: ql
        !> Right state value
        real(real64), intent(in) :: qr

        this%ic%ql = ql
        this%ic%qr = qr
        call this%solver%init(this%ic, wave_speed)
    end subroutine


    !> Returns solution at given time and position.
    function evaluate(this, t, x) result(f)
        class(t_riemann_1d_solution), intent(in) :: this
        !> Time coordinate
        real(real64), intent(in) :: t
        !> Spatial coordinate
        real(real64), intent(in) :: x
        !> Solution value at (t, x)
        real(real64) :: f

        f = this%solver%evaluate(t, x)
    end function


    !> Evaluates Riemann problem initial condition.
    function evaluate_riemann(this, x) result(h)
        class(t_riemann_initial_condition), intent(in) :: this
        !> Spatial coordinate
        real(real64), intent(in) :: x
        !> Function value at x
        real(real64) :: h

        if (x < 0) then
            h = this%ql
        else
            h = this%qr
        end if
    end function
end module
