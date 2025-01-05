!> Module defining abstract class for initial condition.
module abstract_initial_condition
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: t_abstract_initial_condition

    !> Abstract base class for initial conditions
    type, abstract :: t_abstract_initial_condition
        private
        contains
        procedure(evaluate_interface), deferred :: evaluate
    end type


    !> Interface for evaluate method
    abstract interface
        !> Evaluates the initial condition at a given point
        function evaluate_interface(this, x) result(h)
            import :: t_abstract_initial_condition, real64
            class(t_abstract_initial_condition), intent(in) :: this
            !> Spatial coordinate
            real(real64), intent(in) :: x
            !> Function value at x
            real(real64) :: h
        end function
    end interface
end module
