!> Module defining abstract class for 1D LAE solver.
module abstract_lae_1d_solver
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: t_abstract_lae_1d_solver

    !> Abstract class for 1D LAE solver 
    type, abstract :: t_abstract_lae_1d_solver
        private
        contains
        procedure(evaluate_next_interface), deferred :: evaluate_next
    end type


    !> Interface for evaluate_next method
    abstract interface
        !> Evaluates the solutin at next time step
        function evaluate_next_interface(this) result(h)
            import :: t_abstract_lae_1d_solver, real64
            class(t_abstract_lae_1d_solver), intent(inout) :: this
            !> Solution at next time step
            real(real64), allocatable :: h(:)
        end function
    end interface
end module
