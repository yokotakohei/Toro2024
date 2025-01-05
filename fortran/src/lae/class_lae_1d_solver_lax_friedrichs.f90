!> Module defining Lax-Friedrichs method for 1D LAE.
module class_lae_1d_solver_lax_friedrichs
    use, intrinsic :: iso_fortran_env
    use abstract_lae_1d_solver
    implicit none

    private
    public :: t_lae_1d_solver_lax_friedrichs

    !> Class name
    character(*), parameter :: class_name = "t_lae_1d_solver_lax_friedrichs"

    !> Lax-Friedrichs method solver
    type, extends(t_abstract_lae_1d_solver) :: t_lae_1d_solver_lax_friedrichs
        private
        !> Solution at current time step
        real(real64), allocatable :: solution(:)

        !> Time step
        real(real64) :: dt

        !> Spatial interval
        real(real64) :: dx

        !> Wave speed
        real(real64) :: lambda

        !> Courant number
        real(real64) :: c

        !> Number of nodes
        real(real64) :: n_nodes

        !> Left oundary value
        real(real64) :: br

        !> Right boundary value
        real(real64) :: bl

        !> First index of loop in evaluate_next
        integer(int32) :: i_start

        !> Last index of loop in evaluate_next
        integer(int32) :: i_end

        !> Index of left boundary node (upstream end)
        integer(int32) :: i_boundary_l

        !> Index of right boundary node (upstream end)
        integer(int32) :: i_boundary_r

        !> True if initialize is called
        logical :: is_initialized = .false.

        !> True if set_initial_condition is called
        logical :: has_initial_condition = .false.

        !> True if set_initial_condition is called
        logical :: has_boundary_condition = .false.

        contains

        procedure, public :: initialize
        procedure, public :: set_initial_condition
        procedure, public :: set_boundary_condition
        procedure, public :: evaluate_next
    end type

    contains

    !> Initializes the solver with given initial condition.
    subroutine initialize(this, dt, dx, wave_speed)
        class(t_lae_1d_solver_lax_friedrichs), intent(inout) :: this
        !> Time step
        real(real64), intent(in) :: dt
        !> Spatial interval
        real(real64), intent(in) :: dx
        !> Wave speed
        real(real64), intent(in) :: wave_speed
        
        if (allocated(this%solution)) deallocate(this%solution)
        this%dt = dt
        this%dx = dx
        this%lambda = wave_speed
        this%c = wave_speed * dt / dx
        this%is_initialized = .true.

        if(this%c >= 1.0d0) then
            print*, "WARNING: Courant number is ", this%c
        end if
    end subroutine


    !> Set initial condition.
    subroutine set_initial_condition(this, condition)
        class(t_lae_1d_solver_lax_friedrichs), intent(inout) :: this
        !> Initial condition
        real(real64), intent(in) :: condition(:)

        allocate(this%solution, source=condition)
        this%n_nodes = size(condition)
        this%i_start = 2
        this%i_end = this%n_nodes - 1
        this%i_boundary_l = 1
        this%i_boundary_r = this%n_nodes
        this%has_initial_condition = .true.
    end subroutine


    !> Set boudary condition.
    subroutine set_boundary_condition(this, bl, br)
        class(t_lae_1d_solver_lax_friedrichs), intent(inout) :: this
        !> Left boundary condition
        real(real64), intent(in) :: bl
        !> Right boundary condition
        real(real64), intent(in) :: br

        this%bl = bl
        this%br = br
        this%has_boundary_condition = .true.
    end subroutine


    !> Returns solution at next time step.
    function evaluate_next(this) result(h)
        class(t_lae_1d_solver_lax_friedrichs), intent(inout) :: this
        !> Solution at next time step
        real(real64), allocatable :: h(:)
        real(real64) :: n
        real(real64) :: d_flux, a, b
        integer(int32) :: i

        if(.not. this%is_initialized) then
            error stop class_name // ".evaluate_next: Solver is not initialized."
        end if

        if(.not. this%has_initial_condition) then
            error stop class_name // ".evaluate_next: Initial condition is not set."
        end if

        if(.not. this%has_initial_condition) then
            error stop class_name // ".evaluate_next: Boundary condition is not set."
        end if

        n = this%n_nodes
        allocate(h(n))

        ! Calculate solution at next time step
        do i = this%i_start, this%i_end
            ! eq.(9.20) in Toro (2024) p169.
            a = (1 + this%c) * this%solution(i-1)
            b = (1 - this%c) * this%solution(i+1)
            h(i) = 0.5d0 * (a + b)
        end do
        
        ! Set boundary
        h(this%i_boundary_l) = this%bl
        h(this%i_boundary_r) = this%br

        ! Update solution
        this%solution = h
    end function
end module
