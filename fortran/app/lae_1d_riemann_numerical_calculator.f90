!> Outputs the numerical solution of Riemann problem of 1D linear advection equation (LAE).
!| Initial condition is following:
!| q(x) = ql in x < 0
!| q(x) = qr in x > 0
program lae_1d_riemann_numerical_calculator
    use, intrinsic :: iso_fortran_env
    use class_argument_getter, only : argument_getter
    use class_config_lae_1d_riemann
    use class_mesh_1d_reader
    use class_riemann_1d_solution
    use class_lae_1d_solver_godunov
    use class_lae_1d_solver_ftcs
    use class_lae_1d_solver_lax_friedrichs
    use class_mesh_1d_creator
    use class_mesh_1d
    use class_text_writer
    implicit none

    ! Loop counter
    integer(int32) :: i, j, k

    ! Argument getter
    type(argument_getter) :: args

    ! Config file path
    character(:), allocatable :: config_path

    ! Config object
    type(t_config_lae_1d_riemann) :: config

    ! Exact Riemann solution
    type(t_riemann_1d_solution) :: exact_solution

    ! Mesh reader
    type(t_mesh_1d_creator) mesh_creator

    ! x nodes array.
    type(t_mesh_1d) :: mesh

    ! Spatial interval
    real(real64) :: dx

    ! Current time, time interval 
    real(real64) :: t, dt

    ! Total time count (tmax / dt)
    integer(int32) :: total_time_step

    ! Array containing time and solution. The format is [t, f(1), f(2), ...].
    real(real64), allocatable :: initial_condition(:)
    real(real64), allocatable :: solution_array(:)

    ! Solver of 1D LAE
    type(t_lae_1d_solver_lax_friedrichs) solver

    ! Solution writer
    type(t_text_writer) writer

    ! Error check of commandline arguments
    call check_args(args)

    ! Read config file.
    config_path = args%get(1)
    call config%load(config_path)

    ! Generate the mesh.
    call mesh_creator%initialize(config%problem%x_min, config%problem%x_max)
    mesh = mesh_creator%create_uniform_mesh_type(config%numerical%n_node, dx)
    call mesh%save(config%output%mesh_file)

    ! Total time steps
    dt = config%numerical%dt
    total_time_step= int(config%problem%tmax / dt)

    ! Get initial condition
    call exact_solution%initialize( &
        config%problem%wave_speed, &
        config%problem%ql, &
        config%problem%qr &
        )
    t = 0.0d0
    initial_condition = [(exact_solution%evaluate(t, mesh%x(j)), j = 1, size(mesh%x))]

    ! Initial solution
    solution_array = [t, initial_condition]

    ! Initialize solver
    call solver%initialize( &
        config%numerical%dt, &
        dx, &
        config%problem%wave_speed &
        )
    call solver%set_initial_condition(initial_condition)
    call solver%set_boundary_condition( &
        config%problem%boundary_l, &
        config%problem%boundary_r &
        )

    ! Write initial condition.
    call writer%open(config%output%solution_file)
    call writer%writeline(solution_array)
    deallocate(initial_condition)

    ! Solve equation and write solution
    do i = 1, total_time_step
        t = i * dt
        solution_array = [t, solver%evaluate_next()]
        call writer%writeline(solution_array)
    end do
    call writer%close()

    contains

    !> Validate arguments.
    subroutine check_args(args)
        !> Commandline arguments
        type(argument_getter), intent(inout) :: args

        if(args%get_size() /= 1) then
            write(error_unit, *) "ERROR: Commandline arguments are followings."
            write(error_unit, *) "  - config_file (character): Config file path."
            write(error_unit, *) "Program ended."
            error stop
        end if 
    end subroutine
end program
