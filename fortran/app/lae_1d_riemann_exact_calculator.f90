!> Outputs the exact solution of Riemann problem of 1D linear advection equation (LAE).
!| Initial condition is following:
!| q(x) = ql in x < 0
!| q(x) = qr in x > 0
!| In the output file, line i corresponds to the solution for the i-th time step;
!| the first column is the time, and the second and subsequent columns are 
!| the values at each node at i-th time step.
program lae_1d_riemann_exact_calculator
    use, intrinsic :: iso_fortran_env
    use class_argument_getter, only : argument_getter
    use class_config_lae_1d_riemann
    use class_riemann_1d_solution
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

    ! Mesh creator
    type(t_mesh_1d_creator) mesh_creator

    ! x nodes array.
    type(t_mesh_1d) :: mesh

    ! Current time, time interval 
    real(real64) :: t, dt

    ! Total time count (tmax / dt)
    integer(int32) :: total_time_step

    ! Array containing time and solution. The format is [t, f(1), f(2), ...].
    real(real64), allocatable :: solution_array(:)

    ! Exact solution of Riemann problem of 1D LAE
    type(t_riemann_1d_solution) exact_solution

    ! Solution writer
    type(t_text_writer) writer

    ! Error check of commandline arguments
    call check_args(args)

    ! Read config file.
    config_path = args%get(1)
    call config%load(config_path)

    ! Generate the mesh.
    call mesh_creator%initialize(config%problem%x_min, config%problem%x_max)
    mesh = mesh_creator%create_uniform_mesh_type(config%numerical%n_node)
    call mesh%save(config%output%mesh_file)

    ! Total time steps
    dt = config%numerical%dt
    total_time_step= int(config%problem%tmax / dt)

    ! Get exact solution at each time.
    call exact_solution%initialize( &
        config%problem%wave_speed, &
        config%problem%ql, &
        config%problem%qr &
        )
    call writer%open(config%output%solution_file)
    allocate(solution_array(1 + size(mesh%x)))
    do i = 0, total_time_step
        t = i * dt
        solution_array = [t, (exact_solution%evaluate(t, mesh%x(j)), j = 1, size(mesh%x))]
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
