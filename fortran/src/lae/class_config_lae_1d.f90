!> Config for solving a Riemann problem of 1D LAE.
module class_config_lae_1d_riemann
    use, intrinsic :: iso_fortran_env
    use json_module
    use stdlib_hashmaps, only : chaining_hashmap_type
    implicit none

    private
    public :: t_config_lae_1d_riemann

    !> Config parameters on problem definition
    type :: t_problem
        !> Wave speed
        real(real64) :: wave_speed

        !> Left state value of Riemann problem
        real(real64) :: ql 

        !> Right state value of Riemann problem
        real(real64) :: qr

        !> Maximum time of calculation
        real(real64) :: tmax

        !> Minimum value of x
        real(real64) :: x_min

        !> Maximum value of x
        real(real64) :: x_max

        !> Left boundary value
        real(real64) :: boundary_l

        !> Right boundary value
        real(real64) :: boundary_r
    end type


    !> Config parameters on numerical schemes
    type :: t_numerical
        !> Time interval
        real(real64) :: dt

        !> Mesh file path
        character(:), allocatable :: mesh_file

        !> Number of nodes
        integer(int32) :: n_node

        !> Options (e.g. parameters on numerical schemes)
        type(chaining_hashmap_type) :: option
    end type


    !> Config parameters on outputs
    type :: t_output
        !> Output file of solution
        character(:), allocatable :: solution_file

        !> Output file of mesh
        character(:), allocatable :: mesh_file
    end type


    !> Class which contais config parameters
    type :: t_config_lae_1d_riemann
        !> Prameters on a problem definition
        type(t_problem), public :: problem

        !> Prameters on a numerical simulation
        type(t_numerical), public :: numerical

        !> Prameters on an output
        type(t_output), public :: output

        contains

        procedure, public :: load
        procedure, private:: load_problem
        procedure, private:: load_numerical
        procedure, private:: load_output
        procedure, private:: load_option
    end type

    contains

    !> Load parameters from a given file.
    subroutine load(this, file_path)
        class(t_config_lae_1d_riemann), intent(inout) :: this
        !> Config file path
        character(*), intent(in) :: file_path

        ! JSON parser
        type(json_file) :: config

        call config%initialize()
        call config%load(filename=file_path)

        ! Loads problem parameters
        call this%load_problem(config)

        ! Loads numerical parameters
        call this%load_numerical(config)

        ! Loads numerical parameters
        call this%load_output(config)

        call config%destroy()
    end subroutine


    subroutine load_problem(this, json)
        class(t_config_lae_1d_riemann), intent(inout) :: this
        !> JOSN object
        type(json_file), intent(inout) :: json

        real(real64) :: wave_speed
        real(real64) :: boundary_l, boundary_r, ql, qr
        real(real64) :: tmax
        real(real64) :: x_min, x_max

        ! Load parameters
        call json%get("problem.wave_speed", wave_speed)
        call json%get("problem.ql", ql)
        call json%get("problem.qr", qr)
        call json%get("problem.tmax", tmax)
        call json%get("problem.x_min", x_min)
        call json%get("problem.x_max", x_max)
        call json%get("problem.boundary_l", boundary_l)
        call json%get("problem.boundary_r", boundary_r)

        if(json%failed()) then
            call json%print_error_message(error_unit)
            call json%destroy()
            error stop "t_config_lae_1d.load_problem: Loading failed."
        end if

        this%problem%wave_speed = wave_speed
        this%problem%ql = ql
        this%problem%qr = qr
        this%problem%tmax = tmax
        this%problem%x_min = x_min
        this%problem%x_max = x_max
        this%problem%boundary_l = boundary_l
        this%problem%boundary_r = boundary_r
    end subroutine


    !> Loads numrical parameters
    subroutine load_numerical(this, json)
        class(t_config_lae_1d_riemann), intent(inout) :: this
        !> JOSN object
        type(json_file), intent(inout) :: json

        character(:), allocatable :: mesh_file 
        real(real64) :: dt
        integer(int32) :: n_node
        type(json_value), pointer :: option_value
        logical :: n_node_found, option_found

        ! Load parameters
        call json%get("numerical.mesh_file", mesh_file)
        call json%get("numerical.dt", dt)
        call json%get("numerical.n_node", n_node, n_node_found)
        call json%get("numerical.option", option_value, option_found)

        if(json%failed()) then
            call json%print_error_message(error_unit)
            call json%destroy()
            error stop "t_config_lae_1d.load_numerical: Loading failed."
        end if

        if(option_found) then
            call this%load_option(json)
        end if

        if(n_node_found) then
            this%numerical%n_node = n_node
        end if

        this%numerical%mesh_file = mesh_file
        this%numerical%dt = dt
    end subroutine


    !> Loads output parameters
    subroutine load_output(this, json)
        class(t_config_lae_1d_riemann), intent(inout) :: this
        !> JOSN object
        type(json_file), intent(inout) :: json

        character(:), allocatable :: solution_file
        character(:), allocatable :: mesh_file

        ! Load parameters
        call json%get("output.solution_file", solution_file)
        call json%get("output.mesh_file", mesh_file)

        if(json%failed()) then
            call json%print_error_message(error_unit)
            call json%destroy()
            error stop "t_config_lae_1d.load_output: Loading failed."
        end if

        this%output%solution_file = solution_file
        this%output%mesh_file = mesh_file
    end subroutine


    !> Loads numrical options
    subroutine load_option(this, json)
        class(t_config_lae_1d_riemann), intent(inout) :: this
        !> JOSN object
        type(json_file), intent(inout) :: json

        character(:), allocatable :: algorithm

        ! Load parameters
        call json%get("numerical.option.algorithm", algorithm)

        if(json%failed()) then
            call json%print_error_message(error_unit)
            call json%destroy()
            error stop "t_config_lae_1d.load_option: Loading failed."
        end if
    end subroutine
end module
