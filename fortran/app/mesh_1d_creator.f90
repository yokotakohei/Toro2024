!> Generates 1D mesh. The mesh is saved as CSV file.
!! Generated mesh interval is uniform.
program mesh_1d_creator
    use, intrinsic :: iso_fortran_env
    use class_argument_getter, only : argument_getter
    use class_mesh_1d_creator
    implicit none

    !> Loop counter
    integer(int32) :: i

    !> Unit number
    integer(int32) :: file_unit 

    !> Argument getter
    type(argument_getter) :: args

    !> Min value of x
    real(real64) :: x_min

    !> Max value of x
    real(real64) :: x_max

    !> The number of nodes in the [x_min, x_max]"
    integer(int32) :: n_nodes

    !> Output mesh file path
    character(:), allocatable :: ouptut_path

    !> x nodes to be output.
    real(real64), allocatable :: mesh(:)

    !> Interval of nodes
    type(t_mesh_1d_creator) :: creator

    ! Error check of commandline arguments
    call check_args(args)

    x_min = args%to_real64(1)
    x_max = args%to_real64(2)
    n_nodes = args%to_int32(3)
    ouptut_path= args%get(4)

    ! Generates the mesh.
    call creator%initialize(x_min, x_max)
    mesh = creator%create_uniform_mesh_array(n_nodes)

    ! Save the mesh as CSV file.
    open(newunit=file_unit, file=ouptut_path, status="replace")
    do i = 1, size(mesh)
        write(file_unit, *) mesh(i)
    end do
    close(file_unit)

    contains

    !> Validate arguments.
    subroutine check_args(args)
        !> Commandline arguments
        type(argument_getter), intent(inout) :: args

        if(args%get_size() /= 4) then
            write(error_unit, *) "ERROR: Commandline arguments are followings."
            write(error_unit, *) "  - x_min (real64): Min value of x"
            write(error_unit, *) "  - x_min (real64): Min value of x"
            write(error_unit, *) "  - n_nodes(int32): The number of nodes in the [x_min, x_max]"
            write(error_unit, *) "  - output_path (character): Output mesh file path"
            write(error_unit, *) "Program ended."
            error stop
        end if 

        if(args%to_int32(3)<= 0) then
            error stop "Error: n_node must be greater than 1."
        end if
    end subroutine
end program
