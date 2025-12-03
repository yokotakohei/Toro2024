!> Module which contains class to generate 1D mesh.
module class_mesh_1d_creator
    use, intrinsic :: iso_fortran_env
    use class_mesh_1d
    implicit none

    private
    public :: t_mesh_1d_creator

    !> Class which generates 1D mesh.
    type :: t_mesh_1d_creator
        private
        !> Min value of x
        real(real64) :: x_min

        !> Max value of x
        real(real64) :: x_max

        contains

        procedure, public :: initialize
        procedure, public :: create_uniform_mesh_array
        procedure, public :: create_uniform_mesh_type
    end type

    contains

    !> Initialize this instance.
    subroutine initialize(this, x_min, x_max)
        class(t_mesh_1d_creator), intent(inout) :: this
        !> Min value of x
        real(real64), intent(in) :: x_min
        !> Max value of x
        real(real64), intent(in) :: x_max

        this%x_min = x_min
        this%x_max = x_max
    end subroutine


    !> Generates 1D mesh with an uniform interval.
    function create_uniform_mesh_array(this, n_nodes, dx) result(mesh)
        class(t_mesh_1d_creator), intent(in) :: this
        !> Number of nodes in the [x_min, x_max]
        integer(int32), intent(in) :: n_nodes
        !> Interval of nodes
        real(real64), optional, intent(inout) :: dx
        !> x nodes to be output.
        real(real64), allocatable :: mesh(:)
        ! Local variable for calculating dx
        real(real64) :: local_dx
        ! Loop counter
        integer(int32) :: i

        ! Ensure n_node is valid
        if(n_nodes < 2) then
            error stop "t_mesh_1d_creator.create_uniform_mesh: n_nodes mest be grater than 1."
        end if

        ! Calculate the interval
        local_dx = (this%x_max - this%x_min) / (n_nodes - 1)
        if(present(dx)) then
            dx = local_dx
        end if

        ! Generate the mesh
        mesh = [(this%x_min + (i - 1) * local_dx,  i=1, n_nodes)]
    end function


    !> Generates 1D mesh with an uniform interval.
    function create_uniform_mesh_type(this, n_nodes, dx) result(mesh)
        class(t_mesh_1d_creator), intent(in) :: this
        !> Number of nodes in the [x_min, x_max]
        integer(int32), intent(in) :: n_nodes
        !> Interval of nodes
        real(real64), optional, intent(inout) :: dx
        !> x nodes to be output
        type(t_mesh_1d) :: mesh

        mesh%x = this%create_uniform_mesh_array(n_nodes, dx)
    end function
end module
