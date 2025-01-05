module class_mesh_1d
    use, intrinsic :: iso_fortran_env
    implicit none

    private
    public :: t_mesh_1d

    !> Class of 1D mesh
    type :: t_mesh_1d
        real(real64), public, allocatable :: x(:)

        contains

        procedure, public :: save
    end type

    contains

    !> Save this mesh to a given file
    subroutine save(this, file_path)
        class(t_mesh_1d), intent(in) :: this
        !> Output file path
        character(*), intent(in) :: file_path
        integer(int32) :: file_unit
        integer(int32) :: i, n

        n = size(this%x)
        open(newunit=file_unit, file=file_path, status="replace")
        do i = 1, n
            write(file_unit, *) this%x(i)
        end do
        close(file_unit)
    end subroutine
end module
