!> A module which defines 1D mesh reader.
module class_mesh_1d_reader
    use, intrinsic :: iso_fortran_env
    implicit none
    
    public t_mesh_1d_reader
    private

    !> A reader of 1D mesh file.
    type :: t_mesh_1d_reader
        private
        !> IO unit number
        integer(int32) :: unit_number

        !> File path to be read
        character(:), allocatable :: file_path

        !> True if the file is opened, else false
        logical :: is_opened = .false.

        contains

        procedure, public :: open
        procedure, public :: read
        procedure, public :: count_lines
        procedure, public :: close
    end type
    
    contains

    !> Open a given file.
    subroutine open(this, file_path)
        class(t_mesh_1d_reader), intent(inout) :: this
        !> File path to be opened
        character(*), intent(in) :: file_path
        logical :: file_exits

        inquire(file=file_path, exist=file_exits)
        if(.not. file_exits) then
            error stop "t_mesh_1d_reader.open: The file does not exist." // new_line('A') // file_path
        end if

        open(newunit=this%unit_number, file=file_path, status="old")
        this%is_opened = .true.
        this%file_path = file_path
    end subroutine


    !> Returns a mesh array from a mesh file.
    function read(this) result(mesh)
        class(t_mesh_1d_reader), intent(inout) :: this
        !> 1D float array to be written
        real(real64), allocatable :: mesh(:)
        integer(int32) :: line_count
        ! Loop counter
        integer(int32) :: i

        if(.not. this%is_opened) then
            error stop "t_mesh_1d_reader.read: File is not opened." // new_line("A") // this%file_path
        end if

        line_count = this%count_lines()
        allocate(mesh(line_count))
        do i = 1, line_count
            read(this%unit_number, *) mesh(i)
        end do
    end function


    !> Counts lines in the file.
    !| If file is not opened, returns -1.
    function count_lines(this) result(line_count)
        class(t_mesh_1d_reader), intent(inout) :: this
        !> Line count in the file.
        integer(int32) :: line_count
        ! io stat
        integer(int32) :: io_stat

        if(.not. this%is_opened) then
            line_count = -1
            return
        end if

        line_count = 0
        do
            read(this%unit_number, *, iostat=io_stat)
            if(io_stat == iostat_end) exit
            line_count = line_count + 1
        end do

        ! Return to the top of the file.
        rewind(this%unit_number)
    end function


    !> Close the file.
    subroutine close(this)
        class(t_mesh_1d_reader), intent(inout) :: this

        if(.not. this%is_opened) then
            return
        end if

        close(this%unit_number)
    end subroutine
end module
