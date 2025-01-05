!> A module which defines simple text writer.
module class_text_writer
    use, intrinsic :: iso_fortran_env
    implicit none
    
    public t_text_writer
    private

    !> A simple text writer.
    type :: t_text_writer
        private
        !> IO unit number
        integer(int32) :: unit_number

        !> File path to be written
        character(:), allocatable :: file_path

        !> Output format
        character(:), allocatable :: format_char

        !> True if the file is opened, else false
        logical :: is_opened

        contains

        procedure, public :: open
        procedure, public :: writeline
        procedure, public :: close
    end type
    
    contains

    !> Open a given file
    subroutine open(this, file_path, format_char)
        class(t_text_writer), intent(inout) :: this
        !> File path to be opened
        character(*), intent(in) :: file_path
        !> Format character used in `writeline` procedure
        character(*), optional, intent(in) :: format_char

        logical :: file_exits

        ! Set fortmat_char
        if(present(format_char)) then
            this%format_char = format_char
        else
            ! Use "," as a delimiter.
            this%format_char = '(*(g0,:,","))'
        end if

        open(newunit=this%unit_number, file=file_path, status="replace")
        this%file_path = file_path
        this%is_opened = .true.
    end subroutine


    !> Write float array to the file
    subroutine writeline(this, float_array)
        class(t_text_writer), intent(inout) :: this
        !> 1D float array to be written
        real(real64), intent(in) :: float_array(:)

        if(.not. this%is_opened) then
            error stop "text_writer.writeline: The file is not opened."
        end if

        write(this%unit_number, this%format_char) float_array
    end subroutine


    !> Close the file
    subroutine close(this)
        class(t_text_writer), intent(inout) :: this

        if(.not. this%is_opened) return
        close(this%unit_number)
    end subroutine
end module
