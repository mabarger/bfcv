! Provides constants and types for working with atoms
module atoms
    implicit none
    public

    ! Atom type
    type :: atom
        character(8) :: name
        real :: x, y, z
    end type atom

contains
    ! Prints the contents of an atom nicely
    subroutine print_atom(atom_in)
        type(atom), intent(in) :: atom_in
        write(*, "(A,F10.6,F10.6,F10.6)") atom_in%name, atom_in%x, atom_in%y, atom_in%z
    end subroutine

    subroutine print_atoms(atoms_in)
        type(atom), intent(in) :: atoms_in(:)
        integer :: i = 0

        do i = 1, size(atoms_in)
            call print_atom(atoms_in(i))
        enddo
    end subroutine

end module atoms
