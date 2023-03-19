! This program reads in a file describing a molecule and calculates the center of mass
program bfcv
    use cif
    use atoms
    implicit none

    integer :: argc = 0
    character(128) :: filename = "cifs/galena.cif"
    type(atom), allocatable :: atom_list(:)

    ! If an argument has been provided use it as a filename
    argc = command_argument_count()
    if (argc == 1) call get_command_argument(1, filename)

    atom_list = cif_extract_atoms(filename)
    call print_atoms(atom_list)
end program bfcv
