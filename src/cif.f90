! Module that provides functions to read and interpret CIF-files (Crystallographic Information File)
module cif
    use util
    use atoms
    implicit none
    private
    public cif_extract_atoms
    public cif_extract_name

    ! Module variables
    character(*), parameter :: name_placeholder = "[!] Name not found in file"

contains
    ! Extracts the information of all atoms in the file and returns them as an array
    function cif_extract_atoms(filename) result(atom_list)
        character(len=*), intent(in) :: filename
        type(atom), allocatable :: atom_list(:)

        ! Function variables
        integer :: i, n_atoms, atom_start_index, fd, stat, coords_start_index
        character(256) :: line
        character(256), allocatable :: tokens(:)

        ! Open file
        open(newunit=fd, file=filename, status='old', iostat=stat)
        if (stat /= 0) return

        ! Find the start of the atom positions and extract the number of atoms
        atom_start_index = 0
        n_atoms = 0
        coords_start_index = 0
        stat = 0
        line(:) = ""
        i = 0
        do
            ! Read current line and check for errors
            read(fd, '(a)', iostat=stat) line
            if (stat < 0) exit

            ! Find the index of the coordinates in the data
            if (atom_start_index == 1 .and. index(line, '_atom_site_fract_x') > 0) then
                coords_start_index = i - coords_start_index + 1
            endif

            ! Check for the start of the atoms
            if (atom_start_index == 1 .and. index(line, '_atom') == 0) then
                atom_start_index = i
            endif

            ! Count atoms
            if (atom_start_index > 1) then
                ! Check if we have reached the end of the atom list
                if (index(line, "loop_") > 0) exit

                n_atoms = n_atoms + 1
            endif

            ! Find the start of the atom table
            if (atom_start_index == 0 .and. index(line, '_atom') > 0) then
                atom_start_index = 1
                coords_start_index = i
            endif

            ! Increment i
            i = i + 1
        enddo

        ! Allocate atom array
        allocate(atom_list(n_atoms))

        ! Reset fd to start of atom data
        rewind(fd)
        do i = 1, atom_start_index
            read(fd, '(a)')
        enddo

        ! Read atom positions
        do i = 1, n_atoms
            ! Read current line
            read(fd, '(A)', iostat=stat) line
            if (stat < 0) then
                print *, "[!] Failed to read atom information when it was supposed to be there."
                exit
            endif

            ! Split the line into tokens
            tokens = split_string(trim(line), ' ')

            ! Fill in the atom data
            atom_list(i)%name = tokens(1)(1:8)
            read(tokens(coords_start_index+0), *) atom_list(i)%x
            read(tokens(coords_start_index+1), *) atom_list(i)%y
            read(tokens(coords_start_index+2), *) atom_list(i)%z
            deallocate(tokens)
        enddo

        ! Close file
        close(fd)
    end function

    ! Extracts the compound name from the .cif file
    function cif_extract_name(filename) result(crystal_name)
        character(*), intent(in) :: filename
        character(len=:), allocatable :: crystal_name

        ! Function variables
        integer :: fd = 0, stat = 0, str_len = 0
        character(256) :: line
        character(256), allocatable :: tokens(:)

        ! Open file
        open(newunit=fd, file=filename, status='old')
        if (stat /= 0) return

        ! Iterate over lines until we find the name
        line(:) = ""
        do
            ! Read current line and check for errors
            read(fd, '(a)', iostat=stat) line
            if (stat < 0) exit

            ! Check for the mineral name field
            if (index(line, "_chemical_name_mineral") /= 0) then
                tokens = split_string(trim(line), ' ')
                if (len(tokens(2)) < 2) then
                    print *, "[!] Invalid format for CIF file"
                endif

                ! Copy name over and return it
                line = tokens(2)
                str_len = len(trim(line))
                allocate(character(str_len) :: crystal_name)
                crystal_name = line
                deallocate(tokens)

                return
            endif
        enddo

        ! No name was found, add placeholder
        allocate(character(len(name_placeholder)):: crystal_name)
        crystal_name(:) = name_placeholder
    end function
end module cif
