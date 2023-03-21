! Module that provides functions to read and interpret CIF-files (Crystallographic Information File)
module cif
    use util
    use atoms
    implicit none
    private
    public cif_extract_atoms
    public cif_extract_name
    public cif_extract_field
    public cif_extract_field_real

    ! Module variables
    character(*), parameter :: field_placeholder = "[!] Field not found in file"

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
            atom_list(i)%x = cif_parse_real(tokens(coords_start_index+0))
            atom_list(i)%y = cif_parse_real(tokens(coords_start_index+1))
            atom_list(i)%z = cif_parse_real(tokens(coords_start_index+2))
            deallocate(tokens)
        enddo

        ! Close file
        close(fd)
    end function

    ! Extracts the first occurance of a given field from the cif file
    function cif_extract_field(file_name, field_name) result(field_content)
        character(*), intent(in) :: file_name, field_name
        character(len=:), allocatable :: field_content

        ! Function variables
        integer :: fd = 0, stat = 0, str_len = 0
        character(256) :: line
        character(256), allocatable :: tokens(:)

        ! Open file
        open(newunit=fd, file=file_name, status='old')
        if (stat /= 0) return

        ! Iterate over lines until we find the given field
        line(:) = ""
        do
            ! Read current line and check for errors
            read(fd, '(a)', iostat=stat) line
            if (stat < 0) exit

            ! Check for the mineral name field
            if (index(line, field_name) /= 0) then
                tokens = split_string(trim(line), ' ')
                if (len(tokens(2)) < 2) then
                    print *, "[!] Field " // field_name // " not in file"
                endif

                ! Copy name over and return it
                line = tokens(2)
                str_len = len(trim(line))
                allocate(character(str_len) :: field_content)
                field_content = line
                deallocate(tokens)

                return
            endif
        enddo

        ! Close file
        close(fd)

        ! No name was found, add placeholder
        allocate(character(len(field_placeholder)) :: field_content)
        field_content(:) = field_placeholder
    end function

    ! Extracts a field and converts it into a real
    function cif_extract_field_real(file_name, field_name) result(field_content)
        character(*), intent(in) :: file_name, field_name
        character(len=:), allocatable :: field_str
        real(kind=8) :: field_content

        ! Extract and parse the real
        field_str = cif_extract_field(file_name, field_name)
        field_content = cif_parse_real(field_str)
    end function

    ! Extracts the compound name from the .cif file
    function cif_extract_name(file_name) result(crystal_name)
        character(*), intent(in) :: file_name
        character(len=:), allocatable :: crystal_name

        crystal_name = cif_extract_field(file_name, "_chemical_name_mineral")
    end function

    ! Tries to parse a real from a cif file, if not possible returns 0.0
    function cif_parse_real(string) result(res)
        character(*), intent(inout) :: string
        real(kind=8) :: res
        integer :: idx = 0, i = 0

        ! Check if we have a precision specifier and if yes remove it
        res = 0.0
        idx = index(string, '(')
        if (idx > 0) then
            do i = idx, len(string)
                string(i:i) = ' '
            enddo
        endif

        ! Parse the real value
        read(string, *), res
    end function
end module cif
