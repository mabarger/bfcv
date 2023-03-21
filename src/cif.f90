! Module that provides functions to read and interpret CIF-files (Crystallographic Information File)
module cif
    use atoms
    use util
    implicit none
    private

    public cif_extract_atoms
    public cif_extract_name
    public cif_extract_field
    public cif_extract_field_real
    public cif_apply_symops

    ! Module variables
    character(*), parameter :: field_placeholder = "[!] Field not found in file"

contains
    ! Extracts the information of all atoms in the file and returns them as an array
    function cif_extract_atoms(file_name) result(atom_list)
        character(len=*), intent(in) :: file_name
        type(atom), allocatable :: atom_list(:)

        ! Function variables
        integer :: i, n_atoms, atom_start_index, fd, stat, coords_start_index
        character(256) :: line
        character(256), allocatable :: tokens(:)

        ! Open file
        open(newunit=fd, file=file_name, status='old', iostat=stat)
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
        open(newunit=fd, file=file_name, status='old', iostat=stat)
        if (stat /= 0) return

        ! Iterate over lines until we find the given field
        line(:) = ""
        do
            ! Read current line and check for errors
            read(fd, '(a)', iostat=stat) line
            if (stat < 0) exit

            ! Check for the field name
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
        read(string, *) res
    end function

    ! Applys symops listed in the specified to the atom_list and returns them as a new list
    subroutine cif_apply_symops(file_name, atom_list, new_list)
        type(atom), allocatable, intent(inout) :: atom_list(:), new_list(:)
        character(len=*), intent(in) :: file_name

        ! Function variables
        !type(atom), allocatable :: new_list(:)
        integer :: fd = 0, stat = 0, n_symops = 0, atom_idx = 0, total_idx = 0
        character(256) :: line
        character(256), allocatable :: tokens(:)

        ! Open file
        stat = 0
        open(newunit=fd, file=file_name, status='old', iostat=stat)
        if (stat /= 0) return

        ! Check how many symops there are
        line(:) = ""
        n_symops = 0
        do
            ! Read current line and check for errors
            read(fd, '(a)', iostat=stat) line
            if (stat < 0) exit

            ! Check for the start of the symops
            if (index(line, "_symmetry_equiv_pos_as_xyz") /= 0) then
                ! Iterate over the symops
                do
                    ! Read current line and check for errors
                    read(fd, '(a)', iostat=stat) line
                    if (stat < 0) exit
                    if (index(line, "loop_") /= 0) exit

                    n_symops = n_symops + 1
                enddo
            endif
        enddo

        ! Rewind fd
        rewind(fd)

        ! Allocate memory for the new atom list
        allocate(new_list(n_symops * size(atom_list)))
        new_list(:)%name = "N/A"
        new_list(:)%x = 0
        new_list(:)%y = 0
        new_list(:)%z = 0

        ! Iterate over lines until we find the given field
        line(:) = ""
        total_idx = 1
        do
            ! Read current line and check for errors
            read(fd, '(a)', iostat=stat) line
            if (stat < 0) exit

            ! Check for the start of the symops
            if (index(line, "_symmetry_equiv_pos_as_xyz") /= 0) then
                ! Iterate over the symops
                do
                    ! Read current line and check for errors
                    read(fd, '(a)', iostat=stat) line
                    if (stat < 0) exit
                    if (index(line, "loop_") /= 0) exit

                    ! Apply the symop to every base atom
                    do atom_idx = 1, size(atom_list)
                        ! Compute new atom
                        call cif_parse_symop(line, new_list(total_idx), &
                            atom_list(atom_idx)%x, atom_list(atom_idx)%y, atom_list(atom_idx)%z)

                        ! Copy over atom name and increase counter
                        new_list(total_idx)%name(1:6) = atom_list(atom_idx)%name(1:6)
                        total_idx = total_idx + 1
                    enddo
                enddo
            endif
        enddo

        ! Close file
        close(fd)
    end subroutine

    ! Parses and applys a given symop to a given atom
    subroutine cif_parse_symop(symop, curr_atom, x, y, z)
        character(*), intent(in) :: symop
        real(kind=8), intent(in) :: x, y, z
        type(atom), intent(inout) :: curr_atom
        character(256), allocatable :: tokens(:)
        real(kind=8) :: new_x = 0, new_y = 0, new_z = 0

        ! Split symop into tokens for each coordinate
        tokens = split_string(trim(symop), ',')

        ! Apply operations to x
        call cif_parse_symop_single(tokens(1), x, y, z, new_x)

        ! Apply operations to y
        call cif_parse_symop_single(tokens(2), x, y, z, new_y)

        ! Apply operations to z
        call cif_parse_symop_single(tokens(3), x, y, z, new_z)

        curr_atom%x = new_x
        curr_atom%y = new_y
        curr_atom%z = new_z

        ! Deallocate tokens
        deallocate(tokens)
    end subroutine

    ! Parses a symop for a single coordinate and applies it
    subroutine cif_parse_symop_single(symop, x, y, z, res)
        character(256), intent(inout) :: symop
        real(kind=8), intent(in) :: x, y, z
        real(kind=8), intent(out) :: res
        integer :: i = 0
        real(kind=8) :: temp1 = 0.0, temp2 = 0.0

        res = 0.0
        ! Simple case of replacement
        if (len_trim(symop) == 1) then
            if (symop(1:1) == 'x') res = x
            if (symop(1:1) == 'y') res = y
            if (symop(1:1) == 'z') res = z
        ! Actual parsing
        else
            do
                ! Get the next operator if applicable
                call cif_symop_get_next(symop, i)
                if (i == 0) exit

                ! Compute division
                if (symop(i:i) == '/') then
                    temp1 = cif_parse_real(symop(i-1:i-1))
                    temp2 = cif_parse_real(symop(i+1:i+1))
                    res = temp1 / temp2
                    symop(i-1:i+1) = "   "
                endif

                ! Compute adddition/subtraction
                if (symop(i:i) == '+' .or. symop(i:i) == '-') then
                    ! Simple case of +/- {x,y,z}
                    if (symop(i+1:i+1) == 'x' .or. symop(i+1:i+1) == 'y' .or. symop(i+1:i+1) == 'z') then
                        if (symop(i:i) == '+') then
                            if (symop(i+1:i+1) == 'x') res = res + x
                            if (symop(i+1:i+1) == 'y') res = res + y
                            if (symop(i+1:i+1) == 'z') res = res + z
                        endif
                        if (symop(i:i) == '-') then
                            if (symop(i+1:i+1) == 'x') res = res - x
                            if (symop(i+1:i+1) == 'y') res = res - y
                            if (symop(i+1:i+1) == 'z') res = res - z
                        endif

                        ! Remove applied operation
                        symop(i:i+1) = "  "
                        cycle
                    endif
                endif

                ! Remove leading whitespaces if applicable
                call shift_string_left(symop)
            enddo

            res = res
        endif
    end subroutine

    ! Gets the next token with the highest priority from the symop
    subroutine cif_symop_get_next(symop, idx)
        character(256), intent(inout) :: symop
        integer, intent(inout) :: idx
        integer :: i, high = 0
        character :: high_type = '/'

        ! Check if there is anything
        if (symop(1:1) == " ") then
            idx = 0
            return
        endif

        ! Iterate over string to find tokens
        i = 1
        do
            if (symop(i:i) == ' ') exit

            ! '/' has the highest priority
            if (symop(i:i) == '/') then
                idx = i
                return
            endif

            ! +/- have equal priority
            if (symop(i:i) == '+' .or. symop(i:i) == '-') then
                high = i
                high_type = symop(i:i)
            endif

            ! Advance
            i = i + 1
        enddo

        idx = high
    end subroutine
end module cif
