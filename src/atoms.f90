! Provides constants and types for working with atoms
module atoms
    use util
    implicit none
    public

    ! Atom type
    type :: atom
        character(8) :: name
        real(kind=8) :: x, y, z
        integer :: id
    end type atom

    ! Constants and tables
    real(kind=8), parameter :: angs_to_bohr = 1/0.52917721067
    real(kind=8), parameter :: hart_to_kcal = 627.5095
    real(kind=8), parameter :: eps = 0.0001
    character(len=2), dimension(86), parameter :: element_names = (/ &
        "H ", &! Hydrogen
        "He", &! Helium
        "Li", &! Lithium
        "Be", &! Beryllium
        "B ", &! Boron
        "C ", &! Carbon
        "N ", &! Nitrogen
        "O ", &! Oxygen
        "F ", &! Fluorine
        "Ne", &! Neon
        "Na", &! Sodium
        "Mg", &! Magnesium
        "Al", &! Aluminum
        "Si", &! Silicon
        "P ", &! Phosphorus
        "S ", &! Sulfur
        "Cl", &! Chlorine
        "Ar", &! Argon
        "K ", &! Potassium
        "Ca", &! Calcium
        "Sc", &! Scandium
        "Ti", &! Titanium
        "V ", &! Vanadium
        "Cr", &! Chromium
        "Mn", &! Manganese
        "Fe", &! Iron
        "Co", &! Cobalt
        "Ni", &! Nickel
        "Cu", &! Copper
        "Zn", &! Zinc
        "Ga", &! Gallium
        "Ge", &! Germanium
        "As", &! Arsenic
        "Se", &! Selenium
        "Br", &! Bromine
        "Kr", &! Krypton
        "Rb", &! Rubidium
        "Sr", &! Strontium
        "Y ", &! Yttrium
        "Zr", &! Zirconium
        "Nb", &! Niobium
        "Mo", &! Molybdenum
        "Tc", &! Technetium
        "Ru", &! Ruthenium
        "Rh", &! Rhodium
        "Pd", &! Palladium
        "Ag", &! Silver
        "Cd", &! Cadmium
        "In", &! Indium
        "Sn", &! Tin
        "Sb", &! Antimony
        "Te", &! Tellurium
        "I ", &! Iodine
        "Xe", &! Xenon
        "Cs", &! Cesium
        "Ba", &! Barium
        "La", &! Lanthanum
        "Ce", &! Cerium
        "Pr", &! Praseodymium
        "Nd", &! Neodymium
        "Pm", &! Promethium
        "Sm", &! Samarium
        "Eu", &! Europium
        "Gd", &! Gadolinium
        "Tb", &! Terbium
        "Dy", &! Dysprosium
        "Ho", &! Holmium
        "Er", &! Erbium
        "Tm", &! Thulium
        "Yb", &! Ytterbium
        "Lu", &! Lutetium
        "Hf", &! Hafnium
        "Ta", &! Tantalum
        "W ", &! Tungsten
        "Re", &! Rhenium
        "Os", &! Osmium
        "Ir", &! Iridium
        "Pt", &! Platinum
        "Au", &! Gold
        "Hg", &! Mercury
        "Tl", &! Thallium
        "Pb", &! Lead
        "Bi", &! Bismuth
        "Po", &! Polonium
        "At", &! Astatine
        "Rn"  &! Radon
    /)

    ! Most common oxidation states for each element
    integer, dimension(87), parameter :: element_ox_states = [ &
        1, &
        0, &
        1, &
        2, &
        3, &
        4, &
        5, &
        -2, &
        -1, &
        0, &
        1, &
        2, &
        3, &
        -4, &
        -3, &
        -2, &
        -1, &
        0, &
        1, &
        2, &
        3, &
        4, &
        5, &
        6, &
        7, &
        3, &
        3, &
        2, &
        2, &
        2, &
        3, &
        -4, &
        -3, &
        -2, &
        -1, &
        2, &
        1, &
        2, &
        3, &
        4, &
        5, &
        4, &
        4, &
        3, &
        3, &
        3, &
        2, &
        1, &
        2, &
        3, &
        4, &
        -3, &
        -2, &
        2, &
        1, &
        2, &
        3, &
        3, &
        3, &
        3, &
        3, &
        3, &
        3, &
        3, &
        3, &
        3, &
        3, &
        3, &
        3, &
        3, &
        3, &
        4, &
        5, &
        4, &
        4, &
        4, &
        4, &
        4, &
        3, &
        2, &
        1, &
        2, &
        3, &
        -2, &
        -1, &
        0, &
        0 & ! Fallthrough for invalid elements
    ]

    ! Nuclear radii of the elements 
    real, parameter :: element_radius_max = 298
    real(kind=8), dimension(87), parameter :: element_radius = [ &
        53, &
        31, &
        167, &
        112, &
        87, &
        67, &
        56, &
        48, &
        42, &
        38, &
        190, &
        145, &
        118, &
        111, &
        98, &
        88, &
        79, &
        71, &
        243, &
        194, &
        184, &
        176, &
        171, &
        166, &
        161, &
        156, &
        152, &
        149, &
        145, &
        142, &
        136, &
        125, &
        114, &
        103, &
        94, &
        88, &
        265, &
        219, &
        212, &
        206, &
        198, &
        190, &
        183, &
        178, &
        173, &
        169, &
        165, &
        161, &
        156, &
        145, &
        133, &
        123, &
        115, &
        108, &
        298, &
        253, &
        226, &
        210, &
        247, &
        206, &
        205, &
        238, &
        231, &
        233, &
        225, &
        228, &
        226, &
        226, &
        222, &
        222, &
        217, &
        208, &
        200, &
        193, &
        188, &
        185, &
        180, &
        177, &
        174, &
        171, &
        156, &
        154, &
        143, &
        135, &
        127, &
        120, &
        0 & ! Fallthrough for invalid elements
    ]

    ! Covalent radii of the elements
    real(kind=8), dimension(87), parameter :: element_radius_cov = [ &
        32, &
        46, &
        133, &
        102, &
        85, &
        75, &
        71, &
        63, &
        64, &
        67, &
        155, &
        139, &
        126, &
        116, &
        111, &
        103, &
        99, &
        96, &
        196, &
        171, &
        148, &
        136, &
        134, &
        122, &
        119, &
        116, &
        111, &
        110, &
        112, &
        118, &
        124, &
        121, &
        121, &
        116, &
        114, &
        117, &
        210, &
        185, &
        163, &
        154, &
        147, &
        138, &
        128, &
        125, &
        125, &
        120, &
        128, &
        136, &
        142, &
        140, &
        140, &
        136, &
        133, &
        131, &
        232, &
        196, &
        180, &
        163, &
        176, &
        174, &
        173, &
        172, &
        168, &
        169, &
        168, &
        167, &
        166, &
        165, &
        164, &
        170, &
        162, &
        152, &
        146, &
        137, &
        131, &
        129, &
        122, &
        123, &
        124, &
        133, &
        144, &
        144, &
        151, &
        145, &
        147, &
        142, &
        0 & ! Fallthrough for invalid elements
    ]

    ! Mass of the elements in normalized atomic mass units
    real(kind=8), dimension(87), parameter :: element_mass = [ &
        1.00794d0, &
        4.002602d0, &
        6.938d0, &
        9.012183d0, &
        10.81d0, &
        12.011d0, &
        14.007d0, &
        15.999d0, &
        18.998d0, &
        20.180d0, &
        22.990d0, &
        24.305d0, &
        26.982d0, &
        28.086d0, &
        30.974d0, &
        32.066d0, &
        35.45d0, &
        39.948d0, &
        39.098d0, &
        40.078d0, &
        44.956d0, &
        47.867d0, &
        50.942d0, &
        51.996d0, &
        54.938d0, &
        55.845d0, &
        58.933d0, &
        58.693d0, &
        63.546d0, &
        65.380d0, &
        69.723d0, &
        72.630d0, &
        74.922d0, &
        78.960d0, &
        79.904d0, &
        83.798d0, &
        85.468d0, &
        87.620d0, &
        88.906d0, &
        91.224d0, &
        92.906d0, &
        95.940d0, &
        98.000d0, &
        101.070d0, &
        102.905d0, &
        106.420d0, &
        107.868d0, &
        112.410d0, &
        114.820d0, &
        118.710d0, &
        121.760d0, &
        127.600d0, &
        126.905d0, &
        131.293d0, &
        132.905d0, &
        137.327d0, &
        138.905d0, &
        140.116d0, &
        140.908d0, &
        144.242d0, &
        145.000d0, &
        150.360d0, &
        151.964d0, &
        157.250d0, &
        158.925d0, &
        162.500d0, &
        164.930d0, &
        167.259d0, &
        168.934d0, &
        173.054d0, &
        174.967d0, &
        178.490d0, &
        180.948d0, &
        183.840d0, &
        186.207d0, &
        190.230d0, &
        192.217d0, &
        195.084d0, &
        196.568d0, &
        200.27d0, &
        204.01d0, &
        207.20d0, &
        208.980374d0, &
        209.983126d0, &
        210.987481d0, &
        222.017577d0, &
        0.0d0 & ! In case no atom matches, a default value
    ]

    ! Note that only the first 20-ish atoms have well defined colors in literature
    type(color), dimension(86), parameter :: element_colors = [ &
        ! Hydrogen: white-blue
        color(0.850, 0.850, 1.000), &

        ! Helium: gray
        color(0.753, 0.753, 0.753), &

        ! Lithium: maroon
        color(0.612, 0.259, 0.259), &

        ! Beryllium: olive
        color(0.612, 0.612, 0.259), &

        ! Boron: brown
        color(0.757, 0.612, 0.478), &

        ! Carbon: black
        color(0.000, 0.000, 0.000), &

        ! Nitrogen: blue
        color(0.122, 0.498, 0.753), &

        ! Oxygen: red
        color(0.753, 0.122, 0.122), &

        ! Fluorine: greenish-yellow
        color(0.945, 1.000, 0.188), &

        ! Neon: orange-red
        color(1.000, 0.506, 0.063), &

        ! Sodium: dark gray
        color(0.412, 0.412, 0.412), &

        ! Magnesium: dark olive
        color(0.741, 0.714, 0.412), &

        ! Aluminum: light gray
        color(0.753, 0.753, 0.753), &

        ! Silicon: blue-gray
        color(0.400, 0.620, 0.710), &

        ! Phosphorus: orange
        color(1.000, 0.502, 0.000), &

        ! Sulfur: yellow
        color(0.900, 0.900, 0.188), &

        ! Chlorine: green
        color(0.122, 0.941, 0.122), &

        ! Argon: dark gray
        color(0.502, 0.820, 0.890), &

        ! Potassium: purple
        color(0.561, 0.259, 0.957), &

        ! Calcium: gray
        color(0.753, 0.753, 0.753), &

        ! Danger zone: The following elements have no standardized colors
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502), &
        color(0.502, 0.502, 0.502) &
    ]

contains
    ! Prints the contents of an atom nicely
    subroutine print_atom(atom_in)
        type(atom), intent(in) :: atom_in
        write(*, "(A,F10.6,F10.6,F10.6)") atom_in%name, atom_in%x, atom_in%y, atom_in%z
    end subroutine

    ! Prints the atoms in an array neatly
    subroutine print_atoms(atoms_in)
        type(atom), intent(in) :: atoms_in(:)
        integer :: i = 0

        write(*, "(A)") "name      x_pos     y_pos     z_pos   "
        write(*, "(A)") "======================================"
        do i = 1, size(atoms_in)
            call print_atom(atoms_in(i))
        enddo
    end subroutine

    ! Tries to match the given atom name to a normalized name and derive the element id
    subroutine match_atom_name(curr_atom)
        type(atom), intent(inout) :: curr_atom
        integer :: i

        ! Regular check
        do i = 1, size(element_names)
            ! Check if the name matches to omit subspecifiers (e.g. O2 or N_an)
            if (curr_atom%name(1:2) == element_names(i)(1:2)) then
                curr_atom%name(:) = ""
                curr_atom%name(1:2) = element_names(i)(1:2)
                curr_atom%id = i
                return
            endif
        enddo

        ! If no match was found try to look for single letter names
        do i = 1, size(element_names)
            ! Check if the name matches to omit subspecifiers (e.g. O2 or N_an)
            if (curr_atom%name(1:1) == element_names(i)(1:1)) then
                curr_atom%name(:) = ""
                curr_atom%name(1:2) = element_names(i)(1:2)
                curr_atom%id = i
                return
            endif
        enddo

        ! If no match was found then assign a default id
        curr_atom%id = size(element_names)
    end subroutine

    ! Matches all atom names in the given array
    subroutine match_atom_names(atoms_in)
        type(atom), intent(inout) :: atoms_in(:)
        integer :: i

        do i = 1, size(atoms_in)
            call match_atom_name(atoms_in(i))
        enddo
    end subroutine

    ! Sorts atoms based on their z-coordinate
    subroutine sort_atoms_by_z(atoms_in)
        type(atom), intent(inout) :: atoms_in(:)
        integer :: i, j, min_idx, n
        type(atom) :: temp

        n = size(atoms_in)

        ! Loop over all atoms
        do i = 1, n-1
            min_idx = i
            ! Find a matching spot
            do j = i+1, n
                if (atoms_in(j)%z < atoms_in(min_idx)%z) then
                min_idx = j
                endif
            enddo
            ! Place the atom in the correct spot
            if (min_idx /= i) then
                temp = atoms_in(i)
                atoms_in(i) = atoms_in(min_idx)
                atoms_in(min_idx) = temp
            endif
        enddo
    end subroutine

    ! Computes the distance between two atoms in a normalized unit cell
    function atom_distance(atom1, atom2) result(distance)
        type(atom), intent(in) :: atom1, atom2
        real(kind=8) :: distance

        distance = sqrt((atom1%x - atom2%x)**2 + (atom1%y - atom2%y)**2 + (atom1%z - atom2%z)**2)
    end function

    ! Computes the total molecular mass in g/mol
    function compute_total_molecular_mass(atoms_in) result(molecular_mass)
        type(atom), intent(inout) :: atoms_in(:)
        real(kind=8) :: molecular_mass
        integer :: i

        molecular_mass = 0.0

        ! Loop over atoms
        do i = 1, size(atoms_in)
            ! Add the mass of the atom
            molecular_mass = molecular_mass + element_mass(atoms_in(i)%id)
        enddo
    end function

    ! Computes the ionic binding energy by assuming the most common oxidation state for each element
    function compute_ionic_binding_energy(atoms_in) result(binding_energy)
        type(atom), intent(inout) :: atoms_in(:)
        real(kind=8) :: binding_energy, distance, curr_energy
        integer :: i, j

        binding_energy = 0.0

        ! Loop over atoms
        do i = 1, size(atoms_in)
            ! Loop over all other atoms to get unique pairs
            do j = i+1, size(atoms_in)
                ! Compute distance and convert it to bohr
                distance = atom_distance(atoms_in(i), atoms_in(j)) * angs_to_bohr

                ! Compute ionic binding energy with the formula (Q1 * Q2) / distance
                curr_energy = (element_ox_states(atoms_in(i)%id) * element_ox_states(atoms_in(j)%id)) / distance
                binding_energy = binding_energy + curr_energy
            enddo
        enddo

        ! Convert Hartree to kcal/mol
        binding_energy = binding_energy * hart_to_kcal
    end function

    ! Copmutes possible bonds between atoms based on their covalent radii
    subroutine compute_bonds(atoms_in)
        type(atom), intent(in) :: atoms_in(:)
        real(kind=8) :: distance = 0.0, total_cov_radius = 0.0
        integer :: i, j

        ! Loop over atoms
        do i = 1, size(atoms_in)
            ! Loop over all other atoms to get unique pairs
            do j = i+1, size(atoms_in)
                ! Compute distance
                distance = atom_distance(atoms_in(i), atoms_in(j))

                ! Compute total covalent bond radius and convert it from nm to pm
                total_cov_radius = (element_radius_cov(atoms_in(i)%id) + element_radius_cov(atoms_in(j)%id)) / 1000

                ! Check whether a bond is possible by checking the distance between the atoms
                if (total_cov_radius > distance) then
                    print *, "Possible bond between"
                    call print_atom(atoms_in(i))
                    call print_atom(atoms_in(j))
                    print *, ""
                endif
            enddo
        enddo
    end subroutine
end module atoms
