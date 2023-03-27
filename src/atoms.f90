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
        color(1.000, 1.000, 1.000), &
        color(0.851, 1.000, 1.000), &
        color(0.800, 0.502, 1.000), &
        color(0.761, 1.000, 0.000), &
        color(1.000, 0.710, 0.710), &
        color(0.565, 0.565, 0.565), &
        color(0.188, 0.314, 0.973), &
        color(1.000, 0.051, 0.051), &
        color(0.565, 0.878, 0.314), &
        color(0.702, 0.890, 0.961), &
        color(0.667, 0.361, 0.000), &
        color(0.439, 0.502, 0.565), &
        color(0.941, 0.784, 0.627), &
        color(0.314, 0.784, 0.471), &
        color(0.784, 0.502, 0.200), &
        color(0.502, 0.502, 0.502), &
        color(0.831, 0.478, 0.000), &
        color(0.459, 0.502, 0.000), &
        color(0.000, 0.502, 0.000), &
        color(0.122, 0.941, 0.467), &
        ! Danger zone: The following elements have no standardized colors
        color(0.686, 0.318, 0.882), &
        color(0.529, 0.882, 0.318), &
        color(0.882, 0.318, 0.529), &
        color(0.318, 0.529, 0.882), &
        color(0.965, 0.773, 0.275), &
        color(0.275, 0.965, 0.773), &
        color(0.773, 0.275, 0.965), &
        color(0.522, 0.278, 0.780), &
        color(0.780, 0.522, 0.278), &
        color(0.278, 0.780, 0.522), &
        color(0.871, 0.745, 0.059), &
        color(0.059, 0.871, 0.745), &
        color(0.745, 0.059, 0.871), &
        color(0.290, 0.416, 0.373), &
        color(0.373, 0.290, 0.416), &
        color(0.416, 0.373, 0.290), &
        color(0.965, 0.235, 0.141), &
        color(0.141, 0.965, 0.235), &
        color(0.235, 0.141, 0.965), &
        color(0.733, 0.318, 0.408), &
        color(0.408, 0.733, 0.318), &
        color(0.318, 0.408, 0.733), &
        color(0.765, 0.902, 0.184), &
        color(0.184, 0.765, 0.902), &
        color(0.902, 0.184, 0.765), &
        color(0.333, 0.635, 0.608), &
        color(0.608, 0.333, 0.635), &
        color(0.635, 0.608, 0.333), &
        color(0.961, 0.682, 0.208), &
        color(0.208, 0.961, 0.682), &
        color(0.682, 0.208, 0.961), &
        color(0.373, 0.875, 0.745), &
        color(0.745, 0.373, 0.875), &
        color(0.875, 0.745, 0.373), &
        color(0.184, 0.875, 0.765), &
        color(0.765, 0.184, 0.875), &
        color(0.875, 0.765, 0.184), &
        color(0.988, 0.380, 0.663), &
        color(0.663, 0.988, 0.380), &
        color(0.380, 0.663, 0.988), &
        color(0.380, 0.824, 0.988), &
        color(0.988, 0.380, 0.824), &
        color(0.824, 0.988, 0.380), &
        color(0.498, 0.247, 0.847), &
        color(0.078, 0.533, 0.125), &
        color(0.635, 0.082, 0.325), &
        color(0.980, 0.611, 0.109), &
        color(0.835, 0.376, 0.839), &
        color(0.125, 0.286, 0.780), &
        color(0.211, 0.960, 0.337), &
        color(0.811, 0.584, 0.039), &
        color(0.968, 0.427, 0.047), &
        color(0.682, 0.764, 0.952), &
        color(0.619, 0.941, 0.400), &
        color(0.133, 0.321, 0.117), &
        color(0.192, 0.482, 0.905), &
        color(0.890, 0.039, 0.286), &
        color(0.078, 0.352, 0.337), &
        color(0.305, 0.258, 0.678), &
        color(0.427, 0.968, 0.498), &
        color(0.156, 0.901, 0.035), &
        color(0.925, 0.890, 0.282), &
        color(0.780, 0.054, 0.368), &
        color(0.917, 0.560, 0.125), &
        color(0.031, 0.733, 0.611), &
        color(0.800, 0.819, 0.733)  &
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
                ! Compute distance
                distance = atom_distance(atoms_in(i), atoms_in(j))

                ! Compute ionic binding energy with the formula (Q1 * Q2) / distance
                curr_energy = (element_ox_states(atoms_in(i)%id) * element_ox_states(atoms_in(j)%id)) / distance
                binding_energy = binding_energy + curr_energy
            enddo
        enddo
    end function
end module atoms
