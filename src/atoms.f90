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
        color(0.800, 0.819, 0.733) &
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
end module atoms
