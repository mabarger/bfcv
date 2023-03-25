! Provides constants and types for working with atoms
module atoms
    implicit none
    public

    ! Atom type
    type :: atom
        character(8) :: name
        real(kind=8) :: x, y, z
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
end module atoms
