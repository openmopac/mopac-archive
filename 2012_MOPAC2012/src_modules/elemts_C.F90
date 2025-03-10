      module elemts_C 
      USE vast_kind_param, ONLY:  double 
!...Created by Pacific-Sierra Research 77to90  4.4G  08:50:09  03/09/06  
      character, dimension(107) :: elemnt*2 
      character (len=12), dimension(107) :: atom_names
      data elemnt/ ' H', 'He', 'Li', 'Be', ' B', ' C', ' N', ' O', ' F', 'Ne', &
        'Na', 'Mg', 'Al', 'Si', ' P', ' S', 'Cl', 'Ar', ' K', 'Ca', 'Sc', 'Ti'&
        , ' V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Ge', 'As', &
        'Se', 'Br', 'Kr', 'Rb', 'Sr', ' Y', 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh'&
        , 'Pd', 'Ag', 'Cd', 'In', 'Sn', 'Sb', 'Te', ' I', 'Xe', 'Cs', 'Ba', &
        'La', 'Ce', 'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er'&
        , 'Tm', 'Yb', 'Lu', 'Hf', 'Ta', ' W', 'Re', 'Os', 'Ir', 'Pt', 'Au', &
        'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', 'Pa'&
        , ' U', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Mi', 'XX', '+3', '-3', 'Cb', &
        '++', ' +', '--', ' -', 'Tv'/  
               data atom_names / &
   & "    Hydrogen", "      Helium", "     Lithium", "   Beryllium", &
   & "       Boron", "      Carbon", "    Nitrogen", "      Oxygen", &
   & "    Fluorine", "        Neon", "      Sodium", "   Magnesium", &
   & "    Aluminum", "     Silicon", "  Phosphorus", "      Sulfur", &
   & "    Chlorine", "       Argon", "   Potassium", "     Calcium", &
   & "    Scandium", "    Titanium", "    Vanadium", "    Chromium", &
   & "   Manganese", "        Iron", "      Cobalt", "      Nickel", &
   & "      Copper", "        Zinc", "     Gallium", "   Germanium", &
   & "     Arsenic", "    Selenium", "     Bromine", "     Krypton", &   
   & "    Rubidium", "   Strontium", "     Yttrium", "   Zirconium", &
   & "     Niobium", "  Molybdenum", "  Technetium", "   Ruthenium", &
   & "     Rhodium", "   Palladium", "      Silver", "     Cadmium", &
   & "      Indium", "         Tin", "    Antimony", "   Tellurium", &
   & "      Iodine", "       Xenon", "      Cesium", "      Barium", &
   & "   Lanthanum", "      Cerium", "Praseodymium", "   Neodymium", &
   & "  Promethium", "    Samarium", "    Europium", "  Gadolinium", &
   & "     Terbium", "  Dysprosium", "     Holmium", "      Erbium", &
   & "     Thulium", "   Ytterbium", "    Lutetium", &
   & "     Hafnium", "    Tantalum", "    Tungsten", &
   & "     Rhenium", "      Osmium", "     Iridium", "    Platinum", &
   & "        Gold", "     Mercury", "    Thallium", "        Lead", &
   & "     Bismuth", "    Polonium", "    Astatine", "       Radon", &
   & "    Francium", "      Radium", "    Actinium", "     Thorium", &
   & "Protactinium", "     Uranium", "   Neptunium", "   Plutonium", &
   & "   Americium", "      Curium", "   Berkelium", "     Mithril", &
   & "  Dummy atom", &
   & "  3+ Sparkle", "  3- Sparkle", " Capped bond", "  ++ Sparkle", &
   & "   + Sparkle", "  -- Sparkle", "   - Sparkle", "     Tv     "/
      end module elemts_C 
