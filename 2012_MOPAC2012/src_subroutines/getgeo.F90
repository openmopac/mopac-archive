      subroutine getgeo(iread, labels, geo, xyz, lopt, na, nb, nc, int) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!
      use parameters_C, only : ams
!
      use molkst_C, only : natoms, keywrd, numat, maxtxt, line, moperr, &
        numcal
!
      use chanel_C, only : iw
!
      use common_arrays_C, only :atmass, simbol, txtatm, na_store, nat
!
      USE maps_C, ONLY: react
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:17  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use upcase_I    
      use reada_I 
      use mopend_I 
      use geout_I 
      use nuchar_I 
      use gmetry_I 
      use xyzint_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iread 
      integer , intent(out) :: labels(*) 
      integer , intent(out) :: lopt(3,*)
      integer, intent(out)  :: na(*) 
      integer, intent(out)  :: nb(*) 
      integer, intent(out)  :: nc(*) 
      real(double), intent(out)  :: geo(3,*), xyz(3,*)
      logical :: int, lxyzint, lmop, solid

!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(40) :: istart 
      integer :: i, icapa, icapz, iserr, k, icomma, khar, nvalue, label, j, ndmy, &
      jj, ltl, max_atoms
      real(double) :: weight, real, sum 
      logical :: lxyz, velo, leadsp, ircdrc, saddle, lturn, mini
      character , dimension(107) :: elemnt*2 
      character :: space, nine, zero, comma, string*120, ele*2, turn, no

      save elemnt, space, nine, zero, comma 
!-----------------------------------------------
!***********************************************************************
!
!   GETGEO READS IN THE GEOMETRY. THE ELEMENT IS SPECIFIED BY IT'S
!          CHEMICAL SYMBOL, OR, OPTIONALLY, BY IT'S ATOMIC NUMBER.
!
!  ON INPUT   IREAD  = CHANNEL NUMBER FOR READ, NORMALLY 5
!             AMS    = DEFAULT ATOMIC MASSES.
!
! ON OUTPUT LABELS = ATOMIC NUMBERS OF ALL ATOMS, INCLUDING DUMMIES.
!           GEO    = INTERNAL COORDINATES, IN ANGSTROMS, AND DEGREES.
!                    OR CARTESIAN COORDINATES, DEPENDING ON WHETHER
!                    KEYWORD ' XYZ' IS PRESENT
!           LOPT   = INTEGER ARRAY, A '1' MEANS OPTIMIZE THIS PARAMETER,
!                    '0' MEANS DO NOT OPTIMIZE, AND A '-1' LABELS THE
!                    REACTION COORDINATE.
!           NA     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
!           NB     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
!           NC     = INTEGER ARRAY OF ATOMS (SEE DATA INPUT)
!           ATMASS = ATOMIC MASSES OF ATOMS.
!***********************************************************************
      data (elemnt(i),i=1,107)/ 'H', 'HE', 'LI', 'BE', 'B', 'C', 'N', 'O', 'F'&
        , 'NE', 'NA', 'MG', 'AL', 'SI', 'P', 'S', 'CL', 'AR', 'K', 'CA', 'SC', &
        'TI', 'V', 'CR', 'MN', 'FE', 'CO', 'NI', 'CU', 'ZN', 'GA', 'GE', 'AS', &
        'SE', 'BR', 'KR', 'RB', 'SR', 'Y', 'ZR', 'NB', 'MO', 'TC', 'RU', 'RH', &
        'PD', 'AG', 'CD', 'IN', 'SN', 'SB', 'TE', 'I', 'XE', 'CS', 'BA', 'LA', &
        'CE', 'PR', 'ND', 'PM', 'SM', 'EU', 'GD', 'TB', 'DY', 'HO', 'ER', 'TM'&
        , 'YB', 'LU', 'HF', 'TA', 'W', 'RE', 'OS', 'IR', 'PT', 'AU', 'HG', 'TL'&
        , 'PB', 'BI', 'PO', 'AT', 'RN', 'FR', 'RA', 'AC', 'TH', 'PA', 'U', 'NP'&
        , 'PU', 'AM', 'CM', 'BK', 'MI', 'XX', '+T', '-T', 'CB', '++', '+', '--'&
        , '-', 'TV'/  
      data comma, space, nine, zero/ ',', ' ', '9', '0'/  
      ircdrc = index(keywrd,' IRC') + index(keywrd,' DRC') /= 0 
      if (index(keywrd," FORCETS") /= 0) ircdrc = .false.
      icapa = ichar('A') 
      icapz = ichar('Z') 
      lturn = .true.
      saddle = (index(keywrd, "SADDLE") > 0)
      lxyz = (index(keywrd, " XYZ") > 0 .or. saddle)
      int = (index(keywrd, " INT ") > 0)
      velo = (index(keywrd,' VELO') > 0) 
      lmop = (Index (keywrd, " MOPAC") /= 0)
      max_atoms = size(txtatm)
      maxtxt = 0 
      simbol(:natoms*3) = '---------' 
      natoms = 0 
      numat = 0 
     ! call dbreak()
      iserr = 0      
      mini = (index(keywrd, " MINI") /= 0)
   20 continue 
      read (iread, '(A)', end=120, err=210) line 
      if (line == ' ') then
        if(natoms == 0 .and. numcal == 1) then
!
!  Check:  Is this an ARC file?
!
          numcal = 2
          rewind (iread)
          do i = 1, 10000
            read (iread, '(A)', end=120, err=210) line 
            if (index(line, "FINAL GEOMETRY OBTAINED") > 0) exit   
            if (index(line, "GEOMETRY IN CARTESIAN COORDINATE") > 0) exit 
            if (index(line, "GEOMETRY IN MOPAC Z-MATRIX") > 0) exit         
          end do
!
!  Yes, it's an arc file
!
          write(0,"(a)", iostat = j)"The data set was defined as a MOPAC input data file"
          write(0,"(a)", iostat = j)"but it appears to an archive file"
          write(0,"(a)", iostat = j)"An attempt will be made to read it as an .ARC file"
          natoms = -3
          return
        end if
        goto 120
      end if
      if (natoms == max_atoms) then
        write(iw,"(//10x,a)")" Maximum number of atoms exceeded"
        write(iw,"(10x,a,i6)")" Maximum allowed:", max_atoms
        call mopend(" Maximum number of atoms exceeded")
        return
      end if
!
!   SEE IF TEXT IS ASSOCIATED WITH THIS ELEMENT
!
      i = index(line,'(') 
      if (i /= 0) then 
!
!  YES, ELEMENT IS LABELLED.
!
        k = index(line,')')
        if (k == 0) then
          write(iw,"(a,i5,a)")" Atom",natoms," has an opening parenthesis but no closing parenthesis"
          write(iw,"(a)")" Line :'"//line(:len_trim(line))//"'"
          write (iw, '(/,A)') ' GEOMETRY IS FAULTY.  GEOMETRY READ IN IS' 
          atmass(1) = -1.D0 
          nat(1) = 0
          call geout (iw) 
          call mopend ('GEOMETRY IS FAULTY') 
          return  
        end if
        txtatm(natoms + 1) = line(i:k) 
        maxtxt = max(maxtxt,k - i + 1) 
        if (maxtxt > 38) then
          write(iw,"(a)")" Atom labels must not exceed 38 characters"
          string = " "
          write(iw,"(a)")string(:i)//"                1         2         3         4"
          write(iw,"(a)")string(:i)//"       1234567890123456789012345678901234567890"
          write(iw,"(a)")"Line :"""//line(:len_trim(line))//""""
          write (iw, '(/,A)') ' GEOMETRY IS FAULTY.  GEOMETRY READ IN IS' 
          atmass(1) = -1.D0 
          nat(1) = 0
          call geout (iw) 
          call mopend ('GEOMETRY IS FAULTY') 
          return  
        end if
        string = line(1:i-1)//line(k+1:) 
        line = string 
      else 
        txtatm(natoms + 1) = ' ' 
      endif 
!   CLEAN THE INPUT DATA
      ltl = len_trim(line)
      call upcase (line, ltl) 
      icomma = ichar(comma) 
      do i = 1, ltl 
        khar = ichar(line(i:i)) 
        if (khar == icomma) line(i:i) = space 
      end do 
!
!   INITIALIZE ISTART TO INTERPRET BLANKS AS ZERO'S
      istart(:10) = len_trim(line) + 1
!
! FIND INITIAL DIGIT OF ALL NUMBERS, CHECK FOR LEADING SPACES FOLLOWED
!     BY A CHARACTER AND STORE IN ISTART
      leadsp = .TRUE. 
      nvalue = 0 
      do i = 1, ltl 
        if (leadsp .and. line(i:i) /= space) then 
          nvalue = nvalue + 1 
          istart(nvalue) = i  
          if (i > 2 .and. line(i - 1:i - 1) ==  "-" .and. line(i - 2:i - 2) /=  " ") istart(nvalue) = i - 1
        endif 
!
!  set leadsp true if a space is detected, or if a "-" sign is found and it's part of a new number
!
        leadsp = (line(i:i) == space .or. (i /= istart(max(nvalue,1)) .and. line(i:i) ==  "-"))
        if (i > 1 .and. (line(i-1:i) == "D-" .or. line(i-1:i) == "E-")) leadsp = .false.
      end do 
!
! ESTABLISH THE ELEMENT'S NAME AND ISOTOPE, CHECK FOR ERRORS OR E.O.DATA
!
      weight = 0.D0 
      string = line(istart(1):istart(2)-1) 
      if (string == "+3") string = "+T"
      if (string == "-3") string = "-T"
      if (string(1:1)>=zero .and. string(1:1)<=nine) then 
!  ATOMIC NUMBER USED: NO ISOTOPE ALLOWED
        label = nint(reada(string,1)) 
        if (label == 0) go to 120 
        if (label < 0 .or. label > 107) then 
          write (iw, '(''  ILLEGAL ATOMIC NUMBER'')') 
          go to 210 
        endif 
        go to 70 
      endif 
!  ATOMIC SYMBOL USED
      real = abs(reada(string,1)) 
      if (real < 1.D-15) then 
!   NO ISOTOPE
        ele = string(1:2) 
      else 
        weight = real 
        if (string(2:2)>=zero .and. string(2:2)<=nine) then 
          ele = string(1:1) 
        else 
          ele = string(1:2) 
        endif 
      endif 
!   CHECK FOR ERROR IN ATOMIC SYMBOL
      if (ele(1:1)=='-' .and. ele(2:2)/='-') ele(2:2) = ' ' 
      do i = 1, 107 
        if (ele /= elemnt(i)) cycle  
        label = i 
        go to 70 
      end do 
      if (ele(1:1) == 'X') then 
        label = 99 
        go to 70 
      endif 
      if (ele == "D ") then
        label = 1 
        weight = 2.014d0
        go to 70 
      else if (ele == "T ") then
        label = 1 
        weight = 3.016d0
        go to 70 
      else
        write (iw, '(''  UNRECOGNIZED ELEMENT NAME: ('',A,'')'')') ele 
        go to 210 
      end if
!
! ALL O.K.
!
 70   continue 
      natoms = natoms + 1 
      nb(natoms) = 0
      nc(natoms) = 0
      if (label /= 99) numat = numat + 1 
      if (weight /= 0.D0) then 
        atmass(numat) = weight 
      else 
        if (label /= 99) atmass(numat) = ams(label) 
      endif 
      labels(natoms) = label 
      if (nvalue == 4) then !  Cartesian coordinates without optimization flags
        geo(1,natoms) = reada(line,istart(2)) 
        geo(2,natoms) = reada(line,istart(3)) 
        geo(3,natoms) = reada(line,istart(4)) 
        lopt(1,natoms) = 1
        lopt(2,natoms) = 1
        lopt(3,natoms) = 1 
      else
        geo(1,natoms) = reada(line,istart(2)) 
        geo(2,natoms) = reada(line,istart(4)) 
        geo(3,natoms) = reada(line,istart(6)) 
        if (.not. mini .and. ircdrc) then 
          turn = line(istart(3):istart(3)) 
          if (turn == 'T') then 
            lopt(1,natoms) = 1 
            if (lturn) then
              write (iw, '(A)') &
              ' IN DRC MONITOR POTENTIAL ENERGY TURNING POINTS' 
              lturn = .false.
            end if
          else 
            lopt(1,natoms) = 0 
          endif 
          turn = line(istart(5):istart(5)) 
          if (turn == 'T') then 
            lopt(2,natoms) = 1 
          else 
            lopt(2,natoms) = 0 
          endif 
          turn = line(istart(7):istart(7)) 
          if (turn == 'T') then 
            lopt(3,natoms) = 1 
          else 
            lopt(3,natoms) = 0 
          endif 
        else 
          lopt(1,natoms) = nint(reada(line,istart(3))) 
          lopt(2,natoms) = nint(reada(line,istart(5))) 
          lopt(3,natoms) = nint(reada(line,istart(7))) 
          do i = 3, 7, 2 
            if (.not.(ichar(line(istart(i):istart(i)))>=icapa .and. ichar(line(&
              istart(i):istart(i)))<=icapz .and. natoms>1)) cycle  
            iserr = 1 
          end do 
        endif 
      end if
      sum = reada(line,istart(8)) 
      i = index(line(istart(8):), " ") + istart(8) ! Find end of 8'th datum
      if (index(line(istart(8):i), ".") /= 0) sum = 0.D0 ! if 8'th datum contains a decimal, then it's not a connectivity
      na(natoms) = nint(sum) 
      if (natoms == 1) na(1) = 0
      if (lmop .and. natoms == 2) then
        na(2) = 1  
      else if (lmop .and. natoms == 3 .and. na(3) == 0) then
        na(3) = 2 
        nb(3) = 1 
        geo(2,3) = geo(2,3)*1.7453292519943D-02 
        geo(3,3) = 0.D0 
        lopt(3,3) = 0 
      else if (na(natoms) > 0) then
        nb(natoms) = nint(reada(line,istart(9))) 
        nc(natoms) = nint(reada(line,istart(10))) 
        geo(2:3,natoms) = geo(2:3,natoms)*1.7453292519943D-02 
      end if
      
!
!  SPECIAL CASE OF USERS FORGETTING TO ADD DIHEDRAL DATA FOR ATOM 3
!
      if (natoms == 3) then 
        if (lopt(3,3) == 2) then 
          na(3) = 1 
          nb(3) = 2 
          geo(3,3) = 0.D0 
          lopt(3,3) = 0 
        else if (lopt(3,3)==1 .and. line(istart(6):istart(6) + 1) == "2 ") then 
          na(3) = 2 
          nb(3) = 1 
          geo(3,3) = 0.D0 
          geo(2,3) = geo(2,3)*1.7453292519943D-02 
          lopt(3,3) = 0 
        endif 
      endif 
      if ( .not. mini) then
        if ((lopt(1,natoms) > 1 .or. lopt(2,natoms) > 1 .or. lopt(3,natoms) > 1) .and. natoms > 1) iserr = 1 
      end if
      if (iserr == 1) then 
      if (index(line,"ATOM") + index(line,"HETATM") /= 0) then
!
!  Geometry is definitely PDB
!
        natoms = -2
        return
      end if
!
!  MUST BE GAUSSIAN GEOMETRY INPUT
!
        do i = 2, natoms 
          do k = 1, 3 
            j = nint(geo(k,i)) 
            if (abs(geo(k,i)-j) <= 1.D-5) cycle  
!
!   GEOMETRY CANNOT BE GAUSSIAN
!
           write (iw, '(A)') ' GEOMETRY IS FAULTY.  GEOMETRY READ IN IS' 
            atmass(1) = -1.D0 
            nat(1) = 0
            call geout (iw) 
            call mopend ('GEOMETRY IS FAULTY') 
            return  
          end do 
        end do 
        natoms = -1 
        return  
      endif 
      go to 20 
!***********************************************************************
! ALL DATA READ IN, CLEAN UP AND RETURN
!***********************************************************************
  120 continue 
      if (natoms == 0) then 
        call mopend ('Error in GETGEO') 
        return  
      endif  
      if (maxtxt > 0) then
        do i = 1, natoms
          if (txtatm(i)(1:1) /= "(") then
            txtatm(i)(1:1) = "("
            txtatm(i)(maxtxt:maxtxt) = ")"
          else
            j = len_trim(txtatm(i))
            if (j /= maxtxt) then  !  Pad out text with blanks
              txtatm(i)(j:j) = " "
              txtatm(i)(maxtxt:maxtxt) = ")"
            end if
          end if
        end do
      end if
      na(1) = 0
      nb(1) = 0
      nc(1) = 0 
      nb(2) = 0
      nc(2) = 0
      nc(3) = 0    
!
!     READ IN VELOCITY VECTOR, IF PRESENT
!
      if (velo) then 
        j = 0
        do i = 1, natoms
          if (na(i) /= 0) j = 1
        end do
        if ((j /= 0 .or. int .and. index(keywrd,' LET') == 0) .and. index(keywrd,' 0SCF') == 0) then 
          write (iw, '(A)') &
            ' COORDINATES MUST BE CARTESIAN WHEN VELOCITY VECTOR IS USED.' 
          call mopend ('COORDINATES MUST BE CARTESIAN WHEN VELOCITY VECTOR IS USED.') 
          return  
        endif 
        if (allocated(react)) deallocate(react)
        allocate(react(3*natoms))
        do i = 1, natoms 
          read (iread, '(A)') line 
          call nuchar (line, len_trim(line), react((i-1)*3+1), ndmy) 
          if (ndmy == 3) cycle  
          write (iw, '(/10X,A)') &
            '  THERE MUST BE EXACTLY THREE VELOCITY DATA PER LINE' 
          call mopend ('THERE MUST BE EXACTLY THREE VELOCITY DATA PER LINE.') 
          return  
        end do 
      endif
      if (ircdrc) then
        if (numat /= natoms) then
          write(iw,*)" Only real atoms are allowed in IRC and DRC calculations."
          call mopend ('Only real atoms are allowed in IRC and DRC calculations.')
          return
        end if
        call gmetry(geo, xyz)
        call xyzint (xyz, numat, na, nb, nc, 1.D0, geo) 
        geo(:,1:numat) = xyz(:,1:numat)
        na_store = na(:numat) ! Store na - it will be used in printing the DRC
        na(:numat) = 0
      end if
      solid = .false.
      do i = 1, natoms 
        if (labels(i) <= 0) then 
          write (iw, '('' ATOMIC NUMBER OF '',I3,'' ?'')') labels(i) 
          if (i == 1) then 
            write (iw, '(A)') ' THIS WAS THE FIRST ATOM' 
          else 
            write (iw, '(A)') &
              '    GEOMETRY UP TO, BUT NOT INCLUDING, THE FAULTY ATOM' 
            natoms = i - 1  
            call geout (iw) 
          endif  
          call mopend ('Error in READMO') 
          return  
        endif 
        if (labels(i) == 107) solid = .true.
        if (solid .and. labels(i) < 99) then
          write (iw, '('' TRANSLATION VECTORS MUST BE AT THE END OF THE DATA SET'')') 
          call mopend ('TRANSLATION VECTORS MUST BE AT THE END OF THE DATA SET') 
          return
        end if
        if (i == 1 .or. na(i) == 0) cycle
        j = 0
        if (na(i) == nb(i).and. i > 1 .or. &
          (na(i) == nc(i) .or. nb(i) == nc(i)) .and. i > 2 .or. &
          nb(i)*nc(i) == 0 .and. i > 3) j = 1  !  Error condition
        if (na(i) >= i .or. nb(i) >= i .or. nc(i) >= i) then 
!
! An atom is being defined using a connectivity involving one or more atoms
! whose positions have not yet been defined.  This is likely to be an error.
!
          j = j + 1
!
!  Test: The atom is defined in internal coordinates, so check that
!  the dependent atoms are defined in Cartesian coordinates
! 
          if ((na(i) < i .or. na(max(1,na(i))) == 0) .and. & 
              (nb(i) < i .or. na(max(1,nb(i))) == 0) .and. &
              (nc(i) < i .or. na(max(1,nc(i))) == 0) ) j = j - 1 
        end if
        if (j == 0) cycle
        j = max(i, na(i), nb(i), nc(i))
        no = char(Nint(log10(j + 1.0)) + ichar("1"))
        write (iw, '(//10x, " ATOM NUMBER ",I'//no//'," IS ILL-DEFINED")') i 
        write(iw,'(/10x," Connectivity of atom ",i'//no//',": NA=",i'//no//',", NB=",i'//no//',", NC=",i'//no//')') &
        i, na(i), nb(i), nc(i)
        if (na(i) > i .and. na(na(i)) /= 0) &
          write(iw,'(/10x,a)')" NA is defined using internal coordinates" 
        if (nb(i) > i .and. na(nb(i)) /= 0) &
          write(iw,'(/10x,a)')" NB is defined using internal coordinates" 
        if (nc(i) > i .and. na(nc(i)) /= 0) &
          write(iw,'(/10x,a)')" NC is defined using internal coordinates" 
          call web_message(iw,"geometry_specification.html")
        if (i == 1) then 
          return  
        endif 
        write (iw, '(/,''  GEOMETRY READ IN'',/)')
        nat = 0 
        call geout (iw) 
        call mopend ('Error in READMO')  
        numat = 0
        natoms = 0 ! Kill the job - any further calculations would be nonsense
        return  
      end do 
      if (natoms > 0) then
        call gmetry (geo, xyz) 
      else
        call mopend ("No atoms!")
      end if
      if (moperr) return
!
!  Switch for converting between coordinate systems.
!
!      lxyzint is true if 0SCF is present and neither XYZ or INT are specified.
!      If lxyzint is true. then force coordinates to be internal.
!

      lxyzint = (index(keywrd,' 0SCF') /= 0 .and. .not. lxyz .and. .not. int &
      .and. .not. Index(keywrd,' RESTART') /= 0 .and. index(keywrd, " RESID") == 0)
      if (index(keywrd," GEO_REF") /= 0) lxyzint = .false.
      if (lxyz .or. int .or. lxyzint) then 
!
!    Coordinates should all be Cartesian or all be internal. 
!    First, unconditionally convert to Cartesian.
!
        k = 0
        do i = 1, natoms
          do j = 1, 3
            k = k + lopt(j,i)
          end do
        end do
!
!  Get rid of dummy atoms
!
        numat = 0 
        do i = 1, natoms 
          if (labels(i) /= 99) then 
            numat = numat + 1 
            labels(numat) = labels(i)
            txtatm(numat) = txtatm(i)
            lopt(:,numat) = lopt(:,i)
            na(numat) = 0
          endif 
          geo(:,i) = xyz(:,i) 
        end do 
!
!   If everything is marked for optimization then unconditionally mark the first
!   three atoms for optimization
!
        if (k >= 3*numat - 6) lopt(:,:min(3, numat)) = 1
        natoms = numat
        if (saddle) lopt(:,:numat) = 1  ! In a saddle calculation, all parameters must be optimizable.
      end if 
      if (int .or. lxyzint) then 
!
!  Coordinates should be internal. Unconditionally convert to internal
!
        call xyzint (xyz, numat, na, nb, nc, 1.d0, geo) 
!
!  UNCONDITIONALLY SET FLAGS FOR INTERNAL COORDINATES
!
        do i = 1, 3 
          lopt(i:3,i) = 0 
        end do 
      endif 
      return  
! ERROR CONDITIONS
  210 continue 
      j = natoms - 1 
      write (iw, '('' DATA CURRENTLY READ IN ARE '')') 
      do k = 1, j 
        if (na(k) > 0) geo(2:3,k) = geo(2:3,k)/1.7453292519943D-02 
        write (iw, '(i4,2x,3(f10.5,2x,i2,2x),3(i2,1x))') &
        labels(k), (geo(jj,k),lopt(jj,k),jj=1,3), &
        na(k), nb(k), nc(k) 
      end do 
      natoms = 0
      return
      end subroutine getgeo 
      subroutine web_message(channel, txt)
        implicit none
        character (*) ::txt
        integer :: channel
        write(channel,'(/10x,a,/)')" For more information, see: HTTP://OpenMOPAC.net/Manual/"//trim(txt)
        return
      end subroutine web_message
