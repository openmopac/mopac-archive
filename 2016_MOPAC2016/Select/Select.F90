program Select
  !***********************************************************************
  !
  !   AND:  Selects from a subdirectory of reference data sets, those
  !         data sets that contain specified elements.
  !
  !
  !
  !***********************************************************************
  !
  !.. Implicit Declarations ..
    implicit none
  !
  !.. Local Scalars ..
    character (len=3) :: yes
    character (len=9) :: flag
    character (len=14) :: title
    character (len=300) :: name, ref_name
    logical :: newfil, notand, prnt, exists, safe
    integer :: i, ichems, itype, j, ndips, nfiles, ngeos, nheats, nips, &
    & elemnts,end_of_name
    real :: chnine, chzero
    character, dimension(150) :: elemnt*2  
    integer, external :: iargc
    external getarg
    save i     
  !
  !.. Local Arrays ..
    character (len=2), dimension (100) :: chems
  !
  !.. External Calls ..
  !
  !.. Intrinsic Functions ..
    intrinsic Ichar, Index
    data (elemnt(i),i=1,107)/ 'H ', 'HE', 'LI', 'BE', 'B ', 'C ', 'N ', 'O '&
        , 'F ', 'NE', 'NA', 'MG', 'AL', 'SI', 'P ', 'S ', 'CL', 'AR', 'K ', &
        'CA', 'SC', 'TI', 'V ', 'CR', 'MN', 'FE', 'CO', 'NI', 'CU', 'ZN', 'GA'&
        , 'GE', 'AS', 'SE', 'BR', 'KR', 'RB', 'SR', 'Y ', 'ZR', 'NB', 'MO', &
        'TC', 'RU', 'RH', 'PD', 'AG', 'CD', 'IN', 'SN', 'SB', 'TE', 'I ', 'XE'&
        , 'CS', 'BA', 'LA', 'CE', 'PR', 'ND', 'PM', 'SM', 'EU', 'GD', 'TB', &
        'DY', 'HO', 'ER', 'TM', 'YB', 'LU', 'HF', 'TA', 'W ', 'RE', 'OS', 'IR'&
        , 'PT', 'AU', 'HG', 'TL', 'PB', 'BI', 'PO', 'AT', 'RN', 'FR', 'RA', &
        'AC', 'TH', 'PA', 'U ', 'NP', 'PU', 'AM', 'CM', 'BK', 'CF', 'XX', 'FM'&
        , 'MD', 'CB', '++', '+', '--', '-', 'TV'/ 
    end_of_name=185
    i = iargc()
    if (i >= 1) then    
      safe = .false.
      if (i == 2) then
        call getarg(2, name)
        call tidy(name, len_trim(name))
        safe = index(name, "safe")
      end if
      call getarg (1, name)
      call tidy (name, len_trim(name))
    end if
    chzero = Ichar ("0")
    chnine = Ichar ("9")
    elemnts = 211
 !
 !              Types of search
 !      (1) "Only" (Nothing other than specified elements)
 !      (2) "And"  (All occurances of any specified elements)
 !      (3) "All"  (All specified elements plus others)
 !      (4) "Not"  (No cpnds with specified elements)
 ! 
     itype = 0
     if (index(name, "only") + index(name,"1") /= 0) itype = 1
     if (index(name, "and")  + index(name,"2") /= 0) itype = 2
     if (index(name, "all")  + index(name,"3") /= 0) itype = 3
     if (index(name, "not")  + index(name,"4") /= 0) itype = 4
     if (itype == 0) then
       write(6,"(a)")" Select can be used with arguments 'and', 'only', 'not', or 'all'"
       write(6,"(a)")" For example 'C:\\Select and'"
       write(6,"(a)")" The default, used here, is 'Select and'"
       itype = 2
     end if
     
     prnt = .false.
    if (itype == 4) then
      itype = 1
      notand = .false.
    else
      notand = .true.
    end if
    write (6, "(' Enter method name (MNDO, AM1, MNDOD, PM3, PM6, RM1, PM7) or list of elements')")
    read (5, "(A)") name
    call tidy (name, len_trim(name))
!
! Use pre-assigned sets of elements
!
    if (index(name, "mndo ") /= 0) name = " H Li Be B C N O F Na Mg Al Si P S Cl K Ca Zn Ga "// &
      "Ge As Se Br Rb Sr Cd In Sn Sb Te I Cs Ba Hg Tl Pb Bi"
    if (index(name, "am1 ") /= 0) name = " H Li Be    C N O F Na Mg Al Si P S Cl K Ca Zn Ga "// &
      "Ge As Se Br Rb Sr Mo Cd In Sn Sb Te I Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu Hg Tl Pb Bi"
    if (index(name, "pm3 ") /= 0) name = " H Li Be B  C N O F Na Mg Al Si P S Cl K Ca Zn Ga "// &
      "Ge As Se Br Rb Sr Cd In Sn Sb Te I Cs Ba La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu Hg Tl Pb Bi"
    if (index(name, "rm1 ") /= 0) name = " H          C N O F             P S Cl           "// &
      " I La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu "
    if (index(name, "mndod ") /= 0) name = " H He Li Be B C N O F Ne Na Mg Al Si P S Cl    Zn "// &
      "Br Cd I Hg"
    call tidy (name, len_trim(name))
    if (index(name, "pm6 ") /= 0) name = " all"
    if (index(name, "pm7 ") /= 0) name = " all"
    do i = 1, 107
     call tidy (elemnt(i), len_trim(elemnt(i)))
    end do
    if (index(name,"all") /= 0) then
      do i = 1,84
        chems(i) = elemnt(i)
      end do
      ichems = 84
    else
    ichems = 0
      i = 0
      do j = 1, 100
        i = i + 1
        if (name(i:i) /= " ") then
          ichems = ichems + 1
          chems(ichems) = name(i:i+1)
          i = i + 1
        end if
      end do
    end if
    open (unit=7, name="files.out", status="OLD")
    open (unit=12, name="bits.txt", status="UNKNOWN")
    yes = "yes"
    nfiles = 0
!
! dummy read to start of files
!
    do
      read (7, "(A)") name
      if (Index (name, "Emp. Formula") /= 0) exit
    end do
    outer_loop: do      
      read (7, "(A)", END=1100, err=1100) name
      if (Index (name, "TOTAL NUMBER ") /= 0) goto 1100
      name = name(8:)
      ref_name = name
!
! Are there any chemical symbols?
!
      if (name(elemnts:) /= " ") then
        call tidy (name, len_trim(name))
        if (index(name,"param") + index(name,"test") /= 0) cycle
        if (yes == "yes") then
          if (Index (name(99:), "yes") == 0.) cycle
        else if (Index (name(99:), "yes") /= 0) then
          cycle
        end if
        if (itype == 0) then
          itype = 2
        end if
        select case (itype)
        case (1)
        !
        !   only OPTION (must not have any elements not specified)
        !
          do i = 1, ichems
            j = Index (" "//name(elemnts:), " "//chems(i))
            if (j > 0) name(elemnts + j - 1: elemnts + j) = " "
          end do
999       if (name(elemnts:) /= " ") cycle
          if (prnt) then
            write (6,"(a)") name(:end_of_name)
          end if
          call count (name, ngeos, nips, ndips, nheats, flag, nfiles)
        case (2)
        !
        !   OR OPTION (must have at least one of the requested elements)
        !
          do i = 1, ichems
            if (Index (" "//name(elemnts:), " "//chems(i)) /= 0) go to 1000
          end do
          cycle
1000        if (prnt) then
            write (6,"(a)") name(:end_of_name)
          end if
          call count (name, ngeos, nips, ndips, nheats, flag, nfiles)
        case (3)
        !
        !   AND OPTION (must have all elements specified)
        !
          do i = 1, ichems
            if ((Index (" "//name(elemnts:), " "//chems(i)) == 0)) cycle outer_loop
          end do
          if (prnt) then
            write (6,"(a)") name(:end_of_name)
          end if
          call count (name, ngeos, nips, ndips, nheats, flag, nfiles)
        case (4)
        !
        !   Exclusive  NOT (must not have any elements specified)
        !
          do i = 1, ichems
            if (Index (" "//name(elemnts:), " "//chems(i)) /= 0) cycle outer_loop
          end do
          if (prnt) then
            write (6,"(a)") name(:end_of_name)
          end if
          call count (name, ngeos, nips, ndips, nheats, flag, nfiles)
        case default
        !
        !   XAND OPTION (must have at least one or more of the elements specified, and no other elements)
        !
          do i = 1, ichems
            j = Index (" "//name(elemnts:), " "//chems(i))
            if (j /= 0) then
              j = j + elemnts - 1
              name (j:j + 2) = "  "
              if ( .not. notand) cycle outer_loop
            end if
          end do
          if (notand .eqv. name(elemnts:) /= "        ") cycle
          call count (name, ngeos, nips, ndips, nheats, flag, nfiles)
          if (prnt) then
            write (6,"(a)") name(:end_of_name)
          end if
        end select
          do i = end_of_name, 16, -1
            if (name(i:i) /= " ") exit
          end do
          write (12,'(a)') ref_name(22:i)
      end if
    cycle 
1100  if (yes == "yes") then
        rewind (7)
        yes = "   "
        ngeos = nfiles
        nheats = 0
        nips = 0
        ndips = 0
        do
          read (7, "(A)") name
          if (Index (name, "Emp. Formula") /= 0) exit
        end do
      else
        exit
      end if
    end do outer_loop
    write (6, "(///10x,'NO. OF HEATS:',i5,', DIPOLES:',i4,', I.P.s:',i4,'&
         &, GEOREFS:',i4)") nheats, ndips, nips, ngeos
end program Select
subroutine tidy (a, len_a)
  !
  !.. Implicit Declarations ..
    implicit none
    integer, intent(in) :: len_a
    character (len_a), intent (inout) :: a
  !
  !.. Local Scalars ..
    logical :: first = .true.
    integer :: i, icapa, icapz, idiff, ismala, ismalz, j
  !
  !.. Intrinsic Functions ..
    intrinsic Char, Ichar
  !
  ! ... Executable Statements ...
  !
    if (first) then
      icapa = Ichar ("A")
      ismala = Ichar ("a")
      icapz = Ichar ("Z")
      ismalz = Ichar ("z")
      idiff = icapa - ismala
    end if
    do i = 1, len_a
      j = Ichar (a(i:i))
      if (j >= icapa .and. j <= icapz) then
        a(i:i) = Char(j-idiff)
      end if
    end do
end subroutine tidy
subroutine count (name, ngeos, nips, ndips, nheats, flag, n)
  !
  !.. Implicit Declarations ..
    implicit none
  !
  !.. Formal Arguments ..
    character (len=9), intent (out) :: flag
    character (len=300), intent (in) :: name
    integer, intent (inout) :: n, ndips, ngeos, nheats, nips
  !
  !.. Local Scalars ..
    integer :: m, base = 190
  !
  !.. Intrinsic Functions ..
    intrinsic Index
  !
  ! ... Executable Statements ...
  !
    m = 0
    if (Index (name(base + 15:), "yes") /= 0) then
      ngeos = ngeos + 1
      flag = "X1X"
    else
      if (name(base+18:base+18) == ".") then
        m = 2
        nips = nips + 1
      end if
      if (name(base+12:base+12) == ".") then
        ndips = ndips + 1
        m = m + 2
      end if
      if (name(base+6:base+6) == ".") then
        nheats = nheats + 1
        m = m + 2
      end if
    end if
    n = n + 1
end subroutine count
