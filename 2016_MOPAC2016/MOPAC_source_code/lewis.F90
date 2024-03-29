subroutine lewis (use_cvs)
   !***********************************************************************
   !
   !
   !  LEWIS works out what atom is bonded to what atom for the purpose of
   !  generating a Lewis structure.  It starts with the topography generated
   !  by "set_up_dentate" (this is the raw topography), then tidies up the
   !  topography using a series of filters - keywords such as CVB, and criteria
   !  such as a hydrogen atom must bond to exactly one other atom, and that must
   !  be a non-hydrogen atom.
   !
   ! On exit, NBONDS holds the number of bonds to each atom, and
   !          IBONDS contains the atom numbers, in order of increasing
   !                 atomic number.
   !
   ! The maximum number of bonded atoms per atom is 4.  More are allowed
   ! for checking purposes, but for a SCF calculation, 4 is the maximum.
   !
   !***********************************************************************
    use molkst_C, only: natoms, numat, maxtxt, keywrd, &
      line, moperr, numcal, prt_topo
    use chanel_C, only: iw
    use elemts_C, only: elemnt
    use common_arrays_C, only: txtatm, nat, coord, &
      nbonds, ibonds
    implicit none
    logical, intent (in) :: use_cvs
!
    logical :: debug, let
    integer :: i, ibad, j, jbad, k, l, m, n, icalcn = -1
    double precision, allocatable :: store_coord(:,:)
    integer, dimension (:), allocatable :: iz
    save :: store_coord
!
! Most of the time, LET should be false, the commonest exceptions being 0SCF and RESEQ
!
    if (allocated (store_coord) .and. icalcn /= numcal) then
      deallocate (store_coord)
      icalcn = numcal
    end if
    allocate (iz(natoms))
    if (.not. allocated (store_coord)) then
      i = size(coord)/3
      allocate (store_coord(3,i))
      store_coord = coord
    end if
    let = (Index (keywrd, " 0SCF") + Index (keywrd, " RESEQ") + Index (keywrd, " SITE=") +  &
      Index (keywrd, " LET") + Index (keywrd, " ADD-H") /= 0)
    debug = (Index (keywrd, " LEWIS") /= 0)
!
!  Work out raw connectivity.  This depends only on the topology.
!  Arrays nbonds and ibonds are filled here.
!
    call set_up_dentate()   
!
!  Put limit on maximum number of bonds to an atom.
!
!  This is a practical issue.  In theory, it should be possible to make Lewis structures
!  for all systems, but in practice some are just too difficult.  For example, [Fe(ii)(NH3)6](2+)
!  and [Ni(ii)(NH3)6](2+).  In these systems, consider the metal as bonding to a nitrogen atom.
!  The metal obviously has a formal charge of -4 and the nitrogen atoms have a formal charge of +1,
!  to give a net charge on the complex of +2.  However, writing software for this has been too difficult,
!  so it is easier to re-word such complexes as [Fe(ii)(NH3)5](2+).(NH3)  This does not affect the results,
!  but is a useful expedient.
!
    do i = 1, numat
      select case (nat(i))
      case (26, 28, 46, 78)  ! Fe, Ni, Pd, Pt
        do
          if (nbonds(i) > 5) then
            call remove_bond(i)
          else
            exit
          end if   
        end do  
      case default
      end select
    end do
    if (use_cvs) call check_cvs(.true.)
!
!  Check bonding of hydrogen atoms - check for bridges
!
    call check_H(ibad)
    if (ibad == 1) then
      write (iw,*)
    end if
    if (ibad /= 0 .and. .not. let) then
      write(iw,'(//10x,a)') "Add keyword ""LET"" to allow job to continue"
      call mopend ("A hydrogen atom is badly positioned")
    end if
    jbad = ibad
    ibad = 0
!
!  Special check for nitro groups
!
    do i = 1, numat
      if (nat(i) == 7) then
        k = 0
        l = 0
        do j = 1, nbonds(i)
          if (nat(ibonds(j,i)) == 8 .and. nbonds(ibonds(j,i)) == 1) then
            k = ibonds(j,i)
            exit
          end if
        end do
        do j = j + 1, nbonds(i)
          if (nat(ibonds(j,i)) == 8 .and. nbonds(ibonds(j,i)) == 1) then
            l = ibonds(j,i)
            exit
          end if
        end do
!
!  Found a system with N bonded to two oxygen atoms (k and l), 
!  which are not bonded to anything else.
!
        if (k /= 0 .and. l /= 0) then
!
!  Put a bond between the two oxygen atoms
!
          nbonds(k) = 2
          nbonds(l) = 2
          ibonds(2,k) = l
          ibonds(2,l) = k
        end if
      end if
    end do
!
!  End of special check for nitro groups
!
!
!  Put atoms into order of atomic number
!
    do i = 1, numat
      if (nbonds(i) > 1) then
        n = nbonds(i)
        iz(:n) = ibonds(:n,i)
        do j = 1, n
          k = 0
          do m = 1, n
            if (iz(m) /= 0) then
              if (nat(iz(m)) > k) then
                l = m
                 k = nat(iz(m))
              end if 
            end if
          end do         
          ibonds(j,i) = iz(l)
          iz(l) = 0
        end do        
      end if
    end do
    if (prt_topo .and. debug .and. jbad /= 0 .and. .not. let) then
      if (maxtxt == 0) then
        j = 1
      else
        j = maxtxt/2 + 2
      end if
      write (iw, "(/,A,/)") "   TOPOGRAPHY OF SYSTEM"
      write (iw,*) "  ATOM No. "//line(:j)//"  LABEL  "//line(:j)//"Atoms connected to this atom"
      if (j == 0) then
        do i = 1, numat
          write (iw, "(I7,9X,A,9I7)") i, elemnt (nat(i)) // "  ", (ibonds(j, i), j=1, nbonds(i))
        end do
      else
        if (maxtxt > 2) then
          do i = 1, numat
            write (iw, "(I7,9X,A,9I7)") i, elemnt (nat(i)) // " (" // txtatm(i) (:maxtxt) // ") ", (ibonds(j, i), j=1, nbonds(i))
          end do
        else
          do i = 1, numat
            write (iw, "(I7,9X,A,9I7)") i, elemnt (nat(i)), (ibonds(j, i), j=1, nbonds(i))
          end do
        end if     
      end if
    end if
    if (jbad /= 0 .and. .not. let) call mopend ("Geometry is faulty")
    if (let) moperr = .false.
end subroutine lewis
!
!
!
subroutine remove_bond(i)
  use common_arrays_C, only: coord, nbonds, ibonds
!
!  Remove the longest bond from atom i
!
  implicit none
  integer, intent (in) :: i
  double precision :: rmax, sum
  integer :: j, k, l
  rmax = 0.d0
  do j = 1, nbonds(i)
    k = ibonds(j,i)
    sum = (coord(1, i)-coord(1, k)) ** 2 + &
          (coord(2, i)-coord(2, k)) ** 2 + &
          (coord(3, i)-coord(3, k)) ** 2
    if (sum > rmax) then
      rmax = sum
      l = k
    end if
  end do
  k = 0
  do j = 1, nbonds(i)
    if (ibonds(j,i) /= l) then
      k = k + 1
      ibonds(k,i) = ibonds(j,i)
    end if
  end do
  nbonds(i) = nbonds(i) - 1
  k = 0
  do j = 1, nbonds(l)
    if (ibonds(j,l) /= i) then
    k = k + 1
      ibonds(k,l) = ibonds(j,l)
    end if
  end do
  nbonds(l) = nbonds(l) - 1
  end subroutine remove_bond
  subroutine check_h(ibad)
    use molkst_C, only: natoms, numat, line, mozyme, keywrd, numcal
    use chanel_C, only: iw, ilog, log, log_fn
    use elemts_C, only: elemnt
    use mod_atomradii, only: radius
    use common_arrays_C, only: nat, labels, nbonds, ibonds, txtatm
    implicit none
    integer, intent (out) :: ibad
    integer :: i, j, m, n, k, l, ii, heavy(10), icalcn = -1
    integer, allocatable :: im(:)   
    double precision :: rmin, r
    logical :: opend, prt
    double precision, external :: distance
    character :: num1*1, num2*1, num3*1
    save :: icalcn
    if ( .not. mozyme) return
    prt = (icalcn /= numcal)
    if (prt) icalcn = numcal
    allocate (im(natoms))
    inquire(unit=ilog, opened=opend) 
    if (.not. opend) open(unit=ilog, form='FORMATTED', status='UNKNOWN', file=log_fn, position='asis') 
    j = 0
    do i = 1, natoms
      if (labels(i) /= 99) then
        j = j + 1
        im(j) = i
      end if
    end do
!
!  Check bonding of hydrogen atoms - check for bridges
!
    ibad = 0
    do i = 1, numat
      if (nat(i) == 1) then
        if (nbonds(i) == 0) then
!
!  Find nearest heavy atom to hydrogen atom i
!
          rmin = 1.d12
          do j = 1, numat
            if (j == i .or. nat(j) == 1) cycle
            r = distance(i, j)
            if (r < rmin) then
              rmin = r
              k = j
            end if                   
          end do
          nbonds(i) = 1
          ibonds(1,i) = k  
          nbonds(k) = nbonds(k) + 1
          ibonds(nbonds(k),k) = i   
          if (rmin < max(1.3d0, 2.d0*radius(k))) cycle
          if (ibad < 20 .and. &
            index(keywrd, " LET") + index(keywrd, " ADD-H") + index(keywrd, " RESEQ") == 0) then
            num1 = char(Int(log10(i     + 1.0)) + ichar("1")) 
            num2 = char(Int(log10(rmin  + 0.05d0)) + ichar("4")) 
            num3 = char(Int(log10(im(k) + 1.0)) + ichar("1"))
            j = 1
            if (elemnt (nat(k))(1:1) == " ") j = 2             
            write (line, "(a,i"//num1//",a,f"//num2//".1,a, a,i"//num3//",a)") &
              "Hydrogen atom ", i, " is", rmin, " Angstroms from the nearest non-hydrogen atom (", &
              elemnt (nat(k))(j:2), im(k),")"
            call mopend(trim(line))
            write(iw,'(/10x,a)')"(Atom label: '"//trim(txtatm(i))//"')"
            write(iw,'(10x,a)')"(To continue, add ""LET"" to the keyword line)"
            write(iw,"(10x,a,i"//num1//",a,/)")"(Label for atom ", i, ": """//txtatm(i)//""")"
            inquire(unit=ilog, opened=opend) 
            if (log) write (ilog, "(a)")trim(line)
            ibad = ibad + 1
          end if
          cycle
        else if (nbonds(i) == 1) then
!
!  The normal case
!             
          r = distance(i, ibonds(1,i))
        else
!
!  Find the heavy atom, k, nearest to hydrogen atom i
!
          rmin = 1.d12
          m = 0
          k = 0
          do j = 1, nbonds(i)
            if (nat(ibonds(j,i)) == 1) cycle
            m = m + 1
            heavy(m) = ibonds(j,i)
            r = distance(i, ibonds(j,i))
            if (r < rmin .and. r > 0.95d0) then
              rmin = r
              k = ibonds(j,i)
            end if
          end do
          if (k == 0) then
            do j = 1, nbonds(i)
              if (nat(ibonds(j,i)) == 1) cycle
              m = m + 1
              heavy(m) = ibonds(j,i)
              r = distance(i, ibonds(j,i))
              if (r < rmin) then
                rmin = r
                k = ibonds(j,i)
              end if
            end do
          end if
!
! Write out error message
!
          if (m == 2 .and. prt) then
            j = 1            
            m = heavy(1)
            n = heavy(2)
            num1 = char(Int(log10(m     + 1.0)) + ichar("1")) 
            num2 = char(Int(log10(n     + 1.0)) + ichar("1"))
            num3 = char(Int(log10(im(i) + 1.0)) + ichar("1"))
            write (iw, "(a,i"//num3//",a,a2,i"//num1//",a,a2,i"//num2//")") " Hydrogen atom ", im(i), &
              & " has covalent bonds to ", elemnt (nat(m)), im(m), " and ", elemnt (nat(n)), im(n)
            if (log) write (ilog, "(a,i"//num3//",a,a2,i"//num1//",a,a2,i"//num2//")") " Hydrogen atom ", im(i), &
              & " has covalent bonds to ", elemnt (nat(m)), im(m), " and ", elemnt (nat(n)), im(n)
          else if (m > 2) then
            if (ibad < 20) then
               num3 = char(Int(log10(im(i) + 1.0)) + ichar("1"))
              write (iw, "(a,i"//num3//",a,a2,I5,a,a2,i5)") " Hydrogen atom ", im(i), " has more than two covalent bonds"
               if (log) write (ilog, "(a,i"//num3//",a,a2,I5,a,a2,i5)") &
                 " Hydrogen atom ", im(i), " has more than two covalent bonds"
             end if
          end if
          if (m > 1) then
!
!  Remove all bonds to hydrogen atom i except the shortest bond, to atom "k"
!
            do j = 1, nbonds(i)
              ii = ibonds(j,i)
              if (ii == k .or. nat(ii) == 1) cycle
              m = 0
              do l = 1, nbonds(ii)
                if (ibonds(l,ii) /= i) then
                  m = m + 1
                  ibonds(m,ii) = ibonds(l,ii)
                end if              
              end do
              nbonds(ii) = m
            end do
            nbonds(i) = 1
            ibonds(1,i) = k
            num1 = char(Int(log10(k     + 1.0)) + ichar("1")) 
            if (prt) write (iw, "(25x,a,a2,i"//num1//",a)") "(Keeping bond to ", elemnt (nat(k)), im(k), ")"
          end if
        end if  
      end if          
      if (ibad == 21) write (iw,*) " Remaining errors suppressed"
    end do
    return
  end subroutine check_h
  
  
  subroutine check_CVS(let)
  use molkst_C, only: numat, keywrd, line, numcal, moperr
  use common_arrays_C, only: txtatm, nat, coord, &
      nbonds, ibonds
  use elemts_C, only: elemnt
  use mod_atomradii, only: radius
  use chanel_C, only: iw
  implicit none
  logical, intent (in) :: let
  integer :: i, j, k, l, n, m, ii, ncvb, icalcn = -1, ik, cvb_atoms(60)
  logical :: lbond, error, PDB_input
  double precision :: r
  double precision, external :: reada
  double precision, allocatable :: store_coord(:,:)
  double precision, external :: distance
  character :: num*1
  save :: store_coord
    error = .false.
!
! Most of the time, LET should be false, the commonest exceptions being 0SCF and RESEQ
!
    if (allocated (store_coord) .and. icalcn /= numcal) then
      deallocate (store_coord)
      icalcn = numcal
    end if
    if (.not. allocated (store_coord)) then
      i = size(coord)/3
      allocate (store_coord(3,i))
      store_coord = coord
    end if
!
!  Parse CVB(2:3,45:65)
!
     i = index(keywrd, " CVB")
     if (i > 0) then
       j = index(keywrd(i:), ") ") + i
       line = keywrd(i + 1: j - 1)
       ii = len_trim(line)
!
!    If everything is in quotation marks, use the CVB keyword
!
       k = 0
       do 
         k = k + 1
         if (k >= ii) exit
         if (line(k:k) == '"') then
           do
             k = k + 1
             if (line(k:k) == '"') exit
           end do
         end if
         if (line(k:k) >= "0" .and. line(k:k) <= "9") exit
       end do
       PDB_input = (k > ii - 3)
    end if
    i = Index (keywrd, " CVB")
    k = 0
    if (i /= 0) then
      k = Index (keywrd(i:), ")") + i
    end if
    if (k == i .and. i /= 0) then
      k = Index (keywrd(i + 3:), " ") + i + 3
      write(iw,"(//10x,a)")" No closing parenthesis for CVB keyword"
      write(iw,'(10x,a)')" CVB keyword ="//'"'//keywrd(i:k)//'"'
      call mopend ("ERROR IN CVB KEYWORD")
      return
    end if
    if (i /= 0) then
      do
        k = Index (keywrd(i:), ") ") + i
        j = index(keywrd(i:k),"""") + i 
        if (j == i) exit
        call txt_to_atom_no(keywrd, j - 1, let, m)
        if (moperr) return
        if (m > numat) then
          if (let) then
!
!  Jump over the faulty atom label
!
            k = Index (keywrd(i:), ")") + i
            i = index(keywrd(i:k),"""") + i  
            i = index(keywrd(i:k),"""") + i  
            cycle
          end if
        end if        
      end do
      k = Index (keywrd(i:), ")") + i
      ncvb = 0
      do
        j = Nint (reada (keywrd(i:k), 1))
        ii = Index (keywrd(i:k), ":") + i
        if (ii == i) then
          write (iw,*)
          write (iw,*) " ERROR IN CVB KEYWORD - ':' EXPECTED BUT NOT FOUND"
          j = Index (keywrd, " CVB")
          write(iw,'(10x,a)')" CVB keyword ="//'"'//keywrd(j:k)//'"'
          call mopend ("ERROR IN CVB KEYWORD")
          return
        end if
        i = ii
        if (j > numat .or. j == 0 .or. j < -numat) then
          write (iw,*) " AT LEAST ONE ATOM DEFINED BY CVB IS FAULTY"
          write (iw,*) " THE FAULTY ATOM NUMBER IS", j
          j = Index (keywrd, " CVB")
          write(iw,'(10x,a)')" CVB keyword ="//'"'//keywrd(j:k)//'"'
          error = .true.
        end if
        l = Nint (reada (keywrd(i:k), 1))
        if (l > numat .or. l == 0 .or. l < -numat) then
          write (iw,*) " AT LEAST ONE ATOM DEFINED BY CVB IS FAULTY"
          write (iw,*) " THE FAULTY ATOM NUMBER IS", l
          m = Index (keywrd, " CVB")
          write(iw,'(10x,a)')" CVB keyword ="//'"'//keywrd(m:k)//'"'
          error = .true.
        end if
        if (error) goto 99
        n = abs(l)
        m = abs(j)
        if (allocated(store_coord) .and. .not. PDB_input) then
!
!   If an atom has moved, find its new location
!
          do ii = 1, numat
            if (abs(coord(1,ii) - store_coord(1,n)) < 1.d-1) then
               if (abs(coord(2,ii) - store_coord(2,n)) < 1.d-1) then
                 if (abs(coord(3,ii) - store_coord(3,n)) < 1.d-1) exit
               end if
            end if
          end do  
          l = sign(ii,l)
          do ii = 1, numat
            if (abs(coord(1,ii) - store_coord(1,m)) < 1.d-1) then
               if (abs(coord(2,ii) - store_coord(2,m)) < 1.d-1) then
                 if (abs(coord(3,ii) - store_coord(3,m)) < 1.d-1) exit
               end if
            end if
          end do  
          j = sign(ii,j)
        else
          j = sign(m,j)
          l = sign(n,l)         
        end if
        do ii = 1, ncvb
          if (cvb_atoms(ii) == m) exit
        end do
        if (ii > ncvb) then
          ncvb = ncvb + 1
          cvb_atoms(ncvb) = m
        end if
        m = abs(l)
        do ii = 1, ncvb
          if (cvb_atoms(ii) == m) exit
        end do
        if (ii > ncvb) then
          ncvb = ncvb + 1
          cvb_atoms(ncvb) = m
        end if
        if (j > 0 .and. l > 0) then
          do ii = 1, nbonds(j)
            if (ibonds(ii, j) == l .and. .not. let .and. index(keywrd, " GEO-OK") == 0 .and. &
              index(keywrd, " 0SCF") == 0) then
              write(iw,"(a,i5,a,i5,a)")" The bond defined by CVB between atoms",j," and", &
              l, " already exists.  Correct error and re-submit"
              write(iw,'(/30x,a,/)')"PDB label              Coordinates of the two atoms"
              write(iw,"(a,i5,a,3x,a, 3f12.6)")" Atom:", j, ": "//elemnt(nat(j)), "("//txtatm(j)//")", coord(:,j)
              write(iw,"(a,i5,a,3x,a, 3f12.6)")" Atom:", l, ": "//elemnt(nat(l)), "("//txtatm(l)//")", coord(:,l)
              write(iw,"(a,/)") " (If an extra bond needs to be added, use keyword 'SETPI')"
              error = .true.
            end if
          end do
          r = distance(j,l)
          if (r > 3.d0*(radius(nat(j)) + radius(nat(l))) .and. .not. let .and. index(keywrd, " GEO-OK") == 0 .and. &
              index(keywrd, " 0SCF") == 0) then
            write(iw,'(a,i5,a,i5,a,f5.1,a)')" The bond defined by CVB between atoms", j, &
            " and", l, " would be", r, " Angstroms long.  This is unrealistic."
            write(iw,'(/30x,a,/)')"PDB label              Coordinates of the two atoms"
            write(iw,"(a,i5,a,3x,a, 3f12.6)")" Atom:", j, ": "//elemnt(nat(j)), "("//txtatm(j)//")", coord(:,j)
            write(iw,"(a,i5,a,3x,a, 3f12.6)")" Atom:", l, ": "//elemnt(nat(l)), "("//txtatm(l)//")", coord(:,l)
            error = .true.
          end if
          nbonds(j) = nbonds(j) + 1
          nbonds(l) = nbonds(l) + 1
          ibonds(nbonds(j), j) = l
          ibonds(nbonds(l), l) = j
        else
!
!  make sure that the faulty bond actually exists.
!
          j = Abs(j)
          l = Abs(l)
          if (j == 0 .or. l == 0) then
            write(iw,"(10x,a)")" Error detected while reading CVB"
            j = Index (keywrd, " CVB")
            write(iw,'(10x,a)')" CVB keyword ="//'"'//keywrd(j:k)//'"'
            error = .true.    
          end if
          lbond = .false.
          do n = 1, nbonds(j)
            if (ibonds(n,j) == l) then
!
!  Faulty bond exists, therefore delete it.
!          
              ik = l
              do m = n + 1,nbonds(j)
                ibonds(m - 1,j) = ibonds(m,j)
              end do
              ibonds(nbonds(j),j) = ik
              nbonds(j) = nbonds(j) - 1
              lbond = .true.
              exit
            end if
          end do
          do n = 1, nbonds(l)
            if (ibonds(n,l) == j) then
!
!  Faulty bond exists, therefore delete it.
!          
              ik = j
              do m = n + 1,nbonds(l)
                ibonds(m - 1,l) = ibonds(m,l)
              end do
              ibonds(nbonds(l),l) = ik
              nbonds(l) = nbonds(l) - 1
              lbond = .true.
              exit
            end if
          end do
          if (.not. lbond .and. .not. let) then
            if (index(keywrd, " GEO-OK") == 0 .and. index(keywrd, " 0SCF") == 0) then
            write(iw,"(a,i5,a,i5,a)")" The bond defined by CVB between atoms",j," and", &
              l, " did not exist.  Correct error and re-submit"
            write(iw,'(/30x,a,/)')"PDB label              Coordinates of the two atoms"
            write(iw,"(a,i5,a,3x,a, 3f12.6)")" Atom:", j, ": "//elemnt(nat(j)), "("//txtatm(j)//")", coord(:,j)
            write(iw,"(a,i5,a,3x,a, 3f12.6)")" Atom:", l, ": "//elemnt(nat(l)), "("//txtatm(l)//")", coord(:,l)
            else
              if (index(keywrd, " LOCATE-TS") == 0 .and. index(keywrd, " 0SCF") == 0) then          
                write(iw,"(a,i5,a,i5,a,/,a)")" The bond defined by CVB between atoms",j," and", &
                l, " did not exist,", " but, because GEO-OK is present, the job will continue."
              end if
            end if
            error = .true.
          end if
        end if     
99  continue 
        do j = i, k
          if (keywrd(j:j) == "," .or. keywrd(j:j) == ";") exit
        end do   
        if (j <= k) then
          i = j
        else
          exit
        end if
      end do
      do i = 1, ncvb
        j = cvb_atoms(i)
        if (nbonds(j) == 0) then
          if (nat(j) == 1) then
!
!  A bond to hydrogen has been broken, so see if there is a different atom within range.
!
            do_k: do k = 1, numat
              do l = 1, ncvb
                if (cvb_atoms(l) == k) cycle do_k
              end do
              if (distance(j,k) < 1.2d0) then
                nbonds(j) = 1
                ibonds(1,j) = k
                nbonds(k) = nbonds(k) + 1
                ibonds(nbonds(k),k) = j
                exit
              end if
            end do do_k
            if (nbonds(j) == 1) cycle
          end if
           if (txtatm(j) /= " ") then
            num = char(ichar("1") + int(log10(j*1.01)))
            write(line,"(a, i"//num//",a)")"All bonds to atom ", j, ", label: """ &
            //txtatm(j)//""" have been deleted"
           else
             write(line,"(a,i5,a,3f10.5)") &
             " All bonds to atom "//elemnt(nat(j)), j, " have been deleted. Coords:",coord(:,j)  
            
          end if
          call mopend(trim(line))
          error = .true.
        end if
      end do
    end if
    if (error .and. .not. let) then
      if (index(keywrd, " GEO-OK") + index(keywrd, " 0SCF") == 0  ) then
        write(iw,"(/10x,a)")"To continue add ""GEO-OK"" or correct reported faults"
        call mopend("Fault in CVB keyword")
        return 
      else
        moperr = .false.
        error = .false.   
      end if
    end if
    return
  end subroutine check_CVS
  subroutine txt_to_atom_no(text, j_in, let, m)
!
!  Convert atom number from text to a number.  Text can be PDB or Jmol
!
!  On input, "text": Line of text containing PDB or Jmol 
!            j_in: Start of text (location of the character after the leading '"')
!            let:  TRUE if the job is to continue even if a fault is found.
!
! On exit, "text": PDB or Jmol text has been replaced by numbers
!
  use molkst_C, only: numat, maxtxt
  use common_arrays_C, only: txtatm
  use chanel_C, only: iw
  implicit none
  character :: text*(*)
  integer, intent (in) :: j_in
  integer, intent (out) :: m
  logical, intent (in) :: let
  integer :: i, j, l, n, k
  character :: line*80, txt*30, num*1, txt2*30
  if (maxtxt < 26) then
    call mopend("Labeled atoms can only be used when atoms have labels")
    m = 0
    return
  end if
  j = j_in + 1
  l = index(text(j:),"""") + j
  line = text(j:l - 2)
  txt2 = trim(line)
  if (line(1:1) == "[") then
    k = len_trim(line)
    if (k > 14) then
      write(line,'(a)')" CVB Atom defined by '"//trim(line)//"' is not in JSmol format"
      call mopend(trim(line))
      return
    end if    
!
! Atom defined using Jmol format
!
    n = index(text(j:l - 2),".") + j
    k = index(text(j:l - 2),":") + j
    m = index(text(j:l - 2),"]") + j
    if (n == j .or. k == j .or. m == j) then
      write(line,'(a)')" CVB Atom defined by '"//trim(line)//"' is not in JSmol format"
      call mopend(trim(line))
      if (n == j) call mopend("(The dot separator, ""."", is missing.)")
      if (k == j) call mopend("(The colon separator, "":"", is missing.)")
      if (m == j) call mopend("(The close square bracket, ""]"", is missing.)")
      return
    end if    
    line = text(n:l - 2)//text(j + 1:m - 2)//text(k:k)//text(m:k - 2)
  end if
  m = 0
  do k = 1, len_trim(line)
    if (line(k:k) /= " ") then
      m = m + 1
      line(m:m) = line(k:k)
    end if
  end do
  line(m + 1:) = " "
  if (m > 12) then
     write(line,'(a)')" CVB Atom defined by '"//trim(line)//"' is not in PDB format"
     call mopend(trim(line))
     return
  end if    
  line(m + 1:m + 5) = " "          
  do i = 1, numat
    txt = txtatm(i)
    n = 0
    do k = 13, 26
      if (txt(k:k) /= " ") then
        n = n + 1
        txt(n:n) = txt(k:k)
      end if
    end do
    txt(n + 1:m ) = "?"
    n = max(n, m)
    do k = 1, n
      if (txt(k:k) /= line(k:k) .and. line(k:k) /= "*") exit
    end do
    if (k > n) then
      num = char(ichar("1") + int(log10(i*1.0001)))
      write(line,'(i'//num//')')i
      text(j - 1:) = trim(line)//text(l:)
      exit           
    end if
  end do
  if (m > numat) then
    k = len_trim(line)
    if (index(txt2,"[") /= 0) then
      write(line,'(a)')" CVB Atom defined by JSmol label '"//trim(txt2)//"' was not found in the data set"
    else
      write(line,'(a)')" CVB Atom defined by PDB label '"//trim(txt2)//"' was not found in the data set"
    end if
    if(.not. let) then
      call mopend(trim(line))
      write(iw,'(10x,a)') &
      "(Hint: Check that the atom-label in the data set matches the atom-label in the PDB file.)" 
      return
    end if
  end if      
  return
  end subroutine txt_to_atom_no
