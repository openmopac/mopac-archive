subroutine extract_parameter (iparam, ielmnt, param)
!
    use parameters_C, only : guess1, guess2, guess3, zs, zp, zd, &
    betas, betap, betad, alp, zsn, zpn, zdn, uss, upp, udd, gss, gpp, &
    gsp, gp2, hsp, pocord, xfac, alpb, f0sd_store, g2sd_store, par1, par2, par3
    implicit none
    integer, intent (in) :: iparam
    integer, intent (in) :: ielmnt
    double precision, intent (inout) :: param
!------------------------------------------------------------
    integer :: kfn, ni, nj, jparam
!------------------------------------------------------------
    jparam = iparam
    if (jparam > 21 .and. jparam < 34) then
      kfn = (jparam-22) / 3
      jparam = jparam - kfn * 3
      kfn = kfn + 1
    end if
    select case (jparam)
    case (2)
      param = upp(ielmnt)
    case (3)
      param = udd(ielmnt)
    case (4)
      param = zs(ielmnt)
    case (5)
      param = zp(ielmnt)
    case (6)
      param = zd(ielmnt)
    case (7)
      param = betas(ielmnt)
    case (8)
      param = betap(ielmnt)
    case (9)
      param = betad(ielmnt)
    case (10)
      param = gss(ielmnt)
    case (11)
      param = gsp(ielmnt)
    case (12)
      param = gpp(ielmnt)
    case (13)
      param = gp2(ielmnt)
    case (14)
      param = hsp(ielmnt)
    case (15)
      param = f0sd_store(ielmnt)
    case (16)
      param = g2sd_store(ielmnt)
    case (17)
      param = pocord(ielmnt)
    case (18)
      param = alp(ielmnt)
    case (19)
      param = zsn(ielmnt)
    case (20)
      param = zpn(ielmnt)
    case (21)
      param = zdn(ielmnt)
    case (22)
      param = guess1(ielmnt, kfn)
    case (23)
      param = guess2(ielmnt, kfn)
    case (24)
      param = guess3(ielmnt, kfn)
    case (25)
      write (6, "(' YOU ARE NOT ALLOWED TO OPTIMIZE THIS PARAMETER!')")
      stop
    case (34)
      nj = ielmnt/200
      ni = ielmnt - nj*200
      param = alpb(ni, nj)
    case (35)
      nj = ielmnt/200
      ni = ielmnt - nj*200
      param = xfac(ni, nj)
    case default
      param = uss(ielmnt)
    end select
    return
end subroutine extract_parameter
