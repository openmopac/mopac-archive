module common_geosym
    implicit none
    integer, parameter, private :: maxsym = 60000
    integer, dimension (maxsym) :: idepfn, locdep, locpar
    save
end module common_geosym
