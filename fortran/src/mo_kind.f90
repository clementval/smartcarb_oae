module mo_kind
  implicit none
  public
  integer, parameter :: wp = selected_real_kind(13)
  integer, parameter :: i8 = selected_int_kind(14)
  integer, parameter :: iinteger = KIND(1)
end module mo_kind
