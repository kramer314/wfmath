module wfmath_2d

  use iso_fortran_env, only: sp=>real32, dp=>real64, ip=>int32

  implicit none


  interface wfmath_2d_proj
     module procedure wfmath_2d_proj_dp
     module procedure wfmath_2d_proj_sp
  end interface wfmath_2d_proj

  interface wfmath_2d_iprod
     module procedure wfmath_2d_iprod_dp
     module procedure wfmath_2d_iprod_sp
  end interface wfmath_2d_iprod

  interface wfmath_2d_norm
     module procedure wfmath_2d_norm_dp
     module procedure wfmath_2d_norm_sp
  end interface wfmath_2d_norm

  interface wfmath_2d_normalize
     module procedure wfmath_2d_normalize_dp
     module procedure wfmath_2d_normalize_sp
  end interface wfmath_2d_normalize

  interface wfmath_2d_expec_op
     module procedure wfmath_2d_expec_op_dp
     module procedure wfmath_2d_expec_op_sp
  end interface wfmath_2d_expec_op

  interface wfmath_2d_stdev_op
     module procedure wfmath_2d_stdev_op_dp
     module procedure wfmath_2d_stdev_op_sp
  end interface wfmath_2d_stdev_op

  interface wfmath_2d_expec_grid
     module procedure wfmath_2d_expec_grid_dp
     module procedure wfmath_2d_expec_grid_sp
  end interface wfmath_2d_expec_grid

  interface wfmath_2d_stdev_grid
     module procedure wfmath_2d_stdev_grid_dp
     module procedure wfmath_2d_stdev_grid_sp
  end interface wfmath_2d_stdev_grid

contains

  complex(dp) function wfmath_2d_proj_dp(psi1_arr, psi2_arr, dgrid) &
       result(val)
    complex(dp), intent(in) :: psi1_arr(:,:), psi2_arr(:,:)
    real(dp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/proj.src"
  end function wfmath_2d_proj_dp

  complex(sp) function wfmath_2d_proj_sp(psi1_arr, psi2_arr, dgrid) &
       result(val)
    complex(sp), intent(in) :: psi1_arr(:,:), psi2_arr(:,:)
    real(sp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = sp

    include "./wfmath_2d_src/proj.src"
  end function wfmath_2d_proj_sp

  complex(dp) function wfmath_2d_iprod_dp(psi1_arr, func_arr, psi2_arr, &
       dgrid) result(val)
    complex(dp), intent(in) :: psi1_arr(:,:), psi2_arr(:,:), func_arr(:,:)
    real(dp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/iprod.src"
  end function wfmath_2d_iprod_dp

  complex(sp) function wfmath_2d_iprod_sp(psi1_arr, func_arr, psi2_arr, &
       dgrid) result(val)
    complex(sp), intent(in) :: psi1_arr(:,:), psi2_arr(:,:), func_arr(:,:)
    real(sp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/iprod.src"
  end function wfmath_2d_iprod_sp

  real(dp) function wfmath_2d_norm_dp(psi_arr, dgrid) result(val)
    complex(dp), intent(in) :: psi_arr(:,:)
    real(dp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/norm.src"
  end function wfmath_2d_norm_dp

  real(sp) function wfmath_2d_norm_sp(psi_arr, dgrid) result(val)
    complex(sp), intent(in) :: psi_arr(:,:)
    real(sp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = sp

    include "./wfmath_2d_src/norm.src"
  end function wfmath_2d_norm_sp

  subroutine wfmath_2d_normalize_dp(psi_arr, dgrid)
    complex(dp), intent(inout) :: psi_arr(:,:)
    real(dp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/normalize.src"
  end subroutine wfmath_2d_normalize_dp

  subroutine wfmath_2d_normalize_sp(psi_arr, dgrid)
    complex(sp), intent(inout) :: psi_arr(:,:)
    real(sp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = sp

    include "./wfmath_2d_src/normalize.src"
  end subroutine wfmath_2d_normalize_sp

  real(dp) function wfmath_2d_expec_op_dp(psi_arr, op_arr, dgrid) &
       result(val)
    complex(dp), intent(in) :: psi_arr(:,:), op_arr(:,:)
    real(dp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/expec_op.src"
  end function wfmath_2d_expec_op_dp

  real(sp) function wfmath_2d_expec_op_sp(psi_arr, op_arr, dgrid) &
       result(val)
    complex(sp), intent(in) :: psi_arr(:,:), op_arr(:,:)
    real(sp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/expec_op.src"
  end function wfmath_2d_expec_op_sp

  real(dp) function wfmath_2d_stdev_op_dp(psi_arr, op_arr, dgrid) &
       result(val)
    complex(dp), intent(in) :: psi_arr(:,:), op_arr(:,:)
    real(dp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/stdev_op.src"
  end function wfmath_2d_stdev_op_dp

  real(sp) function wfmath_2d_stdev_op_sp(psi_arr, op_arr, dgrid) &
       result(val)
    complex(sp), intent(in) :: psi_arr(:,:), op_arr(:,:)
    real(sp), intent(in) :: dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/stdev_op.src"
  end function wfmath_2d_stdev_op_sp

  real(dp) function wfmath_2d_expec_grid_dp(psi_arr, grid, dgrid) &
       result(val)
    complex(dp), intent(in) :: psi_arr(:,:)
    real(dp), intent(in) :: grid(:,:), dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/expec_grid.src"
  end function wfmath_2d_expec_grid_dp

  real(sp) function wfmath_2d_expec_grid_sp(psi_arr, grid, dgrid) &
       result(val)
    complex(sp), intent(in) :: psi_arr(:,:)
    real(sp), intent(in) :: grid(:,:), dgrid(2)

    integer(ip), parameter :: fp = sp

    include "./wfmath_2d_src/expec_grid.src"
  end function wfmath_2d_expec_grid_sp

  real(dp) function wfmath_2d_stdev_grid_dp(psi_arr, grid, dgrid) &
       result(val)
    complex(dp), intent(in) :: psi_arr(:,:)
    real(dp), intent(in) :: grid(:,:), dgrid(2)

    integer(ip), parameter :: fp = dp

    include "./wfmath_2d_src/stdev_grid.src"
  end function wfmath_2d_stdev_grid_dp

  real(sp) function wfmath_2d_stdev_grid_sp(psi_arr, grid, dgrid) &
       result(val)
    complex(sp), intent(in) :: psi_arr(:,:)
    real(sp), intent(in) :: grid(:,:), dgrid(2)

    integer(ip), parameter :: fp = sp

    include "./wfmath_2d_src/stdev_grid.src"
  end function wfmath_2d_stdev_grid_sp

end module wfmath_2d
