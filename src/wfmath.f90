module wfmath

  use wfmath_1d
  use wfmath_2d

  private

  public :: wfmath_iprod
  interface wfmath_iprod
     procedure wfmath_1d_iprod_dp
     procedure wfmath_1d_iprod_sp
     procedure wfmath_2d_iprod_dp
     procedure wfmath_2d_iprod_sp
  end interface wfmath_iprod

  public :: wfmath_proj
  interface wfmath_proj
     procedure wfmath_1d_proj_dp
     procedure wfmath_1d_proj_sp
     procedure wfmath_2d_proj_dp
     procedure wfmath_2d_proj_sp
  end interface wfmath_proj

  public :: wfmath_norm
  interface wfmath_norm
     procedure wfmath_1d_norm_dp
     procedure wfmath_1d_norm_sp
     procedure wfmath_2d_norm_dp
     procedure wfmath_2d_norm_sp
  end interface wfmath_norm

  public :: wfmath_normalize
  interface wfmath_normalize
     procedure wfmath_1d_normalize_dp
     procedure wfmath_1d_normalize_sp
     procedure wfmath_2d_normalize_dp
     procedure wfmath_2d_normalize_sp
  end interface wfmath_normalize

  public :: wfmath_expec_op
  interface wfmath_expec_op
     procedure wfmath_1d_expec_op_dp
     procedure wfmath_1d_expec_op_sp
     procedure wfmath_2d_expec_op_dp
     procedure wfmath_2d_expec_op_sp
  end interface wfmath_expec_op

  public :: wfmath_stdev_op
  interface wfmath_stdev_op
     procedure wfmath_1d_stdev_op_dp
     procedure wfmath_1d_stdev_op_sp
     procedure wfmath_2d_stdev_op_dp
     procedure wfmath_2d_stdev_op_sp
  end interface wfmath_stdev_op

  public :: wfmath_expec_grid
  interface wfmath_expec_grid
     procedure wfmath_1d_expec_grid_dp
     procedure wfmath_1d_expec_grid_sp
     procedure wfmath_2d_expec_grid_dp
     procedure wfmath_2d_expec_grid_sp
  end interface wfmath_expec_grid

  public :: wfmath_stdev_grid
  interface wfmath_stdev_grid
     procedure wfmath_1d_stdev_grid_dp
     procedure wfmath_1d_stdev_grid_sp
     procedure wfmath_2d_stdev_grid_dp
     procedure wfmath_2d_stdev_grid_sp
  end interface wfmath_stdev_grid

end module wfmath
