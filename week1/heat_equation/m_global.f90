MODULE m_global
  INTEGER, PARAMETER :: Nx = 21 ! Number of grid points in x direction
  INTEGER, PARAMETER :: Ny = 21 ! Number of grid points in y direction
  REAL, PARAMETER :: D = 1
  REAL, PARAMETER :: T_max = 0.125
  REAL, DIMENSION(:,:), ALLOCATABLE :: T, T_old
  REAL :: dx, dy, dt
  INTEGER :: N_iter

END MODULE m_global
