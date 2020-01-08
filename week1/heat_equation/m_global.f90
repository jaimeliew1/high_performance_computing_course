MODULE m_global
  INTEGER, PARAMETER :: N = 21 ! Number of grid points on x and y direction
  REAL, PARAMETER :: D = 1
  REAL, PARAMETER :: T_max = 0.125
  REAL, DIMENSION(:,:), ALLOCATABLE :: T, T_old
  REAL :: dx, dt
  INTEGER :: N_iter

END MODULE m_global
