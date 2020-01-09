MODULE m_global
  INTEGER :: Nx, Ny
  REAL :: D
  REAL :: T_max 
  REAL, DIMENSION(:,:), ALLOCATABLE :: T, T_old
  REAL :: dx, dy, dt
  INTEGER :: N_iter

END MODULE m_global
