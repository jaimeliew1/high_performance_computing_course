MODULE m_global
  INTEGER, PARAMETER :: MK = KIND(1.0E0)
  INTEGER :: Nx, Ny ! Number of grid points on x and y direction
  INTEGER :: N_iter ! Number of iterations

  REAL(MK) :: D, T_max ! Diffusion constant, maximum simulation time.
  REAL(MK) :: dx, dy, dt
  REAL(MK), DIMENSION(:,:), ALLOCATABLE :: T, T_old ! current and previous temperature field.

END MODULE m_global
