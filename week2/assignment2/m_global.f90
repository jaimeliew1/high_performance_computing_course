MODULE m_global
  INTEGER, PARAMETER :: MK = KIND(1.0D0)
  INTEGER :: N ! Number of grid points on x and y direction
  INTEGER  :: N_iter
  REAL(mk) :: thres
  REAL(MK) :: dx
  REAL(MK), DIMENSION(:), ALLOCATABLE :: coords
  REAL(MK), DIMENSION(:,:), ALLOCATABLE :: U, U_old, f ! current and previous temperature field.

END MODULE m_global
