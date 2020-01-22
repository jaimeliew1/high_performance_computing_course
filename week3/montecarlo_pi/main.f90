PROGRAM montecarlo_pi
  IMPLICIT NONE
  INTEGER, PARAMETER :: N = 1000000
  INTEGER :: i, N_in
  REAL, DIMENSION(N, 2) :: coords

  N_in = 0
  CALL random_NUMBER(coords)
  
  DO i = 1, N
     IF (coords(i, 1)**2 + coords(i, 2)**2 < 1) THEN
        N_in = N_in + 1
     ENDIF
  ENDDO


  PRINT*, 'pi: ', 4*N_in/REAL(N)


END PROGRAM montecarlo_pi
