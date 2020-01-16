MODULE m_jacobi_naive2
  INTEGER, PARAMETER :: MK = KIND(1.0D0)
CONTAINS

  SUBROUTINE jacobi_naive2(U, U_old, N, f, dx, N_iter, thres, actual_iters)
    USE m_diagnostic, ONLY: diagnostic
    REAL(MK), DIMENSION(:, :), INTENT(INOUT) :: U, U_old
    REAL(MK), DIMENSION(:, :), INTENT(IN) :: f
    INTEGER, INTENT(IN) :: N, N_iter
    REAL(MK), INTENT(IN) :: dx, thres
    INTEGER, INTENT(OUT) :: actual_iters

    REAL(MK) :: a_quarter = 1/4.0
    REAL(MK) :: norm
    LOGICAL :: converged
    INTEGER :: i, j, k

    converged = .FALSE.

    !$OMP PARALLEL
    DO k=1,N_iter

      !$OMP SINGLE
       norm = 0
       !$OMP END SINGLE

       !$OMP DO
       DO i=2,N+1
          DO j=2,N+1
             U(i, j) = a_quarter*(U_old(i, j-1) + U_old(i,j+1) &
                  + U_old(i-1, j) + U_old(i+1, j)&
                  + dx**2*f(i, j))
              !$OMP CRITICAL
               norm = norm + (U(i, j) - U_old(i, j))**2
              !$OMP END CRITICAL
          ENDDO
       ENDDO
       !$OMP END DO


       !$OMP SINGLE
       norm = SQRT(norm)

       IF (norm < thres) THEN
          converged = .TRUE.
          actual_iters = k
       ENDIF

       U_old = U
       !$OMP END SINGLE

       IF (converged) THEN
          EXIT
       ENDIF
    ENDDO
    !$OMP END PARALLEL

    IF (converged) THEN
    ELSE
       PRINT*, 'NO CONVERGENCE'
    ENDIF

    CALL diagnostic(k, norm, .TRUE.)
  END SUBROUTINE jacobi_naive2

END MODULE m_jacobi_naive2
