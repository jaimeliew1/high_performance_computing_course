MODULE m_jacobi_naive
  INTEGER, PARAMETER :: MK = KIND(1.0D0)
CONTAINS

  SUBROUTINE jacobi_naive(U, U_old, N, f, dx, N_iter, thres, actual_iters)
    USE m_diagnostic, ONLY: diagnostic
    ! Performs a single step of the heat equation solver
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

    DO k=1,N_iter
       norm = 0
       !$OMP PARALLEL
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
       !$OMP END PARALLEL
       norm = SQRT(norm)

       IF (norm < thres) THEN
          converged = .TRUE.
          actual_iters = k
          EXIT
       ENDIF

       U_old = U

    ENDDO

    IF (converged) THEN
       !PRINT*, 'iterations: ', k
    ELSE
       PRINT*, 'NO CONVERGENCE'
    ENDIF

  END SUBROUTINE jacobi_naive

END MODULE m_jacobi_naive
