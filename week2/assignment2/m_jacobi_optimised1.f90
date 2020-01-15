MODULE m_jacobi_optimised1
  INTEGER, PARAMETER :: MK = KIND(1.0E0)
CONTAINS

  SUBROUTINE jacobi_optimised1(U, U_old, N, f, dx, N_iter, thres)
    USE m_diagnostic, ONLY: diagnostic
    REAL(MK), DIMENSION(:, :), INTENT(INOUT) :: U, U_old
    REAL(MK), DIMENSION(:, :), INTENT(IN) :: f
    INTEGER, INTENT(IN) :: N, N_iter
    REAL(MK), INTENT(IN) :: dx, thres

    REAL(MK) :: a_quarter = 1/4.0
    REAL(MK) :: norm, local_norm
    LOGICAL :: converged
    INTEGER :: i, j, k
    norm = 0
    local_norm = 0

    converged = .FALSE.

    !$OMP PARALLEL PRIVATE(local_norm)
    DO k=1,N_iter
      !$OMP SINGLE
      norm = 0
      local_norm = 0
      !$OMP END SINGLE
       !$OMP DO
       DO i=2,N+1
          DO j=2,N+1
             U(i, j) = a_quarter*(U_old(i, j-1) + U_old(i,j+1) &
                  + U_old(i-1, j) + U_old(i+1, j)&
                  + dx**2*f(i, j))
              local_norm = local_norm + (U(i, j) - U_old(i, j))**2
          ENDDO
       ENDDO
       !$OMP END DO

       !$OMP CRITICAL
       norm = norm + local_norm
       !$OMP END CRITICAL

       !$OMP SINGLE
       norm = SQRT(norm)
       U_old = U

       IF (norm < thres) THEN
          converged = .TRUE.
          !PRINT*, 'norm: ', norm
          !PRINT*, 'iterations: ', k
       ENDIF
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
  END SUBROUTINE jacobi_optimised1

END MODULE m_jacobi_optimised1
