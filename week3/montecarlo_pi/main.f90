PROGRAM montecarlo_pi
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER :: rank, size, ierror
  INTEGER :: status(MPI_STATUS_SIZE)
  INTEGER, PARAMETER :: increment = 100000
  INTEGER :: current_iter
  INTEGER :: i, N, N_glob, N_in, N_in_glob
  REAL, DIMENSION(increment, 2) :: coords
  REAL :: pi
  REAL, PARAMETER :: eps_thres = 1e-6
  LOGICAL :: converged = .FALSE.


  CALL MPI_init(ierror)

  CALL MPI_COMM_rank(MPI_COMM_WORLD, rank, ierror)
  CALL MPI_COMM_size(MPI_COMM_WORLD, size, ierror)

  N_in = 0
  N = 0
  current_iter = 0

  ! Main loop. Each worker will check 'increment' number of points, then will check in
  ! with all the other workers.
  DO WHILE (.NOT. converged)
     CALL random_NUMBER(coords)
     N = N + increment
     DO i = 1, increment
        IF (coords(i, 1)**2 + coords(i, 2)**2 < 1) THEN
           N_in = N_in + 1
        ENDIF
     ENDDO

     ! Syncronises the value of N_in_glob and N_glob between all workers.
     ! Enforces this value to be the sum of all N_in and N between all workers.
     CALL MPI_ALLREDUCE(N_in, N_in_glob, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)
     CALL MPI_ALLREDUCE(N, N_glob, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)

     ! Only the root process (rank=0) performs the convergence check.
     IF (rank == 0) THEN
        pi = 4*N_in_glob/REAL(N_glob)
        current_iter = current_iter + 1
        CALL check_convergence(converged, pi, eps_thres)
     ENDIF

     ! The root process will broadcast the convergence information to all
     ! workers.
     CALL mpi_bcast(converged, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, IERROR)

  END DO

  ! Print out results (only root process does this)
  IF (rank == 0) THEN
     PRINT*, 'pi: ', pi
     PRINT*, 'Converged in ', current_iter, ' iterations.'
  ENDIF

  CALL MPI_finalize(ierror)


CONTAINS

  SUBROUTINE check_convergence(converged, pi, thres)
    REAL, INTENT(in) :: pi, thres
    LOGICAL, INTENT(out) :: converged
    REAL :: pi_old = 1e9 ! Note. This variable is saved between calls
    REAL :: eps
    REAL :: actual_pi = 4*ATAN(1.0)


    eps = ABS(pi - pi_old)
    !eps = ABS(pi - actual_pi)
    IF (eps < eps_thres) THEN
       converged = .TRUE.
    ELSE
       converged = .FALSE.
    ENDIF

    pi_old = pi

  END SUBROUTINE check_convergence

END PROGRAM montecarlo_pi
