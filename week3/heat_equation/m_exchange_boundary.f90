MODULE m_exchange_boundary

CONTAINS
  SUBROUTINE exchange_boundary(T_old, Nx_local, Ny, rank, n_proc)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: Nx_local, Ny, rank, n_proc
    REAL, DIMENSION(:, :), INTENT(inout) :: T_old
    INCLUDE 'mpif.h'
    INTEGER :: ierror
    INTEGER :: status(MPI_STATUS_SIZE)


    ! Get boundary conditions on left side
    IF (rank == 0) THEN
       CALL MPI_SEND(T_old(Nx_local+1, :), Ny+2, MPI_REAL, rank+1, 'left', &
            MPI_COMM_WORLD, ierror)
    ELSEIF (rank == n_proc-1) THEN
       CALL MPI_RECV(T_old(1, :), Ny+2, MPI_REAL, rank-1, 'left', &
            MPI_COMM_WORLD, status, ierror)
    ELSE
       CALL MPI_SENDRECV(T_old(Nx_local+1, :), Ny+2, MPI_REAL, rank+1, 'left', &
            T_old(1, :), Ny+2, MPI_REAL, rank-1, 'left', MPI_COMM_WORLD, &
            status, ierror)
    ENDIF

    ! Get boundary conditions on right side
    IF (rank == 0) THEN
       CALL MPI_RECV(T_old(Nx_local+2, :), Ny+2, MPI_REAL, rank+1, 'right', &
            MPI_COMM_WORLD, status, ierror)
    ELSEIF (rank == n_proc-1) THEN
       CALL MPI_SEND(T_old(2, :), Ny+2, MPI_REAL, rank-1, 'right', &
            MPI_COMM_WORLD, ierror)
    ELSE
       CALL MPI_SENDRECV(T_old(2, :), Ny+2, MPI_REAL, rank-1, 'right', &
            T_old(Nx_local+2, :), Ny+2, MPI_REAL, rank+1, 'right', MPI_COMM_WORLD, &
            status, ierror)
    ENDIF
  END SUBROUTINE exchange_boundary
END MODULE m_exchange_boundary
