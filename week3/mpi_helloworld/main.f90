PROGRAM helloworld
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER :: rank, size, ierror
  INTEGER :: status(MPI_STATUS_SIZE)


  CALL MPI_init(ierror)

  CALL MPI_COMM_rank(MPI_COMM_WORLD, rank, ierror)
  CALL MPI_COMM_size(MPI_COMM_WORLD, size, ierror)


  IF (rank == 0) THEN
     PRINT*, 'Hey ;) its me, number ', rank, 'out of ', size
     CALL MPI_SEND(.TRUE., 1, MPI_LOGICAL, rank+1, 'hey', MPI_COMM_WORLD, ierror)
  ELSEIF (rank < size-1) THEN
     CALL MPI_RECV(.TRUE., 1, MPI_LOGICAL, rank-1, 'hey', MPI_COMM_WORLD, status, ierror)
     PRINT*, 'Hey ;) its me, number ', rank, 'out of ', size
     CALL MPI_SEND(.TRUE., 1, MPI_LOGICAL, rank+1, 'hey', MPI_COMM_WORLD, ierror)

  ELSE
     CALL MPI_RECV(.TRUE., 1, MPI_LOGICAL, rank-1, 'hey', MPI_COMM_WORLD, status, ierror)
     PRINT*, 'Hey ;) its me, number ', rank, 'out of ', size
     PRINT*, 'Hello world! :)'

  ENDIF

  CALL MPI_finalize(ierror)
END PROGRAM helloworld
