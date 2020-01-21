PROGRAM helloworld
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER :: rank, size, ierror
  INTEGER :: status(MPI_STATUS_SIZE)
  INTEGER, PARAMETER :: N_pings = 10
  INTEGER :: msg_size
  CHARACTER(len=32) :: argument

  IF (command_argument_COUNT() == 1) THEN
     CALL get_command_ARGUMENT(1, argument)
     READ(argument, *) msg_size
  ELSE
     msg_size = 1
  ENDIF
  CALL MPI_init(ierror)

  CALL MPI_COMM_rank(MPI_COMM_WORLD, rank, ierror)
  CALL MPI_COMM_size(MPI_COMM_WORLD, size, ierror)


  IF (rank == 0) THEN
     CALL ping_process(N_pings, msg_size)

  ELSEIF (rank == 1) THEN
     CALL pong_process()

  ELSE
     PRINT*, 'I am reduntant :('
  ENDIF

  CALL MPI_finalize(ierror)


CONTAINS


  SUBROUTINE ping_process(N_pings, msg_size)
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    INTEGER :: ierror, i, N_pings, msg_size
    REAL :: t_start, t_end
    REAL, DIMENSION(N_pings) :: t_ellapsed
    REAL, DIMENSION(msg_size) :: msg
    LOGICAL :: exist

    PRINT*, 'This is Ping.'
    PRINT*, 'Ping: I will send ', N_pings, ' pings of length ', msg_size

    ! Communicate the number of ping pongs that will occur
    CALL MPI_SEND(N_pings, 1, MPI_INTEGER, 1, 'n_pings', MPI_COMM_WORLD, ierror)
    ! Communicate the message size
    CALL MPI_SEND(msg_size, 1, MPI_INTEGER, 1, 'msg_size', MPI_COMM_WORLD, ierror)

    DO i= 1,N_pings

       !PRINT*, 'ping', i
       CALL CPU_TIME(TIME=t_start)
       CALL MPI_SEND(msg, msg_size, MPI_REAL, 1, 'pingpong', MPI_COMM_WORLD, ierror)
       CALL MPI_RECV(msg, msg_size, MPI_REAL, 1, 'pingpong', MPI_COMM_WORLD, &
            status, ierror)
       CALL CPU_TIME(TIME=t_end)
       t_ellapsed(i) = (t_end - t_start)

    ENDDO
    PRINT*, 'Average bandwidth (Doubles/s): ', msg_size/(SUM(t_ellapsed)/N_pings)
    PRINT*, 'Max bandwidth (Doubles/s): ', msg_size/(MINVAL(t_ellapsed))
    PRINT*, 'Min bandwidth (Doubles/s): ', msg_size/(MAXVAL(t_ellapsed))


    INQUIRE(file="bandwidth.txt", exist=exist)
    IF (exist) THEN
       OPEN(12, file="bandwidth.txt", status="old", position="append", action="write")
    ELSE
       OPEN(12, file="bandwidth.txt", status="new", action="write")
    END IF
    WRITE(12, *) msg_size, msg_size/(SUM(t_ellapsed)/N_pings)
    CLOSE(12)
  END SUBROUTINE ping_process


  SUBROUTINE pong_process()
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    INTEGER :: ierror, i
    INTEGER :: status(MPI_STATUS_SIZE)
    INTEGER :: N_pings, msg_size
    REAL, ALLOCATABLE, DIMENSION(:) :: msg


    PRINT*, 'This is pong.'
    ! Listen to how many ping pongs will occur
    CALL MPI_RECV(N_pings, 1, MPI_INTEGER, 0, 'n_pings', MPI_COMM_WORLD, &
         status, ierror)
    ! Listen to how long the message will be.
    CALL MPI_RECV(msg_size, 1, MPI_INTEGER, 0, 'msg_size', MPI_COMM_WORLD, &
         status, ierror)

    ALLOCATE(msg(msg_size))

    PRINT*, 'Pong: I will receive ', N_pings, ' pings of length ', msg_size


    DO i= 1,N_pings

       CALL MPI_RECV(msg, msg_size, MPI_REAL, 0, 'pingpong', MPI_COMM_WORLD, &
            status, ierror)
       !PRINT*, 'pong', i
       CALL MPI_SEND(msg, msg_size, MPI_REAL, 0, 'pingpong', MPI_COMM_WORLD, ierror)
    ENDDO
  END SUBROUTINE pong_process


END PROGRAM helloworld
