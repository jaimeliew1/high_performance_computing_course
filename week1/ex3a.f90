PROGRAM main
  ! INTEGER, PARAMETER :: n = 698169 ! This one fails for automatic arrays
  !INTEGER, PARAMETER :: n = 698168 ! This one passes for automatic arrays
  INTEGER, PARAMETER :: n =  2000000 !
  REAL, DIMENSION(n) :: A, B

  INTERFACE
     SUBROUTINE swap1(A, B)
       REAL, DIMENSION(:), INTENT(INOUT) :: A, B
       REAL, DIMENSION(SIZE(A)) :: work ! on the stack !
     END SUBROUTINE swap1
     SUBROUTINE swap2(A, B)
       REAL, DIMENSION(:), INTENT(INOUT) :: A, B
       REAL, DIMENSION(:), ALLOCATABLE :: work ! on the stack !
     END SUBROUTINE swap2
  END INTERFACE
  A = 1
  B = 0

  PRINT*, 'BEFORE'
  PRINT*, 'A: ', A(1)
  PRINT*, 'B: ', B(1)

  CALL swap2(A, B)

  PRINT*, 'AFTER'
  PRINT*, 'A: ', A(1)
  PRINT*, 'B: ', B(1)

END PROGRAM main


SUBROUTINE swap1(A, B)
  ! Automatic array
  REAL, DIMENSION(:), INTENT(INOUT) :: A, B
  REAL, DIMENSION(SIZE(A)) :: work ! on the stack !
  work = A
  A = B
  B = work
END SUBROUTINE swap1

SUBROUTINE swap2(A, B)
  ! allocatable array
  REAL, DIMENSION(:), INTENT(INOUT) :: A, B
  REAL, DIMENSION(:), ALLOCATABLE :: work
  INTEGER :: info

  ALLOCATE(work(SIZE(A)), stat=info)
  PRINT*, info
  work = A
  A = B
  B = work
END SUBROUTINE swap2
