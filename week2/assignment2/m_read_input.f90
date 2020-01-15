MODULE m_read_input
  INTEGER, PARAMETER :: MK = KIND(1.0E0)

CONTAINS

  SUBROUTINE read_input(filename, N, N_iter, thres)
    IMPLICIT NONE
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER, INTENT(out) :: N, N_iter
    REAL(MK), INTENT(out) :: thres
    CHARACTER(len=256) :: buffer, key, val
    INTEGER :: sep_loc, ios

    OPEN(10, FILE=filename, ACTION='READ', IOSTAT=ios)

    ! Abort program if input file cannot be opened.
    IF (ios /= 0) THEN
       PRINT*, filename, ' could not be opened. IOSTAT=', ios
       STOP
    ENDIF

    DO ! loop over lines in input file
       READ(10, '(A)', END=200) buffer

       ! ignore blank lines
       IF (LEN(buffer) == 0) THEN
          CYCLE
       ENDIF

       sep_loc = SCAN(buffer, '=')
       ! ignore lines without '='
       IF (sep_loc == 0) THEN
          CYCLE
       ENDIF

       key = buffer(:sep_loc-1)
       val = buffer(sep_loc+1:)

       IF (TRIM(key) == 'N') THEN
          READ(val, *) N
          PRINT*, TRIM(key), '=', N
       ELSEIF (TRIM(key) == 'N_iter') THEN
          READ(val, *) N_iter
          PRINT*, TRIM(key), '=', N_iter
       ELSEIF (TRIM(key) == 'thres') THEN
          READ(val, *) thres
          PRINT*, TRIM(key), '=', thres
       ENDIF


    ENDDO

200 CONTINUE
    !PRINT*, 'EOF'
    CLOSE(10)
  END SUBROUTINE read_input
END MODULE m_read_input
