MODULE m_read_input
  INTEGER, PARAMETER :: MK = KIND(1.0E0)

CONTAINS

  SUBROUTINE read_input(filename, Nx, Ny, D, T_max, rank, n_proc)
    IMPLICIT NONE
    CHARACTER(len=*), INTENT(in) :: filename
    INTEGER, INTENT(out) :: Nx, Ny
    REAL(MK), INTENT(out) :: D, T_max
    INTEGER, INTENT(in) :: rank, n_proc
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

       IF (TRIM(key) == 'Nx') THEN
          READ(val, *) Nx
       ELSEIF (TRIM(key) == 'Ny') THEN
          READ(val, *) Ny
       ELSEIF (TRIM(key) == 'D') THEN
          READ(val, *) D
       ELSEIF (TRIM(key) == 'T_max') THEN
          READ(val, *) T_max
       ENDIF


    ENDDO

200 CONTINUE
    !PRINT*, 'EOF'
    CLOSE(10)

    IF (rank == 0) THEN
       PRINT*, 'Nx =', Nx
       PRINT*, 'Ny =', Ny
       PRINT*, 'D =', D
       PRINT*, 'T_max =', T_max
    ENDIF
    
  END SUBROUTINE read_input
END MODULE m_read_input
