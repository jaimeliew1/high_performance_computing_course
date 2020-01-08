SUBROUTINE salloc(field, Nx, Ny, info)
  ! Allocates an array of size Nx by Ny using SINGLE floating point precision.
  INTEGER, PARAMETER :: MK = KIND(1.0E0)
  REAL(MK), DIMENSION(:, :), ALLOCATABLE, INTENT(out):: field
  INTEGER, INTENT(in) :: Nx, Ny
  INTEGER, INTENT(out) :: info

  IF (.NOT.ALLOCATED(field)) THEN
     ALLOCATE(field(Nx, Ny), stat=info)
  ENDIF
END SUBROUTINE salloc

SUBROUTINE dalloc(field, Nx, Ny, info)
  ! Allocates an array of size Nx by Ny using DOUBLE floating point precision.
  INTEGER, PARAMETER :: MK = KIND(1.0D0)
  REAL(MK), DIMENSION(:, :), ALLOCATABLE, INTENT(out):: field
  INTEGER, INTENT(in) :: Nx, Ny
  INTEGER, INTENT(out) :: info

  IF (.NOT.ALLOCATED(field)) THEN
    ALLOCATE(field(Nx, Ny), stat=info)
  ENDIF
END SUBROUTINE dalloc
