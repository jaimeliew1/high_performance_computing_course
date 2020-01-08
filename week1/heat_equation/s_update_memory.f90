SUBROUTINE update_memory(A, A_old)
  REAL, DIMENSION(:, :), INTENT(inout) :: A, A_old
  A_old = A
END SUBROUTINE update_memory
