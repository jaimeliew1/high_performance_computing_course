FUNCTION step(T_old, D, N, dx, dt)
  ! Performs a single step of the heat equation solver
  REAL, DIMENSION(:, :) :: T_old
  INTEGER :: N
  REAL :: dx, dt, D
  REAL, DIMENSION(:, :), ALLOCATABLE :: step
  ALLOCATE(step(N, N))
  step =  T_old
  step(2:N-1, 2:N-1) =  step(2:N-1, 2:N-1) + D*dt/dx**2*(T_old(2:N-1, 1:N-2) - 2*T_old(2:N-1, 2:N-1) + T_old(2:N-1, 3:N))
  step(2:N-1, 2:N-1) =  step(2:N-1, 2:N-1) + D*dt/dx**2*(T_old(1:N-2, 2:N-1) - 2*T_old(2:N-1, 2:N-1) + T_old(3:N, 2:N-1))
END FUNCTION step
