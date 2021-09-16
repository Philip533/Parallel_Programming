program rotate
     use mpi
     implicit none

     !MPI Variables
     integer                             :: rank, size, request, ierror, comm = MPI_COMM_WORLD
     integer, dimension(MPI_STATUS_SIZE) :: send_status, recv_status

     !Loop variables
     integer :: i

     !Summation variables
     integer :: sum, pass_on, add_on, left, right

     !Initialise MPI
     call MPI_INIT(ierror)

     !Find rank for each process and total amount of processes
     call MPI_COMM_RANK(comm, rank, ierror)
     call MPI_COMM_SIZE(comm, size, ierror)
     
     !Values are passed around clockwise. Left = clockwise, right = anticlockwise
     left = rank + 1
     right = rank - 1

     !Correct boundary conditions for maximum and minimum ranks
     if(rank==0)then
          right = size - 1
     end if

     if(rank==(size-1))then
          left = 0
     end if

     !Set each processes value equal to its rank and set initial sum to 0
     pass_on = rank
     sum = 0
     !Pass each value around the ring P times, where P is the amount of processes
     do i = 1, size
          
          !First issue non-blocking send
          call MPI_ISSEND(pass_on, 1, MPI_INTEGER, left, 0, comm, request,ierror)

          !Issue a receive
          call MPI_RECV(add_on, 1, MPI_INTEGER, right, 0, comm, recv_status, ierror)

          !Wait for the send to complete
          call MPI_WAIT(request, send_status, ierror)

          !Add the values up
          sum = sum + add_on

          !Pass the next value on
          pass_on = add_on

     end do

     !Output the final sum
     write(*,*) "The final sum was ", sum ,"on rank", rank

     !Finalize MPI
     call MPI_FINALIZE(ierror)


end program 
