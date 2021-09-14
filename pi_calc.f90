program pi_calculation
     use mpi
     implicit none

     !Variables
     integer            :: ierror
     integer, parameter :: dp = selected_real_kind(15,300)

     !Pi calculation variables
     real(kind=dp)      :: denominator, reciprocal
     real(kind=dp)      :: pipartial = 0.0_dp, controllerpi, pitotal = 0.0_dp
     real(kind=dp)      :: actual_pi = 4.0_dp*atan(1.0_dp), error

     !Counting variables
     integer            :: N = 840
     integer            :: i

     !MPI variables
     integer            :: comm = MPI_COMM_WORLD
     integer            :: rank, size
     integer, dimension(MPI_STATUS_SIZE) :: status
     integer            :: istart, istop 

     !Timing variables
     real(kind=dp)      :: time_start, time_stop, time

     !Starts the MPI usage
     call MPI_INIT(ierror)

     !Measures the start time 
     time_start = MPI_Wtime()

     !Finds the rank for each separate program
     call MPI_COMM_RANK(comm, rank, ierror)

     !Finds the total amount of processes
     call MPI_COMM_SIZE(comm, size, ierror)

     !Generates the bounds for each rank
     istart = rank*(N/size) + 1
     istop = (rank+1)*(N/size)

     !Pi calculation which uses the different bounds for each rank
     do i = istart,istop
          denominator = 1.0_dp + ((real(i,kind=dp) - 0.5_dp)/real(N,kind=dp))**2
          reciprocal = 1.0_dp/denominator
          pipartial = pipartial + reciprocal
     end do

     !Finds the finished value
     pipartial = (4.0_dp*pipartial) / real(N,kind=dp)

     !Iterates over each rank excluding the controller rank of 0
     do i = 1, size - 1

          !Issues a receive on the controller rank which will correspond to a send from each other rank
          if(rank == 0)then
               call MPI_RECV(controllerpi, 1, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE,0, comm, status, ierror)
               pitotal = pitotal + controllerpi
               print *, controllerpi, "received from rank", status(MPI_SOURCE) 
          end if

          !Sends the partial pi value from each rank other than 0
          if(i == rank)then
               call MPI_SSEND(pipartial, 1, MPI_DOUBLE_PRECISION, 0, 0, comm, ierror)
          end if
     end do 

     !Measures the end time
     time_stop = MPI_WTime()

     !Finishes the calculations on the controller rank
     if(rank ==0) then
          pitotal = pitotal + pipartial 
          time = time_stop - time_start

          !Calculates the percentage error
          error = 100.0_dp*(pitotal - actual_pi)/(actual_pi)

          !Prints out all the values 
          write(*,*)"The time taken on" , size," processes was ", time, "seconds"
          write(*,*)"The final value of pi calculated is ", pitotal
          write(*,*)"The percentage error is", error, "%"
     end if

     !Finishes MPI usage
     call MPI_FINALIZE(ierror)
end program pi_calculation
