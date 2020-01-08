program main
use mpi
implicit none
integer*8 :: i, n
double precision :: sum, step, pi, x, tsum
logical :: continue
integer :: myrank, nprocs, ierr
integer :: status(MPI_STATUS_SIZE)

continue = .true.

call MPI_INIT(ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)

do while(continue)
    if (myrank .eq. 0) then
        print *, "enter the Number of Interals: (n<1: quits)"
        read *, n
   
        do i=1,nprocs-1
            call MPI_SEND(n, 1, MPI_INTEGER8, i, 1, MPI_COMM_WORLD, ierr)
        end do
    else
        call MPI_RECV(n, 1, MPI_INTEGER8, 0, 1, &
                      MPI_COMM_WORLD, status, ierr)
    end if

    if (n .le. 0) then
        continue = .false.
        goto 10
    endif

    step = 1.d0/dble(n)
    sum = 0.d0
    tsum = 0.d0

    do i=myrank+1,n,nprocs
        x = (dble(i)-.5d0)*step
        sum = sum + 4.d0 / (1.d0 + x**2) !F(x)
    end do

    if (myrank .eq. 0) then
        tsum=sum
        do i=1, nprocs-1
            call MPI_RECV(sum, 1, MPI_DOUBLE_PRECISION, &
                          i, 1, MPI_COMM_WORLD, status, ierr)
            tsum = tsum + sum
        end do
        pi = step * tsum
        write(*,400)
        write(*,100), pi, dabs(dacos(-1.d0)-pi)
        write(*,400),
    else
        call MPI_SEND(sum, 1, MPI_DOUBLE_PRECISION, 0, &
                      1, MPI_COMM_WORLD, ierr)
    end if
end do

   
100 format(' PI = ', F17.15,' (Error=',E11.5,')')
400 format('-------------------------------------')

10 call MPI_FINALIZE(ierr)
   stop
end

