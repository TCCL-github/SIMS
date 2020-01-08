program skeleton
use mpi
implicit none
integer :: ierr, rank, size

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
  if (rank .eq. 0) then
      print *, 'master', rank
      print *, 'ierr'  , ierr
  else 
      print *, 'slave' , rank
  end if 
call MPI_FINALIZE(ierr)
END
