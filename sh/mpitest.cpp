#include <iostream>
#include <mpi.h>
#include <stdio.h>

int main(int argc, char **argv)
{
	int rank, size;
	MPI_Init(&argc, &argv);

	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	std::cout << "rank = " << rank << std::endl;
	std::cout << "size = " << size << std::endl;

	MPI_Finalize();
}
