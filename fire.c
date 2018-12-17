#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <mpi.h>
#include <unistd.h>

#define ROW 40
#define COL 80

#define TREE 'T'
#define BURN 'X'
#define SPCE ' '

unsigned char gof[ROW][COL];
unsigned char gof_cp[ROW][COL];

int rank = 0,size = 1;

void prin(){
	int i = 0;
	int j = 0;
	//printf("rank: %d\n", rank);
	//int test = 0;
	//if(rank == 0){
		printf("+----------------------------------------------------------------------------------+\n");
	//}
	while (i<ROW){
		j=0;
		printf("| ");
		while (j<COL){
			printf("%c",gof[i][j]);
			j++;
		}
		printf(" |\n");
		i++;
	}
	//if (rank == size - 1){
		printf("+----------------------------------------------------------------------------------+\n"); 
	//}
	fflush(stdout);
}

//number of neighboors AROUND ro,co that are trees
int numT(int ro, int co){
	int c = 0;
	for(int row = ro - 1; row <= ro + 1; row++){
		for(int col = co - 1; col <= co + 1; col++){
			//printf("Alpha");
			if(row < 0 || col < 0 || row >= ROW || col >= COL || (row == ro && col == co)){
				continue;
			}else if(gof[row][col] == TREE){
				c++;
			}
		}
	}
	return c;
}

//is near FIRE
int fireFriend(int ro, int co){
	for(int row = ro - 1; row <= ro + 1; row++){
		for(int col = co - 1; col <= co + 1; col++){
			if(row < 0 || col < 0 || row >= ROW || col >= COL || (row == ro && col == co)){
				continue;
			}
			if(gof[row][col] == BURN){
				return 1;
			}
		}
	}
	return 0;
}   

//sim one iteration
void future(double igni, double grow){
	char above[COL];
	char bellow[COL];
	
	MPI_Status status;
	if(rank % 2 == 0){
		if(rank != 0){//send our first row to the rank above us
			MPI_Send(gof[(ROW/size)*rank], COL, MPI_CHAR, rank - 1, 0, MPI_COMM_WORLD);
		}
		if(rank != size - 1){//send out last row tro the rank bellow us
			MPI_Send(gof[((ROW/size)*(rank+1))-1], COL, MPI_CHAR, rank + 1, 0, MPI_COMM_WORLD);
		}
		if(rank != 0){//get the row right above our scope
			MPI_Recv(above, COL, MPI_CHAR, rank - 1, 0, MPI_COMM_WORLD, &status);
		}
		if(rank != size - 1){//get the row right bellow our scope
			MPI_Recv(bellow, COL, MPI_CHAR, rank + 1, 0, MPI_COMM_WORLD, &status);
		}
	}else{//flip flop prevent deadlock
		//get the row right above us
		MPI_Recv(above, COL, MPI_CHAR, rank - 1, 0, MPI_COMM_WORLD, &status);
		if(rank != size - 1){//get the row just bellow our scope
			MPI_Recv(bellow, COL, MPI_CHAR, rank + 1, 0, MPI_COMM_WORLD, &status);
		}
		//send our first row to the rank just above us
		MPI_Send(gof[(ROW/size)*rank], COL, MPI_CHAR, rank - 1, 0, MPI_COMM_WORLD);
		if(rank != size -1){//send our last row to the rank bellow us
			MPI_Send(gof[((ROW/size)*(rank+1))-1], COL, MPI_CHAR, rank + 1, 0, MPI_COMM_WORLD);
		}
	}

	//put in info to gof because thats the easy solution
	if(rank != 0){
		for(int col = 0; col < COL; col ++){
			gof[((ROW/size)*rank)-1][col] = above[col];
		}
	}
	if(rank != size -1){
		for(int col = 0; col < COL; col ++){
			gof[(ROW/size)*(rank+1)][col] = bellow[col];
		}
	}

	//find new values for the forrest
	for(int row = (ROW/size)*rank; row < (ROW/size)*(rank+1); row++){
		//printf("row: %d\n", row);
		for(int col = 0; col < COL; col++){
			//printf("col: %d, char: |%c|\n", col, gof[row][col]);
			gof_cp[row][col] = gof[row][col];

			//step one, a tree that is next to a burning tree will become one itself
			if(gof[row][col] == TREE && fireFriend(row,col) == 1){
				gof_cp[row][col] = BURN;
			}
			
			//step two, a tree that is burning becomes empty
			if(gof[row][col] == BURN){
				gof_cp[row][col] = SPCE;
			}
			
			//step three, a tree with no burning buddies has a igni chance to be a hipster and set themselves on fire like a dumbass
			if(gof[row][col] == TREE && fireFriend(row,col) == 0){
				if(((double)rand()/(double)RAND_MAX) <= igni){
					gof_cp[row][col] = BURN;
				}	
			}

			//step four, and empty space will turn into a tree based on formula
			if(gof[row][col] == SPCE){
				//printf("TRIP a value = %f\n",((double)rand()/(double)RAND_MAX));
				if(((double)rand()/(double)RAND_MAX) <= (grow * (double)(numT(row,col) + 1))){
					gof_cp[row][col] = TREE;
				}
			}
		}//inner for loop
	}//endo of double for loop

	//now that we got all of our info from the mibober we can transfer our knowledge of the forrest into the forrest
	for(int row = (ROW/size)*rank; row < (ROW/size)*(rank+1); row ++){
		for(int col = 0; col < COL; col ++){
			gof[row][col] = gof_cp[row][col];
		}
	}

}//end of function


// a.out / file / iterations / ignight chance / grow chance /
int main(int argc, char** argv){
	
	if(argc != 5){
		printf("insert proper parameters\n");
		return -1;
	}

	//int rank, size;
	MPI_Init(&argc, &argv);
	
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	char f;
	int c = 0;
	FILE* file;
	
	srand(time(NULL)+rank);
	
	file = fopen(argv[1], "r");

	do{// rip the info from the file
		f = getc(file);
		if(f == '\n'){
			continue;
		}
		gof[c/COL][c%COL] = f;
		c++;
	}
	while(f != EOF);

	//run sim for argv[2] iterations
	for (int base = 0; base < atoi(argv[2]); base ++){
		//printf("Welcome Iteration %d\n",base);
		MPI_Barrier(MPI_COMM_WORLD);
		future(atof(argv[3]), atof(argv[4]));
	}
	//printf("Rank: %d done with the sims sir!\n",rank);
	//semaphore?
	if(rank > 0){
		//MPI_Status status;
		//int wait;
		//printf("rank: %d is waiting\n",rank);
		for(int row = (ROW/size)*rank; row < (ROW/size)*(rank+1); row ++){
				//gof[row][col] = gof_cp[row][col];
				MPI_Send(gof[row], COL, MPI_CHAR, 0, 0, MPI_COMM_WORLD);
		} 
		//MPI_Recv(&wait, 1, MPI_INT, rank - 1, 0, MPI_COMM_WORLD, &status);
		//printf("rank: %d is going\n",rank);
	}else{//for 0
		MPI_Status status;
		char item[COL];
		for(int row = (ROW/size)*(rank+1); row < ROW; row ++){
			//printf("need rank: %d\n", row/(ROW/size)); 
			MPI_Recv(item, COL, MPI_CHAR, row/(ROW/size), 0, MPI_COMM_WORLD, &status);
			for(int col = 0; col < COL; col ++){
				gof[row][col] = item[col];
			}
		}
	}

	//printf("rank: %d printing now\n",rank);
	if(rank == 0){
		prin();//output the view of the forrest;
	}
	//printf("rank: %d done\n", rank);
	
	/*
	if(rank < size - 1){
		int go = 1;
		MPI_Send(&go, 1, MPI_INT, rank + 1, 0, MPI_COMM_WORLD);
	}*/

	MPI_Finalize();
	return 0;
}
