// Nicholas Aristizabal
// COP3402 Systems Software
// June 2nd 2023

#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define ARR_SIZE 500

// Actual stack to store everything
int pas[ARR_SIZE];

// These are used strictly for printing purposes. Stack base will keep track of the very beginning of the stack.
// arLoc will keep track of the different activation record locations (in order to accurately put a | in between them).
// arLoc will add an address whenever we do CAL to store the address of where the previous activation record finished.
// currentAr is to keep track which activation record we are keeping track of
int stackBase;
int arLoc[(int) ARR_SIZE/3];
int currentAr = 0;


int base(int BP, int L);
void printStack(int SP);

int main(int argc, char *argv[]) {

    // Initialize the stack and Base Pointer
    int BP = 0;

    for(int i=0; i<ARR_SIZE; i++)
        pas[i] = 0;

    // File for I/O
    FILE *fp;
    fp = fopen(argv[1], "r");

    // Read the instructions from the file and put them into the stack
    // until we reach 9 0 3 (halt)
    while(1){
        fscanf(fp, "%d", &pas[BP]);
        if(BP > 2 && pas[BP] == 3 && pas[BP-1] == 0 && pas[BP-2] == 9){
            break;
        }
        BP++;
    }

    // Adjust BP to be one past the last M value and stack pointer one less than BP
    BP++;
    int SP = BP - 1;
    int PC = 0;
    stackBase = BP;

    // 8-Bit integer to store the halt signal
    uint8_t halt = 1;

    // 8-Bit integers for op and L and normal integer for M
    uint8_t OP;
    uint8_t L;
    int M;

    // Print initial values
    printf("\t\t\tPC\tBP\tSP\tstack\n");
    printf("Initial Values:\t\t%d\t%d\t%d\n\n", PC, BP, SP);

    // char array to store the instruction string
    char instruct[4];

    // While we do not receive the halt command
    while(halt){

        // Fetch
        OP = pas[PC];
        L = pas[PC+1];
        M = pas[PC+2];
        PC +=3;

        // Check if valid instruction
        if (OP == 0 || OP>9){
            printf("Error, invalid operation for %d %d %d\n", OP, L, M);
            break;
        }

        // Execute
        switch (OP) {

            // LIT 0 M, Pushes a constant value (literal) M onto the stack
            case 1:{
                SP++;
                pas[SP] = M;
                strcpy(instruct, "LIT");
                break;
            }
            // OPR 0 M, Operation to be performed on the data at the top of the stack.
            // (or return from function)
            case 2:{

                if (M<0 || M>11){
                    printf("Error, invalid operation for OPR %d %d %d\n", OP, L, M);
                    break;
                }
                // ANOTHER SWITCH FOR OPERATIONS

                switch (M) {
                    // RTN
                    case 0:{
                        SP = BP - 1;
                        BP = pas[SP + 2];
                        PC = pas[SP + 3];
                        strcpy(instruct, "RTN");

                        break;
                    }
                    // ADD
                    case 1:{
                        pas[SP-1] = pas[SP-1] + pas[SP];
                        SP--;
                        strcpy(instruct, "ADD");
                        break;
                    }
                    // SUB
                    case 2:{
                        pas[SP-1] = pas[SP-1] - pas[SP];
                        SP--;
                        strcpy(instruct, "SUB");
                        break;
                    }
                    // MUL
                    case 3:{
                        pas[SP-1] = pas[SP-1] * pas[SP];
                        SP--;
                        strcpy(instruct, "MUL");
                        break;
                    }
                    // DIV
                    case 4:{
                        pas[SP-1] = pas[SP-1] / pas[SP];
                        SP--;
                        strcpy(instruct, "DIV");
                        break;
                    }
                    // EQL
                    case 5:{
                        pas[SP-1] = pas[SP-1] == pas[SP];
                        SP--;
                        strcpy(instruct, "EQL");
                        break;
                    }
                    // NEQ
                    case 6:{
                        pas[SP-1] = pas[SP-1] != pas[SP];
                        SP--;
                        strcpy(instruct, "NEQ");
                        break;
                    }
                    // LSS
                    case 7:{
                        pas[SP-1] = pas[SP-1] < pas[SP];
                        SP--;
                        strcpy(instruct, "LSS");
                        break;
                    }
                    // LEQ
                    case 8:{
                        pas[SP-1] = pas[SP-1] <= pas[SP];
                        SP--;
                        strcpy(instruct, "LEQ");
                        break;
                    }
                    // GTR
                    case 9:{
                        pas[SP-1] = pas[SP-1] > pas[SP];
                        SP--;
                        strcpy(instruct, "GTR");
                        break;
                    }
                    // GEQ
                    case 10:{
                        pas[SP-1] = pas[SP-1] >= pas[SP];
                        SP--;
                        strcpy(instruct, "GEQ");
                        break;
                    }
                    // ODD
                    case 11:{
                        pas[SP] = pas[SP] % 2;
                        break;
                    }
                    
                }
                
                break;
            }
            // LOD L M, Load value to top of stack from the stack location at offset M
            // from L lexicographical levels down
            case 3:{
                SP++;
                pas[SP] = pas[base(BP, L) + M];
                strcpy(instruct, "LOD");
                break;
            }
            // STO L M, Store value at top of stack in the stack location at offset M
            // from L lexicographical levels down
            case 4:{
                pas[base(BP, L) + M] = pas[SP];
                SP--;
                strcpy(instruct, "STO");
                break;
            }
            // CAL L M, Call procedure at code index M (generates new Activation Record and PC = M)
            case 5:{
                pas[SP+1] = base(BP, L); // static link (SL)
                pas[SP+2] = BP; // dynamic link (DL)
                pas[SP+3] = PC; // return address (RA)
                BP = SP + 1;
                PC = M;
                arLoc[currentAr++] = SP+1;
                strcpy(instruct, "CAL");
                break;
            }
            // INC 0 M, "Allocate" M memory words (increment SP by M).
            // First three are reserved to Static Link (SL), Dynamic Link (DL), and Return Address (RA)
            case 6:{
                SP += M;
                strcpy(instruct, "INC");
                break;
            }
            // JMP 0 M, Jump to instruction M (PC = M)
            case 7:{
                PC = M;
                strcpy(instruct, "JMP");
                break;
            }
            // JPC 0 M, Jump to instruction M if stack element is 0
            case 8:{
                if(pas[SP] == 0){
                    PC = M;
                }
                SP--;
                strcpy(instruct, "JPC");
                break;
            }
            // SYS 0 M, System instructions
            case 9:{

                // SYS 0 1, Write the top stack element to the screen
                switch (M) {
                    case 1:{
                        printf("Output result is: %d\n", pas[SP]);
                        SP--;
                        break;
                    }
                    // SYS 0 2, Read in input from the user and store it on top of the stack
                    case 2:{
                        SP++;
                        printf("Please Enter an Integer:\n");
                        scanf("%d", &pas[SP]);
                        break;
                    }
                    // SYS 0 3, End of program
                    case 3:{
                        halt = 0;
                        break;
                    }
                }

                strcpy(instruct, "SYS");
                break;
            }
        }

        // Print the instruction, L, M, PC, BP, and SP then the stack
        printf("%s\t%d\t%d\t%d\t%d\t%d\t ", instruct, L, M, PC, BP, SP);
        printStack(SP);

    }

    fclose(fp);
    return 0;
}

int base(int BP, int L){
    int arb = BP; // arb = activation record base
    while ( L > 0) //find base L levels down
    {
        arb = pas[arb];
        L--;
    }
    return arb;
}

void printStack(int SP){

    // i to keep track of where we are in the stack
    int i = stackBase;
    // arCheck to keep track of which activation record we are at
    int arCheck = 0;
    // While we are not at the end of the stack print it
    while(i <= SP){
        // Check if our current address was one we cached to have a | in it
        if(i == arLoc[arCheck]) {
            printf("| ");
            arCheck++;
        }
        printf("%d ", pas[i]);
        i++;
    }
    printf("\n");
}