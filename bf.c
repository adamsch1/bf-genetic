#include <assert.h>
#include <stdlib.h>

typedef struct _vm  {
  int  sig;
  char mem[256]; // Memory segment
  char *code;      // Code segment
  int  code_size;  // Code size
  int  pc;
  unsigned char  pm;       // We can index 256 bytes of memory
  int  loop_count; // loop count
  int  slice;      // Number of ticks
  float fitness;   // fitness
} vm;

// Structure to store a population of bf programs
#define MAX_POP_SIZE 10000
typedef struct _population  {
  int population_size;
  vm  pop[MAX_POP_SIZE];
} population;

// Structure to store aspects about your simulation
typedef struct _simulation  {
  // Read a byte for us please
  unsigned char (*readbyte)(int p);
  // Write a byte for us please
  void (*writebyte)( int p, unsigned char b );
  // Calc the fitness for this beast please
  float (*calc_fitness)( vm *p, int id );
} simulation;

// Global structure to store aspects of the experiement currently running
typedef struct _global  {
  // Datastructure of the simulation running
  simulation *s;
  // Populations
  population pop[2];

  int current_population; // 0 or 1 index to pop array
  int max_generations;  // Max number of generations to simulate
  int max_depth; // For building random population in gen 0
  int current_generation; // Current generation
  int slice; // Number of instructions to run each bf program
  int max_slice; // Max number of instructions to run each bf program

  // Function selection
  float (*fitness)( unsigned char input, unsigned char output );
  void  (*selection)( int *p1, int *p2 );

  // Statistica information
  float best_fitness; // Best fitness
  int   best_index;   // Index to best member of population

} global;

// Default write byte
void _write_byte( int p, unsigned char b )  {
  printf("%d ", b );
}

global g;

// Generate random number between 1 and sides inclusive
int bf_num( int sides )  {
  return 1 + (int) ((float)sides*rand()/(RAND_MAX+1.0));
}

void bf_init( unsigned seed)  {
  if( seed )  {
    srand(seed);
  } else {
    srand(time(0));
  }
}

// Recursivly generate bf code
static char * _generate( char *p, int depth )  {
  char *t;
  int j;

  while( depth > 0 )  {
    // Generate # between 1 - 6
    j = bf_num( 6 );

    switch( j )  {
      case 1:
        *p = '<';
        depth--;
        break;
      case 2:
        *p = '>';
        depth--;
        break;
      case 3:
        *p = '+';
        depth--;
        break;
      case 4:
        *p = '-';
        depth--;
        break;
      case 5:
        *p = '.';
        depth--;
        break;
      case 6:
        // All [] must contain a valid instruction
        if( depth > 3 )  {
          *p = '[';  // depth -= 1
          p++;
          t = _generate( p, depth - 2 );
          depth -= t - p;
          p = t;
          *p = ']';  // depth -= 1, (or 2 in total) 
          depth -= 2;
        } else {
          continue;
        }
        break;
    }
    p++;
  }
  return p;
}


// Generate a random bf program of max length max_depth
int bf_generate( vm *p, int max_depth )  {
  int j;
  char *t;

  // Generate random max depth
  j = bf_num(max_depth);
  p->code = (char *)calloc(1, j+1);
  assert(p->code);
  assert(j>0);

  t = _generate( p->code, j );
  assert( (int)(t - p->code) <= j );
  assert( strlen(p->code) == j );
  p->code_size = j;

}

// Given a string of code that begins with [ find the matching evenly nested
// closing ].  Stores byte offset in end
static void _find_matching( char *string, int start, int *end )  {
  int depth = 1;
  int k;

  k = start;
  while( depth )  {
    k++;
    switch( *(string + k) )  {
      case ']': depth--; break;
      case '[': depth++; break;
    }
  }
  *end = k;
}

// Pick a legal random chunk of bf code from a bf program.
static int _substring( vm *p1, int *s, int *e )  {
  int k;
  int start,end;
  int depth;

  start = 0;
  end = -1;

  // Pick a point see if we are between [], if we are, use that segment
  while( 1 )  {
    k = bf_num( p1->code_size ) - 1;
    switch( p1->code[k] )  {
      case '[': // Find matching ]
        start = k;
        _find_matching( p1->code, start, &end );
        goto done;
        break;
      case '<':
      case '>':
      case '.':
      case '+':
      case '-':
        while( k-- && k > -1 )  {
          switch( p1->code[k] )  {
            case '[': 
              start = k;
             _find_matching( p1->code, start, &end );
             goto done;
             break;
            case ']':
              start = ++k;
              while( p1->code[k] != '[' && p1->code[k] != ']' && k < p1->code_size )  {
                k++;
              }
              end = --k;
              goto done;
          }
        }
        start = 0;
        while( p1->code[k] != '[' && p1->code[k] != ']' && k < p1->code_size)  {
          k++;
        }
        end = --k;
        goto done;
        break;
    }
  }

done:

  *s = start;
  *e = end;
}

// Crossover p1 and p2 creating child p3.  p3 code will be filled out
void bf_crossover( vm *p1, vm *p2, vm *p3 )  {
  int s1, s2, e1, e2;
  int l1, l2;
  int temp2;

  assert( p1->sig == 0xcafebabe );
  assert( p2->sig == 0xcafebabe );
  assert( p3->sig == 0xcafebabe );

  assert( strlen(p1->code) > 0 );
  assert( strlen(p2->code) > 0 );

  // Pick to random substrings
  _substring( p1, &s1, &e1 );
  _substring( p2, &s2, &e2 );

  // Lets take a random sublength
  if( p1->code[s1] != '[' && e1-s1 > 0 )  {
    e1 = s1 +  bf_num(e1-s1);
    assert( e1 > s1 && e1 < p1->code_size );
  }
 
  if( p2->code[s2] != '[' && e2-s2 > 0 )  {
    e2 = s2 +  bf_num(e2-s2);
    assert( e2 > s2 && e2 < p2->code_size );
  }

  // Lengths of strings
  l1 = e1 - s1;
  l2 = e2 - s2;

  // Free any code already there
  if( p3->code )  {
    free(p3->code);
  }

  // Reallocate memory 
  if( l1 < l2 )  {
    temp2 = p1->code_size + l2 - l1;
    assert(temp2>0);
    p3->code = (char *)calloc( 1, temp2 + 1);
    p3->code_size = p1->code_size + l2 - l1;
  } else {
    temp2 = p1->code_size;
    assert(temp2>0);
    p3->code = (char *)calloc( 1, temp2 + 1);
    p3->code_size = p1->code_size;
  }
  assert( p3->code_size > 0 );

  // fill out first our new child
  memset( p3->code, 0x20, p3->code_size );
  p3->code[p3->code_size] = 0;

  // See if what we are replacing is the entire code segment
  if( s1 != 0 )  {
    temp2 = s1;
    memcpy( p3->code, p1->code, s1 );
  } else {
    temp2 = 0;
  }

  // Middle bit
  memcpy( p3->code + temp2, p2->code + s2, e2-s2 + 1);
  temp2 += e2-s2 + 1;
  // Last bit
  //assert( p1->code_size - e1 - 1 >= 0 );
  if( p1->code_size - e1 - 1 < 0 )  {
    printf("Holy shit batman\n");
    _substring( p1, &s1, &e1 );
  }
  memcpy( p3->code + temp2, p1->code + e1+1, p1->code_size - e1 -1 );
  temp2 += p1->code_size - e1 - 1;

  p3->code[temp2] = 0;
  assert( strlen(p3->code) <= p3->code_size );
  printf( "bf_crossover: final %s\n", p3->code );
  p3->code_size = temp2;

  assert( strlen(p3->code) <= p3->code_size );
  assert( p3->code_size > 0 );
}

// Execute a bf program
int bf_execute( vm *p, int slice, int id )  {
  int l;
  assert(p);
  assert( slice > 0 );
  while( slice-- && p->pc < p->code_size )  {
    switch( *(p->code + p->pc) )  {
      case '<': p->pm--; break;
      case '>': p->pm++; break;
      case '+': ++p->mem[p->pm]; break;
      case '-': --p->mem[p->pm]; break;
      case '.': g.s->writebyte(id, p->mem[p->pm]); break;
      case ',': p->mem[p->pm] = g.s->readbyte(id); break;
      case '[':
        if( !p->mem[p->pm] )  {
          for(p->loop_count = 1, l=p->pc+1; l<p->code_size && p->loop_count; 
               l++)  {
            if( p->code[l] == '[' ) p->loop_count++;
            else if( p->code[l] == ']' ) p->loop_count--;
          }
          p->pc = l-1;
        }
        break;
      case ']':
        for(p->loop_count = 1, l=p->pc-1; l>0 && p->loop_count; l--)  {
          if( p->code[l] == ']' ) p->loop_count++;
          else if( p->code[l] == '[' ) p->loop_count--;
        }
        p->pc = l;
        break;
      default:
        break;
    }
    p->pc++;
  }
  p->slice = slice;
}


// continuous fitness function
static float continuous_fitness_function( unsigned char input, 
    unsigned char output )  {

  return (float)input - output;
}

// squared error fitness function (dampens small deviations)
static float squred_fitness_function( unsigned char input, 
    unsigned char output )  {

  return (float)(input - output)*(input - output);
}


// Generate initial population
static void _gen_population() {
  int l;

  printf("Generating generation [%d]\n", g.current_generation );

  // Iterate through the population size;
  for(l=0; l<g.pop[0].population_size; l++)  {
    // Generate random bf strings of max_depth 
    bf_generate( &g.pop[0].pop[l], g.max_depth );
    printf("bf_generate: pop = %d, code=#%s#\n", l, g.pop[0].pop[l].code );
  }

}

// One of our selection operators - selects two parents for crossover
static void _tournament_selection( int *p1, int *p2)  {
  int l;
  int old_population = g.current_population ^ 1;
  int a,b;

  // Select random a,b for parent
  a = bf_num( g.pop[0].population_size ) - 1;
  b = bf_num( g.pop[0].population_size ) - 1;
  if( g.pop[old_population].pop[a].fitness < 
      g.pop[old_population].pop[b].fitness )  {
    *p1 = a;
  } else {
    *p1 = b;
  }

  // Select random a,b making sure they aren't already one of the parents (p1)
  do  {
    a = bf_num( g.pop[0].population_size ) - 1;
    b = bf_num( g.pop[0].population_size ) - 1;
  } while( a == *p1 || b == *p1 );

  if( g.pop[old_population].pop[a].fitness < 
      g.pop[old_population].pop[b].fitness )  {
    *p2 = a;
  } else {
    *p2 = b;
  }

  assert( a > -1 && a < g.pop[0].population_size );
  assert( b > -1 && b < g.pop[0].population_size );
  assert( *p1 != *p2 );
}

#define MAX_POP 500
unsigned char state[MAX_POP];

void bf_run_generation()  {
  int l;
  int old_population = g.current_population;
  int a, b;

  // Rest best of generation numbers
  g.best_fitness = 0.0;
  g.best_index = 0;

  memset( state, 0, sizeof(state));
  printf("Executing generation [%d]\n",
      g.current_generation );

  // Iterate through every member of the population
  for(l=0; l<g.pop[g.current_population].population_size; l++)  {
    // Execute the bf program
    bf_execute( &g.pop[g.current_population].pop[l], g.slice, l ); 
    // Calc the fitness
    g.s->calc_fitness( &g.pop[g.current_population].pop[l], l );
    // Store best fitness so far
    if( g.pop[g.current_population].pop[l].fitness > g.best_fitness )  {
      g.best_fitness = g.pop[g.current_population].pop[l].fitness;
      g.best_index = l;
    }
  }

  // Print summary information
  printf("Best of generation [%d] Pop-Size [%d] Fitness %f Code %s\n",
      g.current_generation, 
      g.pop[0].population_size,
      g.best_fitness, 
      g.pop[g.current_population].pop[g.best_index].code );

  // cycle between 1 and 0
  g.current_population ^= 1;

  // One more generation
  g.current_generation++;

  printf("Generating generation [%d]\n", g.current_generation );

  // Breed the new generation
  for(l=0; l<g.pop[g.current_population].population_size; l++)  {
    char *temp = g.pop[g.current_population].pop[l].code;
    g.selection( &a, &b ); // A, B are indexes to the parents
    assert(a != b );
    // Reset the individual
    memset( &g.pop[g.current_population].pop[l], 0, sizeof(vm) );
    g.pop[g.current_population].pop[l].sig = 0xcafebabe;
    g.pop[g.current_population].pop[l].code = temp;
    g.pop[g.current_population].pop[l].fitness = 0.00;

    printf("selection [%d] A = %s,%f, B = %s,%f ",
        l,
        g.pop[old_population].pop[a].code,
        g.pop[old_population].pop[a].fitness,
        g.pop[old_population].pop[b].code,
        g.pop[old_population].pop[b].fitness );
    // Procreate
    bf_crossover( &g.pop[old_population].pop[a],
        &g.pop[old_population].pop[b],
        &g.pop[g.current_population].pop[l] );
  }
}

// Run the simulation for N generations defined in bf_init_global
void bf_run()  {
  int l;

  // Run N generations 
  for(l=0; l<g.max_generations; l++)  {
    bf_run_generation();
  }
}

// Initialize global datastructure
void bf_init_global( int population_size, 
                     int max_generations, 
                     int max_depth,  
                     int slice,
                     int max_slice,
                     simulation *s )  {

  int l;

  // Set size of population
  g.pop[0].population_size = population_size;
  g.pop[1].population_size = population_size;

  // Set sig
  for(l=0; l<population_size; l++)  {
    g.pop[0].pop[l].sig = 0xcafebabe;
    g.pop[1].pop[l].sig = 0xcafebabe;
  }

  for(l=0; l<population_size; l++)  {
    g.pop[0].pop[l].sig == 0xcafebabe;
    g.pop[1].pop[l].sig == 0xcafebabe;
  }

  assert(g.pop[1].pop[0].sig == 0xcafebabe);

  // Set current active population
  g.current_population = 0;

  // Set maximum generations to evalulate
  g.max_generations = max_generations;

  // Set maximum depth for generation random bf strings
  g.max_depth = max_depth;

  // Set variables that control execute of bf program
  g.slice = slice;
  g.max_slice = max_slice;

  // Record user defined simulation datastructure
  g.s = s;

  // Set default fitness functino
  g.fitness = continuous_fitness_function;
  g.selection = _tournament_selection;

  _gen_population();
}

/*
#define UMHELLO "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>
++.<<++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<"
*/

#define UMHELLO ".+[.+]"


void sim_write_byte( int id, unsigned char b )  {
  int c = g.current_population;

  // Print ascii values simulation
  if( state[id] == 255 )  {
    g.pop[c].pop[id].fitness = 0.0;
  } else if( state[id] == b )  {
    g.pop[c].pop[id].fitness += 1.0;
    state[id] = b+1;
  } else {
    g.pop[c].pop[id].fitness = 0.0;
  }
}

unsigned char sim_read_byte(int id)  {
  return 0;
}

float sim_calc_fitness( vm *p, int id )  {
  int l;

  p->fitness = 255.0 - p->fitness;
}

int main()  {
#if 1
  simulation s;

  // Set simulation attributes
  s.writebyte = &sim_write_byte;
  s.readbyte = &sim_read_byte;
  s.calc_fitness = &sim_calc_fitness;

  bf_init_global( MAX_POP, 100, 20, 10000, 10000, &s );
  bf_run();

  //bf_run_generation();
#else 
  char buff[] = ".+[.+]";
  vm p;
  vm p1, p2, p3;
  simulation s;

  // Set simulation attributes
  s.writebyte = &_write_byte;
  s.readbyte = &sim_read_byte;
  g.s = &s;

  p.sig = 0xcafebabe;
  memset(&p, 0, sizeof(vm));
  p.code = malloc(sizeof(buff));
  strcpy(p.code, buff);
  p.code_size = strlen(buff);
  bf_execute( &p, 10000 , 0);

#endif
}









