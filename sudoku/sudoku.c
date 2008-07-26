#include <stdio.h>
#include <time.h>

typedef struct {
  int row;
  int col;
} pos_t;

typedef int puzzle_t[9][9];

puzzle_t p = { {  1, -1, -1,  -1, -1, -1,  -1, -1, -1 },
               { -1, -1,  2,   7,  4, -1,  -1, -1, -1 },
               { -1, -1, -1,   5, -1, -1,  -1, -1,  4 },

               { -1,  3, -1,  -1, -1, -1,  -1, -1, -1 },
               {  7,  5, -1,  -1, -1, -1,  -1, -1, -1 },
               { -1, -1, -1,  -1, -1,  9,   6, -1, -1 },

               { -1,  4, -1,  -1, -1,  6,  -1, -1, -1 },
               { -1, -1, -1,  -1, -1, -1,  -1,  7,  1 },
               { -1, -1, -1,  -1, -1,  1,  -1,  3, -1 } };

#ifdef _MSC_VER
#define inline __inline
#endif

#define TRUE 1
#define FALSE 0

static inline int
get_cell_pos(puzzle_t p, pos_t pos)
{
  return p[pos.row][pos.col];
}

static inline void
set_cell_pos(puzzle_t p, pos_t pos, int value)
{
  p[pos.row][pos.col] = value;
}

static inline int
get_cell(puzzle_t p, int row, int col)
{
  pos_t pos;
  pos.row = row;
  pos.col = col;
  return get_cell_pos(p, pos);
}

static inline pos_t
find_subsquare(pos_t pos)
{
  pos_t spos;
  spos.row = pos.row - (pos.row % 3);
  spos.col = pos.col - (pos.col % 3);
  return spos;
}

pos_t
find_blank(puzzle_t p)
{
  int row, col;
  pos_t pos;

  pos.row = -1;
  pos.col = -1;

  for (row = 0; row < 9; row++) {
    for (col = 0; col < 9; col++) {
      if (get_cell(p, row, col) == -1) {
        pos.row = row;
        pos.col = col;
        return pos;
      }
    }
  }

  return pos;
}

void
pretty_print_cell(puzzle_t p, pos_t pos)
{
  int c = get_cell_pos(p, pos);
  fputc(' ', stdout);
  if (c < 0)
    putc('.', stdout);
  else
    putc(c + '0', stdout);
}

void
pretty_print(puzzle_t p)
{
  int row, col;

  for (row = 0; row < 9; row++) {
    if (row % 3 == 0)
       fputc('\n', stdout);

    for (col = 0; col < 9; col++) {
      pos_t pos;

      pos.row = row;
      pos.col = col;

      if (col % 3 == 0)
        putc(' ', stdout);

      pretty_print_cell(p, pos);
    }

    putc('\n', stdout);
  }
}

int
check_number(puzzle_t p, pos_t pos, int num)
{
  int i, j;
  int row, col;
  pos_t spos;

  // check the column
  for (row = 0; row < 9; row++) {
    if (get_cell(p, row, pos.col) == num) {
      return FALSE;
    }
  }

  // check the row
  for (col = 0; col < 9; col++) {
    if (get_cell(p, pos.row, col) == num) {
      return FALSE;
    }
  }

  // check the square
  spos = find_subsquare(pos);
  for (i = 0; i < 3; i++) {
    for (j = 0; j < 3; j++) {
      int row, col;
      row = spos.row + i;
      col = spos.col + j;

      if (get_cell(p, row, col) == num)
        return FALSE;
    }
  }

  // printf("%d is ok in (row,col) = (%d,%d)\n", num, pos.row, pos.col);
  return TRUE;
}

int
solve()
{
  int i;
  pos_t blank;

  // take first found blank
  blank = find_blank(p);

  // solved?
  if (blank.row == -1)
    return TRUE;

  // try to assign a number between 1 and 9
  for (i = 1; i <= 9; i++) {

    // was the number ok?
    if (check_number(p, blank, i)) {

      // if so, assign it, and try to solve the remaining puzzle
      set_cell_pos(p, blank, i);
      if (solve()) {
        return TRUE;
      } else {
        set_cell_pos(p, blank, -1);
      }
    }
  }

  return FALSE;
}


int
main(int argc, char *argv[])
{
  time_t t0 = clock();

  if (!solve()) {
    printf("no solution found!\n");
  }

  t0 = clock() - t0;
  printf("Time: %g secs\n", ((double)t0)/CLOCKS_PER_SEC);

  pretty_print(p);
}
