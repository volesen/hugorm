#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint64_t hugorm_val;
const uint64_t BOOL_TAG = 0x0000000000000003;
const uint64_t PAIR_TAG = 0x0000000000000001;
const hugorm_val BOOL_TRUE = 0xFFFFFFFFFFFFFFFF;
const hugorm_val BOOL_FALSE = 0x7FFFFFFFFFFFFFFF;

const uint64_t ERR_NOT_NUMBER = 1;
const uint64_t ERR_NOT_BOOLEAN = 2;

extern hugorm_val our_code_starts_here(uint64_t *) asm("our_code_starts_here");
void error(uint64_t errCode, hugorm_val val) asm("error");
hugorm_val print(hugorm_val val) asm("print");

void error(uint64_t errCode, hugorm_val val) {
  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "Expected number, but got %010llx\n", val);
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Expected boolean, but got %010llx\n", val);
  }
  exit(errCode);
}

void _print(hugorm_val val) {
  if ((val & PAIR_TAG) == 0) {
    printf("%lld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } else if (val == PAIR_TAG) {
    // Equivalent to null pointer, after removing tag
    printf("()");
  } else if ((val & PAIR_TAG) == 1) {
    // Remove tag from pair pointer
    hugorm_val *pair = (hugorm_val *)(val - PAIR_TAG);

    printf("(");
    _print(pair[0]);
    printf(", ");
    _print(pair[1]);
    printf(")");
  } else {
    printf("Unknown value: %#018llx", val); // print unknown val in hex
  }
}

hugorm_val print(hugorm_val val) {
  _print(val);
  printf("\n");
  return val;
}

int main(int argc, char **argv) {
  uint64_t *HEAP =
      calloc(1024, sizeof(uint64_t)); // Allocate 8KB of memory for now
  int64_t result = our_code_starts_here(HEAP);
  print(result);
  free(HEAP);
  return 0;
}
