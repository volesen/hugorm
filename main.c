#include <stdint.h>
#include <stdio.h>

typedef uint64_t hugorm_val;
const uint64_t BOOL_TAG = 0x0000000000000001;
const hugorm_val BOOL_TRUE = 0xFFFFFFFFFFFFFFFFL;
const hugorm_val BOOL_FALSE = 0x7FFFFFFFFFFFFFFFL;

hugorm_val print(hugorm_val val) {
  if ((val & BOOL_TAG) == 0) {            // val is even ==> number
    printf("%lld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } else {
    printf("Unknown value: %#018llx", val); // print unknown val in hex
  }
  return val;
}

extern int64_t our_code_starts_here() asm("our_code_starts_here");

int main(int argc, char **argv) {
  int64_t result = our_code_starts_here();

  print(result);
  putchar('\n');
  return 0;
}
