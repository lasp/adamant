echo '
#include "file3.h"

// This file should not built not linked.
int get_t(void) {
  return 6;
}
'
