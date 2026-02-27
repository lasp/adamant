echo '
#include "file3.h"

// This file should not be built or linked.
int get_t(void) {
  return 6;
}
'
