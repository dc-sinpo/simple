#ifndef SIMPLE_BASIC_NAME_H
#define SIMPLE_BASIC_NAME_H

#include "simple/Basic/LLVM.h"

namespace simple {

struct Name {
  const char *Id; ///< Name's text
  int Kind;       ///< Name's kind (one of TokenKind)
  size_t Length;  ///< Length of the text
};

} // namespace simple

#endif