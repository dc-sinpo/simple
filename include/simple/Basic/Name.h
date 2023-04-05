#ifndef SIMPLE_BASIC_NAME_H
#define SIMPLE_BASIC_NAME_H

#include "simple/Basic/LLVM.h"

namespace simple {

struct Name {
  const char *Id; ///< Name's text
  int Kind;       ///< Name's kind (one of TokenKind)
  size_t Length;  ///< Length of the text

  static Name *Super;  ///< Name for super keyword
  static Name *This;   ///< Name for this keyword
  static Name *Ctor;   ///< Name for constructor
  static Name *Dtor;   ///< Name for destructor
  static Name *New;    ///< Name for new keyword
  static Name *Delete; ///< Name for delete keyword
};

} // namespace simple

#endif