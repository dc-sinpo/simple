#ifndef SIMPLE_BASIC_TOKENKINDS_H
#define SIMPLE_BASIC_TOKENKINDS_H

#include "llvm/Support/Compiler.h"

namespace simple {

namespace tok {
enum TokenKind : unsigned short {
#define TOK(ID, TEXT) ID,
#include "simple/Basic/TokenKinds.def"
};

const char *getKeywordSpelling(TokenKind Kind) LLVM_READONLY;

const char *toString(TokenKind tok);
} // namespace tok

} // namespace simple

#endif