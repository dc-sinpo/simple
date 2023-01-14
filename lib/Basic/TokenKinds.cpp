#include "simple/Basic/TokenKinds.h"

namespace simple {

const char *tok::getKeywordSpelling(TokenKind Kind) {
  switch (Kind) {
#define KEYWORD(ID, TEXT)                                                      \
    case ID:                                                                   \
      return TEXT;
#include "simple/Basic/TokenKinds.def"
    default:
      break;
  }

  return nullptr;
}

const char* tok::toString(TokenKind Kind) {
  static const char *TokenStrings[] = {
#define TOK(ID, TEXT) TEXT,
#include "simple/Basic/TokenKinds.def"
  };

  return TokenStrings[Kind];
}

} // namespace simple