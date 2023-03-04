#ifndef SIMPLE_LEXER_TOKEN_H
#define SIMPLE_LEXER_TOKEN_H

#include "simple/Basic/LLVM.h"
#include "simple/Basic/Name.h"
#include "simple/Basic/TokenKinds.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace simple {

class Lexer;

class Token {
  friend class Lexer;

  const char *Ptr;
  size_t Length;
  tok::TokenKind Kind;

  union {
    Name *Id;
    char *Literal;
    char Chr;
  };

public:
  ~Token() {
    if (isOneOf(tok::IntNumber, tok::FloatNumber, tok::StringConstant)) {
      delete[] Literal;
    }
  }

  tok::TokenKind getKind() const { return Kind; }

  /// is/isNot - Predicates to check if this token is a
  /// specific kind, as in "if (Tok.is(tok::EndOfFile))
  /// {...}".
  bool is(tok::TokenKind K) const { return Kind == K; }
  bool isNot(tok::TokenKind K) const { return Kind != K; }
  bool isOneOf(tok::TokenKind K1, tok::TokenKind K2) const {
    return is(K1) || is(K2);
  }
  template <typename... Ts>
  bool isOneOf(tok::TokenKind K1, tok::TokenKind K2, Ts... Ks) const {
    return is(K1) || isOneOf(K2, Ks...);
  }

  SMLoc getLocation() const {
    return SMLoc::getFromPointer(Ptr);
  }

  size_t getLength() const {
    return Length;
  }

  Name *getIdentifier() const {
    assert(is(tok::Identifier) && "Cannot get identifier of no-identifier");
    return Id;
  }

  StringRef getLiteral() const {
    assert(isOneOf(tok::IntNumber, tok::FloatNumber, tok::StringConstant) &&
      "Cannot get literal data of non-literal");
    return StringRef(Literal, Length);
  }

  char getChar() const {
    assert(is(tok::CharLiteral) && "Cannot get char literal of non-char literal");
    return Chr;
  }
};
} // namespace simple

#endif