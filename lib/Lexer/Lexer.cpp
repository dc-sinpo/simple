#include "simple/Lexer/Lexer.h"

namespace simple {

Name *NamesMap::addName(StringRef Id, tok::TokenKind TokenCode) {
  auto newKey = HashTable.insert(std::make_pair(Id, Name()));

  if (!newKey.second) {
    return &newKey.first->getValue();
  }

  Name *name = &newKey.first->getValue();

  name->Id = newKey.first->getKeyData();
  name->Length = Id.size();
  name->Kind = TokenCode;

  return name;
}

void NamesMap::addKeywords() {
  if (IsInit) {
    return;
  }

#define KEYWORD(NAME, TEXT)                               \
  addName(StringRef(TEXT), tok::NAME);
#include "simple/Basic/TokenKinds.def"

  IsInit = true;
}

Name *NamesMap::getName(StringRef Id) {
  return addName(Id, tok::Identifier);
}

namespace charinfo {
LLVM_READNONE inline bool isASCII(char Ch) {
  return static_cast<unsigned char>(Ch) <= 127;
}

LLVM_READNONE inline bool isVerticalWhitespace(char Ch) {
  return isASCII(Ch) && (Ch == '\r' || Ch == '\n');
}

LLVM_READNONE inline bool isHorizontalWhitespace(char Ch) {
  return isASCII(Ch) && (Ch == ' ' || Ch == '\t' || Ch == '\f' || Ch == '\v');
}

LLVM_READNONE inline bool isWhitespace(char Ch) {
  return isHorizontalWhitespace(Ch) || isVerticalWhitespace(Ch);
}

LLVM_READNONE inline bool isDigit(char Ch) {
  return isASCII(Ch) && Ch >= '0' && Ch <= '9';
}

LLVM_READNONE inline bool isIdentifierHead(char Ch) {
  return isASCII(Ch) &&
         (Ch == '_' || (Ch >= 'A' && Ch <= 'Z') || (Ch >= 'a' && Ch <= 'z'));
}

LLVM_READNONE inline bool isIdentifierBody(char Ch) {
  return isIdentifierHead(Ch) || isDigit(Ch);
}
}

NamesMap Lexer::IdsMap;

void Lexer::next(Token &Result) {
  // Set token to invalid state
  Result.Kind = tok::Invalid;

  // We should change original read position only when token is fully read
  const char *p = CurPos;

  // Read all tokens in the loop until we find a valid token
  while (Result.Kind == tok::Invalid) {
    const char *tokenStart = p;

#define CHECK_ONE(CHR, TOK) \
      case CHR: \
        Result.Length = 1; \
        Result.Kind = TOK; \
        break

#define CHECK_TWO(CH1, CH2, T1, T2) \
      case CH1: \
        if (*p == CH2) { \
          ++p; \
          Result.Length = 2; \
          Result.Kind = T1; \
        } else { \
          Result.Length = 1; \
          Result.Kind = T2; \
        } \
        break

    switch (char ch = *p++) {
      case 0:
        // It's end of file. Backtrack to '\0' (we can reread it again)
        --p;
        Result.Kind = tok::EndOfFile;
        break;

      case '\n':
      case '\r':
      case ' ':
      case '\t':
      case '\v':
      case '\f':
        // Skip all whitespaces
        while (charinfo::isWhitespace(*p)) {
          ++p;
        }
        // Read next token
        continue;

      
      CHECK_ONE('~', tok::Tilda);
      CHECK_ONE('*', tok::Mul);
      CHECK_ONE('%', tok::Mod);
      CHECK_ONE('^', tok::BitXor);
      CHECK_ONE(',', tok::Comma);
      CHECK_ONE('?', tok::Question);
      CHECK_ONE(':', tok::Colon);
      CHECK_ONE(';', tok::Semicolon);
      CHECK_ONE('(', tok::OpenParen);
      CHECK_ONE(')', tok::CloseParen);
      CHECK_ONE('{', tok::BlockStart);
      CHECK_ONE('}', tok::BlockEnd);

      CHECK_TWO('-', '-', tok::MinusMinus, tok::Minus);
      CHECK_TWO('+', '+', tok::PlusPlus, tok::Plus);
      CHECK_TWO('!', '=', tok::NotEqual, tok::Not);
      CHECK_TWO('=', '=', tok::Equal, tok::Assign);
      CHECK_TWO('|', '|', tok::LogOr, tok::BitOr);
      CHECK_TWO('&', '&', tok::LogAnd, tok::BitAnd);

      case '/':
        if (*p == '/') {
          ++p;

          while (*p && (*p != '\r' && *p != '\n')) {
            ++p;
          }
          break;
        } else if (*p == '*') {
          unsigned Level = 1;
          ++p;

          while (*p && Level) {
            // Check for nested comment
            if (*p == '/' && p[1] == '*') {
              p += 2;
              ++Level;
            // Check for end of comment
            } else if (*p == '*' && p[1] == '/' && Level) {
              p += 2;
              --Level;
            } else {
              ++p;
            }
          }

          if (Level) {
            Diags.report(getLoc(p), diag::ERR_UnterminatedBlockComment);
          }

          continue;
        } else {
          Result.Length = 1;
          Result.Kind = tok::Div;
          break;
        }

      case '<':
        if (*p == '=') {
          ++p;
          Result.Length = 2;
          Result.Kind = tok::LessEqual;
          break;
        } else if (*p == '<') {
          ++p;
          Result.Length = 2;
          Result.Kind = tok::LShift;
          break;
        } else {
          Result.Length = 1;
          Result.Kind = tok::Less;
          break;
        }

      case '>':
        if (*p == '=') {
          ++p;
          Result.Length = 2;
          Result.Kind = tok::GreaterEqual;
          break;
        } else if (*p == '>') {
          ++p;
          Result.Length = 2;
          Result.Kind = tok::RShift;
          break;
        } else {
          Result.Length = 1;
          Result.Kind = tok::Greater;
          break;
        }

      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        while (charinfo::isDigit(*p)) {
          ++p;
        }

      case '0':
        Result.Kind = tok::IntNumber;

        if (*p == '.') {
          // Keep 1st digit after .
          const char *firstDigit = p++;

          while (charinfo::isDigit(*p)) {
            ++p;
          }

          // Check for exponent
          if (*p == 'e' || *p == 'E') {
            ++p;

            if (*p == '+' || *p == '-') {
              ++p;
            }

            // Keep 1st digit after [eE][+-]?
            firstDigit = p;

            while (charinfo::isDigit(*p)) {
              ++p;
            }

            // We should have at least 1 digit in the exponent part
            if (p == firstDigit) {
              Diags.report(getLoc(p), diag::ERR_FloatingPointNoDigitsInExponent);
            }
          }

          Result.Kind = tok::FloatNumber;
        }

        // Copy literal content in the token
        Result.Length = (int)(p - tokenStart);
        Result.Literal = new char[Result.Length + 1];
        memcpy(Result.Literal, tokenStart, Result.Length);
        Result.Literal[Result.Length] = 0;
        break;

      default:
        // Check for identifier
        if (charinfo::isIdentifierHead(ch)) {
          // Read full identifier
          while (charinfo::isIdentifierBody(*p)) {
            ++p;
          }

          size_t length = (size_t)(p - tokenStart);
          Name *name = IdsMap.getName(StringRef(tokenStart, length));

          Result.Id = name;
          Result.Kind = (tok::TokenKind)name->Kind;
          Result.Length = name->Length;

          break;
        } else {
          Diags.report(getLoc(p), diag::ERR_InvalidCharacter);
          break;
        }
    }

    Result.Ptr = tokenStart;
  }

  // Update current read position
  CurPos = p;
}

} // namespace simple