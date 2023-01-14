#ifndef SIMPLE_LEXER_TOKENSTREAM_H
#define SIMPLE_LEXER_TOKENSTREAM_H

#include "simple/Basic/TokenKinds.h"
#include "simple/Lexer/Lexer.h"
#include "simple/Lexer/Token.h"
#include "llvm/Support/SMLoc.h"

namespace simple {

/// Stream of tokens
class TokenStream {
  /// Stream's node (for double-linked list)
  struct StreamNode {
    /// Constructor
    /// \param[in] next - next node
    /// \param[in] prev - previous node
    StreamNode(StreamNode *next, StreamNode *prev)
      : Next(next),
        Prev(prev) {
    }

    Token Tok;        ///< Token data
    StreamNode *Next; ///< Next node
    StreamNode *Prev; ///< Previous node
  };

  /// Token's stream iterator
  class TokenStreamIterator {
  public:
    /// Copy constructor
    /// \param[in] other - iterator to copy
    TokenStreamIterator(const TokenStreamIterator &other)
      : CurStream(other.CurStream),
        CurPos(other.CurPos) {
    }

    /// Assignment operator
    /// \param[in] other - iterator to copy
    /// \return *this
    TokenStreamIterator &operator=(const TokenStreamIterator &other) {
      if (&other != this) {
        CurStream = other.CurStream;
        CurPos = other.CurPos;
      }

      return *this;
    }

    /// Check for empty iterator
    bool empty() const {
      return (CurStream == nullptr && CurPos == nullptr);
    }

    /// Get location in the source file
    llvm::SMLoc getLocation() const {
      assert(CurPos);
      return CurPos->Tok.getLocation();
    }

    /// Check for token kind equality
    bool operator ==(tok::TokenKind tok) const {
      return CurPos->Tok.getKind() == tok;
    }
    
    /// Check for token kind inequality
    bool operator !=(tok::TokenKind tok) const {
      return CurPos->Tok.getKind() != tok;
    }

    /// Dereference
    const Token &operator *() const {
      return CurPos->Tok;
    }

    /// Access through pointer member access
    const Token *operator ->() const {
      return &CurPos->Tok;
    }

    /// Post increment
    TokenStreamIterator operator ++(int) {
      TokenStreamIterator tmp = *this;
      ++(*this);
      return tmp;
    }

    /// Prefix increment
    TokenStreamIterator &operator ++() {
      CurPos = CurStream->next(CurPos);
      return *this;
    }

    /// Post decrement
    TokenStreamIterator operator --(int) {
      TokenStreamIterator tmp = *this;
      --(*this);
      return tmp;
    }

    /// Prefix decrement
    TokenStreamIterator &operator --() {
      if (CurPos->Prev != 0) {
        CurPos = CurPos->Prev;
      }
      return *this;
    }

    /// Advance to \c count tokens
    /// \param[in] count - number of token to advance
    TokenStreamIterator operator +(int count) {
      TokenStreamIterator tmp = *this;
      while (count--) {
        ++tmp;
      }
      return tmp;
    }

    /// Backward to \c count tokens
    /// \param[in] count - number of token to backward
    TokenStreamIterator operator -(int count) {
      TokenStreamIterator tmp = *this;
      while (count--) {
        --tmp;
      }
      return tmp;
    }

    friend class TokenStream;

  private:
    TokenStream *CurStream; ///< Source stream associated with this iterator
    StreamNode *CurPos;     ///< Current position in the token stream

    /// Constructor
    /// \param[in] source - source stream
    /// \param[in] curPos - current position
    TokenStreamIterator(TokenStream *source = nullptr, StreamNode *curPos = nullptr)
      : CurStream(source), 
        CurPos(curPos) {
    }
  };

  Lexer *Lex;       ///< Lexical analyzer with the source file
  StreamNode *Head; ///< Head element
  StreamNode *Tail; ///< Tail element
  bool ScanDone;    ///< true - if end of file reached

  /// Read next token
  /// \param[in] curPos - current position
  /// \return Next token
  StreamNode *next(StreamNode *curPos);

public:
  /// Type for iterator
  typedef TokenStreamIterator iterator;

  /// Constructor
  /// \param[in] lexer - lexical analyzer with the source file
  TokenStream(Lexer *lexer);

  /// Destructor
  ~TokenStream();

  DiagnosticsEngine &getDiagnostics() const {
    return Lex->getDiagnostics();
  }

  /// Get position of the 1st element
  TokenStreamIterator begin();
};

} // namespace simple

#endif