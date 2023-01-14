#include "simple/Lexer/TokenStream.h"

namespace simple {

TokenStream::TokenStream(Lexer *lex)
  : Lex(lex),
    Head(nullptr),
    Tail(nullptr),
    ScanDone(false) {
}

TokenStream::~TokenStream() {
  StreamNode *p = Head;

  while (p) {
    StreamNode *tmp = p;
    p = p->Next;
    delete tmp;
  }
}

TokenStream::TokenStreamIterator TokenStream::begin() {
  // Check for 1st time usage
  if (Head == 0) {
    // Create Head, Tail and read 1st token
    StreamNode *p = new StreamNode(nullptr, nullptr);

    Lex->next(p->Tok);
    ScanDone = p->Tok.getKind() == tok::EndOfFile;
    Tail = Head = p;
  }

  return TokenStreamIterator(this, Head);
}

TokenStream::StreamNode *TokenStream::next(StreamNode *curPos) {
  // We should check for 1st scan first
  if (curPos->Next == 0) {
    // If end of file reached then we should return curPos again
    if (ScanDone) {
      return curPos;
    }

    // Read next token and adjust Tail
    StreamNode *p = new StreamNode(nullptr, Tail);

    Lex->next(p->Tok);
    ScanDone = p->Tok.getKind() == tok::EndOfFile;
    Tail->Next = p;
    Tail = p;
    
    return Tail;
  } else {
    // Return previously scanned token
    return curPos->Next;
  }
}

} // namespace simple