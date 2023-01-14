#ifndef SIMPLE_LEXER_LEXER_H
#define SIMPLE_LEXER_LEXER_H

#include "simple/Basic/Diagnostic.h"
#include "simple/Basic/LLVM.h"
#include "simple/Basic/Name.h"
#include "simple/Lexer/Token.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

namespace simple {

class NamesMap {
  bool IsInit;
  llvm::StringMap<Name> HashTable;

  Name *addName(StringRef Id, tok::TokenKind TokenCode);

public:
  NamesMap(): IsInit(false) { }

  void addKeywords();

  Name *getName(StringRef Id);
};

class Lexer {
  SourceMgr &SrcMgr;
  DiagnosticsEngine &Diags;

  StringRef BufferStart;
  const char *CurPos;

  /// CurBuffer - This is the current buffer index we're
  /// lexing from as managed by the SourceMgr object.
  unsigned CurBuffer = 0;

  static NamesMap IdsMap;

  llvm::SMLoc getLoc(const char *Pos) const {
    return llvm::SMLoc::getFromPointer(Pos);
  }

public:
  Lexer(SourceMgr &SrcMgr, DiagnosticsEngine &Diags)
    : SrcMgr(SrcMgr), Diags(Diags) {
    CurBuffer = SrcMgr.getMainFileID();
    BufferStart = SrcMgr.getMemoryBuffer(CurBuffer)->getBuffer();
    CurPos = BufferStart.begin();
    IdsMap.addKeywords();
  }

  DiagnosticsEngine &getDiagnostics() const {
    return Diags;
  }

  void next(Token &Result);
};
} // namespace simple

#endif