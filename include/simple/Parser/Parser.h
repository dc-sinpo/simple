#ifndef SIMPLE_PARSER_PARSER_H
#define SIMPLE_PARSER_PARSER_H

#include "simple/AST/AST.h"
#include "simple/Basic/TokenKinds.h"
#include "simple/Lexer/Lexer.h"
#include "simple/Lexer/TokenStream.h"

namespace simple {

/// Parser
class Parser {
  /// Source stream iterator
  typedef TokenStream::iterator iterator;

  TokenStream Src; ///< Source stream
  iterator CurPos; ///< Current position

  DiagnosticsEngine& getDiagnostics() const {
    return Src.getDiagnostics();
  }

public:
  /// Constructor
  /// \param[in] buffer - content of a source file
  Parser(Lexer *lex);

  /// Parse primary expression
  ExprAST *parsePrimaryExpr();

  /// Parse postfix expression
  ExprAST *parsePostfixExpr();

  /// Parse unary expression
  ExprAST *parseUnaryExpr();

  /// Parse right operand of an expression
  /// \param[in] lhs - left operand
  /// \param[in] maxPrec - max allowed operator precedence
  ExprAST *parseRHS(ExprAST *lhs, int maxPrec);

  /// Parse assignment expression
  ExprAST *parseAssignExpr();

  /// Parse comma delimited list of expressions
  ExprAST *parseExpr();

  /// Parse type
  TypeAST *parseType();

  /// Parse module
  ModuleDeclAST *parseModule();

  /// Parse function declaration
  SymbolList parseFuncDecl();

  /// Parse function's prototype
  SymbolAST *parseFuncProto();

  /// Parse declarations
  SymbolList parseDecls();

  /// Parse variable declaration
  /// \param[in] needSemicolon - true if ; should be at the end of the
  ///   declaration
  /// \param[in] isClassMember - true if it's class/struct member
  SymbolList parseDecl(bool needSemicolon, bool isClassMember = false);

  /// Parse statement and add it to block statement if needed
  StmtAST *parseStmtAsBlock();

  /// Parse statement
  StmtAST *parseStmt();

  /// Check iterator for needed token
  /// \param[in] tok - needed token
  void check(tok::TokenKind tok);
};

SymbolAST *parseFuncProto(llvm::StringRef Proto);

} // namespace simple

#endif