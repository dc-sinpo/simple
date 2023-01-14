#include "simple/Parser/Parser.h"

namespace simple {

using llvm::isa;

Parser::Parser(Lexer *lex)
  : Src(lex),
    CurPos(Src.begin()) {
}

void Parser::check(tok::TokenKind tok) {
  if (CurPos != tok) {
    getDiagnostics().report(CurPos.getLocation(),
                            diag::ERR_Expected,
                            tok::toString(tok),
                            tok::toString(CurPos->getKind()));
  }

  ++CurPos;
}

/// primary-expr
///   ::= floating-point-literal
///   ::= integral-literal
///   ::= identifier
///   ::= '(' expr ')'
ExprAST *Parser::parsePrimaryExpr() {
  ExprAST *result = nullptr;

  switch (CurPos->getKind()) {
    case tok::FloatNumber:
      result = new FloatExprAST(CurPos.getLocation(), strtod(CurPos->getLiteral().data(), nullptr));
      ++CurPos;
      return result;

    case tok::IntNumber:
      result = new IntExprAST(CurPos.getLocation(), atoi(CurPos->getLiteral().data()));
      ++CurPos;
      return result;

    case tok::Identifier:
      result = new IdExprAST(CurPos.getLocation(), CurPos->getIdentifier());
      ++CurPos;
      return result;

    case tok::OpenParen:
      ++CurPos;
      result = parseExpr();
      check(tok::CloseParen);
      return result;

    default:
      getDiagnostics().report(CurPos.getLocation(), diag::ERR_ExpectedExpression);
      return nullptr;
  }
}

/// call-arguments
///   ::= assign-expr
///   ::= call-arguments ',' assign-expr
/// postfix-expr
///   ::= primary-expr
///   ::= postfix-expr '++'
///   ::= postfix-expr '--'
///   ::= postfix-expr '(' call-arguments ? ')'
ExprAST *Parser::parsePostfixExpr() {
  ExprAST *result = parsePrimaryExpr();

  for (;;) {
    llvm::SMLoc loc = CurPos.getLocation();

    switch (int op = CurPos->getKind()) {
      case tok::PlusPlus:
      case tok::MinusMinus:
        result = new BinaryExprAST(loc, op, result, nullptr);
        ++CurPos;
        continue;

      case tok::OpenParen: {
        ++CurPos;

        ExprList args;

        // Check for function call with 0 arguments
        if (CurPos != tok::CloseParen) {
          for (;;) {
            ExprAST *arg = parseAssignExpr();
            args.push_back(arg);

            // If we have , then we should parse at least 1 more argument
            if (CurPos != tok::Comma) {
              break;
            }

            ++CurPos;
          }
        }

        check(tok::CloseParen);
        result = new CallExprAST(loc, result, args);
        continue;
      }

      default:
        return result;
    }
  }
}

/// unary-expr
///   ::= postfix-expr
///   ::= '+' unary-expr
///   ::= '-' unary-expr
///   ::= '++' unary-expr
///   ::= '--' unary-expr
///   ::= '~' unary-expr
///   ::= '!' unary-expr
ExprAST *Parser::parseUnaryExpr() {
  ExprAST *result = nullptr;
  llvm::SMLoc loc = CurPos.getLocation();

  switch (int op = CurPos->getKind()) {
    case tok::Plus:
    case tok::Minus:
    case tok::PlusPlus:
    case tok::MinusMinus:
    case tok::Not:
    case tok::Tilda:
      ++CurPos;
      result = parseUnaryExpr();
      return new UnaryExprAST(loc, op, result);

    default:
      return parsePostfixExpr();
  }
}

enum OpPrecedenceLevel {
  OPL_Unknown = 0,        ///< Not binary operator.
  OPL_Comma = 1,          ///< ,
  OPL_Assignment = 2,     ///< =
  OPL_Conditional = 3,    ///< ?
  OPL_LogicalOr = 4,      ///< ||
  OPL_LogicalAnd = 5,     ///< &&
  OPL_InclusiveOr = 6,    ///< |
  OPL_ExclusiveOr = 7,    ///< ^
  OPL_And = 8,            ///< &
  OPL_Equality = 9,       ///< ==, !=
  OPL_Relational = 10,    ///<  >=, <=, >, <
  OPL_Shift = 11,         ///< <<, >>
  OPL_Additive = 12,      ///< -, +
  OPL_Multiplicative = 13 ///< *, /, %
};

OpPrecedenceLevel getBinOpPrecedence(tok::TokenKind op) {
  switch (op) {
    case tok::Greater:
    case tok::Less:
    case tok::GreaterEqual:
    case tok::LessEqual:
      return OPL_Relational;

    case tok::LShift:
    case tok::RShift:
      return OPL_Shift;

    case tok::Comma:
      return OPL_Comma;

    case tok::Assign:
      return OPL_Assignment;

    case tok::Question:
      return OPL_Conditional;

    case tok::LogOr:
      return OPL_LogicalOr;

    case tok::LogAnd:
      return OPL_LogicalAnd;

    case tok::BitOr:
      return OPL_InclusiveOr;

    case tok::BitXor:
      return OPL_ExclusiveOr;

    case tok::BitAnd:
      return OPL_And;

    case tok::Equal:
    case tok::NotEqual:
      return OPL_Equality;

    case tok::Plus:
    case tok::Minus:
      return OPL_Additive;

    case tok::Mul:
    case tok::Div:
    case tok::Mod:
      return OPL_Multiplicative;

    default:
      return OPL_Unknown;
  }
}

// expr
//   ::= assign-expr
//   ::= expr ',' assign-expr
// assign-expr
//   ::= cond-expr
//   ::= cond-expr '=' assign-expr
// cond-expr
//   ::= assoc-expr
//   ::= assoc-expr '?' expr ':' cond-expr
// op
//   ::= '||' | '&&' | '|' | '^' | '&' | '==' | '!=' | '>=' | '<=' | '>' | '<' | '<<' | '>>' | '-' | '+' | '*' | '/' | '%'
// assoc-expr
//   ::= unary-expr
//   ::= assoc-expr op unary-expr
ExprAST *Parser::parseRHS(ExprAST *lhs, int maxPrec) {
  OpPrecedenceLevel newPrec = getBinOpPrecedence(CurPos->getKind());

  for (;;) {
    tok::TokenKind tok = CurPos->getKind();
    llvm::SMLoc loc = CurPos.getLocation();

    if (newPrec < maxPrec) {
      return lhs;
    }

    ++CurPos;

    // Check for ? :
    if (newPrec == OPL_Conditional) {
      ExprAST *thenPart = nullptr;

      if (CurPos == tok::Colon) {
        getDiagnostics().report(CurPos.getLocation(),
                                diag::ERR_ExpectedExpressionAfterQuestion);
      } else {
        thenPart = parseExpr();
      }

      check(tok::Colon);

      ExprAST *elsePart = parseAssignExpr();
      lhs = new CondExprAST(loc, lhs, thenPart, elsePart);

      newPrec = getBinOpPrecedence(CurPos->getKind());
      continue;
    }

    ExprAST *rhs = parseUnaryExpr();
    OpPrecedenceLevel thisPrec = newPrec;
    newPrec = getBinOpPrecedence(CurPos->getKind());
    bool isRightAssoc = (thisPrec == OPL_Assignment);

    if (thisPrec < newPrec || (thisPrec == newPrec && isRightAssoc)) {
      if (isRightAssoc) {
        rhs = parseRHS(rhs, thisPrec);
      } else {
        rhs = parseRHS(rhs, (OpPrecedenceLevel)(thisPrec + 1));
      }

      newPrec = getBinOpPrecedence(CurPos->getKind());
    }

    lhs = new BinaryExprAST(loc, tok, lhs, rhs);
  }
}

ExprAST *Parser::parseAssignExpr() {
  ExprAST *lhs = parseUnaryExpr();
  return parseRHS(lhs, OPL_Assignment);
}

ExprAST *Parser::parseExpr() {
  ExprAST *lhs = parseUnaryExpr();
  return parseRHS(lhs, OPL_Comma);
}

/// type ::= 'int' | 'float' | 'void'
TypeAST *Parser::parseType() {
  TypeAST *type = nullptr;
  bool isVoid = false;
  llvm::SMLoc loc = CurPos.getLocation();

  switch (CurPos->getKind()) {
    case tok::Int:
      ++CurPos;
      type = BuiltinTypeAST::get(TypeAST::TI_Int);
      break;

    case tok::Float:
      ++CurPos;
      type = BuiltinTypeAST::get(TypeAST::TI_Float);
      break;

    case tok::Void:
      ++CurPos;
      type = BuiltinTypeAST::get(TypeAST::TI_Void);
      isVoid = true;
      break;

    default:
      getDiagnostics().report(CurPos.getLocation(), diag::ERR_InvalidType);
      return nullptr;
  }

  if (type == BuiltinTypeAST::get(TypeAST::TI_Void)) {
    getDiagnostics().report(loc, diag::ERR_VoidAsNonPointer);
    return nullptr;
  }

  return type;
}

/// parameter ::= identifier ':' type
/// parameters-list
///   ::= parameter
///   ::= parameter-list ',' parameter
/// return-type ::= ':' type
/// func-proto ::= identifier '(' parameters-list ? ')' return-type ?
SymbolAST *Parser::parseFuncProto() {
  Name *name = nullptr;
  TypeAST *returnType = BuiltinTypeAST::get(TypeAST::TI_Void);
  ParameterList params;
  int Tok = CurPos->getKind();
  llvm::SMLoc loc = CurPos.getLocation();

  ++CurPos;

  // Function should start with identifier
  if (CurPos == tok::Identifier) {
    name = CurPos->getIdentifier();
    ++CurPos;
  } else {
    check(tok::Identifier);
  }

  check(tok::OpenParen);

  // We should parse all parameters if it's not ( )
  if (CurPos != tok::CloseParen) {
    for (;;) {
      Name *paramName = nullptr;

      if (CurPos == tok::Identifier) {
        paramName = CurPos->getIdentifier();
        ++CurPos;

        // check for anonymouse parameter
        if (strcmp(paramName->Id, "_") == 0) {
          paramName = nullptr;
        }
      } else {
        check(tok::Identifier);
      }

      check(tok::Colon);

      TypeAST *type = parseType();

      params.push_back(new ParameterAST(type, paramName));

      if (CurPos != tok::Comma) {
        break;
      }

      ++CurPos;
    }
  }

  check(tok::CloseParen);

  // We can have : type to set return type
  // Note: : void not allowed
  if (CurPos == tok::Colon) {
    ++CurPos;
    returnType = parseType();
  }

  returnType = new FuncTypeAST(returnType, params);
  return new FuncDeclAST(loc, returnType, name, nullptr, Tok);
}

/// func-decl ::= 'fn' func-proto block-stmt
SymbolList Parser::parseFuncDecl() {
  SymbolList result;
  FuncDeclAST *decl = (FuncDeclAST *)parseFuncProto();

  if (CurPos != tok::BlockStart) {
    getDiagnostics().report(CurPos.getLocation(), diag::ERR_ExpectedFuncBody);
  } else {
    StmtAST *body = parseStmt();
    // We should fix some values for declaration
    decl->Body = body;
    result.push_back(decl);
  }

  return result;
}

/// decls
///  ::= func-decl
///  ::= decls func-decl
SymbolList Parser::parseDecls() {
  SymbolList result;

  for (;;) {
    SymbolList tmp;
    llvm::SMLoc loc = CurPos.getLocation();

    switch (int Tok = CurPos->getKind()) {
      case tok::Def:
        tmp = parseFuncDecl();
        result.push_back(tmp.pop_back_val());
        continue;

      case tok::EndOfFile:
        break;

      default:
        break;
    }

    break;
  }

  return result;
}

/// module-decl ::= decls
ModuleDeclAST *Parser::parseModule() {
  SymbolList decls = parseDecls();

  if (CurPos != tok::EndOfFile) {
    getDiagnostics().report(CurPos.getLocation(), diag::ERR_ExpectedEndOfFile);
    return nullptr;
  }

  return new ModuleDeclAST(getDiagnostics(), decls);
}

/// var-init ::= '=' assign-expr
/// var-decl ::= identifier ':' type var-init?
/// var-decls
///   ::= var-decl
///   ::= var-decls ',' var-decl
/// decl-stmt ::= var-decls ';'
SymbolList Parser::parseDecl(bool needSemicolon) {
  SymbolList result;

  for (;;) {
    llvm::SMLoc loc = CurPos.getLocation();

    if (CurPos != tok::Identifier) {
      getDiagnostics().report(CurPos.getLocation(), diag::ERR_ExpectedIdentifierInDecl);
    } else {
      Name *name = CurPos->getIdentifier();
      ExprAST *value = nullptr;

      ++CurPos;

      check(tok::Colon);

      TypeAST *type = parseType();

      if (CurPos == tok::Assign) {
        ++CurPos;
        value = parseAssignExpr();
      }

      result.push_back(new VarDeclAST(loc, type, name, value));

      if (CurPos != tok::Comma) {
        break;
      }

      ++CurPos;
    }
  }

  if (needSemicolon) {
    check(tok::Semicolon);
  }

  return result;
}

StmtAST *Parser::parseStmtAsBlock() {
  llvm::SMLoc loc = CurPos.getLocation();
  StmtAST *result = parseStmt();

  if (isa<BlockStmtAST>(result)) {
    return result;
  }

  StmtList body;
  body.push_back(result);
  return new BlockStmtAST(loc, body);
}

/// block-stmt ::= '{' stmt* '}'
/// for-init
///   ::= 'let' decl-stmt
///   ::= expr
/// for-stmt ::= 'for' for-init? ';' expr? ';' expr? block-stmt
/// stmt
///   ::= expr? ';'
///   ::= 'let' decl-stmt
///   ::= 'if' expr block-stmt ( 'else' block-stmt )?
///   ::= 'while' expr block-stmt
///   ::= for-stmt
///   ::= 'break'
///   ::= 'continue'
///   ::= 'return' expr? ';'
///   ::= block-stmt
StmtAST *Parser::parseStmt() {
  StmtAST *result = nullptr;
  llvm::SMLoc loc = CurPos.getLocation();

  switch (CurPos->getKind()) {
    case tok::Var:
      ++CurPos;
      return new DeclStmtAST(loc, parseDecl(true));

    case tok::Plus:
    case tok::Minus:
    case tok::PlusPlus:
    case tok::MinusMinus:
    case tok::Tilda:
    case tok::Not:
    case tok::Identifier:
    case tok::IntNumber:
    case tok::FloatNumber:
    case tok::OpenParen: {
      ExprAST *expr = parseExpr();
      check(tok::Semicolon);
      return new ExprStmtAST(loc, expr);
    }

    case tok::If: {
      check(tok::If);
      ExprAST *expr = parseExpr();

      if (CurPos != tok::BlockStart) {
        check(tok::BlockStart);
      }

      StmtAST *thenPart = parseStmtAsBlock();
      StmtAST *elsePart = nullptr;

      if (CurPos == tok::Else) {
        ++CurPos;

        if (CurPos != tok::BlockStart) {
          check(tok::BlockStart);
        }

        elsePart = parseStmtAsBlock();
      }

      return new IfStmtAST(loc, expr, thenPart, elsePart);
    }

    case tok::While: {
      check(tok::While);
      ExprAST *expr = parseExpr();

      if (CurPos != tok::BlockStart) {
        check(tok::BlockStart);
      }

      result = parseStmtAsBlock();
      return new WhileStmtAST(loc, expr, result);
    }

    case tok::For: {
      check(tok::For);

      ExprAST *initExpr = nullptr, *condExpr = nullptr, *postExpr = nullptr;
      SymbolList decls;

      if (CurPos != tok::Semicolon) {
        if (CurPos == tok::Var) {
          ++CurPos;
          decls = parseDecl(true);
        } else {
          initExpr = parseExpr();
          check(tok::Semicolon);
        }
      } else {
        check(tok::Semicolon);
      }

      if (CurPos != tok::Semicolon) {
        condExpr = parseExpr();
      }

      check(tok::Semicolon);

      if (CurPos != tok::BlockStart) {
        postExpr = parseExpr();
      }

      if (CurPos != tok::BlockStart) {
        check(tok::CloseParen);
      }

      result = parseStmtAsBlock();
      return new ForStmtAST(loc, initExpr, decls, condExpr, postExpr, result);
    }

    case tok::Break:
      check(tok::Break);
      check(tok::Semicolon);
      return new BreakStmtAST(loc);

    case tok::Continue:
      check(tok::Continue);
      check(tok::Semicolon);
      return new ContinueStmtAST(loc);

    case tok::Return: {
      check(tok::Return);

      if (CurPos == tok::Semicolon) {
        ++CurPos;
        return new ReturnStmtAST(loc, nullptr);
      }

      ExprAST *expr = parseExpr();
      check(tok::Semicolon);
      return new ReturnStmtAST(loc, expr);
    }

    case tok::BlockStart: {
      StmtList stmts;

      ++CurPos;

      while (CurPos != tok::BlockEnd && CurPos != tok::EndOfFile) {
        result = parseStmt();
        stmts.push_back(result);
      }

      check(tok::BlockEnd);
      return new BlockStmtAST(loc, stmts);
    }

    case tok::Semicolon:
      ++CurPos;
      return new ExprStmtAST(loc, nullptr);

    default:
      getDiagnostics().report(CurPos.getLocation(), diag::ERR_InvalidStatement);
      return nullptr;
  }
}

SymbolAST *parseFuncProto(llvm::StringRef Proto) {
  llvm::SourceMgr SrcMgr;
  DiagnosticsEngine Diags(SrcMgr);
  std::unique_ptr<llvm::MemoryBuffer> Buff = llvm::MemoryBuffer::getMemBuffer(Proto);

  // Tell SrcMgr about this buffer, which is what the
  // parser will pick up.
  SrcMgr.AddNewSourceBuffer(std::move(Buff), llvm::SMLoc());

  Lexer Lex(SrcMgr, Diags);
  Parser P(&Lex);

  return P.parseFuncProto();
}

} // namespace simple