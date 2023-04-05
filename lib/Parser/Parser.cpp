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
///   ::= '.'? identifier
///   ::= char-literal
///   ::= string-literal
///   ::= '(' expr ')'
///   ::= 'this'
///   ::= 'super'
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

    case tok::CharLiteral:
      result = new IntExprAST(CurPos.getLocation(), CurPos->getChar());
      ++CurPos;
      return result;

    case tok::StringConstant:
      result = new StringExprAST(CurPos.getLocation(), CurPos->getLiteral());
      ++CurPos;
      return result;

    case tok::Identifier:
      result = new IdExprAST(CurPos.getLocation(), CurPos->getIdentifier());
      ++CurPos;
      return result;

    case tok::Dot:
      if ((CurPos + 1) != tok::Identifier) {
        check(tok::Identifier);
      }
      // identifier will be parsed in parsePostfixExpr
      return new IdExprAST(CurPos.getLocation(), nullptr);

    case tok::Super:
      result = new IdExprAST(CurPos.getLocation(), Name::Super);
      ++CurPos;
      return result;

    case tok::This:
      result = new IdExprAST(CurPos.getLocation(), Name::This);
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
///   ::= postfix-expr '[' expr ']'
///   ::= postfix-expr '.' identifier
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

      case tok::OpenBrace: {
        check(tok::OpenBrace);
        ExprAST *expr = parseExpr();
        check(tok::CloseBrace);
        result = new IndexExprAST(loc, result, expr);
        continue;
      }

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

      case tok::Dot: {
        while (CurPos == tok::Dot) {
          ++CurPos;

          Name *name = nullptr;

          if (CurPos == tok::Identifier) {
            name = CurPos->getIdentifier();
          }

          check(tok::Identifier);
          result = new MemberAccessExprAST(loc, result, name);
        }

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
///   ::= '*' unary-expr
///   ::= '&' unary-expr
///   ::= 'del' unary-expr
///   ::= new-expr
/// new-expr
///   ::= 'new' type [' assign-expr ']'
///   ::= 'new' type ('(' call-arguments? ')') ?
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

    case tok::Mul:
      ++CurPos;
      result = parseUnaryExpr();
      return new DerefExprAST(loc, result);

    case tok::BitAnd:
      ++CurPos;
      result = parseUnaryExpr();
      return new AddressOfExprAST(loc, result);

    case tok::Delete: {
      ++CurPos;
      result = parsePostfixExpr();
      return new DeleteExprAST(loc, result);
    }

    case tok::New: {
      ++CurPos;
      TypeAST *type = parseType();
      ExprList args;
      ExprAST *dynamicSize = nullptr;

      // Check for [ expression ]
      // Note: [ integral-constant ] could be parsed in parseType
      if (CurPos == tok::OpenBrace) {
        if (isa<QualifiedTypeAST>(type)) {
          getDiagnostics().report(CurPos.getLocation(),
                                  diag::ERR_DynArrayAggregate);
        }

        ++CurPos;
        dynamicSize = parseAssignExpr();
        check(tok::CloseBrace);
      }

      if (isa<QualifiedTypeAST>(type) && CurPos == tok::OpenParen) {
        ++CurPos;

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
      }

      return new NewExprAST(loc, type, dynamicSize, args);
    }

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

/// basic-type 
///  ::= 'int'
///  ::= 'float'
///  ::= 'void'
///  ::= 'char'
///  ::= 'string'
///  ::= '.'? identifier ('.' identifier)*
/// type
///   ::= basic-type
///   ::= type '*'
///   ::= type '[' integral-literal ']'
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

    case tok::Char:
      ++CurPos;
      type = BuiltinTypeAST::get(TypeAST::TI_Char);
      break;

    case tok::String:
      ++CurPos;
      type = BuiltinTypeAST::get(TypeAST::TI_String);
      break;

    case tok::Void:
      ++CurPos;
      type = BuiltinTypeAST::get(TypeAST::TI_Void);
      isVoid = true;
      break;

    case tok::Dot:
    case tok::Identifier: {
      Name *name = nullptr;
      QualifiedName qualName;

      if (CurPos == tok::Identifier) {
        name = CurPos->getIdentifier();
        ++CurPos;
      }

      qualName.push_back(name);

      while (CurPos == tok::Dot) {
        ++CurPos;

        if (CurPos == tok::Identifier) {
          name = CurPos->getIdentifier();
        }

        check(tok::Identifier);
        qualName.push_back(name);
      }

      type = new QualifiedTypeAST(qualName);
      break;
    }

    default:
      getDiagnostics().report(CurPos.getLocation(), diag::ERR_InvalidType);
      return nullptr;
  }

  for (;;) {
    switch (CurPos->getKind()) {
      case tok::OpenBrace: {
        if (isVoid) {
          getDiagnostics().report(loc, diag::ERR_VoidAsNonPointer);
          return nullptr;
        }

        ++CurPos;

        int dim = 0;

        if (CurPos == tok::IntNumber) {
          dim = atoi(CurPos->getLiteral().data());
        } else {
          // [ expression ] will be parsed later in parseUnaryExpr (in other
          // cases it's error)
          --CurPos;
          break;
        }

        check(tok::IntNumber);
        check(tok::CloseBrace);

        type = new ArrayTypeAST(type, dim);
        continue;
      }

      case tok::Mul:
        ++CurPos;
        type = new PointerTypeAST(type, false);
        continue;

      default:
        break;
    }

    break;
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

  // Function can start with identifier, new or delete keywords
  if (CurPos == tok::Identifier) {
    name = CurPos->getIdentifier();
    ++CurPos;
  } else if (CurPos == tok::New) {
    ++CurPos;
    name = Name::New;
  } else if (CurPos == tok::Delete) {
    ++CurPos;
    name = Name::Delete;
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
  return new FuncDeclAST(loc, returnType, name, nullptr, false, Tok);
}

/// func-decl ::= func-decl-kwd func-proto block-stmt
/// func-decl-kwd
///   ::= 'fn'
///   ::= 'virt'
///   ::= 'impl'
SymbolList Parser::parseFuncDecl(bool isClassMember) {
  SymbolList result;
  FuncDeclAST *decl = (FuncDeclAST *)parseFuncProto();

  if (CurPos != tok::BlockStart) {
    getDiagnostics().report(CurPos.getLocation(), diag::ERR_ExpectedFuncBody);
  } else {
    StmtAST *body = parseStmt();
    // We should fix some values for declaration
    decl->Body = body;
    decl->NeedThis = isClassMember;
    result.push_back(decl);
  }

  return result;
}

/// decl
///  ::= func-decl
///  ::= dtor-decl
///  ::= struct-decl
///  ::= class-decl
/// 
/// decls
///  ::= decl
///  ::= decls decl
///
/// struct-decl
///  ::= 'struct' identifier '{' decl-stmt * '}'
///
/// class-decl
///  ::= 'class' identifier ( 'extends' type ) ? '{' decls '}'
///
/// dtor-decl ::= ('virt' | 'impl')? 'del' '(' ')' block-stmt
/// ctor-decl ::= 'new' '(' parameters-list ? ')' base-ctor-call ? block-stmt
/// base-ctor-call ::= ':' 'super' '(' call-arguments ? ')'
SymbolList Parser::parseDecls(bool isClassMember) {
  SymbolList result;

  for (;;) {
    SymbolList tmp;
    llvm::SMLoc loc = CurPos.getLocation();

    switch (int Tok = CurPos->getKind()) {
      case tok::Virtual:
      case tok::Override:
        if (!isClassMember) {
          getDiagnostics().report(CurPos.getLocation(),
                                  diag::ERR_VirtOverAsNonClassMember);
          return result;
        }

        ++CurPos;

        if (CurPos != tok::Delete) {
          --CurPos;
          tmp = parseFuncDecl(isClassMember);
          result.push_back(tmp.pop_back_val());
          continue;
        }
        // Fall through

      case tok::Delete: {
        if (!isClassMember) {
          getDiagnostics().report(CurPos.getLocation(),
                                  diag::ERR_UnexpectedDestructorDecl);
          return result;
        }

        check(tok::Delete);
        check(tok::OpenParen);
        check(tok::CloseParen);

        if (CurPos != tok::BlockStart) {
          getDiagnostics().report(CurPos.getLocation(),
                                  diag::ERR_ExpectedDestructorBody);
          return result;
        }

        StmtAST *body = parseStmt();
        ParameterList params;
        TypeAST *funcType =
            new FuncTypeAST(BuiltinTypeAST::get(TypeAST::TI_Void), params);
        result.push_back(
            new FuncDeclAST(loc, funcType, Name::Dtor, body, true, Tok));
        continue;
      }

      case tok::Def:
        tmp = parseFuncDecl(isClassMember);
        result.push_back(tmp.pop_back_val());
        continue;

      case tok::New: {
        if (!isClassMember) {
          getDiagnostics().report(CurPos.getLocation(),
                                  diag::ERR_UnexpectedConstructorDecl);
          return result;
        }

        ExprAST *superCtorCall = nullptr;
        ParameterList params;

        ++CurPos;
        check(tok::OpenParen);

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

        if (CurPos == tok::Colon) {
          check(tok::Colon);

          if (CurPos != tok::Super) {
            getDiagnostics().report(CurPos.getLocation(),
                                    diag::ERR_ExpectedSuperAfterNew);
            return result;
          } else {
            ++CurPos;

            if (CurPos != tok::OpenParen) {
              getDiagnostics().report(CurPos.getLocation(),
                                      diag::ERR_ExpectedFuncArguments);
              return result;
            }

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

            superCtorCall = new CallExprAST(
              loc,
              new MemberAccessExprAST(loc, new IdExprAST(loc, Name::Super),
                                      Name::Ctor),
              args);
          }
        }

        if (CurPos != tok::BlockStart) {
          getDiagnostics().report(CurPos.getLocation(),
                                  diag::ERR_ExpectedConstructorBody);
          return result;
        }

        StmtAST *body = parseStmt();

        // Create function with such body:
        // {
        //   opt super.__ctor(args);
        //   body
        // }
        StmtList ctorBody;

        if (superCtorCall) {
          ctorBody.push_back(new ExprStmtAST(loc, superCtorCall));
        }

        ctorBody.push_back(body);
        body = new BlockStmtAST(loc, ctorBody);
        TypeAST *funcType =
            new FuncTypeAST(BuiltinTypeAST::get(TypeAST::TI_Void), params);
        result.push_back(
            new FuncDeclAST(loc, funcType, Name::Ctor, body, true, tok::New));
        continue;
      }

      case tok::Struct: {
        ++CurPos;
        Name *name = nullptr;

        if (CurPos == tok::Identifier) {
          name = CurPos->getIdentifier();
        }

        check(tok::Identifier);

        check(tok::BlockStart);

        // Structures don't allow any declarations except variables
        while (CurPos != tok::BlockEnd && CurPos != tok::EndOfFile) {
          SymbolList tmp2 = parseDecl(true, true);
          tmp.append(tmp2.begin(), tmp2.end());
        }

        check(tok::BlockEnd);
        result.push_back(new StructDeclAST(loc, name, tmp));
        continue;
      }

      case tok::Class: {
        ++CurPos;
        Name *name = nullptr;
        TypeAST *baseType = nullptr;

        if (CurPos == tok::Identifier) {
          name = CurPos->getIdentifier();
        }
        check(tok::Identifier);

        if (CurPos == tok::Extends) {
          check(tok::Extends);

          if (CurPos != tok::Identifier && CurPos != tok::Dot) {
            getDiagnostics().report(CurPos.getLocation(),
                                    diag::ERR_ExpectedQualifiedNameAsBase);
            return result;
          }

          baseType = parseType();
        }

        check(tok::BlockStart);

        while (CurPos != tok::BlockEnd && CurPos != tok::EndOfFile) {
          SymbolList tmp2 = parseDecls(true);
          tmp.append(tmp2.begin(), tmp2.end());
        }

        check(tok::BlockEnd);
        result.push_back(new ClassDeclAST(loc, name, baseType, tmp));
        continue;
      }

      case tok::BlockEnd:
      case tok::EndOfFile:
        break;

      default:
        if (isClassMember) {
          SymbolList tmp2 = parseDecl(true, isClassMember);
          result.append(tmp2.begin(), tmp2.end());
          continue;
        }
        break;
    }

    break;
  }

  return result;
}

/// module-decl ::= decls
ModuleDeclAST *Parser::parseModule() {
  SymbolList decls = parseDecls(false);

  if (CurPos != tok::EndOfFile) {
    getDiagnostics().report(CurPos.getLocation(), diag::ERR_ExpectedEndOfFile);
    return nullptr;
  }

  return new ModuleDeclAST(getDiagnostics(), decls);
}

/// var-init
///   ::= '=' assign-expr
///   ::= '(' call-arguments ? ')'
/// var-decl ::= identifier ':' type var-init?
/// var-decls
///   ::= var-decl
///   ::= var-decls ',' var-decl
/// decl-stmt ::= var-decls ';'
SymbolList Parser::parseDecl(bool needSemicolon, bool isClassMember) {
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
        if (isa<ArrayTypeAST>(type) || isa<QualifiedTypeAST>(type)) {
          getDiagnostics().report(CurPos.getLocation(),
                                  diag::ERR_AggregateOrArrayInitializer);
          return result;
        }

        ++CurPos;
        value = parseAssignExpr();
      }

      if (isa<QualifiedTypeAST>(type) && CurPos == tok::OpenParen) {
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

        value = new CallExprAST(
          loc,
          new MemberAccessExprAST(loc, new IdExprAST(loc, name), Name::Ctor),
          args);
      }

      result.push_back(new VarDeclAST(loc, type, name, value, isClassMember));

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
    case tok::Super:
    case tok::This:
    case tok::OpenParen:
    case tok::Delete:
    case tok::Mul:
    case tok::BitAnd:
    case tok::New:
    case tok::Dot: {
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