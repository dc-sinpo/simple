#include "simple/AST/AST.h"

using namespace llvm;

namespace simple {

// StmtAST hierarchy implementation
// StmtAST implementation
bool StmtAST::hasReturn() {
  return false;
}

bool StmtAST::hasJump() {
  return false;
}

StmtAST* StmtAST::semantic(Scope* scope) {
  if (SemaState > 0) {
    return this;
  }

  ++SemaState;
  return doSemantic(scope);
}

StmtAST* StmtAST::doSemantic(Scope* ) {
  assert(0 && "StmtAST::semantic should never be reached");
  return this;
}

// ExprStmtAST implementation
StmtAST* ExprStmtAST::doSemantic(Scope* scope) {
  if (Expr) {
    // Check semantic for an expression
    Expr = Expr->semantic(scope);

    // If expression has nullptr type then it's error
    if (!Expr->ExprType) {
      scope->report(Loc, diag::ERR_SemaNoTypeForExpression);
      return nullptr;
    }
  }

  return this;
}

/// Check does this block need to be promoted in new block or not
/// \param[in] oldStmt - old statement
/// \param[in] scope - current scope
bool needPromoteBodyToBlock(StmtAST* oldStmt, Scope* scope) {
  // Only declarations can have new block
  if (!isa<DeclStmtAST>(oldStmt)) {
    return false;
  }

  // Perform semantic on this statement. We need to be sure that type is resolved
  // for variable
  DeclStmtAST* declStmt = (DeclStmtAST*)oldStmt->semantic(scope);
  SymbolList::iterator it = declStmt->Decls.begin();
  assert(isa<VarDeclAST>(*it));
  VarDeclAST* var = (VarDeclAST*)*it;

  // Only class can lead to new block
  if (isa<ClassTypeAST>(var->ThisType)) {
    ClassDeclAST* classDecl = (ClassDeclAST*)var->ThisType->getSymbol();

    // Only class with destructor can lead to new block
    if (classDecl->Dtor) {
      return true;
    }
  }

  return false;
}

// BlockStmtAST implementation
bool BlockStmtAST::hasReturn() {
  return HasReturn;
}

bool BlockStmtAST::hasJump() {
  return HasJump;
}

StmtAST* BlockStmtAST::doSemantic(Scope* scope) {
  // For block statement we should create new scope
  ThisBlock = new ScopeSymbol(Loc, SymbolAST::SI_Block, nullptr);
  Scope* s = scope->push((ScopeSymbol*)ThisBlock);
  
  // Create landing pad 
  // Note: We patch NeedCleanup later if needed
  LandingPad = new LandingPadAST(s->LandingPad, false);
  LandingPad->OwnerBlock = this;
  LandingPad->NeedCleanup = s->LandingPad->NeedCleanup;
  s->LandingPad = LandingPad;

  bool atStart = true; // We need that for new blocks creation
  ExprList args;

  // Check all nested statements
  for (StmtList::iterator it = Body.begin(), end = Body.end(); it != end; ++it) {
    // Jump statement should be last in a block
    if (HasJump) {
      scope->report(Loc, diag::ERR_SemaDeadCode);
      return nullptr;
    }

    if (needPromoteBodyToBlock(*it, s)) {
      // It's need cleanup
      LandingPad->NeedCleanup = true;

      if (!atStart) {
        // Append rest statements to new block
        StmtList body(it, end);
        BlockStmtAST* newBlockStmt = new BlockStmtAST(Loc, body);
        Body.erase(it, end);
        Body.push_back(newBlockStmt);
        // Perform semantic on just created block and get HasJump and HasReturn
        // from it
        newBlockStmt->IsPromoted = true;
        newBlockStmt->semantic(s);
        HasJump = newBlockStmt->HasJump;
        HasReturn = newBlockStmt->HasReturn;
        break;
      }

      DeclStmtAST* decls = (DeclStmtAST*)*it;
      
      for (SymbolList::iterator it2 = decls->Decls.begin(), end2 = decls->Decls.end();
        it2 != end2; ++it2) {
        VarDeclAST* var = (VarDeclAST*)*it2;
        // Create destructor call for this variable
        ExprAST* cleanupExpr = new CallExprAST(
          Loc,
          new MemberAccessExprAST(
            Loc,
            new IdExprAST(Loc, var->Id),
            Name::Dtor),
          args);
        // Perform semantic on just created expression
        cleanupExpr = cleanupExpr->semantic(s);
        // Add it to the list
        CleanupList.push_back(cleanupExpr);
      }
    } else if (!isa<DeclStmtAST>(*it)) {
      // If it's not declaration then we should set atStart to false
      atStart = false;
    }

    // Check semantic for statement
    *it = (*it)->semantic(s);

    // Check for jump statement
    if ((*it)->isJump()) {
      HasJump = true;

      // Check for return statement
      if ((*it)->hasReturn()) {
        HasReturn = true;
      }
    } else {
      // It wasn't jump but if it was other block or control statement then
      // it could have return statement too
      HasJump = (*it)->hasJump();
      HasReturn = (*it)->hasReturn();
    }
  }

  // If we need cleanup then we should set some states
  if (LandingPad->NeedCleanup) {
    // Get escaped function and patch it's NeedCleanup
    FuncDeclAST* fncDecl = (s->EnclosedFunc);
    fncDecl->LandingPad->NeedCleanup = true;

    if (!LandingPad->Prev->OwnerBlock) {
      // If previous landing pad isn't block set NeedCleanup to true
      LandingPad->Prev->NeedCleanup = true;
    } else {
      // Previous landing pad is block but we need set NeedCleanup for it only
      // if we have return, continue or break statement in our body
      if (LandingPad->Returns || LandingPad->Continues || LandingPad->Breaks) {
        LandingPad->Prev->NeedCleanup = true;
      }
    }
  }

  // Remove created scope
  s->pop();

  return this;
}

// DeclStmtAST implementation
StmtAST* DeclStmtAST::doSemantic(Scope* scope) {
  // Check semantic for every declaration
  for (SymbolList::iterator it = Decls.begin(), end = Decls.end(); it != end; ++it) {
    (*it)->semantic(scope);
    (*it)->semantic2(scope);
    (*it)->semantic3(scope);
    (*it)->semantic4(scope);
    (*it)->semantic5(scope);
  }

  return this;
}

// BreakStmtAST implementation
bool BreakStmtAST::hasJump() {
  return true;
}

StmtAST* BreakStmtAST::doSemantic(Scope* scope) {
  // We allow break only in the loops
  if (!scope->BreakLoc) {
    scope->report(Loc, diag::ERR_SemaInvalidJumpStatement);
    return nullptr;
  }

  // Save break location for later use
  BreakLoc = scope->LandingPad;
  ++BreakLoc->Breaks;
  return this;
}

// ContinueStmtAST implementation
bool ContinueStmtAST::hasJump() {
  return true;
}

StmtAST* ContinueStmtAST::doSemantic(Scope* scope) {
  // We allow continue only in the loops
  if (!scope->ContinueLoc) {
    scope->report(Loc, diag::ERR_SemaInvalidJumpStatement);
    return nullptr;
  }

  // Save continue location for later use
  ContinueLoc = scope->LandingPad;
  ++ContinueLoc->Continues;
  return this;
}

// ReturnStmtAST implementation
bool ReturnStmtAST::hasReturn() {
  return true;
}

bool ReturnStmtAST::hasJump() {
  return true;
}

StmtAST* ReturnStmtAST::doSemantic(Scope* scope) {
  assert(scope->LandingPad);
  ReturnLoc = scope->LandingPad;
  ++ReturnLoc->Returns;
  // Check return value
  if (Expr) {
    // Check for void value function
    if (!scope->EnclosedFunc->ReturnType || scope->EnclosedFunc->ReturnType->isVoid()) {
      scope->report(Loc, diag::ERR_SemaReturnValueInVoidFunction);
      return nullptr;
    }

    // Perform semantic of return value
    Expr = Expr->semantic(scope);

    if (!scope->EnclosedFunc->ReturnType->equal(Expr->ExprType)) {
      Expr = new CastExprAST(Loc, Expr, scope->EnclosedFunc->ReturnType);
      Expr = Expr->semantic(scope);
    }

    return this;
  }

  // We don't have return value check should function have return value or not
  if (scope->EnclosedFunc->ReturnType && !scope->EnclosedFunc->ReturnType->isVoid()) {
    scope->report(Loc, diag::ERR_SemaReturnVoidFromFunction);
    return nullptr;
  }

  return this;
}

// WhileStmtAST implementation
bool WhileStmtAST::hasReturn() {
  // Always return false because loop can has 0 iterations
  return false;
}

StmtAST* WhileStmtAST::doSemantic(Scope* scope) {
  // Perform semantic of a condition
  Cond = Cond->semantic(scope);

  // Condition should has non void type
  if (!Cond->ExprType || Cond->ExprType->isVoid()) {
    scope->report(Loc, diag::ERR_SemaConditionIsVoid);
    return nullptr;
  }

  // Check for conversion of conditional expression to boolean
  if (!Cond->ExprType->implicitConvertTo(BuiltinTypeAST::get(TypeAST::TI_Bool))) {
    scope->report(Loc, diag::ERR_SemaCantConvertToBoolean);
    return nullptr;
  }

  // Backup old break and continue locations if any
  StmtAST* oldBreak = scope->BreakLoc;
  StmtAST* oldContinue = scope->ContinueLoc;

  // Set new break and continue locations to this instance
  scope->BreakLoc = this;
  scope->ContinueLoc = this;
  // Create new landing pad
  LandingPad = new LandingPadAST(scope->LandingPad, false);
  LandingPad->NeedCleanup = scope->LandingPad->NeedCleanup;
  LandingPad->IsLoop = true;
  scope->LandingPad = LandingPad;

  Body = Body->semantic(scope);

  // Restore old break and continue locations
  scope->BreakLoc = oldBreak;
  scope->ContinueLoc = oldContinue;
  scope->LandingPad = LandingPad->Prev;

  if (PostExpr) {
    // Perform semantic for PostExpr if this instance was for-loop before semantic
    PostExpr = PostExpr->semantic(scope);
  }

  return this;
}

// ForStmtAST implementation
StmtAST* ForStmtAST::doSemantic(Scope* scope) {
  // Rewrite for-loop statement to 
  // {
  //   init
  //   while (cond) {
  //     body
  //     continueZone: post
  //   }
  // }

  StmtAST* init = nullptr;

  // We should convert initialization expression or declaration to new
  // statement
  if (InitExpr) {
    init = new ExprStmtAST(Loc, InitExpr);
  } else if (!InitDecls.empty()) {
    init = new DeclStmtAST(Loc, InitDecls);
  }

  // We have different cases for loop with non empty and empty condition
  if (Cond) {
    StmtList stmts;

    // Add initialization as 1st member of new block statement (if was set)
    if (init) {
      stmts.push_back(init);
    }

    // Create new while-loop and add it as 2nd statement for the block
    WhileStmtAST* newLoop = new WhileStmtAST(Loc, Cond, Body);
    newLoop->PostExpr = Post;
    stmts.push_back(newLoop);

    // Clear and delete this
    InitExpr = nullptr;
    InitDecls.clear();
    Cond = nullptr;
    Post = nullptr;
    Body = nullptr;
    delete this;

    // Create new block statement and perform semantic on it
    StmtAST* res = new BlockStmtAST(Loc, stmts);
    return res->semantic(scope);
  } else {
    StmtList stmts;

    // Add initialization as 1st member of new block statement (if was set)
    if (init) {
      stmts.push_back(init);
    }

    // Create new while-loop and add it as 2nd statement for the block
    WhileStmtAST* newLoop = new WhileStmtAST(Loc, new IntExprAST(Loc, 1), Body);
    newLoop->PostExpr = Post;
    stmts.push_back(newLoop);

    // Clear and delete this
    InitExpr = nullptr;
    InitDecls.clear();
    Cond = nullptr;
    Post = nullptr;
    Body = nullptr;
    delete this;

    // Create new block statement and perform semantic on it
    StmtAST* res = new BlockStmtAST(Loc, stmts);
    return res->semantic(scope);
  }
}

// IfStmtAST implementation
bool IfStmtAST::hasReturn() {
  if (!ElseBody) {
    return false;
  }

  // We should return true if both then and else parts have return
  return ThenBody->hasReturn() && ElseBody->hasReturn();
}

bool IfStmtAST::hasJump() {
  if (!ElseBody) {
    return false;
  }

  // We should return true if both then and else parts have jumps
  return ThenBody->hasJump() && ElseBody->hasJump();
}

StmtAST* IfStmtAST::doSemantic(Scope* scope) {
  // Perform semantic for condition expression
  Cond = Cond->semantic(scope);

  // We not allow void condition expression
  if (!Cond->ExprType || Cond->ExprType == BuiltinTypeAST::get(TypeAST::TI_Void)) {
    scope->report(Loc, diag::ERR_SemaConditionIsVoid);
    return nullptr;
  }

  if (!Cond->ExprType->implicitConvertTo(BuiltinTypeAST::get(TypeAST::TI_Bool))) {
    scope->report(Loc, diag::ERR_SemaCantConvertToBoolean);
    return nullptr;
  }

  // Create new landing pad
  LandingPad = new LandingPadAST(scope->LandingPad, false);
  LandingPad->NeedCleanup = scope->LandingPad->NeedCleanup;
  scope->LandingPad = LandingPad;

  // Perform semantic for then and else parts
  ThenBody = ThenBody->semantic(scope);

  if (ElseBody) {
    ElseBody = ElseBody->semantic(scope);
  }

  // Restore old landing pad
  scope->LandingPad = LandingPad->Prev;

  return this;
}

} // namespace simple