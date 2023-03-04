#include "simple/AST/AST.h"

using namespace llvm;

namespace simple {

Value* StmtAST::generateCode(SLContext& Context) {
  assert(0 && "StmtAST::generateCode should never be reached");
  return nullptr;
}

Value* ExprStmtAST::generateCode(SLContext& Context) {
  if (Expr) {
    // Generate code for an expression
    return Expr->getRValue(Context);
  }
  
  return nullptr;
}

llvm::Value* BlockStmtAST::generatePartialCode(SLContext& Context, 
  StmtList::iterator it, StmtList::iterator end) {
  // Get break, continue and return locations from parent landing pad
  LandingPadAST* parent = LandingPad->Prev;
  LandingPad->BreakLoc = parent->BreakLoc;
  LandingPad->ContinueLoc = parent->ContinueLoc;
  LandingPad->ReturnLoc = parent->ReturnLoc;
  
  // Create new block for FalltrhoughLoc
  LandingPad->FallthroughLoc = BasicBlock::Create(getGlobalContext(), "block");
  
  // Generate code for every nested statement
  for (; it != end; ++it) {
    // Save FallthroughLoc
    BasicBlock* fallthroughBB = LandingPad->FallthroughLoc;

    // Generate code for nested statement
    (*it)->generateCode(Context);

    // Check was FallthroughLoc used in nested statement or not
    if (!LandingPad->FallthroughLoc) {
      // It was. Add it fall through block the end of the function and set
      // it as insert point
      Context.TheFunction->getBasicBlockList().push_back(fallthroughBB);
      Context.TheBuilder->SetInsertPoint(fallthroughBB);

      // Create new block for FallthroughLoc
      LandingPad->FallthroughLoc = BasicBlock::Create(getGlobalContext(), "block");
    }
  }

  // Calculate numbers of break, continue and return statements
  parent->Breaks += LandingPad->Breaks;
  parent->Continues += LandingPad->Continues;
  parent->Returns += LandingPad->Returns;

  // If last current block doesn't have terminator we should add jump
  // to fall through location
  if (!Context.TheBuilder->GetInsertBlock()->getTerminator()) {
    Context.TheBuilder->CreateBr(parent->FallthroughLoc);
    parent->FallthroughLoc = nullptr;
    
    if (!LandingPad->FallthroughLoc->hasNUsesOrMore(1)) {
      delete LandingPad->FallthroughLoc;
    }
  // We need to add fall through block if was used
  } else if (LandingPad->FallthroughLoc->hasNUsesOrMore(1)) {
    // Add fall through block to the end of the function and create jump to
    // parent's fall through block
    Context.TheFunction->getBasicBlockList().push_back(LandingPad->FallthroughLoc);
    Context.TheBuilder->SetInsertPoint(LandingPad->FallthroughLoc);
    Context.TheBuilder->CreateBr(parent->FallthroughLoc);
    parent->FallthroughLoc = nullptr;
  } else {
    delete LandingPad->FallthroughLoc;
  }

  return nullptr;
}

Value* BlockStmtAST::generateCode(SLContext& Context) {
  return generatePartialCode(Context, Body.begin(), Body.end());
}

Value* DeclStmtAST::generateCode(SLContext& Context) {
  // Generate code for every declaration
  for (SymbolList::iterator it = Decls.begin(), end = Decls.end(); it != end; ++it) {
    (*it)->generateCode(Context);
  }

  return nullptr;
}

// LandingPadAST implementation
Value* LandingPadAST::getReturnValue() {
  LandingPadAST* prev = this;

  // Find root landing pad
  while (prev->Prev) {
    prev = prev->Prev;
  }

  // Return it's return variable
  return prev->ReturnValue;
}

Value* BreakStmtAST::generateCode(SLContext& Context) {
  // Create jump to break location
  Context.TheBuilder->CreateBr(BreakLoc->BreakLoc);
  return nullptr;
}

Value* ContinueStmtAST::generateCode(SLContext& Context) {
  // Create jump to continue location
  Context.TheBuilder->CreateBr(ContinueLoc->ContinueLoc);
  return nullptr;
}

Value* ReturnStmtAST::generateCode(SLContext& Context) {
  // Check return value
  if (Expr) {
    // Generate code for return value and create return instruction
    Value* retVal = Expr->getRValue(Context);
    Context.TheBuilder->CreateStore(retVal, ReturnLoc->getReturnValue());
    Context.TheBuilder->CreateBr(ReturnLoc->ReturnLoc);
    return nullptr;
  }

  // Generate void return instruction
  Context.TheBuilder->CreateBr(ReturnLoc->ReturnLoc);
  return nullptr;
}

Value* WhileStmtAST::generateCode(SLContext& Context) {
  LandingPadAST* prev = LandingPad->Prev;
  LandingPad->ReturnLoc = prev->ReturnLoc;

  // We have special case for loop with constant value condition
  if (Cond->isConst()) {
    // For loop with false condition we don't need any generated code
    if (!Cond->isTrue()) {
      return nullptr;
    }

    // Get blocks for body of the loop and post loop (also store them for 
    // break and continue locations)
    BasicBlock* bodyBB = LandingPad->ContinueLoc = BasicBlock::Create(getGlobalContext(), 
      "loopbody", Context.TheFunction);
    BasicBlock* endBB = LandingPad->BreakLoc = BasicBlock::Create(getGlobalContext(), 
      "loopend");

    if (PostExpr) {
      // For ex for-loop we should create block for post expression too and
      // it will also be location for continue
      LandingPad->ContinueLoc = BasicBlock::Create(getGlobalContext(), "postbody");
    }

    // Create jump to body branch and set body branch as insertion point
    Context.TheBuilder->CreateBr(bodyBB);
    Context.TheBuilder->SetInsertPoint(bodyBB);

    // Set fall through location to appropriate location
    if (PostExpr) {
      LandingPad->FallthroughLoc = LandingPad->ContinueLoc;
    } else {
      LandingPad->FallthroughLoc = bodyBB;
    }

    // Generate code for loop's body
    Body->generateCode(Context);

    // We have special case for ex for-loop
    if (PostExpr) {
      // Add continue branch to the end of the function and set it as insertion 
      // point
      Context.TheFunction->getBasicBlockList().push_back(LandingPad->ContinueLoc);
      Context.TheBuilder->SetInsertPoint(LandingPad->ContinueLoc);

      // Generate code for post expression and create jump to body of the loop
      PostExpr->getRValue(Context);
      Context.TheBuilder->CreateBr(bodyBB);
    }

    // Add end branch to the end of the function and set it as insertion 
    // point
    Context.TheFunction->getBasicBlockList().push_back(endBB);
    Context.TheBuilder->SetInsertPoint(endBB);
    prev->Returns += LandingPad->Returns;

    // Create jump to parent's fall through location
    Context.TheBuilder->CreateBr(prev->FallthroughLoc);
    prev->FallthroughLoc = nullptr;

    return nullptr;
  }
  
  // Get blocks for condition, body of the loop and post loop (also store them 
  // for break and continue locations)
  BasicBlock* condBB = LandingPad->ContinueLoc = BasicBlock::Create(getGlobalContext(), 
    "loopcond", Context.TheFunction);
  BasicBlock* bodyBB = BasicBlock::Create(getGlobalContext(), "loopbody");
  BasicBlock* endBB = LandingPad->BreakLoc = BasicBlock::Create(getGlobalContext(), "loopend");

  if (PostExpr) {
    // For ex for-loop we should create block for post expression too and
    // it will also be location for continue
    LandingPad->ContinueLoc = BasicBlock::Create(getGlobalContext(), "postbody");
  }

  // Create jump to condition branch and set continue branch as insertion point
  Context.TheBuilder->CreateBr(condBB);
  Context.TheBuilder->SetInsertPoint(condBB);

  // Generate code for condition
  Value* cond = Cond->getRValue(Context);
  // Promote result to bool
  cond = promoteToBool(cond, Cond->ExprType, *Context.TheBuilder);
  // Create jump to either body of the loop or it's end based on condition
  // result
  Context.TheBuilder->CreateCondBr(cond, bodyBB, endBB);

  // Add body branch to the end of the function and set it as insertion 
  // point
  Context.TheFunction->getBasicBlockList().push_back(bodyBB);
  Context.TheBuilder->SetInsertPoint(bodyBB);

  // Set fall through location to appropriate location
  if (PostExpr) {
    LandingPad->FallthroughLoc = LandingPad->ContinueLoc;
  } else {
    LandingPad->FallthroughLoc = condBB;
  }

  // Generate code for loop's body
  Body->generateCode(Context);

  // We have special case for ex for-loop
  if (PostExpr) {
    // Add continue branch to the end of the function and set it as insertion 
    // point
    Context.TheFunction->getBasicBlockList().push_back(LandingPad->ContinueLoc);
    Context.TheBuilder->SetInsertPoint(LandingPad->ContinueLoc);

    // Generate code for post expression and create jump to loop's condition
    PostExpr->getRValue(Context);
    Context.TheBuilder->CreateBr(condBB);
  }

  // Add end branch to the end of the function and set it as insertion 
  // point
  Context.TheFunction->getBasicBlockList().push_back(endBB);
  Context.TheBuilder->SetInsertPoint(endBB);
  prev->Returns += LandingPad->Returns;

  // Create jump to parent's fall through location
  Context.TheBuilder->CreateBr(prev->FallthroughLoc);
  prev->FallthroughLoc = nullptr;

  return nullptr;
}

llvm::Value* ForStmtAST::generateCode(SLContext& Context) {
  assert(0 && "ForStmtAST::semantic should never be reached");
  return nullptr;
}


llvm::Value* IfStmtAST::generateCode(SLContext& Context) {
  // Get all locations from parent landing pad
  LandingPadAST* prev = LandingPad->Prev;
  LandingPad->ReturnLoc = prev->ReturnLoc;
  LandingPad->FallthroughLoc = prev->FallthroughLoc;
  LandingPad->ContinueLoc = prev->ContinueLoc;
  LandingPad->BreakLoc = prev->BreakLoc;

  // We have special case for if with constant conditional expression
  if (Cond->isConst()) {
    if (Cond->isTrue()) {
      // For true variant generate code for then branch
      ThenBody->generateCode(Context);

      // Calculate number of return, break and continue statements for the parent
      // landing pad
      prev->Returns += LandingPad->Returns;
      prev->Breaks += LandingPad->Breaks;
      prev->Continues += LandingPad->Continues;

      if (!Context.TheBuilder->GetInsertBlock()->getTerminator()) {
        // Create jump to old fall through location
        Context.TheBuilder->CreateBr(prev->FallthroughLoc);
        prev->FallthroughLoc = nullptr;
      }

      return nullptr;
    }
      
    // For false variant generate code for else branch (if exists)
    if (ElseBody) {
      ElseBody->generateCode(Context);

      // Calculate number of return, break and continue statements for the parent
      // landing pad
      prev->Returns += LandingPad->Returns;
      prev->Breaks += LandingPad->Breaks;
      prev->Continues += LandingPad->Continues;

      if (!Context.TheBuilder->GetInsertBlock()->getTerminator()) {
        // Create jump to old fall through location
        Context.TheBuilder->CreateBr(prev->FallthroughLoc);
        prev->FallthroughLoc = nullptr;
      }
    }

    return nullptr;
  }
   
  // Generate code for condition
  Value* cond = Cond->getRValue(Context);
  // Promote result to bool
  cond = promoteToBool(cond, Cond->ExprType, *Context.TheBuilder);

  // Get blocks for then, else and continue locations
  BasicBlock* thenBB = BasicBlock::Create(getGlobalContext(), "thenpart", 
    Context.TheFunction);
  BasicBlock* elseBB = nullptr;
  BasicBlock* endBB = BasicBlock::Create(getGlobalContext(), "ifcont");

  if (ElseBody) {
    elseBB = BasicBlock::Create(getGlobalContext(), "elsepart");
    // Create jump to then or else (if else branch exists)
    Context.TheBuilder->CreateCondBr(cond, thenBB, elseBB);
  } else {
    // Create jump to then or continue
    Context.TheBuilder->CreateCondBr(cond, thenBB, endBB);
  }

  // Set insertion block to then block
  Context.TheBuilder->SetInsertPoint(thenBB);
  
  // We should set fall through location
  LandingPad->FallthroughLoc = endBB;

  // Generate code for then body
  ThenBody->generateCode(Context);

  // We should set fall through location
  LandingPad->FallthroughLoc = endBB;

  // Does else branch exist?
  if (ElseBody) {
    // Add else branch to the end of the function and set it as insertion 
    // point
    Context.TheFunction->getBasicBlockList().push_back(elseBB);
    Context.TheBuilder->SetInsertPoint(elseBB);

    // Generate code for else body
    ElseBody->generateCode(Context);
  }

  if (endBB->hasNUsesOrMore(1)) {
    // Add end branch to the end of the function and set it as insertion 
    // point
    Context.TheFunction->getBasicBlockList().push_back(endBB);
    Context.TheBuilder->SetInsertPoint(endBB);

    // Create jump to old fall through location
    Context.TheBuilder->CreateBr(prev->FallthroughLoc);
    prev->FallthroughLoc = nullptr;
  } else {
    delete endBB;
  }

  // Calculate number of return, break and continue statements for the parent
  // landing pad
  prev->Returns += LandingPad->Returns;
  prev->Breaks += LandingPad->Breaks;
  prev->Continues += LandingPad->Continues;

  return nullptr;
}

} // namespace simple