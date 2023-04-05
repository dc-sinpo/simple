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
  
  // We should set CleanupLoc differently for case without and with cleanup
  if (!LandingPad->NeedCleanup || CleanupList.empty()) {
    // Without cleanup we get CleanupLoc from previous landing pad
    LandingPad->CleanupLoc = parent->CleanupLoc;
  } else {
    // We need to add new CleanupLoc if we need cleanup
    LandingPad->CleanupLoc = BasicBlock::Create(getGlobalContext());
  }

  // Create new block for FalltrhoughLoc
  LandingPad->FallthroughLoc = BasicBlock::Create(getGlobalContext(), "block");
  
  StmtAST* lastStmt = nullptr;
  BasicBlock* lastFallThrough = nullptr;
  BasicBlock* lastInsertionPoint = nullptr;

  // Generate code for every nested statement
  for (; it != end; ++it) {
    // Save FallthroughLoc
    BasicBlock* fallthroughBB = LandingPad->FallthroughLoc;
    lastFallThrough = LandingPad->FallthroughLoc;

    lastStmt = *it;
    // Generate code for nested statement
    lastStmt->generateCode(Context);

    // Check was FallthroughLoc used in nested statement or not
    if (!LandingPad->FallthroughLoc) {
      // It was. Add it fall through block the end of the function and set
      // it as insert point
      lastInsertionPoint = Context.TheBuilder->GetInsertBlock();
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

  // Check do we need cleanup or not
  if (LandingPad->NeedCleanup) {
    // We need

    if (!LandingPad->FallthroughLoc->hasNUsesOrMore(1)) {
      // Block wasn't used delete it
      delete LandingPad->FallthroughLoc;
    }

    // If we don't have any cleanup variables then we don't need any actions
    if (CleanupList.empty()) {
      if (!Context.TheBuilder->GetInsertBlock()->getTerminator()) {
        Context.TheBuilder->CreateBr(parent->FallthroughLoc);
        parent->FallthroughLoc = nullptr;
        return nullptr;
      }

      return nullptr;
    }

    // If last current block doesn't have terminator we should add jump
    // to cleanup location
    if (!Context.TheBuilder->GetInsertBlock()->getTerminator()) {
      if (lastStmt && isa<BlockStmtAST>(lastStmt) && ((BlockStmtAST*)lastStmt)->IsPromoted) {
        Context.TheBuilder->CreateBr(LandingPad->CleanupLoc);
      } else {
        // We should set fall through value
        Context.TheBuilder->CreateStore(getConstInt(0), parent->getCleanupValue());
        Context.TheBuilder->CreateBr(LandingPad->CleanupLoc);
      }
    }

    // Add cleanup location to the end of the function and set it as insert point
    Context.TheFunction->getBasicBlockList().push_back(LandingPad->CleanupLoc);
    Context.TheBuilder->SetInsertPoint(LandingPad->CleanupLoc);
    LandingPad->CleanupLoc->setName("cleanup.block");

    // Generated destructor calls in reverse order
    for (ExprList::reverse_iterator it = CleanupList.rbegin(), end = CleanupList.rend();
      it != end; ++it) {
      (*it)->getRValue(Context);
    }

    // Check do we have return, break or continue statements or not
    if (!LandingPad->Returns && !LandingPad->Breaks && !LandingPad->Continues) {
      // We don't and we should create jump to FallthroughLoc
      Context.TheBuilder->CreateBr(parent->FallthroughLoc);
      parent->FallthroughLoc = nullptr;
    } else {
      // Does parent landing pad has CleanupLoc or not
      if (parent->CleanupLoc) {
        if (IsPromoted) {
          Context.TheBuilder->CreateBr(parent->FallthroughLoc);
        } else {
          // It has we need to create switch with fall through and cleanup locations
          Value* val = Context.TheBuilder->CreateLoad(
            Type::getInt32Ty(getGlobalContext()),
            LandingPad->getCleanupValue()
          );
          SwitchInst* switchBB = Context.TheBuilder->CreateSwitch(val, parent->CleanupLoc);

          switchBB->addCase(getConstInt(0), parent->FallthroughLoc);
        }        
      } else {
        // It's root cleanup
        LandingPadAST* prev = parent;

        // Try to find previous landing pad with cleanup
        while (prev->Prev) {
          if (prev->CleanupLoc) {
            break;
          }

          prev = prev->Prev;
        }

        // Does parent have cleanup location
        if (prev->CleanupLoc) {
          // It does. We need to generate switch with cleanup and fall through
          // locations and probably also with continue and break
          Value* val = Context.TheBuilder->CreateLoad(
            Type::getInt32Ty(getGlobalContext()),
            LandingPad->getCleanupValue()
          );
          SwitchInst* switchBB = Context.TheBuilder->CreateSwitch(val, prev->CleanupLoc);

          switchBB->addCase(getConstInt(0), parent->FallthroughLoc);

          // If we have continue then we should add case for it
          if (LandingPad->Continues) {
            switchBB->addCase(getConstInt(2), parent->ContinueLoc);
          }

          // If we have break then we should add case for it
          if (LandingPad->Breaks) {
            switchBB->addCase(getConstInt(3), parent->BreakLoc);
          }
        } else {
          if (LandingPad->Returns || LandingPad->Breaks || LandingPad->Continues) {
            if (prev->ReturnLoc == parent->FallthroughLoc && !LandingPad->Breaks
              && !LandingPad->Continues) {
              Context.TheBuilder->CreateBr(prev->ReturnLoc);
            } else {
              // We should create switch
              Value* val = Context.TheBuilder->CreateLoad(
                Type::getInt32Ty(getGlobalContext()),
                LandingPad->getCleanupValue()
              );
              SwitchInst* switchBB = Context.TheBuilder->CreateSwitch(val, prev->ReturnLoc);

              switchBB->addCase(getConstInt(0), parent->FallthroughLoc);

              // If we have continue then we should add case for it
              if (LandingPad->Continues) {
                switchBB->addCase(getConstInt(2), parent->ContinueLoc);
              }

              // If we have break then we should add case for it
              if (LandingPad->Breaks) {
                switchBB->addCase(getConstInt(3), parent->BreakLoc);
              }
            }
          } else {
            // We don't have previous blocks and don't have return. Jump to
            // fall through
            Context.TheBuilder->CreateBr(parent->FallthroughLoc);
          }
        }
      }

      parent->FallthroughLoc = nullptr;
    }
  } else {
    // We don't have cleanup

    // If last current block doesn't have terminator we should add jump
    // to fall through location
    if (!Context.TheBuilder->GetInsertBlock()->getTerminator()) {
      const unsigned u = Context.TheBuilder->GetInsertBlock()->size();

      if (u == 0) {
        BasicBlock *thisBlock = Context.TheBuilder->GetInsertBlock();

        if (thisBlock->hasNUses(1)) {
          BasicBlock *prevBlock = thisBlock->getSinglePredecessor();
          Instruction *prevTerminator = prevBlock->getTerminator();

          if (parent->IsLoop) {
            thisBlock->replaceAllUsesWith(parent->FallthroughLoc);
          }

          if (!LandingPad->FallthroughLoc->hasNUsesOrMore(1)) {
            delete LandingPad->FallthroughLoc;
          }

          return nullptr;
        }
      }
      
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

Value* LandingPadAST::getCleanupValue() {
  LandingPadAST* prev = this;

  // Find root landing pad
  while (prev->Prev) {
    prev = prev->Prev;
  }

  // Return it's cleanup variable
  return prev->CleanupValue;
}

Value* BreakStmtAST::generateCode(SLContext& Context) {
  // Create jump to break location
  if (BreakLoc->NeedCleanup) {
    // If we need cleanup then we should save 3 to cleanup variable and jump
    // to cleanup location
    Context.TheBuilder->CreateStore(getConstInt(3), BreakLoc->getCleanupValue());
    Context.TheBuilder->CreateBr(BreakLoc->CleanupLoc);
    return nullptr;
  }

  // Create jump to break location
  Context.TheBuilder->CreateBr(BreakLoc->BreakLoc);
  return nullptr;
}

Value* ContinueStmtAST::generateCode(SLContext& Context) {
  // Create jump to continue location
  if (ContinueLoc->NeedCleanup) {
    // If we need cleanup then we should save 2 to cleanup variable and jump
    // to cleanup location
    Context.TheBuilder->CreateStore(getConstInt(2), ContinueLoc->getCleanupValue());
    Context.TheBuilder->CreateBr(ContinueLoc->CleanupLoc);
    return nullptr;
  }

  // Create jump to continue location
  Context.TheBuilder->CreateBr(ContinueLoc->ContinueLoc);
  return nullptr;
}

Value* ReturnStmtAST::generateCode(SLContext& Context) {
  if (ReturnLoc->NeedCleanup) {
    // If we need cleanup then we should save 1 to cleanup variable and jump
    // to cleanup location
    Context.TheBuilder->CreateStore(getConstInt(1), ReturnLoc->getCleanupValue());

    if (Expr) {
      // If we have return value we should store it to return variable
      Value* retVal = Expr->getRValue(Context);
      Context.TheBuilder->CreateStore(retVal, ReturnLoc->getReturnValue());
    }

    // Create jump to cleanup location
    Context.TheBuilder->CreateBr(ReturnLoc->CleanupLoc);
    return nullptr;
  }

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

  return nullptr;
}

llvm::Value* ForStmtAST::generateCode(SLContext& Context) {
  assert(0 && "ForStmtAST::semantic should never be reached");
  return nullptr;
}


llvm::Value* IfStmtAST::generateCode(SLContext& Context) {
  // Get all locations from parent landing pad
  LandingPadAST* prev = LandingPad->Prev;
  LandingPad->CleanupLoc = prev->CleanupLoc;
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