#include "simple/AST/AST.h"

using namespace llvm;

namespace simple {

/// Generate code for integral constant
/// \param[in] value - constant's value
ConstantInt* getConstInt(uint64_t value) {
  return ConstantInt::get(Type::getInt32Ty(getGlobalContext()), value, true);
}

/// Promote result of operation to bool type
/// \param[in] val - value to convert
/// \param[in] type - type to convert from
/// \param[in] builder - IR builder for code generation pass
Value* promoteToBool(Value* val, TypeAST* type, IRBuilder< >& builder) {
  if (type == BuiltinTypeAST::get(TypeAST::TI_Bool)) {
    // Value already has bool type return without changes
    return val;
  }

  if (type->isInt()) {
    // Convert integral value to bool by comparison with 0
    return builder.CreateICmpNE(val, ConstantInt::get(type->getType(), 0));
  } else {
    assert(type->isFloat());
    // Convert floating point value to bool by comparison with 0.0
    return builder.CreateFCmpUNE(val, ConstantFP::get(
      Type::getDoubleTy(getGlobalContext()), 0.0));
  }
}

Value* ExprAST::getLValue(SLContext& ) {
  assert(0 && "");
  return nullptr;
}

Value* IntExprAST::getRValue(SLContext& ) {
  return ConstantInt::get(ExprType->getType(), Val, true);
}

Value* FloatExprAST::getRValue(SLContext& ) {
  return ConstantFP::get(ExprType->getType(), Val);
}

Value* IdExprAST::getLValue(SLContext& Context) {
  return ThisSym->getValue(Context);
}

Value* IdExprAST::getRValue(SLContext& Context) {
  if (isa<FuncDeclAST>(ThisSym)) {
    // If it's function we should generate type for it
    return ThisSym->getValue(Context);
  }

  // For variable we should generate load instruction
  return Context.TheBuilder->CreateLoad(
    ExprType->getType(),
    ThisSym->getValue(Context), 
    StringRef(Val->Id, Val->Length)
  );
}

Value* CastExprAST::getRValue(SLContext& Context) {
  // Check convert to int first
  if (ExprType->isInt()) {
    if (Val->ExprType->isBool()) {
      // If old expression type is bool then extend value with 0
      return Context.TheBuilder->CreateZExt(Val->getRValue(Context), 
        ExprType->getType());
    }

    assert(Val->ExprType->isFloat());
    // It's floating point value, convert it to int
    return Context.TheBuilder->CreateFPToSI(Val->getRValue(Context), 
      ExprType->getType());
  }

  // Check convert to bool now
  if (ExprType->isBool()) {
    // We should promote old value to bool type
    return promoteToBool(Val->getRValue(Context), Val->ExprType, 
      *Context.TheBuilder);
    // Check for conversion to char
  } else if (Val->ExprType->isInt()) {
    // It's char or int value, convert it to float
    return Context.TheBuilder->CreateSIToFP(Val->getRValue(Context), 
      ExprType->getType());
  } else if (Val->ExprType->isBool()) {
    // It's bool value, convert it to float
    return Context.TheBuilder->CreateUIToFP(Val->getRValue(Context),
      ExprType->getType());
  }

  assert(0 && "should never be reached");
  return nullptr;
}

Value* UnaryExprAST::getRValue(SLContext& Context) {
  assert(0 && "Should never happen");
  return nullptr;
}

Value* BinaryExprAST::getRValue(SLContext& Context) {
  // Check all special case binary expressions 1st

  // = operator
  if (Op == tok::Assign) {
    
    // Generate code for right operand of the expression
    Value* right = RightExpr->getRValue(Context);
    // Get address of a variable to hold result
    Value* res = LeftExpr->getLValue(Context);

    // Generate store to variable and return right operand
    Context.TheBuilder->CreateStore(right, res);
    return right;
  }

  // , operator
  if (Op == tok::Comma) {
    // Generate code for left and right operands
    LeftExpr->getRValue(Context);
    Value* rhs = RightExpr->getRValue(Context);
    // Return result of right operand
    return rhs;
  }

  // postfix versions of ++ and -- operators
  if (Op == tok::PlusPlus || Op == tok::MinusMinus) {
    // Get address of a variable and generate load instruction
    Value* var = LeftExpr->getLValue(Context);
    Value* val = LeftExpr->getRValue(Context);

    if (Op == tok::PlusPlus) {
      // Create integral constant 1 and add it to loaded variable
      Value* tmp = getConstInt(1);
      tmp = Context.TheBuilder->CreateAdd(val, tmp, "inctmp");
      // Store result and return old value
      Context.TheBuilder->CreateStore(tmp, var);
      return val;
    } else {
      // Create integral constant -1 and add it to loaded variable
      Value* tmp = getConstInt(~0ULL);
      tmp = Context.TheBuilder->CreateAdd(val, tmp, "dectmp");
      // Store result and return old value
      Context.TheBuilder->CreateStore(tmp, var);
      return val;
    }
  }

  // || operator
  if (Op == tok::LogOr) {
    //Pseudo code for || operator

    //if (bool result = (left != 0))
    //  return result;
    //else
    //  return (right != 0);

    // Generate code for left operand
    Value* lhs = LeftExpr->getRValue(Context);
    // Promote just generated result to bool
    lhs = promoteToBool(lhs, LeftExpr->ExprType, *Context.TheBuilder);

    // Get blocks for just generated value, result and false branches
    BasicBlock* condBB = Context.TheBuilder->GetInsertBlock();
    BasicBlock* resultBB = BasicBlock::Create(getGlobalContext(), "result");
    BasicBlock* falseBB = BasicBlock::Create(getGlobalContext(), "false");

    // Create conditional jump to result or false branches based on result of
    // left operand
    Context.TheBuilder->CreateCondBr(lhs, resultBB, falseBB);

    // Add false branch to the end of the function and set it as insertion point
    Context.TheFunction->getBasicBlockList().push_back(falseBB);
    Context.TheBuilder->SetInsertPoint(falseBB);
    // Generate code for right operand
    Value* rhs = RightExpr->getRValue(Context);
    // Update false branch if it was changed by right operand
    falseBB = Context.TheBuilder->GetInsertBlock();
    // Promote value of right operand to bool
    rhs = promoteToBool(rhs, RightExpr->ExprType, *Context.TheBuilder);
    // Create jump to result branch
    Context.TheBuilder->CreateBr(resultBB);

    // Add result branch to the end of the function and set it as insertion point
    Context.TheFunction->getBasicBlockList().push_back(resultBB);
    Context.TheBuilder->SetInsertPoint(resultBB);

    // Now we need to create PHI node which should have result from conditional
    // branch or false branch
    PHINode* PN = Context.TheBuilder->CreatePHI(Type::getInt1Ty(getGlobalContext()), 2);

    PN->addIncoming(lhs, condBB);
    PN->addIncoming(rhs, falseBB);

    return PN;
  }

  // && operator
  if (Op == tok::LogAnd) {
    //Pseudo code for && operator

    //if (left != 0 && right != 0)
    //    return true;
    //else
    //  return false;

    // Generate code for left operand
    Value* lhs = LeftExpr->getRValue(Context);
    // Promote just generated result to bool
    lhs = promoteToBool(lhs, LeftExpr->ExprType, *Context.TheBuilder);

    // Get blocks for just generated value, result and true branches
    BasicBlock* condBB = Context.TheBuilder->GetInsertBlock();
    BasicBlock* resultBB = BasicBlock::Create(getGlobalContext(), "result");
    BasicBlock* trueBB = BasicBlock::Create(getGlobalContext(), "true");

    // Create conditional jump to true or result branches based on result of
    // left operand
    Context.TheBuilder->CreateCondBr(lhs, trueBB, resultBB);

    // Add false branch to the end of the function and set it as insertion point
    Context.TheFunction->getBasicBlockList().push_back(trueBB);
    Context.TheBuilder->SetInsertPoint(trueBB);
    // Generate code for right operand
    Value* rhs = RightExpr->getRValue(Context);
    // Update true branch if it was changed by right operand
    trueBB = Context.TheBuilder->GetInsertBlock();
    // Promote value of right operand to bool
    rhs = promoteToBool(rhs, RightExpr->ExprType, *Context.TheBuilder);
    // Create jump to result branch
    Context.TheBuilder->CreateBr(resultBB);

    // Add result branch to the end of the function and set it as insertion point
    Context.TheFunction->getBasicBlockList().push_back(resultBB);
    Context.TheBuilder->SetInsertPoint(resultBB);

    // Now we need to create PHI node which should have result from conditional
    // branch or true branch
    PHINode* PN = Context.TheBuilder->CreatePHI(Type::getInt1Ty(getGlobalContext()), 2);

    PN->addIncoming(lhs, condBB);
    PN->addIncoming(rhs, trueBB);

    return PN;
  }

  // Generate code for left and right operands
  Value* lhs = LeftExpr->getRValue(Context);
  Value* rhs = RightExpr->getRValue(Context);

  // Check type of left operand
  if (LeftExpr->ExprType == BuiltinTypeAST::get(TypeAST::TI_Int)) {
    // Generate code for result value based on operator
    switch (Op) {
      case tok::Plus: return Context.TheBuilder->CreateAdd(lhs, rhs, "addtmp");
      case tok::Minus: return Context.TheBuilder->CreateSub(lhs, rhs, "subtmp");
      case tok::Mul: return Context.TheBuilder->CreateMul(lhs, rhs, "multmp");
      case tok::Div: return Context.TheBuilder->CreateSDiv(lhs, rhs, "divtmp");
      case tok::Mod: return Context.TheBuilder->CreateSRem(lhs, rhs, "remtmp");
      case tok::BitOr: return Context.TheBuilder->CreateOr(lhs, rhs, "ortmp");
      case tok::BitAnd: return Context.TheBuilder->CreateAnd(lhs, rhs, "andtmp");
      case tok::BitXor: return Context.TheBuilder->CreateXor(lhs, rhs, "xortmp");
      case tok::LShift: return Context.TheBuilder->CreateShl(lhs, rhs, "shltmp");
      case tok::RShift: return Context.TheBuilder->CreateAShr(lhs, rhs, "shrtmp");
      case tok::Less: return Context.TheBuilder->CreateICmpSLT(lhs, rhs, "cmptmp");
      case tok::Greater: return Context.TheBuilder->CreateICmpSGT(lhs, rhs, "cmptmp");
      case tok::LessEqual: return Context.TheBuilder->CreateICmpSLE(lhs, rhs, "cmptmp");
      case tok::GreaterEqual: return Context.TheBuilder->CreateICmpSGE(lhs, rhs, "cmptmp");
      case tok::Equal: return Context.TheBuilder->CreateICmpEQ(lhs, rhs, "cmptmp");
      case tok::NotEqual: return Context.TheBuilder->CreateICmpNE(lhs, rhs, "cmptmp");
      default: assert(0 && "Invalid integral binary operator"); return nullptr;
    }
  }
  
  // Generate code for result value based on operator
  switch (Op) {
    case tok::Plus: return Context.TheBuilder->CreateFAdd(lhs, rhs, "addtmp");
    case tok::Minus: return Context.TheBuilder->CreateFSub(lhs, rhs, "subtmp");
    case tok::Mul: return Context.TheBuilder->CreateFMul(lhs, rhs, "multmp");
    case tok::Div: return Context.TheBuilder->CreateFDiv(lhs, rhs, "divtmp");
    case tok::Mod: return Context.TheBuilder->CreateFRem(lhs, rhs, "remtmp");
    case tok::Less: return Context.TheBuilder->CreateFCmpULT(lhs, rhs, "cmptmp");
    case tok::Greater: return Context.TheBuilder->CreateFCmpUGT(lhs, rhs, "cmptmp");
    case tok::LessEqual: return Context.TheBuilder->CreateFCmpULE(lhs, rhs, "cmptmp");
    case tok::GreaterEqual: return Context.TheBuilder->CreateFCmpUGE(lhs, rhs, "cmptmp");
    case tok::Equal: return Context.TheBuilder->CreateFCmpUEQ(lhs, rhs, "cmptmp");
    case tok::NotEqual: return Context.TheBuilder->CreateFCmpUNE(lhs, rhs, "cmptmp");
    default: assert(0 && "Invalid floating point binary operator"); return nullptr;
  }
}

/// Generate code for conditional expression
/// \param[in] Context - code generation context
/// \param[in] Cond - condition
/// \param[in] IfExpr - if part
/// \param[in] ElseExpr - else part
/// \param[in] isLValue - true - if we need generate code for lvalue
Value* generateCondExpr(SLContext& Context, ExprAST* Cond, ExprAST* IfExpr, 
  ExprAST* ElseExpr, bool isLValue) {
  // Generate code for conditional part
  Value* cond = Cond->getRValue(Context);
  // Promote just generated value to bool
  cond = promoteToBool(cond, Cond->ExprType, *Context.TheBuilder);

  // Get blocks for then, else and continue branches
  BasicBlock* thenBB = BasicBlock::Create(getGlobalContext(), "then", 
    Context.TheFunction);
  BasicBlock* elseBB = BasicBlock::Create(getGlobalContext(), "else");
  BasicBlock* mergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");

  // Create conditional jump to then or else branches based on result of
  // conditional part
  Context.TheBuilder->CreateCondBr(cond, thenBB, elseBB);
  // Set insert point to a then branch
  Context.TheBuilder->SetInsertPoint(thenBB);

  // Generate code for then part
  Value* thenValue;
  
  // For lvalue we should generate lvalue
  if (isLValue) {
    thenValue = IfExpr->getLValue(Context);
  } else {
    thenValue = IfExpr->getRValue(Context);
  }

  // Create jump to continue branch and update then part if it was changed
  Context.TheBuilder->CreateBr(mergeBB);
  thenBB = Context.TheBuilder->GetInsertBlock();

  // Add else branch to the end of the function and set it as insertion point
  Context.TheFunction->getBasicBlockList().push_back(elseBB);
  Context.TheBuilder->SetInsertPoint(elseBB);

  Value* elseValue;
  
  // For lvalue we should generate lvalue
  if (isLValue) {
    elseValue = ElseExpr->getLValue(Context);
  } else {
    elseValue = ElseExpr->getRValue(Context);
  }

  // Create jump to continue branch and update else part if it was changed
  Context.TheBuilder->CreateBr(mergeBB);
  elseBB = Context.TheBuilder->GetInsertBlock();

  // Add continue branch to the end of the function and set it as insertion 
  // point
  Context.TheFunction->getBasicBlockList().push_back(mergeBB);
  Context.TheBuilder->SetInsertPoint(mergeBB);

  // For void type we are done
  if (!IfExpr->ExprType || IfExpr->ExprType->isVoid()) {
    return nullptr;
  }

  // Now we need to create PHI node which should have result from then or else
  // branches
  PHINode* PN;
  
  if (isLValue) {
    // For lvalue we get pointer to original expression's type
    PN = Context.TheBuilder->CreatePHI(
      PointerType::get(
        getGlobalContext(),
        Context.TheTarget->getProgramAddressSpace()
      ),
      2
    );
  } else {
    PN = Context.TheBuilder->CreatePHI(IfExpr->ExprType->getType(), 2);
  }

  PN->addIncoming(thenValue, thenBB);
  PN->addIncoming(elseValue, elseBB);
  
  return PN;
}

Value* CondExprAST::getLValue(SLContext& Context) {
  return generateCondExpr(Context, Cond, IfExpr, ElseExpr, true);
}

Value* CondExprAST::getRValue(SLContext& Context) {
  return generateCondExpr(Context, Cond, IfExpr, ElseExpr, false);
}

Value* CallExprAST::getRValue(SLContext& Context) {
  // Generate code for callee (actually only get function's address)
  Value* callee = nullptr;
  std::vector< Value* > args;
  ExprList::iterator it = Args.begin();

  // Check is it virtual call or not
  assert(isa<FuncDeclAST>(CallFunc));
  FuncDeclAST* funcDecl = (FuncDeclAST*)CallFunc;
  Type* funcRawType = CallFunc->getType()->getType();
  assert(isa<FunctionType>(funcRawType));
  FunctionType* funcType = static_cast<FunctionType*>(funcRawType);

  // It's regular function call
  callee = CallFunc->getValue(Context);

  // Perform code generation for every function parameter
  for (ExprList::iterator end = Args.end(); it != end; ++it) {
    Value* v = (*it)->getRValue(Context);
    args.push_back(v);
  }
  
  // Generate function's call
  return Context.TheBuilder->CreateCall(funcType, callee, args);
}

} // namespace simple