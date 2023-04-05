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

  if (type->isInt() || type->isChar()) {
    // Convert integral value to bool by comparison with 0
    return builder.CreateICmpNE(val, ConstantInt::get(type->getType(), 0));
  } else if (isa<PointerTypeAST>(type) || type->isString()) {
    // Convert pointer or string value to bool by comparison with null
    return builder.CreateICmpNE(val, 
      ConstantPointerNull::get((PointerType*)type->getType()));
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

Value* StringExprAST::getRValue(SLContext& Context) {
  // For string literal we need:
  // 1. Create global string;
  // 2. Create GetElementPtr instruction to get just created value from global
  //   variable
  GlobalValue* val = Context.TheBuilder->CreateGlobalString(Val);
  std::vector< Value* > idx;

  idx.push_back(getConstInt(0));
  idx.push_back(getConstInt(0));

  return Context.TheBuilder->CreateInBoundsGEP(val->getValueType(), val, idx);
}

Value* ProxyExprAST::getRValue(SLContext& Context) {
  return OriginalExpr->getRValue(Context);
}

Value* NewExprAST::getRValue(SLContext& Context) {
  // We need to get size of type to allocate and patch old 0 value with actual
  // size
  uint64_t allocSize = Context.TheTarget->getTypeAllocSize(NewType->getType());
  SizeExpr->Val = (int)allocSize;
  // Generate code for call expression
  Value* val = CallExpr->getRValue(Context);

  if (NeedCast) {
    // It wasn't constructor call and we should create cast to resulted type
    return Context.TheBuilder->CreateBitCast(val, ExprType->getType());
  }
  
  return val;
}

Value* DeleteExprAST::getRValue(SLContext& Context) {
  Value* val = DeleteCall->getRValue(Context);
  return val;
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

Value* IndexExprAST::getLValue(SLContext& Context) {
  // We have 2 different implementations for array and pointer
  if (isa<ArrayTypeAST>(Left->ExprType)) {
    // We should get array as lvalue
    Value* val = Left->getLValue(Context);
    Value* index = Right->getRValue(Context);

    std::vector< Value* > idx;

    idx.push_back(getConstInt(0));
    idx.push_back(index);

    return Context.TheBuilder->CreateInBoundsGEP(Left->ExprType->getType(), val, idx);
  } else {
    // We should get pointer as rvalue
    Value* val = Left->getRValue(Context);
    Value* index = Right->getRValue(Context);

    std::vector< Value* > idx;

    idx.push_back(index);

    return Context.TheBuilder->CreateInBoundsGEP(ExprType->getType(), val, idx);
  }
}

Value* IndexExprAST::getRValue(SLContext& Context) {
  Value* val = getLValue(Context);
  return Context.TheBuilder->CreateLoad(ExprType->getType(), val);
}

/// Convert pointer to class to pointer to base class
/// \param[in] Context - code generator context
/// \param[in] val - pointer to convert
/// \param[in] var - symbol of aggregate to convert to
/// \param[in] agg - symbol of aggregate to convert from
/// \param[in] canBeNull - true - value \c val can contain null
Value* convertToBase(SLContext& Context, Value* val, SymbolAST* var, 
  SymbolAST* agg, bool canBeNull) {
  ClassDeclAST* thisDecl = (ClassDeclAST*)agg;
  SymbolAST* baseDecl = thisDecl->BaseClass->getSymbol();
  bool baseHasVTable = false;

  // If base class is class and not struct then it can have VTable
  if (isa<ClassDeclAST>(baseDecl)) {
    ClassDeclAST* baseClassDecl = (ClassDeclAST*)baseDecl;
    baseHasVTable = baseClassDecl->VTbl != nullptr;
  }
  
  // If base class don't have VTable and this class has then we should make
  // adjustments
  if (thisDecl->VTbl && baseHasVTable == false) {
    // If value can be 0 then we should have some checking before cast
    if (canBeNull) {
      // Right now val is the pointer to some derived class. We should check
      // it for null for safe cast
      Value* tmp = ConstantPointerNull::get((PointerType*)val->getType());
      tmp = Context.TheBuilder->CreateICmpNE(val, tmp);

      BasicBlock* thenBlock = BasicBlock::Create(getGlobalContext(), "then",
        Context.TheFunction);
      BasicBlock* elseBlock = BasicBlock::Create(getGlobalContext(), "else");
      BasicBlock* contBlock = BasicBlock::Create(getGlobalContext(), "ifcont");

      tmp = Context.TheBuilder->CreateCondBr(tmp, thenBlock, elseBlock);
      Context.TheBuilder->SetInsertPoint(thenBlock);

      // 1. Adjust pointer and skip VTable slot
      val = Context.TheBuilder->CreateGEP(
        Type::getInt8Ty(getGlobalContext()),
        val,
        getConstInt(
          Context.TheTarget->getTypeAllocSize(
            PointerType::get(
              getGlobalContext(),
              getSLContext().TheTarget->getProgramAddressSpace()
            )
          )
        )
      );

      // 2. Get real address of base class
      if (var != baseDecl) {
        // It has 1 more nesting level. Check it
        val = convertToBase(Context, val, var, baseDecl, canBeNull);
      }

      thenBlock = Context.TheBuilder->GetInsertBlock();
      Context.TheBuilder->CreateBr(contBlock);

      // Note: elseBlock contain only jump and no code
      Context.TheFunction->getBasicBlockList().push_back(elseBlock);
      Context.TheBuilder->SetInsertPoint(elseBlock);

      Context.TheBuilder->CreateBr(contBlock);

      Context.TheFunction->getBasicBlockList().push_back(contBlock);
      Context.TheBuilder->SetInsertPoint(contBlock);

      // Create PHI node with resulted value
      PHINode* PN = Context.TheBuilder->CreatePHI(val->getType(), 2);

      PN->addIncoming(val, thenBlock);
      PN->addIncoming(ConstantPointerNull::get((PointerType*)val->getType()), 
        elseBlock);

      return PN;
    } else {
      // Value can't be 0. We don't need any checking

      // 1. Adjust pointer and skip VTable slot
      val = Context.TheBuilder->CreateGEP(
        Type::getInt8Ty(getGlobalContext()),
        val,
        getConstInt(
          Context.TheTarget->getTypeAllocSize(
            PointerType::get(
              getGlobalContext(),
              getSLContext().TheTarget->getProgramAddressSpace()
            )
          )
        )
      );

      // 2. Get real address of base class
      if (var != baseDecl) {
        // It has 1 more nesting level. Check it
        return convertToBase(Context, val, var, baseDecl, canBeNull);
      }

      return val;
    }
  } else {
    // There is no VTable or both types have VTables

    if (var == baseDecl) {
      return val;
    }

    // It has 1 more nesting level. Check it
    return convertToBase(Context, val, var, baseDecl, canBeNull);
  }
}

Value* MemberAccessExprAST::getLValue(SLContext& Context) {
  // For function simply return value
  if (isa<FuncTypeAST>(ExprType)) {
    return ThisSym->getValue(Context);
  }

  // Generate lvalue for this
  Value* val = Val->getLValue(Context);

  // If symbol's parent not found aggregate then we need convert it to base
  // class/struct
  if (ThisSym->Parent != ThisAggr) {
    val = convertToBase(Context, val, ThisSym->Parent, ThisAggr, false);
  }

  // Generate code for member's offset
  Value* index = getConstInt(((VarDeclAST*)ThisSym)->OffsetOf);

  std::vector< Value* > idx;

  idx.push_back(getConstInt(0));
  idx.push_back(index);

  // Generate GetElementPtr for needed value
  return Context.TheBuilder->CreateGEP(ThisSym->Parent->getType()->getType(), val, idx);
}

Value* MemberAccessExprAST::getRValue(SLContext& Context) {
  // Generate lvalue for this
  Value* val = getLValue(Context);

  // For function simply return value
  if (isa<FuncTypeAST>(ExprType)) {
    return val;
  }

  // For rvalue we always need to load just generated value
  return Context.TheBuilder->CreateLoad(ExprType->getType(), val);
}

Value* PointerAccessExprAST::getLValue(SLContext& Context) {
  assert(0 && "PointerAccessExprAST::getLValue should never be reached");
  return nullptr;
}

Value* PointerAccessExprAST::getRValue(SLContext& Context) {
  assert(0 && "PointerAccessExprAST::getRValue should never be reached");
  return nullptr;
}

Value* DerefExprAST::getLValue(SLContext& Context) {
  // Check is it lvalue or not
  if (Val->isLValue()) {
    Value* val = Val->getLValue(Context);
    return Context.TheBuilder->CreateLoad(
      PointerType::get(
        getGlobalContext(),
        Context.TheTarget->getProgramAddressSpace()
      ),
      val
    );
  } else {
    // Special case for function's call and binary expression
    return Val->getRValue(Context); 
  }
}

Value* DerefExprAST::getRValue(SLContext& Context) {
  Value* val = getLValue(Context);

  // If it's function return it's value
  if (isa<FuncTypeAST>(ExprType)) {
    return val;
  }

  // For rvalue we always need to generate load
  return Context.TheBuilder->CreateLoad(ExprType->getType(), val);
}

Value* AddressOfExprAST::getRValue(SLContext& Context) {
  // For address expression rvalue use lvalue of nested expression
  // Note: We can't have lvalue
  return Val->getLValue(Context);
}

Value* CastExprAST::getRValue(SLContext& Context) {
  // Check convert to int first
  if (ExprType->isInt()) {
    if (Val->ExprType->isBool() || Val->ExprType->isChar()) {
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
  } else if (ExprType->isChar()) {
    // If it's bool then extend it
    if (Val->ExprType->isBool()) {
      return Context.TheBuilder->CreateZExt(Val->getRValue(Context),
        ExprType->getType());
    }

    assert(Val->ExprType->isInt());
    // Truncate integral value
    return Context.TheBuilder->CreateTrunc(Val->getRValue(Context),
      ExprType->getType());
  // Check for conversion to string
  } else if (ExprType->isString()) {
    if (isa<ArrayTypeAST>(Val->ExprType)) {
      Value* val = Val->getLValue(Context);
      Value* tmp = getConstInt(0);

      std::vector< Value* > idx;

      idx.push_back(tmp);
      idx.push_back(tmp);

      if (isa<AllocaInst>(val)) {
        AllocaInst *alloca = (AllocaInst*)val;

        return Context.TheBuilder->CreateGEP(alloca->getAllocatedType(), val, idx);
      } else {
        assert(0);
      }
    } else {
      Value* val = Val->getRValue(Context);
      // Generate cast
      return Context.TheBuilder->CreateBitCast(val, ExprType->getType());
    }
  // Check for conversion to pointer
  } else if (isa<PointerTypeAST>(ExprType)) {
    // If converted expression is pointer too then check it
    if (isa<PointerTypeAST>(Val->ExprType)) {
      // Generate rvalue expression
      Value* val = Val->getRValue(Context);

      TypeAST* next1 = ((PointerTypeAST*)ExprType)->Next;
      TypeAST* next2 = ((PointerTypeAST*)Val->ExprType)->Next;

      // We have special case for aggregates and their base classes
      if (next1->isAggregate() && next1->isBaseOf(next2)) {
        SymbolAST* s1 = next1->getSymbol();
        SymbolAST* s2 = next2->getSymbol();

        return convertToBase(Context, val, s1, s2, Val->canBeNull());
      }
      
      return val;
    } else if (isa<ArrayTypeAST>(Val->ExprType)) {
      Value* val = Val->getLValue(Context);
      Value* tmp = getConstInt(0);

      std::vector< Value* > idx;

      idx.push_back(tmp);
      idx.push_back(tmp);

      if (isa<AllocaInst>(val)) {
        AllocaInst *alloca = (AllocaInst*)val;

        tmp = Context.TheBuilder->CreateGEP(alloca->getAllocatedType(), val, idx);
      } else {
        assert(0);
      }

      PointerTypeAST* ptrType = (PointerTypeAST*)ExprType;

      if (ptrType->Next->isVoid()) {
        tmp = Context.TheBuilder->CreateBitCast(tmp, ptrType->getType());
      }

      return tmp;
    } else {
      Value* val = Val->getRValue(Context);
      // Generate cast
      return Context.TheBuilder->CreateBitCast(val, ExprType->getType());
    }
  } else if (Val->ExprType->isInt() || Val->ExprType->isChar()) {
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
  assert(Op == tok::PlusPlus || Op == tok::MinusMinus);
  assert(isa<IndexExprAST>(Val) || isa<MemberAccessExprAST>(Val) || ExprType->isString());
  // Generate code for lvalue
  Value* val = Val->getLValue(Context);
  // Generate load with add 1 or add -1
  Value* res = Context.TheBuilder->CreateLoad(ExprType->getType(), val);
  Value* tmp = getConstInt((Op == tok::PlusPlus) ? 1ULL : ~0ULL);

  if (isa<PointerTypeAST>(ExprType) || ExprType->isString()) {
    Type *resType;

    if (isa<PointerTypeAST>(ExprType)) {
      PointerTypeAST *ptrType = (PointerTypeAST*)ExprType;

      if (ptrType->Next->isVoid()) {
        resType = Type::getInt8Ty(getGlobalContext());
      } else {
        resType = ptrType->Next->getType();
      }
    } else {
      resType = Type::getInt8Ty(getGlobalContext());
    }

    // Special case for pointers
    res = Context.TheBuilder->CreateGEP(resType, res, tmp);
  } else {
    res = Context.TheBuilder->CreateAdd(res, tmp);
  }

  // Store resulted value back
  Context.TheBuilder->CreateStore(res, val);
  return res;
}

Value* BinaryExprAST::getRValue(SLContext& Context) {
  // Check all special case binary expressions 1st

  // = operator
  if (Op == tok::Assign) {
    
    // Generate code for right operand of the expression
    Value* right;
    
    // We have special case for pointer
    if (isa<PointerTypeAST>(ExprType)) {
      // If it's integral constant 0 then we should convert it to null
      if (RightExpr->ExprType->isInt()) {
        right = ConstantPointerNull::get((PointerType*)ExprType->getType());
      } else {
        // Generate rvalue otherwise
        right = RightExpr->getRValue(Context);
      }
    } else {
      // Generate rvalue
      right = RightExpr->getRValue(Context);
    }

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
    Value* val = nullptr;

    if (isa<IndexExprAST>(LeftExpr) || isa<MemberAccessExprAST>(LeftExpr)) {
      // Special case for indexing and member access expressions, because we
      // don't want to generate GetElementPtr instruction twice
      val = Context.TheBuilder->CreateLoad(ExprType->getType(), var);
    } else {
      val = LeftExpr->getRValue(Context);
    }

    if (!LeftExpr->ExprType->isInt()) {
      // Special case for pointer types. We should generate GetElementPtr
      // instruction
      Value* tmp = getConstInt((Op == tok::PlusPlus) ? 1ULL : ~0ULL);
      Type *resType;

      if (isa<PointerTypeAST>(ExprType)) {
        PointerTypeAST *ptrType = (PointerTypeAST*)ExprType;

        if (ptrType->Next->isVoid()) {
          resType = Type::getInt8Ty(getGlobalContext());
        } else {
          resType = ptrType->Next->getType();
        }
      } else {
        resType = Type::getInt8Ty(getGlobalContext());
      }

      tmp = Context.TheBuilder->CreateGEP(resType, val, tmp);
      // Store result and return old value
      Context.TheBuilder->CreateStore(tmp, var);
      return val;
    } else {
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

  // Special case for +/- on pointers
  if (isa<PointerTypeAST>(ExprType)) {
    Value* ptr = LeftExpr->getRValue(Context);
    Value* index = RightExpr->getRValue(Context);

    if (Op == tok::Minus) {
      index = Context.TheBuilder->CreateSub(getConstInt(0), index);
    }

    PointerTypeAST *ptrType = (PointerTypeAST*)ExprType;
    Type *resType = ptrType->Next->isVoid()
      ? Type::getInt8Ty(getGlobalContext())
      : ptrType->Next->getType();

    return Context.TheBuilder->CreateGEP(resType, ptr, index);
  }

  // It's regular binary operator

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
  Value* forcedReturn = nullptr;

  // Check is it virtual call or not
  assert(isa<FuncDeclAST>(CallFunc));
  FuncDeclAST* funcDecl = (FuncDeclAST*)CallFunc;
  Type* funcRawType = CallFunc->getType()->getType();
  assert(isa<FunctionType>(funcRawType));
  FunctionType* funcType = static_cast<FunctionType*>(funcRawType);

  if (funcDecl->isVirtual() || funcDecl->isOverride()) {
    // It's virtual function perform virtual call
    assert(isa<MemberAccessExprAST>(Callee));
    MemberAccessExprAST* memAccess = (MemberAccessExprAST*)Callee;

    // We should check is Callee pointer or not
    if (isa<DerefExprAST>(memAccess->Val)) {
      // It's dereference. Now we should check was it super or not. Because we
      // should make sure that super not use virtual calls
      DerefExprAST* derefExpr = (DerefExprAST*)memAccess->Val;

      if ((isa<IdExprAST>(derefExpr->Val) && 
        ((IdExprAST*)derefExpr->Val)->Val == Name::Super)) {
        // It's call with super as this parameter. Call it as non virtual function
        callee = CallFunc->getValue(Context);
      } else {
        // It's virtual function. Make virtual call

        // Get this pointer
        callee = memAccess->Val->getLValue(Context);

        // Virtual functions table is always 1st member. But we still need cast
        // this to class with function
        PointerType* ptrType = PointerType::get(
          getGlobalContext(),
          getSLContext().TheTarget->getProgramAddressSpace()
        );

        // Load virtual functions table
        callee = Context.TheBuilder->CreateLoad(ptrType, callee);
        // Get function from virtual functions table by it's index
        callee = Context.TheBuilder->CreateGEP(
          ptrType,
          callee,
          getConstInt(((FuncDeclAST*)CallFunc)->OffsetOf)
        );
        callee = Context.TheBuilder->CreateLoad(ptrType, callee);
      }
    } else {
      if (memAccess->ForceThis) {
        // It's virtual function. Make virtual call

        // Get this pointer
        callee = memAccess->Val->getRValue(Context);

        // Virtual functions table is always 1st member. But we still need cast
        // this to class with function
        PointerType* ptrType = PointerType::get(
          getGlobalContext(),
          getSLContext().TheTarget->getProgramAddressSpace()
        );

        // Load virtual functions table
        callee = Context.TheBuilder->CreateLoad(ptrType, callee);
        // Get function from virtual functions table by it's index
        callee = Context.TheBuilder->CreateGEP(
          ptrType,
          callee,
          getConstInt(((FuncDeclAST*)CallFunc)->OffsetOf)
        );
        callee = Context.TheBuilder->CreateLoad(ptrType, callee);
      } else {
        // It's not pointer. Use regular function call
        callee = CallFunc->getValue(Context);
      }
    }
  } else {
    // It's regular function call
    callee = CallFunc->getValue(Context);
  }
  
  // We have special case for class member function
  if (isa<MemberAccessExprAST>(Callee)) {
    MemberAccessExprAST* memAccess = (MemberAccessExprAST*)Callee;
    // Check for forced this
    if (!memAccess->ForceThis) {
      // It's not forced this. Get lvalue from left part of member access
      // expression
      Value* val = (*it)->getLValue(Context);
      MemberAccessExprAST* memAccess = (MemberAccessExprAST*)Callee;

      // Perform conversion from derived class to base class if needed
      if (memAccess->ThisSym->Parent != memAccess->ThisAggr) {
        val = convertToBase(Context, val, memAccess->ThisSym->Parent,
          memAccess->ThisAggr, false);
      }

      // Add this parameter
      args.push_back(val);
      ++it;
    } else {
      // Special case for member access with forced this
      MemberAccessExprAST* memAccess = (MemberAccessExprAST*)Callee;
      assert(memAccess->ThisSym->Parent == memAccess->ThisAggr);
      forcedReturn = (*it)->getRValue(Context);

      // Add this parameter
      args.push_back(forcedReturn);
      ++it;
    }
  }

  // Perform code generation for every function parameter
  for (ExprList::iterator end = Args.end(); it != end; ++it) {
    Value* v = (*it)->getRValue(Context);
    args.push_back(v);
  }

  if (forcedReturn) {
    // It's forced this. Create call and return calculated return value
    Context.TheBuilder->CreateCall(funcType, callee, args);
    return forcedReturn;
  } 
  
  // Generate function's call
  return Context.TheBuilder->CreateCall(funcType, callee, args);
}

} // namespace simple