#include "simple/AST/AST.h"

using namespace llvm;

cl::opt< std::string > OutputFilename("o", cl::desc("Specify output filename"), 
  cl::value_desc("filename"));

namespace simple {

Value* SymbolAST::getValue(SLContext& ) {
  assert(0 && "SymbolAST::getValue should never be reached");
  return nullptr;
}

Value* SymbolAST::generateCode(SLContext& Context) {
  assert(0 && "SymbolAST::generateCode should never be reached");
  return nullptr;
}

Value* VarDeclAST::generateCode(SLContext& Context) {
  assert(SemaState >= 5);
  // Get address of variable and generate code for an initialization
  Value* val = getValue(Context);

  // Special case for class variables with constructors
  if (Val && isa<ClassTypeAST>(ThisType)) {
    Val->getRValue(Context);
    return val;
  }

  // If we have initialization we should generate code for it
  if (Val) {
    Value* init;

    // Check for pointer
    if (isa<PointerTypeAST>(ThisType)) {
      // For integral constant generate null
      if (Val->isIntConst()) {
        init = ConstantPointerNull::get((PointerType*)ThisType->getType());
      } else {
        if (isa<ArrayTypeAST>(Val->ExprType)) {
          init = Val->getLValue(Context);
          Value* tmp = getConstInt(0);

          std::vector< Value* > idx;

          idx.push_back(tmp);
          idx.push_back(tmp);

          if (isa<AllocaInst>(init)) {
            AllocaInst *alloca = (AllocaInst*)init;

            init = Context.TheBuilder->CreateGEP(alloca->getAllocatedType(), init, idx);
          } else {
            assert(0);
          }
        } else {
          init = Val->getRValue(Context);
        }
      }
    } else {
      // Otherwise generate code for initialization
      init = Val->getRValue(Context);
    }

    // Create store instruction
    return Context.TheBuilder->CreateStore(init, val);
  }

  return val;
}

Value* VarDeclAST::getValue(SLContext& Context) {
  // We should create value only once
  if (CodeValue) {
    return CodeValue;
  }

  // Create alloca instruction for this variable
  CodeValue = Context.TheBuilder->CreateAlloca(ThisType->getType(),
    nullptr, StringRef(Id->Id, Id->Length));
  return CodeValue;
}

Value* StructDeclAST::getValue(SLContext& Context) {
  // Make sure that type was generated
  ThisType->getType();
  return nullptr;
}

Value* StructDeclAST::generateCode(SLContext& Context) {
  assert(SemaState >= 5);
  // Make sure that type was generated
  ThisType->getType();
  return nullptr;
}

Value* VTableAST::generateCode(SLContext& Context) {
  if (CodeValue) {
    return CodeValue;
  }

  // Allocate virtual table (all slots will be filled later)
  std::vector< Constant* > vtblEntries(CurOffset);

  // Check every function and overload set in the virtual table
  for (SymbolMap::iterator it = Decls.begin(), end = Decls.end();
    it != end; ++it) {
    if (isa<OverloadSetAST>(it->second)) {
      // It's overload set
      OverloadSetAST* overloadSet = (OverloadSetAST*)it->second;

      // Check every function in the overload set
      for (SymbolList::iterator it2 = overloadSet->Vars.begin(),
        end2 = overloadSet->Vars.end(); it2 != end2; ++it2) {
        FuncDeclAST* fnc = (FuncDeclAST*)*it2;
        // Add function to the appropriate slot
        vtblEntries[fnc->OffsetOf] = ConstantExpr::getBitCast(
          (Function*)fnc->getValue(Context),
          PointerType::get(
            getGlobalContext(),
            getSLContext().TheTarget->getProgramAddressSpace()
          )
        );
      }
    } else {
      // It's function
      FuncDeclAST* fnc = (FuncDeclAST*)it->second;
      // Add function to the appropriate slot
      vtblEntries[fnc->OffsetOf] = ConstantExpr::getBitCast(
        (Function*)fnc->getValue(Context),
        PointerType::get(
          getGlobalContext(),
          getSLContext().TheTarget->getProgramAddressSpace()
        )
      );
    }
  }

  llvm::SmallString< 128 > s;
  llvm::raw_svector_ostream output(s);

  // Generate name for virtual function's table
  output << "_PTV";
  mangleAggregateName(output, Parent);

  // Create array of int8*
  ArrayType* tableType = ArrayType::get(
    PointerType::get(
      getGlobalContext(),
      getSLContext().TheTarget->getProgramAddressSpace()
    ),
    CurOffset
  );
  // Insert virtual table value to the global list of variables
  GlobalVariable* val = (GlobalVariable*)Context.TheModule->getOrInsertGlobal(
    output.str(), tableType);
  // Set virtual function's table data
  val->setInitializer(ConstantArray::get(tableType, vtblEntries));
  VTblType = tableType;

  return CodeValue = val;
}


llvm::Value* ClassDeclAST::getValue(SLContext& Context) {
  ThisType->getType();

  if (BaseClass) {
    BaseClass->getType();
  }

  return nullptr;
}

llvm::Value* ClassDeclAST::generateCode(SLContext& Context) {
  assert(SemaState >= 5);
  getValue(Context);

  // Generate code for VTable if needed
  if (VTbl) {
    VTbl->generateCode(Context);
  }

  // Generate code for every struct/class or function declaration in the class
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    if (!isa<VarDeclAST>(*it)) {
      (*it)->generateCode(Context);
    }
  }

  return nullptr;
}

llvm::Value* ParameterSymbolAST::getValue(SLContext& Context) {
  // We should create value only once
  if (Param->CodeValue) {
    return Param->CodeValue;
  }

  // Create alloca instruction for this parameter
  Param->CodeValue = Context.TheBuilder->CreateAlloca(Param->Param->getType());
  return Param->CodeValue;
}

llvm::Value* ParameterSymbolAST::generateCode(SLContext& Context) {
  assert(SemaState >= 5);
  // We need only return value of this parameter other actions will perform
  // FuncDeclAST code generation
  return getValue(Context);
}

Value* FuncDeclAST::getValue(SLContext& Context) {
  // We should create value only once
  if (CodeValue) {
    return CodeValue;
  }

  SmallString< 128 > str;
  raw_svector_ostream output(str);

  // Generate function name (we have special case for main)
  if (!(Id->Length == 4 && memcmp(Id->Id, "main", 4) == 0)) {
    if (needThis()) {
      // For aggregate members we should generate fully qualified name
      assert(AggregateSym);
      output << "_P";
      mangleAggregateName(output, AggregateSym);
      output << Id->Length << StringRef(Id->Id, Id->Length);
      ThisType->toMangleBuffer(output);
    } else {
      // Generate only mangle name
      output << "_P" << Id->Length << StringRef(Id->Id, Id->Length);
      ThisType->toMangleBuffer(output);
    }
  } else {
    output << "main";
  }

  // Create function with external linkage for this declaration
  CodeValue = Function::Create((FunctionType*)ThisType->getType(), 
    Function::ExternalLinkage, output.str(), nullptr);
  Context.TheModule->getFunctionList().push_back(CodeValue);

  return CodeValue;
}

Value* FuncDeclAST::generateCode(SLContext& Context) {
  if (Compiled) {
    return CodeValue;
  }

  assert(SemaState >= 5);
  // Get value for function declaration
  getValue(Context);

  BasicBlock* oldBlock = Context.TheBuilder->GetInsertBlock();

  Function::arg_iterator AI = CodeValue->arg_begin();
  ParameterList::iterator PI = ((FuncTypeAST*)ThisType)->Params.begin();
  ParameterList::iterator PE = ((FuncTypeAST*)ThisType)->Params.end();

  // Create entry block for function and set it as insert point
  BasicBlock* BB = BasicBlock::Create(getGlobalContext(), "entry", CodeValue);
  Context.TheBuilder->SetInsertPoint(BB);

  // We need create alloca instructions for every variable declared in the 
  // function
  for (std::vector< SymbolAST* >::iterator it = FuncVars.begin(), end = FuncVars.end();
    it != end; ++it) {
    (*it)->getValue(Context);
  }

  // Check all function parameters
  for ( ; PI != PE; ++PI, ++AI) {
    ParameterAST* p = *PI;

    // We need certain actions for named parameter
    if (p->Id) {
      // Set it's name
      AI->setName(StringRef(p->Id->Id, p->Id->Length));
      // Generate alloca instruction (if needed) and store value passed to 
      // function to named value
      if (!p->CodeValue) {
        p->CodeValue = Context.TheBuilder->CreateAlloca(p->Param->getType());
      }

      Context.TheBuilder->CreateStore(AI, p->CodeValue);
    }
  }

  // If function has return type we should add variable which will hold it's
  // value
  if (!ReturnType->isVoid()) {
    LandingPad->ReturnValue = Context.TheBuilder->CreateAlloca(
      ReturnType->getType(),
      nullptr,
      "return.value"
    );
  }

  // Create return location and set fall through location
  LandingPad->ReturnLoc = BasicBlock::Create(getGlobalContext(), "return.block");
  LandingPad->FallthroughLoc = LandingPad->ReturnLoc;

  // Create cleanup variable if needed
  if (LandingPad->NeedCleanup) {
    LandingPad->CleanupValue = Context.TheBuilder->CreateAlloca(
      Type::getInt32Ty(getGlobalContext()),
      0U,
      "cleanup.value"
    );
  }

  Function* oldFunction = Context.TheFunction;
  Context.TheFunction = CodeValue;

  if (isCtor()) {
    // We have special case for constructor
    assert(isa<BlockStmtAST>(Body));
    assert(isa<ClassDeclAST>(Parent));
    BlockStmtAST* blockStmt = (BlockStmtAST*)Body;
    ClassDeclAST* classDecl = (ClassDeclAST*)Parent;
    VTableAST* parentVtbl = nullptr;

    StmtList::iterator it = blockStmt->Body.begin();
    StmtList::iterator end = blockStmt->Body.end();

    // If we have base class and it's class and not structure then we probably
    // have virtual functions table
    if (classDecl->BaseClass) {
      // 1st instruction is super variable declaration
      (*it)->generateCode(Context);
      ++it;
      
      if (isa<ClassTypeAST>(classDecl->BaseClass)) {
        ClassDeclAST* baseDecl = (ClassDeclAST*)classDecl->BaseClass->getSymbol();
        parentVtbl = baseDecl->VTbl;

        // Base class can have no constructor. Check it
        if (baseDecl->Ctor) {
          // We have. Generate code for it's call and advance to next instruction
          (*it)->generateCode(Context);
          ++it;
        }
      }
    }

    // If our virtual table not equal to base class virtual table then we should
    // generate code for it
    if (classDecl->VTbl != parentVtbl) {
      SymbolAST* thisParam = find(Name::This);
      assert(thisParam != nullptr);
      // Generate code for this parameter and load it
      Value* val = thisParam->getValue(Context);

      val = Context.TheBuilder->CreateLoad(
        PointerType::get(
          getGlobalContext(),
          getSLContext().TheTarget->getProgramAddressSpace()
        ),
        val
      );
      
      // Generate code for virtual function's table
      Value* vtblVal = classDecl->VTbl->generateCode(Context);

      std::vector< Value* > idx;

      idx.push_back(getConstInt(0));
      idx.push_back(getConstInt(0));

      // Generate GetElementPtr instruction to get virtual table from globals
      vtblVal = Context.TheBuilder->CreateInBoundsGEP(classDecl->VTbl->VTblType, vtblVal, idx);
      // Store virtual table in the class instance
      val = Context.TheBuilder->CreateStore(vtblVal, val);
    }

    // Generate code for rest of the function
    blockStmt->generatePartialCode(Context, it, end);
  } else if (isDtor()) {
    // We have special case for destructor
    assert(isa<BlockStmtAST>(Body));
    assert(isa<ClassDeclAST>(Parent));
    BlockStmtAST* blockStmt = (BlockStmtAST*)Body;
    ClassDeclAST* classDecl = (ClassDeclAST*)Parent;

    // If we have base class we should check for call of it's destructor
    if (classDecl->BaseClass && isa<ClassTypeAST>(classDecl->BaseClass)) {
      ClassDeclAST* baseDecl = (ClassDeclAST*)classDecl->BaseClass->getSymbol();

      // We have special case if we need base class destructor call
      if (baseDecl->Dtor) {
        StmtList::iterator it = blockStmt->Body.begin();
        StmtList::iterator end = blockStmt->Body.end();
        --end;

        // For destructor we should create new fall through location because
        // it can be used during generatePartialCode call
        BasicBlock* falthroughBB = BasicBlock::Create(getGlobalContext());
        LandingPad->FallthroughLoc = falthroughBB;

        // Generate code for all members but last
        blockStmt->generatePartialCode(Context, it, end);

        // Add or delete fall through block
        if (falthroughBB->hasNUsesOrMore(1)) {
          CodeValue->getBasicBlockList().push_back(falthroughBB);
          Context.TheBuilder->SetInsertPoint(falthroughBB);
        } else {
          delete falthroughBB;
        }

        // Set fall through location back to return location
        LandingPad->FallthroughLoc = LandingPad->ReturnLoc;

        // Current instruction should be destructor call. Before it's call
        // we should adjust VTbl (we should make sure that they are different,
        // but if base class doesn't have VTbl do nothing)
        if (classDecl->VTbl != baseDecl->VTbl && baseDecl->VTbl) {
          SymbolAST* thisParam = find(Name::This);
          assert(!thisParam);
          // Generate code for this parameter and load it
          Value* val = thisParam->getValue(Context);
          val = Context.TheBuilder->CreateLoad(
            PointerType::get(
              getGlobalContext(),
              getSLContext().TheTarget->getProgramAddressSpace()
            ),
            val
          );
          // Generate code for base class virtual function's table
          Value* vtblVal = baseDecl->VTbl->generateCode(Context);

          std::vector< Value* > idx;

          idx.push_back(getConstInt(0));
          idx.push_back(getConstInt(0));

          // Generate GetElementPtr instruction to get virtual table from globals
          vtblVal = Context.TheBuilder->CreateInBoundsGEP(baseDecl->VTbl->VTblType, vtblVal, idx);
          // Store virtual table in the class instance
          val = Context.TheBuilder->CreateStore(vtblVal, val);
        }

        // Generate code for base class destructor's call
        (*end)->generateCode(Context);
      } else {
        // We don't need any special handling here
        Body->generateCode(Context);
      }
    } else {
      // We don't need any special handling here
      Body->generateCode(Context);
    }
  } else {
    // Generate code for function's body
    Body->generateCode(Context);
  }
 
  Context.TheFunction = oldFunction;

  // Add return if there was no generated code for return yet
  if (!Context.TheBuilder->GetInsertBlock()->getTerminator()) {
    Context.TheBuilder->CreateBr(LandingPad->ReturnLoc);
  }

  // Add return block to the end of the function and set it as insert point
  CodeValue->getBasicBlockList().push_back(LandingPad->ReturnLoc);
  Context.TheBuilder->SetInsertPoint(LandingPad->ReturnLoc);
  
  // Generate load from return value if we have return value
  if (!ReturnType->isVoid()) {
    Value* ret = Context.TheBuilder->CreateLoad(
      ReturnType->getType(),
      LandingPad->ReturnValue
    );
    Context.TheBuilder->CreateRet(ret);
  } else {
    // Return void
    Context.TheBuilder->CreateRetVoid();
  }

  // Restore old insert point if needed
  if (oldBlock) {
    Context.TheBuilder->SetInsertPoint(oldBlock);
  }

  Function::BasicBlockListType &blocksList = CodeValue->getBasicBlockList();

  for (Function::BasicBlockListType::iterator it = blocksList.begin(), lastBlock = blocksList.end();
    it != lastBlock; ) {
    if (!it->getTerminator()) {
      Function::BasicBlockListType::iterator cur = it;

      ++it;

      blocksList.erase(cur);
    } else {
      ++it;
    }
  }

  // Verify function's body and run optimization
  verifyFunction(*CodeValue, &llvm::errs());
  
  Compiled = true;

  return CodeValue;
}

void ModuleDeclAST::generateCode() {
  SLContext& Context = getSLContext();
  
  // Generate code for every declaration
  for (SymbolList::iterator it = Members.begin(), end = Members.end();
    it != end; ++it) {
    (*it)->generateCode(Context);
  }

  Context.TheModule->dump();

  if (!OutputFilename.empty()) {
    std::error_code errorInfo;
    raw_fd_ostream fd(OutputFilename.c_str(), errorInfo);

    if (errorInfo) {
      llvm::errs() << "Can't write to \"" << OutputFilename.c_str() << "\" file\n";
    }

    // Print module's content
    Context.TheModule->print(fd, 0);
  }

  MainPtr = (double (*)())(intptr_t)getJITMain();
}

} // namespace simple