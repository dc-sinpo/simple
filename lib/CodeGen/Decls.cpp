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


  // If we have initialization we should generate code for it
  if (Val) {
    Value* init = Val->getRValue(Context);

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
    // Generate only mangle name
    output << "_P" << Id->Length << StringRef(Id->Id, Id->Length);
    ThisType->toMangleBuffer(output);
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

  Function* oldFunction = Context.TheFunction;
  Context.TheFunction = CodeValue;

  // Generate code for function's body
  Body->generateCode(Context);
 
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