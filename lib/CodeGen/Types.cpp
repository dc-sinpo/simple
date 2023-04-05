#include "simple/AST/AST.h"

using namespace llvm;

namespace simple {

Type* BuiltinTypeAST::getType() {
  static Type* builtinTypes[] = {
    Type::getVoidTy(getGlobalContext()),
    Type::getInt1Ty(getGlobalContext()),
    Type::getInt32Ty(getGlobalContext()),
    Type::getDoubleTy(getGlobalContext()),
    Type::getInt8Ty(getGlobalContext()),
    PointerType::get(
      getGlobalContext(),
      getSLContext().TheTarget->getProgramAddressSpace()
    )
  };

  if (ThisType) {
    return ThisType;
  }

  return ThisType = builtinTypes[TypeKind];
}

Type* ArrayTypeAST::getType() {
  if (ThisType) {
    return ThisType;
  }

  return ThisType = ArrayType::get(Next->getType(), Dim);
}

Type* PointerTypeAST::getType() {
  if (ThisType) {
    return ThisType;
  }

  return ThisType = PointerType::get(
    getGlobalContext(),
    getSLContext().TheTarget->getProgramAddressSpace()
  );
}

/// Calculate aggregate name
/// \param[in] output - output buffer
/// \param[in] thisSym - aggregate which name we should get
/// \note It will generate names like A.B.C for nested aggregates
void calcAggregateName(llvm::raw_ostream& output, SymbolAST* thisSym) {
  assert(thisSym && thisSym->isAggregate());
  SymbolAST* sym = thisSym;

  for ( ; ; ) {
    output << StringRef(sym->Id->Id, sym->Id->Length);
    sym = sym->Parent;

    if (!sym || !sym->isAggregate()) {
      return;
    }

    output << ".";
  }
}

Type* StructTypeAST::getType() {
  // Calculate type only once
  if (ThisType) {
    return ThisType;
  }

  llvm::SmallString< 128 > s;
  llvm::raw_svector_ostream output(s);

  // For struct we always add struct. at it's start
  output << "struct.";
  calcAggregateName(output, ThisDecl);

  // Note: we should create and assign ThisType now not at the end of the 
  // function because members can point to this type too
  ThisType = StructType::create(getGlobalContext(), output.str());

  std::vector< Type* > vars;
  StructDeclAST* structDecl = (StructDeclAST*)ThisDecl;

  // Fill list of members
  for (SymbolList::iterator it = structDecl->Vars.begin(), 
    end = structDecl->Vars.end(); it != end; ++it) {
    vars.push_back(((VarDeclAST*)(*it))->ThisType->getType());
  }

  // If struct has 0 members then we forcedly add member with 1 byte length.
  // Because otherwise size of this type will be 0
  if (vars.empty()) {
    vars.push_back(Type::getInt8Ty(getGlobalContext()));
  }

  // Set members of struct and return generated type
  ((StructType*)ThisType)->setBody(vars);
  return ThisType;
}

Type* ClassTypeAST::getType() {
  if (ThisType) {
    return ThisType;
  }

  llvm::SmallString< 128 > s;
  llvm::raw_svector_ostream output(s);

  // For class we always add class. at it's start
  output << "class.";
  calcAggregateName(output, ThisDecl);

  // Note: we should create and assign ThisType now not at the end of the 
  // function because members can point to this type too
  ThisType = StructType::create(getGlobalContext(), output.str());

  std::vector< Type* > vars;
  ClassDeclAST* classDecl = (ClassDeclAST*)ThisDecl;
  VTableAST* baseVtbl = nullptr;
  Type* baseType = nullptr;

  // Check does this class have base class or not
  if (classDecl->BaseClass) {
    // Check for VTable 1st
    if (isa<ClassTypeAST>(classDecl->BaseClass)) {
      ClassDeclAST* baseDecl = (ClassDeclAST*)classDecl->BaseClass->getSymbol();
      baseVtbl = baseDecl->VTbl;
    }

    // Get type of base class
    baseType = classDecl->BaseClass->getType();
  }

  if (classDecl->VTbl && !baseVtbl) {
    // We have VTable but base class don't. Add VTable slot
    // Note: We want VTable always as the first member in the class layout
    vars.push_back(
      PointerType::get(
        getGlobalContext(),
        getSLContext().TheTarget->getProgramAddressSpace()
      )
    );
  }

  // If we have base class we should add variable with it's type
  // Example:
  //   class A { int a; }
  //   class B : A { int b; }
  // will give
  //   class.A = { int32 }
  //   class.B = { class.A, int32 }
  if (baseType) {
    vars.push_back(baseType);
  }

  // Fill list of members
  for (SymbolList::iterator it = classDecl->Vars.begin(),
    end = classDecl->Vars.end(); it != end; ++it) {
    // We need add only variables and not aggregates and functions
    if (isa<VarDeclAST>(*it)) {
      vars.push_back(((VarDeclAST*)(*it))->ThisType->getType());
    }
  }

  // If class has 0 members then we forcedly add member with 1 byte length.
  // Because otherwise size of this type will be 0
  if (vars.empty()) {
    vars.push_back(Type::getInt8Ty(getGlobalContext()));
  }

  // Set members of struct and return generated type
  ((StructType*)ThisType)->setBody(vars);
  return ThisType;
}

Type* FuncTypeAST::getType() {
  // Calculate type only once
  if (ThisType) {
    return ThisType;
  }

  // If ReturnType wasn't set then we set it as void type
  Type* returnType = (ReturnType ? ReturnType->getType() : 
    Type::getVoidTy(getGlobalContext()));

  if (Params.empty()) {
    // Return version without parameters
    return ThisType = FunctionType::get(returnType, false);
  }
  
  std::vector< Type* > params;

  // Create list of function parameters for llvm
  for (ParameterList::iterator it = Params.begin(), end = Params.end();
    it != end; ++it) {
    params.push_back((*it)->Param->getType());
  }

  // Return version with parameters
  return ThisType = FunctionType::get(returnType, params, false);
}

Type* QualifiedTypeAST::getType() {
  assert(0 && "QualifiedTypeAST::getType should never be reached");
  return nullptr;
}

} // namespace simple