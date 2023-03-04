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