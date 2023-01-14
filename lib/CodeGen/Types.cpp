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
    Type::getInt8PtrTy(getGlobalContext())
  };

  if (ThisType) {
    return ThisType;
  }

  return ThisType = builtinTypes[TypeKind];
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

} // namespace simple