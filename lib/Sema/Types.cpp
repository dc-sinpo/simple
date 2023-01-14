#include "simple/AST/AST.h"

using namespace llvm;

namespace simple {

// TypeAST implementation
StringSet< > TypeAST::TypesTable;
static std::vector< TypeAST* > TypesToDelete;

void TypeAST::registerTypeForDeletion(TypeAST* thisType) {
  TypesToDelete.push_back(thisType);
}

void TypeAST::clearAllTypes() {
  for (std::vector< TypeAST* >::iterator it = TypesToDelete.begin(),
    end = TypesToDelete.end(); it != end; ++it) {
    delete *it;
  }

  TypesToDelete.clear();
}

void TypeAST::calcMangle() {
  if (!MangleName.empty()) {
    return;
  }

  // 128 bytes is enough for most types
  llvm::SmallString< 128 > s;
  llvm::raw_svector_ostream output(s);

  // Write mangle name for type into buffer and add it to the types table
  toMangleBuffer(output);

  TypesTable.insert(output.str());

  // Get just added value from table and set MangleName from this value
  StringSet< >::iterator pos = TypesTable.find(s);
  assert(pos != TypesTable.end());
  MangleName = pos->getKeyData();
}

bool TypeAST::equal(TypeAST* otherType) {
  // equal only valid if 2 types have MangleName
  // note: We check types equivalence by their mangle name and not their 
  // content, because it's much cheaper and give same result
  assert(!MangleName.empty() && !otherType->MangleName.empty());
  return MangleName == otherType->MangleName;
}

TypeAST* BuiltinTypeAST::semantic(Scope* scope) {
  calcMangle();
  return this;
}

// BuiltinTypeAST implementation
TypeAST *BuiltinTypeAST::get(int type) {
  static TypeAST *builtinTypes[] = {
    new BuiltinTypeAST(TI_Void),
    new BuiltinTypeAST(TI_Bool),
    new BuiltinTypeAST(TI_Int),
    new BuiltinTypeAST(TI_Float),
  };

  assert(type >= TI_Void && type <= TI_Float);
  return builtinTypes[type];
}

bool BuiltinTypeAST::implicitConvertTo(TypeAST* newType) {
  static bool convertResults[TI_Float + 1][TI_Float + 1] = {
    // void  bool   int    float
    { false, false, false, false }, // void
    { false, true,  true,  true  }, // bool
    { false, true,  true,  true  }, // int
    { false, true,  true,  true  }, // float
  };

  if (newType->TypeKind > TI_Float)
    return false;

  return convertResults[TypeKind][newType->TypeKind];
}

void BuiltinTypeAST::toMangleBuffer(llvm::raw_ostream& output) {
  switch (TypeKind) {
    case TI_Void : output << "v"; break;
    case TI_Bool : output << "b"; break;
    case TI_Int : output << "i"; break;
    case TI_Float : output << "f"; break;
    default: assert(0 && "Should never happen"); break;
  }
}

// FunctionTypeAST implementation
TypeAST* FuncTypeAST::semantic(Scope* scope) {
  // We should run semantic for return type if it's exist
  if (!ReturnType) {
    ReturnType = BuiltinTypeAST::get(TypeAST::TI_Void);
  }
  
  ReturnType = ReturnType->semantic(scope);
  
  // Perform semantic on function parameters
  for (ParameterList::iterator it = Params.begin(), end = Params.end();
    it != end; ++it) {
    (*it)->Param = (*it)->Param->semantic(scope);
  }

  calcMangle();
  return this;
}

bool FuncTypeAST::implicitConvertTo(TypeAST* newType) {
  return false;
}

void FuncTypeAST::toMangleBuffer(llvm::raw_ostream& output) {
  // Append v if function has 0 parameters
  if (Params.empty()) {
    output << "v";
    return;
  }
  
  ParameterList::iterator it = Params.begin(), end = Params.end();

  // Append v if function has 0 parameters beside this
  if (it == end) {
    output << "v";
    return;
  }

  // Write parameters' types to mangle buffer 
  for ( ; it != end; ++it) {
    (*it)->Param->toMangleBuffer(output);
  }
}

} // namespace simple