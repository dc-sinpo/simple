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

SymbolAST* TypeAST::getSymbol() {
  return nullptr;
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
    new BuiltinTypeAST(TI_Char),
    new BuiltinTypeAST(TI_String)
  };

  assert(type >= TI_Void && type <= TI_String);
  return builtinTypes[type];
}

bool BuiltinTypeAST::implicitConvertTo(TypeAST* newType) {
  static bool convertResults[TI_String + 1][TI_String + 1] = {
    // void  bool   int    float  char   string
    { false, false, false, false, false, false }, // void
    { false, true,  true,  true,  true,  false }, // bool
    { false, true,  true,  true,  true,  false }, // int
    { false, true,  true,  true,  false, false }, // float
    { false, true,  true,  true,  true,  false }, // char
    { false, true,  false, false, false, true  }  // string
  };

  if (newType->TypeKind > TI_String) {
    return false;
  }

  return convertResults[TypeKind][newType->TypeKind];
}

void BuiltinTypeAST::toMangleBuffer(llvm::raw_ostream& output) {
  switch (TypeKind) {
    case TI_Void : output << "v"; break;
    case TI_Bool : output << "b"; break;
    case TI_Int : output << "i"; break;
    case TI_Float : output << "f"; break;
    case TI_Char : output << "c"; break;
    case TI_String : output << "PKc"; break;
    default: assert(0 && "Should never happen"); break;
  }
}

// ArrayTypeAST implementation
TypeAST* ArrayTypeAST::semantic(Scope* scope) {
// Check semantic of inner type
  Next = Next->semantic(scope);

  // Calculate MangleName
  calcMangle();
  return this;
}

bool ArrayTypeAST::implicitConvertTo(TypeAST* newType) {
  TypeAST* next1 = Next;
  TypeAST* next2 = nullptr;

  // char[] can be converted to string
  if (Next->isChar() && newType->isString()) {
    return true;
  }

  // If it's not type[] or type* then. We can't convert types 
  if (!(isa<PointerTypeAST>(newType) || isa<ArrayTypeAST>(newType))) {
    return false;
  }

  // Get inner type from pointer/array
  if (isa<PointerTypeAST>(newType)) {
    next2 = ((PointerTypeAST*)newType)->Next;
  } else if (isa<ArrayTypeAST>(newType)) {
    ArrayTypeAST* arrayType = (ArrayTypeAST*)newType;

    // If it's array then dimensions should be equal too
    if (arrayType->Dim != Dim) {
      return false;
    }

    next2 = arrayType->Next;
  }

  // We allow any array to void* conversion
  if (next2->isVoid()) {
    return true;
  }

  // We allow only top level type[] to type* conversion
  return next1->equal(next2);
}

void ArrayTypeAST::toMangleBuffer(llvm::raw_ostream& output) {
  output << "A" << Dim << "_";
  Next->toMangleBuffer(output);
}

// PointerTypeAST implementation
TypeAST* PointerTypeAST::semantic(Scope* scope) {
  // Check semantic on inner type and calculate MangleName
  Next = Next->semantic(scope);
  calcMangle();
  return this;
}

bool PointerTypeAST::implicitConvertTo(TypeAST* newType) {
  // We can convert pointer to boolean
  if (newType->isBool()) {
    return true;
  }
  
  // char* can be converted to string
  if (Next->isChar() && newType->isString()) {
    return true;
  }
  
  // We disable any conversions from pointer to non pointer
  if (!isa<PointerTypeAST>(newType)) {
    return false;
  }

  PointerTypeAST* ptr2 = (PointerTypeAST*)newType;
  // Get inner type for newType
  TypeAST* next2 = ptr2->Next;

  // Check are inner types equal or not
  if (Next->equal(next2)) {
    return true;
  }

  TypeAST* next1 = Next;

  // We allow conversion of any type* to void*
  if (next2->isVoid()) {
    return true;
  }

  return false;
}

void PointerTypeAST::toMangleBuffer(llvm::raw_ostream& output) {
  output << "P";
  Next->toMangleBuffer(output);
}

// StructTypeAST implementation
/// Calculate mangle name for aggregate
/// \param[in] output - output buffer
/// \param[in] thisSym - aggregate which mangle name we should get
void mangleAggregateName(llvm::raw_ostream& output, SymbolAST* thisSym) {
  assert(thisSym && thisSym->isAggregate());
  SymbolAST* sym = thisSym;

  output << "N";

  // We create N Length Name E for aggregate or N Length1 Name1 Length2 Name2 E
  // for nested aggregates.
  // example:
  //   A::B::C will be N1A1B1CE
  //   A will be N1AE

  for ( ; ; ) {
    if (!sym || !sym->isAggregate()) {
      output << "E";
      return;
    }

    output << sym->Id->Length << StringRef(sym->Id->Id, sym->Id->Length);
    sym = sym->Parent;
  }
}

TypeAST* StructTypeAST::semantic(Scope* scope) {
  calcMangle();
  return this;
}


bool StructTypeAST::implicitConvertTo(TypeAST* newType) {
  return false;
}

void StructTypeAST::toMangleBuffer(llvm::raw_ostream& output) {
  mangleAggregateName(output, ThisDecl);
}

SymbolAST* StructTypeAST::getSymbol() {
  return ThisDecl;
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

    if ((*it)->Param->isAggregate()) {
      scope->report(SMLoc(), diag::ERR_SemaAggregateAsFunctionParameter);
      return nullptr;
    }
  }
    
  // We disallow arrays or aggregates as return type
  if (ReturnType && (ReturnType->isAggregate() || isa<ArrayTypeAST>(ReturnType))) {
    scope->report(SMLoc(), diag::ERR_SemaArrayOrAggregateAsReturnType);
    return nullptr;
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

  // Write parameters' types to mangle buffer 
  for ( ; it != end; ++it) {
    (*it)->Param->toMangleBuffer(output);
  }
}

// QualifiedTypeAST implementation 
TypeAST* QualifiedTypeAST::semantic(Scope* scope) {
  SymbolAST* sym = nullptr;
  
  // For every nested name we should resolve it in the scope
  for (QualifiedName::iterator it = QualName.begin(), end = QualName.end();
    it != end; ++it) {

    if (sym) {
      // Now we should search as member of previously found symbol
      sym = sym->find((*it));

      if (!sym) {
        scope->report(SMLoc(), diag::ERR_SemaUndefinedIdentifier, (*it)->Id);
        return nullptr;
      }
    } else {
      // It's 1st name and we should search it in the scope
      sym = scope->find((*it));

      if (!sym) {
        scope->report(SMLoc(), diag::ERR_SemaUndefinedIdentifier, (*it)->Id);
        return nullptr;
      }
    }
  }

  return sym->getType();
}

bool QualifiedTypeAST::implicitConvertTo(TypeAST* ) {
  assert(0 && "QualifiedTypeAST::implicitConvertTo should never be reached");
  return false;
}

void QualifiedTypeAST::toMangleBuffer(llvm::raw_ostream& ) {
  assert(0 && "QualifiedTypeAST::toMangleBuffer should never be reached");
}

} // namespace simple