#include "simple/AST/AST.h"
#include "simple/Lexer/Lexer.h"
#include "simple/Parser/Parser.h"

using namespace llvm;

namespace simple {

// SymbolAST hierarchy implementation
// SymbolAST implementation
bool SymbolAST::needThis() {
  return false;
}

TypeAST* SymbolAST::getType() {
  assert(0 && "SymbolAST::getType should never be reached");
  return nullptr;
}

void SymbolAST::semantic(Scope* scope) {
  if (SemaState > 0) {
    return;
  }

  doSemantic(scope);

  ++SemaState;
}

void SymbolAST::semantic2(Scope* scope) {
  assert(SemaState >= 1);
  if (SemaState > 1) {
    return;
  }

  doSemantic2(scope);

  ++SemaState;
}

void SymbolAST::semantic3(Scope* scope) {
  assert(SemaState >= 2);
  if (SemaState > 2) {
    return;
  }

  doSemantic3(scope);

  ++SemaState;
}

void SymbolAST::semantic4(Scope* scope) {
  assert(SemaState >= 3);
  if (SemaState > 3) {
    return;
  }

  doSemantic4(scope);

  ++SemaState;
}

void SymbolAST::semantic5(Scope* scope) {
  assert(SemaState >= 4);
  if (SemaState > 4) {
    return;
  }

  doSemantic5(scope);

  ++SemaState;
}

void SymbolAST::doSemantic(Scope* ) {
  assert(0 && "SymbolAST::semantic should never be reached");
}

void SymbolAST::doSemantic2(Scope* ) {
  // By default do nothing
}

void SymbolAST::doSemantic3(Scope* ) {
  // By default do nothing
}

void SymbolAST::doSemantic4(Scope* ) {
  // By default do nothing
}

void SymbolAST::doSemantic5(Scope* ) {
  // By default do nothing
}

bool SymbolAST::canBeNull() {
  return true;
}

SymbolAST* SymbolAST::find(Name* id, int flags) {
  if (id == Id) {
    return this;
  }

  return nullptr;
}

bool SymbolAST::contain(TypeAST* type) {
  return false;
}

// VarDeclAST implementation
bool VarDeclAST::needThis() {
  return NeedThis;
}

TypeAST* VarDeclAST::getType() {
  return ThisType;
}

void VarDeclAST::doSemantic(Scope* scope) {
  // We not allow any variable redefinitions
  if (needThis()) {
    // We still can hide variables from base class
    if (scope->findMember(Id, 1)) {
      scope->report(Loc, diag::ERR_SemaIdentifierRedefinition);
      return;
    }
  } else {
    if (scope->find(Id)) {
      scope->report(Loc, diag::ERR_SemaIdentifierRedefinition);
      return;
    }
  }

  // Add variable to both current scope and enclosed function (if set)
  ((ScopeSymbol*)scope->CurScope)->Decls[Id] = this;
  Parent = scope->CurScope;

  if (scope->EnclosedFunc) {
    scope->EnclosedFunc->FuncVars.push_back(this);
  }

  ThisType = ThisType->semantic(scope);
}

void VarDeclAST::doSemantic3(Scope* scope) {
  // For empty initialization we define own default
  if (Val) {
    // Perform semantic for initialization
    Val = Val->semantic(scope);

    // If it's structure we should generate error
    if (ThisType->isAggregate()) {
      scope->report(Loc, diag::ERR_SemaConstructionCallInStruct);
      return;
    }

    // We disallow initialization with void type
    if (!Val->ExprType || Val->ExprType->isVoid()) {
      scope->report(Loc, diag::ERR_SemaVoidInitializer);
      return;
    }

    if (isa<PointerTypeAST>(ThisType)) {
      // If it's int
      if (Val->ExprType->isInt()) {
        // check for integral constant 0
        if (Val->isIntConst() && ((IntExprAST*)Val)->Val != 0) {
          scope->report(Loc, diag::ERR_SemaPointerInitialization);
        }

        return;
      // Check conversion of types
      } else if (!Val->ExprType->implicitConvertTo(ThisType)) {
        scope->report(Loc, diag::ERR_SemaPointerInitialization);
        return;
      }
    }

    // If types not match we should create cast expression
    if (!Val->ExprType->equal(ThisType)) {
      Val = new CastExprAST(Loc, Val, ThisType);
      Val = Val->semantic(scope);
    }
  }
}

bool VarDeclAST::contain(TypeAST* type) {
  if (ThisType->equal(type)) {
    return true;
  }

  return false;
}

bool VarDeclAST::canBeNull() {
  if (!isa<PointerTypeAST>(ThisType)) {
    return false;
  }

  return true;
}

// ScopeSymbol implementation
ScopeSymbol::~ScopeSymbol() {
}

SymbolAST* ScopeSymbol::find(Name* id, int /*flags*/) {
  SymbolMap::iterator it = Decls.find(id);

  if (it == Decls.end()) {
    return nullptr;
  }

  return it->second;
}

// StructDeclAST implementation
TypeAST* StructDeclAST::getType() {
  return ThisType;
}

void StructDeclAST::doSemantic(Scope* scope) {
  if (scope->find(Id)) {
    scope->report(Loc, diag::ERR_SemaIdentifierRedefinition);
    return;
  }

  Scope* s = scope->push(this);

  ThisType = ThisType->semantic(s);

  ((ScopeSymbol*)scope->CurScope)->Decls[Id] = this;
  Parent = scope->CurScope;

  s->pop();
}

void StructDeclAST::doSemantic3(Scope* scope) {
  Scope* s = scope->push(this);
  int curOffset = 0;

  // Check every variable declaration
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    VarDeclAST* var = (VarDeclAST*)(*it);
    
    // Perform semantic and generate offset for it
    var->semantic(s);
    var->OffsetOf = curOffset++;
  }

  // Check for circular reference
  if (contain(ThisType)) {
    scope->report(Loc, diag::ERR_SemaCricularReference, Id->Id);
    return;
  }

  s->pop();
}

bool StructDeclAST::contain(TypeAST* type) {
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    if ((*it)->contain(type)) {
      return true;
    }
  }

  return false;
}

// ParameterSymbolAST implementation
TypeAST* ParameterSymbolAST::getType() {
  return Param->Param;
}

bool ParameterSymbolAST::canBeNull() {
  if (!isa<PointerTypeAST>(Param->Param)) {
    return false;
  }

  return true;
}

void ParameterSymbolAST::doSemantic(Scope* ) {
}

SymbolAST* ParameterSymbolAST::find(Name* id, int ) {
  if (id == Param->Id) {
    return this;
  }

  return nullptr;
}

// FuncDeclAST implementation
TypeAST* FuncDeclAST::getType() {
  return ThisType;
}

void FuncDeclAST::doSemantic(Scope* scope) {
  ThisType = ThisType->semantic(scope);
  Parent = scope->CurScope;
  // Set return type
  ReturnType = ((FuncTypeAST*)ThisType)->ReturnType;
  
  // If it's main then we should check that it have 0 arguments and return
  // float
  if (Id->Length == 4 && memcmp(Id->Id, "main", 4) == 0) {
    FuncTypeAST* thisType = (FuncTypeAST*)ThisType;

    if (thisType->Params.size()) {
      scope->report(Loc, diag::ERR_SemaMainParameters);
      return;
    }

    if (ReturnType != BuiltinTypeAST::get(TypeAST::TI_Float)) {
      scope->report(Loc, diag::ERR_SemaMainReturnType);
      return;
    }
  }

  // Find in this scope
  if (SymbolAST* fncOverload = scope->findMember(Id, 1)) {
    scope->report(Loc, diag::ERR_SemaFunctionRedefined, Id->Id);
    return;
  }

  // Add function declaration to current scope
  ((ScopeSymbol*)Parent)->Decls[Id] = this;
}

void FuncDeclAST::doSemantic5(Scope* scope) {
  FuncTypeAST* func = (FuncTypeAST*)ThisType;
  // If Body is 0 then it's prototype and no semantic needed
  if (Body) {
    // Create own scope for function and set enclosed function for new scope to
    // this
    Scope* s = scope->push(this);
    s->EnclosedFunc = this;

    // Perform checks for every function parameter
    for (ParameterList::iterator it = func->Params.begin(), end = func->Params.end();
      it != end; ++it) {
      ParameterAST* p = *it;
      // We need special care for parameter with name
      if (p->Id) {
        // Disallow redefinition
        if (find(p->Id)) {
          scope->report(Loc, diag::ERR_SemaIdentifierRedefinition);
          return;
        }

        // We should create new symbol for parameter and add it to a list of
        // declarations for this function and to list of all declared variables
        SymbolAST* newSym = new ParameterSymbolAST(p);
        Decls[p->Id] = newSym;
        FuncVars.push_back(newSym);
      }
    }

    // Set landing pad
    LandingPadAST* oldLandingPad = s->LandingPad;
    LandingPad = new LandingPadAST();
    s->LandingPad = LandingPad;

    // Perform semantic for function's body 
    Body = Body->semantic(s);

    // Restore old landing pad
    s->LandingPad = oldLandingPad;

    // Functions with non void return value should have return statement in their body
    if (!ReturnType->isVoid() && !Body->hasReturn()) {
      scope->report(Loc, diag::ERR_SemaMissingReturnValueInFunction);
      return;
    }

    // Remove function's scope
    s->pop();
  }
}

// ModuleDeclAST implementation
extern "C" void lle_X_printDouble(double val) {
  outs() << val;
}

extern "C" void lle_X_printInt(int val) {
  outs() << val;
}

extern "C" void lle_X_printChar(char val) {
  outs() << val;
}

extern "C" void lle_X_printString(char* str) {
  outs() << str;
}

extern "C" void lle_X_printLine(char* str) {
  outs() << str << "\n";
}

extern "C" void* lle_X_new(int size) {
  return malloc(size);
}

extern "C" void lle_X_delete(void* block) {
  free(block);
}

/// Add runtime function
/// \param[in] protoString - function's prototype
/// \param[in] newName - name for this function during code generation
/// \param[in] modDecl - module for this function
/// \param[in] fncPtr - function's address
FuncDeclAST* addDynamicFunc(const char* protoString, const char* newName, 
  ModuleDeclAST* modDecl, void* fncPtr) {
  // Parse function's prototype
  FuncDeclAST* func = (FuncDeclAST*)parseFuncProto(protoString);
  // Create define for this function and set that it was compiled
  func->CodeValue = Function::Create(
    (FunctionType*)func->ThisType->getType(),
    Function::ExternalLinkage,
    Twine(newName),
    getSLContext().TheModule
  );
  func->Compiled = true;
  // Add this function to the module
  modDecl->Members.insert(modDecl->Members.begin(), func);

  // Add runtime function to the LLVM
  ExitOnError ExitOnErr;

  ExitOnErr(getJIT().addSymbol(newName, fncPtr));

  return func;
}

void initRuntimeFuncs(ModuleDeclAST* modDecl) {
  addDynamicFunc("fn printChar(_: char)", "lle_X_printChar", modDecl, (void*)lle_X_printChar);
  addDynamicFunc("fn printInt(_: int)", "lle_X_printInt", modDecl, (void*)lle_X_printInt);
  addDynamicFunc("fn printFloat(_: float)", "lle_X_printDouble", modDecl, (void*)lle_X_printDouble);
  addDynamicFunc("fn printString(_: string)", "lle_X_printString", modDecl, (void*)lle_X_printString);
  addDynamicFunc("fn printLn(_: string)", "lle_X_printLine", modDecl, (void*)lle_X_printLine);
  addDynamicFunc("fn new(_: int) : void*", "lle_X_new", modDecl, (void*)lle_X_new);
  addDynamicFunc("fn delete(_: void*)", "lle_X_delete", modDecl, (void*)lle_X_delete);
}

ModuleDeclAST* ModuleDeclAST::load(SourceMgr &SrcMgr, DiagnosticsEngine &Diags, StringRef fileName) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
        FileOrErr = llvm::MemoryBuffer::getFile(fileName);

  if (std::error_code BufferError = FileOrErr.getError()) {
    llvm::WithColor::error(llvm::errs(), "simple")
        << "Error reading " << fileName << ": "
        << BufferError.message() << "\n";
  }

  // Tell SrcMgr about this buffer, which is what the
  // parser will pick up.
  SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr),
                            llvm::SMLoc());

  Lexer Lex(SrcMgr, Diags);
  Parser P(&Lex);

  return P.parseModule();
}

void ModuleDeclAST::semantic() {
  Scope s(this);
  
  // Add runtime functions
  initRuntimeFuncs(this);

  // Perform semantic on all built-in types
  for (int i = TypeAST::TI_Void; i <= TypeAST::TI_String; ++i) {
    BuiltinTypeAST::get(i)->semantic(&s);
  }

  // Check semantic of all aggregates (we need this because functions can
  // use this aggregates as return types)
  for (SymbolList::iterator it = Members.begin(), end = Members.end();
    it != end; ++it) {
    if (!isa<FuncDeclAST>(*it))
      (*it)->semantic(&s);
  }

  // Check semantic of all functions
  for (SymbolList::iterator it = Members.begin(), end = Members.end();
    it != end; ++it) {
    if (isa<FuncDeclAST>(*it))
      (*it)->semantic(&s);
  }

  // Check semantic2 for all declarations
  for (SymbolList::iterator it = Members.begin(), end = Members.end();
    it != end; ++it) {
    (*it)->semantic2(&s);
  }

  // Check semantic3 for all declarations
  for (SymbolList::iterator it = Members.begin(), end = Members.end();
    it != end; ++it) {
    (*it)->semantic3(&s);
  }

  // Check semantic4 for all declarations
  for (SymbolList::iterator it = Members.begin(), end = Members.end();
    it != end; ++it) {
    (*it)->semantic4(&s);
  }

  // Check semantic5 for all declarations
  for (SymbolList::iterator it = Members.begin(), end = Members.end();
    it != end; ++it) {
    (*it)->semantic5(&s);
  }
}

// Scope implementation
SymbolAST* Scope::find(Name* id) {
  Scope* s = this;

  if (!id) {
    return ThisModule;
  }

  for ( ; s; s = s->Enclosed) {
    if (s->CurScope) {
      SymbolAST* sym = s->CurScope->find(id);

      if (sym) {
        return sym;
      }
    }
  }

  return nullptr;
}

SymbolAST* Scope::findMember(Name* id, int flags) {
  if (CurScope) {
    return CurScope->find(id, flags);
  }

  return nullptr;
}

Scope* Scope::push() {
  Scope* s = new Scope(this);
  return s;
}

Scope* Scope::push(ScopeSymbol* sym) {
  Scope* s = push();
  s->CurScope = sym;
  return s;
}

Scope* Scope::pop() {
  Scope* res = Enclosed;
  delete this;
  return res;
}

Scope* Scope::recreateScope(Scope* scope, SymbolAST* sym) {
  Scope* p = scope;

  while (p->Enclosed) {
    p = p->Enclosed;
  }

  // Now p contain module's scope

  // Create list of all sym's parents
  SymbolList symList;
  SymbolAST* tmp = sym;

  while (tmp->Parent) {
    symList.push_back(tmp);
    tmp = tmp->Parent;
  }

  // Now when we have module's scope and list of all sym's parents we can
  // recreate scope
  // Note: we should add symbols in reverse order
  for (SymbolList::reverse_iterator it = symList.rbegin(), end = symList.rend();
    it != end; ++it) {
    p = p->push((ScopeSymbol*)*it);
  }

  // Return just recreated scope (it still can contain only 1 member)
  return p;
}

void Scope::clearAllButModule(Scope* scope) {
  Scope* p = scope;

  while (p->Enclosed) {
    p = p->pop();
  }
}

} // namespace simple