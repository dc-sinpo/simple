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

    SymbolAST* foundInBase = scope->findMember(Id);

    // Disable function hiding
    if (foundInBase && (isa<FuncDeclAST>(foundInBase) || isa<OverloadSetAST>(foundInBase))) {
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
  if (!Val) {
    // Disable default initializers for aggregates
    if (needThis()) {
      return;
    }

    // Create new initialization based on variable's type
    if (isa<ArrayTypeAST>(ThisType)) {
      return;
    }

    // Check initialization for aggregates
    if (ThisType->isAggregate()) {
      if (isa<ClassTypeAST>(ThisType)) {
        ClassDeclAST* thisClass = (ClassDeclAST*)ThisType->getSymbol();

        if (thisClass->Ctor) {
          // Generate constructor's call
          ExprList args;
          Val = new CallExprAST(
            Loc,
            new MemberAccessExprAST(
              Loc,
              new IdExprAST(Loc, Id),
              Name::Ctor),
            args);
          Val = Val->semantic(scope);
        }
      }

      return;
    // Check initialization for pointers and string
    } else if (isa<PointerTypeAST>(ThisType) || ThisType->isString())
      Val = new IntExprAST(Loc, 0);
    // Check initialization for int
    else if (ThisType->isInt())
      Val = new IntExprAST(Loc, 0);
    // Check initialization for char
    else if (ThisType->isChar()) {
      Val = new IntExprAST(Loc, 0);
      Val = new CastExprAST(Loc, Val, BuiltinTypeAST::get(TypeAST::TI_Char));
    } else {
      // It's initialization for floating point variable
      Val = new FloatExprAST(Loc, 0.0);
    }

    // Perform semantic for just created initialization
    Val = Val->semantic(scope);

    return;
  }

  // Perform semantic for initialization
  Val = Val->semantic(scope);

  if (isa<ClassTypeAST>(ThisType)) {
    return;
  }

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

bool VarDeclAST::contain(TypeAST* type) {
  if (ThisType->equal(type) || type->isBaseOf(ThisType)) {
    return true;
  }

  return false;
}

bool VarDeclAST::canBeNull() {
  if (Id == Name::Super || !isa<PointerTypeAST>(ThisType)) {
    return false;
  }

  return true;
}

// ScopeSymbol implementation
ScopeSymbol::~ScopeSymbol() {
  for (SymbolMap::iterator it = Decls.begin(), end = Decls.end(); it != end; ++it) {
    if (isa<OverloadSetAST>(it->second)) {
      delete it->second;
    }
  }
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

    // Disallow instances of class as structure members
    if (isa<ClassTypeAST>(var->getType())) {
      scope->report(Loc, diag::ERR_SemaClassAsStructMember);
      return;
    }
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

// VTableAST implementation
bool VTableAST::isExist(SymbolAST* func) {
  assert(isa<FuncDeclAST>(func));
  FuncDeclAST* fncDecl = (FuncDeclAST*)func;
  SymbolMap::iterator pos = Decls.find(func->Id);

  // If nothing found return false
  if (pos == Decls.end()) {
    return false;
  }

  // Does found function is function or overload set
  if (isa<FuncDeclAST>(pos->second)) {
    // It's function
    FuncDeclAST* existDecl = (FuncDeclAST*)pos->second;

    // If found and searched functions have same type then return true
    if (existDecl->ThisType->equal(fncDecl->ThisType)) {
      return true;
    }

    return false;
  } else {
    // It's an overload set
    assert(isa<OverloadSetAST>(pos->second));
    OverloadSetAST* existDecls = (OverloadSetAST*)pos->second;

    // Try to find this function in the used overloads
    return existDecls->OverloadsUsed.find(fncDecl->ThisType->MangleName) !=
      existDecls->OverloadsUsed.end();
  }
}

void VTableAST::addOrReplace(Scope* scope, SymbolAST* func) {
  assert(isa<FuncDeclAST>(func));
  FuncDeclAST* fncDecl = (FuncDeclAST*)func;
  SymbolMap::iterator pos = Decls.find(func->Id);
  
  if (pos == Decls.end()) {
    // No function's found. Add new
    fncDecl->OffsetOf = CurOffset++;
    Decls[fncDecl->Id] = func;
    return;
  } else {
    // Check for overload set 1st
    if (isa<OverloadSetAST>(pos->second)) {
      OverloadSetAST* overloadSet = (OverloadSetAST*)pos->second;
      
      // Check was function added or not
      if (overloadSet->OverloadsUsed.find(fncDecl->ThisType->MangleName) !=
        overloadSet->OverloadsUsed.end()) {
        // Already exists. Find and replace old one
        for (SymbolList::iterator it = overloadSet->Vars.begin(),
          end = overloadSet->Vars.end(); it != end; ++it) {
          FuncDeclAST* oldFnc = (FuncDeclAST*)*it;
          if (oldFnc->ThisType->equal(fncDecl->ThisType)) {
            // Replace old one
            fncDecl->OffsetOf = oldFnc->OffsetOf;
            (*it) = fncDecl;

            if (!fncDecl->ReturnType->equal(oldFnc->ReturnType)) {
              scope->report(fncDecl->Loc, diag::ERR_SemaVirtualFunctionReturnTypeOverload, fncDecl->Id->Id, Parent->Id->Id);
            }
            return;
          }
        }
      } else {
        // Not yet added. Add new
        fncDecl->OffsetOf = CurOffset++;
        overloadSet->push(scope, func);
        return;
      }
    } else {
      // It's single function
      assert(isa<FuncDeclAST>(pos->second));
      FuncDeclAST* oldFnc = (FuncDeclAST*)pos->second;

      if (oldFnc->ThisType->equal(fncDecl->ThisType)) {
        // It's already exist. Replace old one
        fncDecl->OffsetOf = oldFnc->OffsetOf;
        Decls[fncDecl->Id] = func;

        if (!fncDecl->ReturnType->equal(oldFnc->ReturnType)) {
          scope->report(fncDecl->Loc, diag::ERR_SemaVirtualFunctionReturnTypeOverload, fncDecl->Id->Id, Parent->Id->Id);
        }
        return;
      } else {
        // Create new overload set and add it
        OverloadSetAST* newSet = new OverloadSetAST(fncDecl->Id);
        fncDecl->OffsetOf = CurOffset++;
        newSet->push(scope, oldFnc);
        newSet->push(scope, fncDecl);
        Decls[fncDecl->Id] = newSet;
        return;
      }
    }
  }
}

VTableAST* VTableAST::clone(SymbolAST* parent) {
  return new VTableAST(*this, parent);
}

// ClassDeclAST implementation
/// Generate list default initializations for class members
/// \param[in] classDecl - class
ExprList generateDefaultInitializer(ClassDeclAST* classDecl) {
  ExprList result;

  // Check every variable declaration for default initializer
  for (SymbolList::iterator it = classDecl->Vars.begin(),
    end = classDecl->Vars.end(); it != end; ++it) {
    if (isa<VarDeclAST>(*it)) {
      VarDeclAST* var = (VarDeclAST*)*it;

      // If variable has initializer add this variable
      if (var->Val) {
        ExprAST* expr = new BinaryExprAST(classDecl->Loc, tok::Assign, 
          new IdExprAST(classDecl->Loc, var->Id),
          var->Val->clone());
        result.push_back(expr);
      }
      // Check variable for class instance
      else if (isa<ClassTypeAST>(var->ThisType)) {
        ClassDeclAST* innerDecl = (ClassDeclAST*)var->ThisType->getSymbol();
        // If class has constructor we should call it
        if (innerDecl->Ctor) {
          // var.__ctor()
          ExprAST* expr = new MemberAccessExprAST(classDecl->Loc,
            new IdExprAST(classDecl->Loc, (*it)->Id), Name::Ctor);
          ExprList args;
          expr = new CallExprAST(classDecl->Loc, expr, args);
          result.push_back(expr);
        }
      }
    }
  }

  return result;
}

/// Generate default constructor if needed
/// \param[in] classDecl - class for which constructor should be generated
FuncDeclAST* generateDefaultConstructor(ClassDeclAST* classDecl) {
  ExprList valsInit = generateDefaultInitializer(classDecl);

  ParameterList params;
  StmtList body;
  StmtAST* ctorCall = nullptr;

  // Check base class for constructor
  if (classDecl->BaseClass && isa<ClassTypeAST>(classDecl->BaseClass)) {
    ClassDeclAST* baseClass = (ClassDeclAST*)classDecl->BaseClass->getSymbol();
    // If has constructor generate it's call
    if (baseClass->Ctor) {
      // super.__ctor()
      ExprAST* expr = new MemberAccessExprAST(classDecl->Loc, 
        new IdExprAST(classDecl->Loc, Name::Super), Name::Ctor);
      ExprList args;
      expr = new CallExprAST(classDecl->Loc, expr, args);
      ctorCall = new ExprStmtAST(classDecl->Loc, expr);
    }
  }

  // If there are at least 1 variable with initializer
  if (!valsInit.empty()) {
    // generate initialization for every variable with initializer
    for (ExprList::iterator it = valsInit.begin(), end = valsInit.end();
      it != end; ++it) {
      body.push_back(new ExprStmtAST(classDecl->Loc, *it));
    }
  }

  // If we don't have VTable and members with initialization and parent constructor then return 0
  if (!classDecl->VTbl && body.empty() && !ctorCall) {
    return nullptr;
  }

  StmtList ctorBody;

  if (ctorCall) {
    ctorBody.push_back(ctorCall);
  }

  ctorBody.push_back(new BlockStmtAST(classDecl->Loc, body));

  FuncTypeAST* ctorType = new FuncTypeAST(nullptr, params);
  FuncDeclAST* ctorDecl = new FuncDeclAST(classDecl->Loc, ctorType, Name::Ctor, 
    new BlockStmtAST(classDecl->Loc, ctorBody), true, tok::New);
  return ctorDecl;
}

/// Generate default cleanup actions for class
/// \param[in] classDecl - class
StmtList generateCleanupList(ClassDeclAST* classDecl) {
  StmtList result;

  // Check every variable declaration for destructor (we should do this in
  // reverse order)
  for (SymbolList::reverse_iterator it = classDecl->Vars.rbegin(),
    end = classDecl->Vars.rend(); it != end; ++it) {
    if (!isa<VarDeclAST>(*it)) {
      continue;
    }

    VarDeclAST* var = (VarDeclAST*)*it;

    // Check variable for class instance
    if (!isa<ClassTypeAST>(var->ThisType)) {
      continue;
    }

    ClassDeclAST* innerDecl = (ClassDeclAST*)var->ThisType->getSymbol();
    // If class has destructor we should call it
    if (innerDecl->Dtor) {
      // var.__dtor()
      ExprAST* expr = new MemberAccessExprAST(classDecl->Loc,
        new IdExprAST(classDecl->Loc, (*it)->Id), Name::Dtor);
      ExprList args;
      expr = new CallExprAST(classDecl->Loc, expr, args);
      result.push_back(new ExprStmtAST(classDecl->Loc, expr));
    }   
  }

  return result;
}

TypeAST* ClassDeclAST::getType() {
  return ThisType;
}

void ClassDeclAST::doSemantic(Scope* scope) {
  // Try to find already defined symbol with same name
  if (scope->find(Id)) {
    scope->report(Loc, diag::ERR_SemaIdentifierRedefinition);
    return;
  }

  Scope* s = scope->push(this);

  // Perform semantic on this type
  ThisType = ThisType->semantic(s);

  ((ScopeSymbol*)scope->CurScope)->Decls[Id] = this;
  Parent = scope->CurScope;

  // Perform semantic on all aggregate members
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    if ((*it)->isAggregate()) {
      (*it)->semantic(s);
    }
  }

  s->pop();
}

void ClassDeclAST::doSemantic2(Scope* scope) {
  Scope* s = scope->push(this);

  // If we have base class we should check it
  if (BaseClass) {
    // Check it's semantic
    BaseClass = BaseClass->semantic(s);

    if (!BaseClass->isAggregate()) {
      scope->report(Loc, diag::ERR_SemaInvalidBaseClass, Id->Id);
      return;
    }

    // Check for circular reference
    if (BaseClass->equal(ThisType) || ThisType->isBaseOf(BaseClass)) {
      scope->report(Loc, diag::ERR_SemaCricularReference, Id->Id);
      return;
    }
  }

  s->pop();
}

void ClassDeclAST::doSemantic3(Scope* scope) {
  Scope* s = scope->push(this);

  // Check all functions for constructor
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    if (!isa<FuncDeclAST>(*it)) {
      continue;
    }

    FuncDeclAST* fncDecl = (FuncDeclAST*)*it;

    // If it's constructor set that this class has constructor
    if (fncDecl->isCtor()) {
      Ctor = true;
      continue;
    }

    // If it's destructor set that this class has destructor
    if (fncDecl->isDtor()) {
      Dtor = true;
      continue;
    }
  }

  // Perform semantic on every variable declaration
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    if (!isa<VarDeclAST>(*it)) {
      continue;
    }

    (*it)->semantic(s);

    if (!isa<ClassTypeAST>((*it)->getType())) {
      continue;
    }

    ClassDeclAST* classDecl = (ClassDeclAST*)((*it)->getType()->getSymbol());

    // To properly generate destructor/constructors we should be sure that
    // all class instance members had their semantic4 passed
    if (classDecl->SemaState < 4) {
      // Recreate scope for member
      Scope* memberScope = Scope::recreateScope(scope, classDecl);

      // Perform all needed semantic passes
      classDecl->semantic2(memberScope);
      classDecl->semantic3(memberScope);
      classDecl->semantic4(memberScope);

      Scope::clearAllButModule(memberScope);
    }
  }

  if (contain(ThisType)) {
    scope->report(Loc, diag::ERR_SemaCricularReference, Id->Id);
    return;
  }

  // Make sure that base class know about it's constructors
  if (BaseClass && isa<ClassTypeAST>(BaseClass)) {
    ClassDeclAST* baseDecl = (ClassDeclAST*)BaseClass->getSymbol();

    // Make sure that semantic4 was done in the base class
    if (baseDecl->SemaState < 4) {
      // Recreate scope for base class
      Scope* parenScope = Scope::recreateScope(scope, baseDecl);

      // Perform all needed semantic passes
      baseDecl->semantic2(parenScope);
      baseDecl->semantic3(parenScope);
      baseDecl->semantic4(parenScope);
    
      Scope::clearAllButModule(parenScope);
    }
  }

  ExprList valsInit;
  
  if (Ctor) {
    valsInit = generateDefaultInitializer(this);
  }

  // Now we can safely perform semantic on every function
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    if (!isa<FuncDeclAST>(*it)) {
      continue;
    }

    FuncDeclAST* fncDecl = (FuncDeclAST*)*it;

    if (fncDecl->isCtor()) {
      StmtList initBody;

      for (ExprList::iterator it = valsInit.begin(), end = valsInit.end();
        it != end; ++it) {
        initBody.push_back(new ExprStmtAST(Loc, (*it)->clone()));
      }

      BlockStmtAST* fncBody = (BlockStmtAST*)fncDecl->Body;

      // Check for constructors call
      if (isa<ExprStmtAST>(fncBody->Body[0])) {
        // We should add initialization right after constructors call
        fncBody->Body.insert(fncBody->Body.begin() + 1, new BlockStmtAST(
          fncDecl->Loc, initBody));
      } else {
        // We should add initialization as first instruction
        fncBody->Body.insert(fncBody->Body.begin(), new BlockStmtAST(
          fncDecl->Loc, initBody));
      }
    } else if (fncDecl->isDtor()) {
      // We should try generate default cleanup if needed
      StmtList defaultCleanup = generateCleanupList(this);

      if (!defaultCleanup.empty()) {
        BlockStmtAST* fncBody = (BlockStmtAST*)fncDecl->Body;
        fncBody->Body.insert(fncBody->Body.end(), defaultCleanup.begin(),
          defaultCleanup.end());
      }
    }

    if (BaseClass && isa<ClassTypeAST>(BaseClass)) {
      Name* memberId = (*it)->Id;

      SymbolAST* foundInBase = ((ClassTypeAST*)BaseClass)->ThisDecl->find(memberId);

      if (foundInBase && memberId != Name::Ctor && memberId != Name::Dtor) {
        if (isa<FuncDeclAST>(foundInBase)) {
          Decls[memberId] = foundInBase;
        } else if (isa<OverloadSetAST>(foundInBase)) {
          Decls[memberId] = ((OverloadSetAST*)foundInBase)->clone();
        }
      }
    }

    (*it)->semantic(s);
  }

  if (!valsInit.empty()) {
    for (ExprList::iterator it = valsInit.begin(), end = valsInit.end();
      it != end; ++it) {
      delete *it;
    }
  }

  s->pop();
}

void ClassDeclAST::doSemantic4(Scope* scope) {
  Scope* s = scope->push(this);

  bool vtableOwner = true; // If VTable instance is our not from base class
  VTableAST* vtbl = nullptr;
  int curOffset = 0; // Current offset for variables
  bool generateDtor = false; // true - if destructor should be generated
  bool baseHasVtbl = false;
  StmtList defaultDtorBody;
  
  if (!Dtor) {
    defaultDtorBody = generateCleanupList(this);
  }

  // Try to find VTable in the base class
  if (BaseClass) {
    if (isa<ClassTypeAST>(BaseClass)) {
      ClassDeclAST* baseDecl = (ClassDeclAST*)BaseClass->getSymbol();

      if (baseDecl->VTbl) {
        // Get VTable from base class and set that we got it from base class
        vtableOwner = false;
        vtbl = baseDecl->VTbl;
        baseHasVtbl = true;
      }

      // If we don't have destructor but base class have then we should generate it
      if (!Dtor && baseDecl->Dtor) {
        generateDtor = true;
      }

      // We should increase offset for variable because we have 1 slot used for
      // base class
      ++curOffset;
    } else {
      // We should increase offset for variable because we have 1 slot used for
      // base class
      ++curOffset;
    }
  }

  if (!Dtor && !defaultDtorBody.empty()) {
    generateDtor = true;
  }

  // If no VTable found in the base class then create new
  if (!vtbl) {
    vtbl = new VTableAST(this);
  }

  // Check any function declaration for virtual function
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    if (!isa<FuncDeclAST>(*it)) {
      continue;
    }
    
    FuncDeclAST* fncDecl = (FuncDeclAST*)*it;

    // Skip constructors
    if (fncDecl->isCtor()) {
      continue;
    }

    // Check does function exist in virtual table or not
    if (vtbl->isExist(fncDecl)) {
      // It does then it should be override
      if (!fncDecl->isOverride()) {
        scope->report(Loc, diag::ERR_SemaVirtualFunctionExists);
        return;
      }
    } else {
      // It doesn't. It's error if it has override
      if (fncDecl->isOverride()) {
        scope->report(Loc, diag::ERR_SemaOverrideFunctionDoesntExists);
        return;
      }
    }

    // If it's virtual or override function then we should add it
    if (fncDecl->isVirtual() || fncDecl->isOverride()) {
      // If we don't have own VTable clone from base
      if (vtableOwner == false) {
        vtbl = vtbl->clone(this);
        vtableOwner = true;
      }

      // Add this function or replace old
      vtbl->addOrReplace(scope, fncDecl);
    }
  }

  // If VTable has at least 1 function then we should add it
  if (vtbl->CurOffset > 0) {
    VTbl = vtbl;
    OwnVTable = vtableOwner;

    // If base class doesn't have VTable then we should add slot for it
    if (!baseHasVtbl) {
      ++curOffset;
    }
  }

  // Now we should calculate offsets for variables
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    if (isa<VarDeclAST>(*it)) {
      ((VarDeclAST*)(*it))->OffsetOf = curOffset++;
    }
  }

  if (!Ctor) {
    // We should try to generate default constructor
    SymbolAST* ctorDecl = generateDefaultConstructor(this);

    // If constructor was generated. Perform semantic on it and add it
    if (ctorDecl) {
      ctorDecl->semantic(s);
      Vars.push_back(ctorDecl);
      Ctor = true;
    }
  }

  // Check do we need destructor or not
  if (generateDtor) {
    // Generate destructor with empty body. All needed stuff will be generated
    // during semantic
    ParameterList params;
    FuncTypeAST* dtorType = new FuncTypeAST(BuiltinTypeAST::get(TypeAST::TI_Void),
      params);
    SymbolAST* dtorDecl = new FuncDeclAST(Loc, dtorType, Name::Dtor, 
      new BlockStmtAST(Loc, defaultDtorBody), true, tok::Def);

    dtorDecl->semantic(s);

    // Check was destructor in base class virtual or not. If yes replace it
    if (VTbl && VTbl->isExist(dtorDecl)) {
      ((FuncDeclAST*)dtorDecl)->Tok = tok::Override;
      
      if (!vtableOwner) {
        VTbl = VTbl->clone(this);
      }

      VTbl->addOrReplace(scope, dtorDecl);
    }

    Vars.push_back(dtorDecl);
    Dtor = true;
  }

  s->pop();
}

void ClassDeclAST::doSemantic5(Scope* scope) {
  Scope* s = scope->push(this);

  // Perform semantic2 for all inner declarations
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    (*it)->semantic2(s);
  }

  // Perform semantic3 and semantic4 for all inner declarations
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    (*it)->semantic3(s);
    (*it)->semantic4(s);
  }

  // Now we can perform semantic5 for all inner declarations
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    (*it)->semantic5(s);
  }

  s->pop();
}

SymbolAST* ClassDeclAST::find(Name* id, int flags) {
  SymbolAST* res = ScopeSymbol::find(id, flags);

  if (res) {
    return res;
  }

  // Try to find symbol in the base class if needed
  if ((flags & 1) == 0) {
    if (BaseClass && BaseClass->isAggregate()) {
      return BaseClass->getSymbol()->find(id, flags);
    }
  }

  return res;
}

bool ClassDeclAST::contain(TypeAST* type) {
  // Try to find circular references in own members first
  for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
    if ((*it)->contain(type)) {
      return true;
    }
  }

  // If we have base class we should check it for circular references too
  if (BaseClass && BaseClass->isAggregate()) {
    return BaseClass->getSymbol()->contain(type);
  }

  return false;
}

// ParameterSymbolAST implementation
TypeAST* ParameterSymbolAST::getType() {
  return Param->Param;
}

bool ParameterSymbolAST::canBeNull() {
  if (Param->Id == Name::This || !isa<PointerTypeAST>(Param->Param)) {
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
bool FuncDeclAST::needThis() {
  return NeedThis;
}

TypeAST* FuncDeclAST::getType() {
  return ThisType;
}

void FuncDeclAST::doSemantic(Scope* scope) {
  // We have special case for class member function
  if (needThis()) {
    assert(isa<ClassDeclAST>(scope->CurScope));
    ClassDeclAST* classDecl = ((ClassDeclAST*)scope->CurScope);
    ParameterAST* thisParam = new ParameterAST(
      new PointerTypeAST(classDecl->ThisType, true),
      Name::This);
    FuncTypeAST* thisType = (FuncTypeAST*)ThisType;
    thisType->HasThis = true;
    thisType->Params.insert(thisType->Params.begin(), thisParam);
    AggregateSym = classDecl;

    if (Body) {
      BlockStmtAST* body = (BlockStmtAST*)Body;
      
      if (isCtor()) {
        // Try to check for base class constructor call without base class or
        // if base class doesn't have constructors to call
        if (!body->Body.empty() && isa<ExprStmtAST>(*body->Body.begin())) {
          if (!classDecl->BaseClass || !isa<ClassTypeAST>(classDecl->BaseClass)) {
            scope->report(Loc, diag::ERR_SemaBaseClassNoConstructorToCall);
            return;
          }

          ClassDeclAST* baseDecl = (ClassDeclAST*)classDecl->BaseClass->getSymbol();

          if (!baseDecl->Ctor) {
            scope->report(Loc, diag::ERR_SemaBaseClassNoConstructorToCall);
            return;
          }
        } else {
          // Constructor doesn't have base class constructor yet. Try to check
          // if we need it or not. If yes, then we should add default
          if (classDecl->BaseClass && isa<ClassTypeAST>(classDecl->BaseClass)) {
            ClassDeclAST* baseDecl = (ClassDeclAST*)classDecl->BaseClass->getSymbol();

            if (baseDecl->Ctor) {
              ExprList args;
              ExprStmtAST* exprStmt = new ExprStmtAST(
                Loc,
                new CallExprAST(
                  Loc,
                  new MemberAccessExprAST(
                    Loc,
                    new IdExprAST(Loc, Name::Super),
                    Name::Ctor), 
                  args));
              // Add default constructor call as 1st instruction. (Note: we
              // will add super variable declaration later)
              body->Body.insert(body->Body.begin(), exprStmt);
            }
          }
        }
      // Check destructor
      } else if (isDtor()) {
        // If we have base class with destructor then we should add it's call
        if (classDecl->BaseClass && isa<ClassTypeAST>(classDecl->BaseClass)) {
          ClassDeclAST* baseDecl = (ClassDeclAST*)classDecl->BaseClass->getSymbol();

          if (baseDecl->Dtor) {
            ExprList args;
            ExprStmtAST* exprStmt = new ExprStmtAST(
              Loc,
              new CallExprAST(
                Loc,
                new MemberAccessExprAST(
                  Loc,
                  new IdExprAST(Loc, Name::Super),
                  Name::Dtor), 
                args));
            body->Body.push_back(exprStmt);
          }
        }
      }

      // if we have base class we should define super variable
      if (classDecl->BaseClass) {
        VarDeclAST* superVar = new VarDeclAST(
          Loc, 
          new PointerTypeAST(classDecl->BaseClass, true), 
          Name::Super,
          new IdExprAST(Loc, Name::This),
          false);
        SymbolList tmp;

        tmp.push_back(superVar);
        body->Body.insert(body->Body.begin(), new DeclStmtAST(Loc, tmp));
      }
    }
  }

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
    // If it's function then we should create overload set
    if (isa<FuncDeclAST>(fncOverload)) {
      OverloadSetAST* newOverloadSet = new OverloadSetAST(Id);
      
      newOverloadSet->Parent = Parent;
      newOverloadSet->push(scope, fncOverload);
      newOverloadSet->push(scope, this);

      ((ScopeSymbol*)Parent)->Decls[Id] = newOverloadSet;
      return;
    }

    // It's overload set add function to this set
    if (isa<OverloadSetAST>(fncOverload)) {
      ((OverloadSetAST*)fncOverload)->push(scope, this);
      return;
    }
    
    scope->report(Loc, diag::ERR_SemaFunctionRedefined, Id->Id);
    return;
  }

  // Add function declaration to current scope
  ((ScopeSymbol*)Parent)->Decls[Id] = this;
}

void FuncDeclAST::doSemantic5(Scope* scope) {
  FuncTypeAST* func = (FuncTypeAST*)ThisType;
  // If Body is nullptr then it's prototype and no semantic needed
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

// OverloadSetAST implementation
void OverloadSetAST::push(Scope *scope, SymbolAST* func) {
  assert(isa<FuncDeclAST>(func));
  FuncDeclAST* funcDecl = (FuncDeclAST*)func;

  // If function don't have mangle name yet calculate it
  if (!funcDecl->ThisType->MangleName.empty()) {
    funcDecl->ThisType->calcMangle();
  }

  // We should check that function with same name and such arguments not yet in 
  // overload set
  std::pair<llvm::StringSet< >::iterator, bool> res = OverloadsUsed.insert(funcDecl->ThisType->MangleName);

  if (!res.second) {
    for (SymbolList::iterator it = Vars.begin(), end = Vars.end(); it != end; ++it) {
      FuncDeclAST* oldFn = (FuncDeclAST*)(*it);
      
      if (oldFn->ThisType->equal(funcDecl->ThisType)) {
        if (oldFn->Parent != func->Parent) {
          (*it) = func;
          return;
        }

        break;
      }
    }

    scope->report(funcDecl->Loc, diag::ERR_SemaDuplicateFunctions, funcDecl->Id->Id);
    return;
  }

  Vars.push_back(func);
}

OverloadSetAST* OverloadSetAST::clone() {
  OverloadSetAST* newSet = new OverloadSetAST(Id);

  newSet->Parent = Parent;
  newSet->Vars.assign(Vars.begin(), Vars.end());
  newSet->OverloadsUsed = OverloadsUsed;

  return newSet;
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
  addDynamicFunc("fn print(_: char)", "lle_X_printChar", modDecl, (void*)lle_X_printChar);
  addDynamicFunc("fn print(_: int)", "lle_X_printInt", modDecl, (void*)lle_X_printInt);
  addDynamicFunc("fn print(_: float)", "lle_X_printDouble", modDecl, (void*)lle_X_printDouble);
  addDynamicFunc("fn print(_: string)", "lle_X_printString", modDecl, (void*)lle_X_printString);
  addDynamicFunc("fn printLn(_: string)", "lle_X_printLine", modDecl, (void*)lle_X_printLine);
  addDynamicFunc("fn new(_: int) : void*", "lle_X_new", modDecl, (void*)lle_X_new);
  addDynamicFunc("fn delete(_: void*)", "lle_X_delete", modDecl, (void*)lle_X_delete);
}

ModuleDeclAST* ModuleDeclAST::load(SourceMgr &SrcMgr, DiagnosticsEngine &Diags, StringRef fileName) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
        FileOrErr = llvm::MemoryBuffer::getFile(fileName);

  if (std::error_code BufferError =
          FileOrErr.getError()) {
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