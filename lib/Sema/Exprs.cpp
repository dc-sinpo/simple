#include "simple/AST/AST.h"

using namespace llvm;

namespace simple {

// ExprAST hierarchy implementation

// ExprAST implementation
bool ExprAST::isTrue() { 
  return false; 
}

bool ExprAST::isLValue() {
  return false;
}

SymbolAST* ExprAST::getAggregate(Scope *) {
  assert(0 && "ExprAST::getAggregaet should never be reached");
  return nullptr;
}

bool ExprAST::canBeNull() {
  return false;
}

// IntExprAST implementation
bool IntExprAST::isTrue() {
  return Val != 0;
}

ExprAST* IntExprAST::semantic(Scope* ) {
  return this;
}

ExprAST* IntExprAST::clone() {
  return new IntExprAST(Loc, Val);
}

// FloatExprAST implementation
bool FloatExprAST::isTrue() {
  return Val != 0.0;
}

ExprAST* FloatExprAST::semantic(Scope* ) {
  return this;
}

ExprAST* FloatExprAST::clone() {
  return new FloatExprAST(Loc, Val);
}

// StringExprAST implementation
bool StringExprAST::isTrue() {
  return true;
}

ExprAST* StringExprAST::semantic(Scope* ) {
  return this;
}

ExprAST* StringExprAST::clone() {
  return new StringExprAST(Loc, StringRef(Val));
}

// ProxyExprAST implementation
ExprAST* ProxyExprAST::semantic(Scope* scope) {
  // Make sure that it was used for expressions which can't change self during semantic
  ExprAST* tmp = OriginalExpr->semantic(scope);
  assert(tmp == OriginalExpr);
  ExprType = OriginalExpr->ExprType;
  return this;
}

ExprAST* ProxyExprAST::clone() {
  return new ProxyExprAST(Loc, OriginalExpr);
}

bool NewExprAST::canBeNull() {
  return true;
}

// NewExprAST implementation
ExprAST* NewExprAST::semantic(Scope* scope) {
  if (ExprType) {
    return this;
  }

  // Perform semantic on NewType
  NewType = NewType->semantic(scope);

  // Check for array type
  if (isa<ArrayTypeAST>(NewType) && !DynamicSize) {
    // We convert resulted type to pointer type but still allocate as array 
    ArrayTypeAST* arrayType = (ArrayTypeAST*)NewType;
    ExprType = new PointerTypeAST(arrayType->Next, false);
    ExprType = ExprType->semantic(scope);
  } else {
    // Result type is pointer to NewType
    ExprType = new PointerTypeAST(NewType, false);
    ExprType = ExprType->semantic(scope);
  }

  // Create SizeExpr as integral constant and wrap it with proxy expression
  // because we need patch it with actual type's size during code generation
  SizeExpr = new IntExprAST(Loc, 0);
  ExprAST* proxy = new ProxyExprAST(Loc, SizeExpr);

  if (DynamicSize) {
    proxy = new BinaryExprAST(Loc, tok::Mul, DynamicSize, proxy);
  }

  // Create allocMem call with size of allocated type
  ExprList args;
  args.push_back(proxy);
  CallExpr = new CallExprAST(Loc, new IdExprAST(Loc, Name::New), args);

  // Perform semantic for just created call expression
  CallExpr = CallExpr->semantic(scope);

  return this;
}

ExprAST* NewExprAST::clone() {
  ExprList exprs;
  ExprList::iterator it = Args.begin();
  ExprList::iterator end = Args.end();

  for ( ; it != end; ++it) {
    exprs.push_back((*it)->clone());
  }

  return new NewExprAST(Loc, NewType, DynamicSize, exprs);
}

// DeleteExprAST implementation
ExprAST* DeleteExprAST::semantic(Scope* scope) {
  if (DeleteCall) {
    return this;
  }

  // Check semantic of expression to delete
  Val = Val->semantic(scope);

  // void value not allowed
  if (!Val->ExprType || Val->ExprType->isVoid()) {
    scope->report(Loc, diag::ERR_SemaCantDeleteVoid);
    return nullptr;
  }

  // Type of the expression to delete should have pointer type
  if (!isa<PointerTypeAST>(Val->ExprType)) {
    scope->report(Loc, diag::ERR_SemaCantDeleteNonPointer);
    return nullptr;
  }

  PointerTypeAST* ptrType = (PointerTypeAST*)Val->ExprType;

  ExprAST* deleteArg = Val->clone();
  ExprList args;

  // Add argument for freeMem call
  // Note: It can be Val or result of destructor's call
  args.push_back(deleteArg);
  
  // Create freeMem call and check it's semantic
  DeleteCall = new CallExprAST(Loc, new IdExprAST(Loc, Name::Delete), args);
  DeleteCall = DeleteCall->semantic(scope);

  ExprType = BuiltinTypeAST::get(TypeAST::TI_Void);

  return this;
}

ExprAST* DeleteExprAST::clone() {
  return new DeleteExprAST(Loc, Val->clone());
}

// IdExprAST implementation
bool IdExprAST::isLValue() {
  if (isa<PointerTypeAST>(ExprType)) {
    return !((PointerTypeAST*)ExprType)->IsConstant;
  }

  return true;
}

SymbolAST* IdExprAST::getAggregate(Scope *) {
  return ThisSym;
}

bool IdExprAST::canBeNull() {
  return ThisSym->canBeNull();
}

ExprAST* IdExprAST::semantic(Scope* scope) {
  // if ThisSym isn't nullptr then semantic pass was already passed for this
  // expression
  if (!ThisSym) {
    // Try to find identifier in the current scope
    ThisSym = scope->find(Val);

    if (!Val) {
      return this;
    }

    if (!ThisSym) {
      // Undefined identifier
      scope->report(Loc, diag::ERR_SemaUndefinedIdentifier, Val->Id);
      return nullptr;
    }

    ExprType = ThisSym->getType();
  }

  return this;
}

ExprAST* IdExprAST::clone() {
  return new IdExprAST(Loc, Val);
}

// IndexExprAST implementation
bool IndexExprAST::isLValue() {
  if (Left->ExprType->isString()) {
    return false;
  }

  return true;
}

SymbolAST* IndexExprAST::getAggregate(Scope *scope) {
  if (!ExprType->isAggregate()) {
    scope->report(Loc, diag::ERR_SemaNonAggregateDotOperand);
    return nullptr;
  }

  return ExprType->getSymbol();
}

bool IndexExprAST::canBeNull() {
  if (isa<PointerTypeAST>(ExprType)) {
    return true;
  }

  return false;
}

ExprAST* IndexExprAST::semantic(Scope* scope) {
  if (ExprType) {
    return this;
  }

  // Perform semantic on left and right operand
  Left = Left->semantic(scope);
  Right = Right->semantic(scope);

  // Indexed value should have array, pointer or string types
  if (Left->ExprType && !isa<ArrayTypeAST>(Left->ExprType) && !isa<PointerTypeAST>(Left->ExprType) &&
    !Left->ExprType->isString()) {
    scope->report(Loc, diag::ERR_SemaNonIndexableType);
    return nullptr;
  }

  // Cast index if needed
  if (!Right->ExprType->isInt()) {
    Right = new CastExprAST(Right->Loc, Right, BuiltinTypeAST::get(TypeAST::TI_Int));
    Right = Right->semantic(scope);
  }

  // Set result type
  if (isa<ArrayTypeAST>(Left->ExprType)) {
    ExprType = ((ArrayTypeAST*)Left->ExprType)->Next;
  } else if (isa<PointerTypeAST>(Left->ExprType)) {
    ExprType = ((PointerTypeAST*)Left->ExprType)->Next;
  } else {
    ExprType = BuiltinTypeAST::get(TypeAST::TI_Char);
  }

  return this;
}

ExprAST* IndexExprAST::clone() {
  return new IndexExprAST(Loc, Left->clone(), Right->clone());
}

// MemberAccessExprAST implementation
bool MemberAccessExprAST::isLValue() {
  return true;
}

SymbolAST* MemberAccessExprAST::getAggregate(Scope *) {
  return ThisSym;
}

bool MemberAccessExprAST::canBeNull() {
  return Val->canBeNull();
}

ExprAST* MemberAccessExprAST::semantic(Scope* scope) {
  if (SemaDone) {
    return this;
  }

  Val = Val->semantic(scope);

  // Special case for module name
  if (isa<IdExprAST>(Val) && !((IdExprAST*)Val)->Val) {
    SymbolAST* moduleSym = scope->find(0);
    assert(moduleSym);
    SymbolAST* thisSym = moduleSym->find(MemberName);

    if (!thisSym) {
      scope->report(Loc, diag::ERR_SemaUndefinedMember, MemberName->Id);
      return nullptr;
    }

    // We replace MemberAccessExprAST with IdExprAST with preset values
    IdExprAST* newExpr = new IdExprAST(Loc, MemberName);

    newExpr->ExprType = thisSym->getType();

    newExpr->ThisSym = thisSym;
    delete this;
    return newExpr;
  }
  
  if (!Val->ExprType) {
    scope->report(Loc, diag::ERR_SemaInvalidOperandForMemberAccess);
    return nullptr;
  }

  // If Val is pointer to aggregate type then we should add deref. We treat
  // agg->a same as agg.a
  if (isa<PointerTypeAST>(Val->ExprType) && ((PointerTypeAST*)Val->ExprType)->Next->isAggregate()) {
    Val = new DerefExprAST(Loc, Val);
    Val = Val->semantic(scope);
  }

  // Val should be lvalue
  if (!Val->isLValue()) {
    scope->report(Loc, diag::ERR_SemaNonLValueForMemberAccess);
    return nullptr;
  }

  SymbolAST* aggSym = Val->getAggregate(scope);

  // We disable A.b usage. It should be a.b where a is instance of A
  if (isa<IdExprAST>(Val) && aggSym && aggSym->isAggregate()) {
    // Check special case for base class function or variable
    scope->report(Loc, diag::ERR_SemaMemberAccessOnAggregateType);
    return nullptr;
  }
  
  if (!aggSym || !aggSym->getType()->isAggregate()) {
    scope->report(Loc, diag::ERR_SemaNonAggregateDotOperand);
    return nullptr;
  }

  if (!aggSym->isAggregate()) {
    aggSym = aggSym->getType()->getSymbol();
  }

  ThisSym = aggSym->find(MemberName);
  ThisAggr = aggSym;

  // We should have symbol or it's error  
  if (!ThisSym) {
    scope->report(Loc, diag::ERR_SemaUndefinedMember, MemberName->Id);
    return nullptr;
  }
  
  // We disallow a.A where A is aggregate type
  if (ThisSym->isAggregate()) {
    scope->report(Loc, diag::ERR_SemaAggregateTypeAsMemberAccessOperand);
    return nullptr;
  }

  ExprType = ThisSym->getType();

  SemaDone = true;

  return this;
}

ExprAST* MemberAccessExprAST::clone() {
  return new MemberAccessExprAST(Loc, Val->clone(), MemberName);
}

// PointerAccessExprAST implementation
bool PointerAccessExprAST::isLValue() {
  return true;
}

ExprAST* PointerAccessExprAST::semantic(Scope* scope) {
  /// Rewrite expr->member to (*expr).member, perform semantic on it and delete
  // this
  ExprAST* newExpr = new DerefExprAST(Loc, Val);
  newExpr = new MemberAccessExprAST(Loc, newExpr, MemberName);
  Val = nullptr;
  delete this;
  return newExpr->semantic(scope);
}

ExprAST* PointerAccessExprAST::clone() {
  return new PointerAccessExprAST(Loc, Val->clone(), MemberName);
}

// DerefExprAST implementation
bool DerefExprAST::isLValue() {
  return true;
}

SymbolAST* DerefExprAST::getAggregate(Scope *scope) {
  // Check for aggregate
  if (!ExprType->isAggregate()) {
    scope->report(Loc, diag::ERR_SemaNonAggregateDotOperand);
    return nullptr;
  }

  return ExprType->getSymbol();
}

bool DerefExprAST::canBeNull() {
  return true;
}

ExprAST* DerefExprAST::semantic(Scope* scope) {
  if (ExprType) {
    return this;
  }

  // Check semantic for nested value
  Val = Val->semantic(scope);

  // Check for address expression
  if (isa<AddressOfExprAST>(Val)) {
    // Remove both dereference and address and delete this
    ExprAST* result = ((AddressOfExprAST*)Val)->Val->clone();
    delete this;
    return result->semantic(scope);
  }

  // Check for valid pointer type
  if (Val->ExprType) {
    if (Val->ExprType->isString()) {
      if (Val->isConst()) {
        scope->report(Loc, diag::ERR_SemaConstStringDeRef);
        return nullptr;
      }

      ExprType = BuiltinTypeAST::get(TypeAST::TI_Char);
      return this;
    } else if (!isa<PointerTypeAST>(Val->ExprType)) {
      scope->report(Loc, diag::ERR_SemaUnDereferencableType);
      return nullptr;
    }
  } else {
    scope->report(Loc, diag::ERR_SemaUnDereferencableType);
    return nullptr;
  }

  ExprType = ((PointerTypeAST*)Val->ExprType)->Next;
  return this;
}

ExprAST* DerefExprAST::clone() {
  return new DerefExprAST(Loc, Val->clone());
}

// AddressOfExprAST implementation
SymbolAST* AddressOfExprAST::getAggregate(Scope *scope) {
  return Val->getAggregate(scope);
}

bool AddressOfExprAST::canBeNull() {
  if (isa<IdExprAST>(Val) && !Val->canBeNull()) {
    return false;
  }

  return true;
}

ExprAST* AddressOfExprAST::semantic(Scope* scope) {
  if (ExprType) {
    return this;
  }

  // Check semantic for nested value
  Val = Val->semantic(scope);

  // We need valid type for nested value
  if (!Val->ExprType) {
    scope->report(Loc, diag::ERR_SemaAddressExpressionNoType);
    return nullptr;
  }

  // Check for dereference
  if (isa<DerefExprAST>(Val)) {
    // Remove both dereference and address expressions, perform semantic and
    // delete this
    ExprAST* result = ((DerefExprAST*)Val)->Val->clone();
    delete this;
    return result->semantic(scope);
  }

  // Address expression should always be lvalue
  if (!Val->isLValue()) {
    scope->report(Loc, diag::ERR_SemaAddressOfNonLValue);
    return nullptr;
  }

  // Set expression's type as pointer to type of the nested value
  ExprType = new PointerTypeAST(Val->ExprType, false);
  ExprType = ExprType->semantic(scope);

  return this;
}

ExprAST* AddressOfExprAST::clone() {
  return new AddressOfExprAST(Loc, Val->clone());
}

// CastExprAST implementation
ExprAST* CastExprAST::semantic(Scope* scope) {
  if (SemaDone) {
    return this;
  }

  // Make sure that we set type for cast
  assert(ExprType != nullptr && "Type for cast not set"); 
  if (ExprType->isVoid()) {
    scope->report(Loc, diag::ERR_SemaCastToVoid);
    return nullptr;
  }

  // Check semantic of inner expression
  Val = Val->semantic(scope);

  // Check for validity of inner expression's type
  if (!Val->ExprType || Val->ExprType->isVoid()) {
    scope->report(Loc, diag::ERR_SemaCastToVoid);
    return nullptr;
  }

  // Disallow cast from and to function type
  if (isa<FuncTypeAST>(ExprType) || isa<FuncTypeAST>(Val->ExprType)) {
    scope->report(Loc, diag::ERR_SemaFunctionInCast);
    return nullptr;
  }

  // Disallow cast to array type
  if (isa<ArrayTypeAST>(ExprType)) {
    scope->report(Loc, diag::ERR_SemaArrayInCast);
    return nullptr;
  }

  // Check for pointer and it's conversion
  if (isa<PointerTypeAST>(ExprType) && !Val->ExprType->implicitConvertTo(ExprType)) {
    scope->report(Loc, diag::ERR_SemaIncompatiblePointerTypes);
    return nullptr;
  }

  // Check for conversion
  if (!Val->ExprType->implicitConvertTo(ExprType)) {
    scope->report(Loc, diag::ERR_SemaInvalidCast);
    return nullptr;
  }

  SemaDone = true;
  return this;
}

ExprAST* CastExprAST::clone() {
  return new CastExprAST(Loc, Val->clone(), ExprType);
}

// UnaryExprAST implementation
ExprAST* UnaryExprAST::semantic(Scope* scope) {
  // Unary expression always should have operand
  if (!Val) {
    assert(0 && "Invalid expression value");
    return nullptr;
  }

  // Check semantic of operand
  Val = Val->semantic(scope);

  // Check for void value as operand (can be from void function call)
  if (!Val->ExprType || Val->ExprType->isVoid()) {
    scope->report(Loc, diag::ERR_SemaOperandIsVoid);
    return nullptr;
  }

  // We disallow aggregates
  if (Val->ExprType->isAggregate()) {
    scope->report(Loc, diag::ERR_SemaAggregateAsExpression);
    return nullptr;
  }

  // strings (except ++/--)
  if (Val->ExprType->isString() && (Op != tok::MinusMinus && Op != tok::PlusPlus)) {
    scope->report(Loc, diag::ERR_SemaInvalidUnaryExpressionForString);
    return nullptr;
  }

  // arrays 
  if (isa<ArrayTypeAST>(Val->ExprType)) {
    scope->report(Loc, diag::ERR_SemaInvalidUnaryExpressionForArray);
    return nullptr;
  }

  // pointers (except ++/--)
  if (isa<PointerTypeAST>(Val->ExprType) && (Op != tok::MinusMinus && Op != tok::PlusPlus)) {
    scope->report(Loc, diag::ERR_SemaInvalidUnaryExpressionForPointer);
    return nullptr;
  }

  if (Val->ExprType->isBool() && (Op == tok::Plus || Op == tok::Minus)) {
    scope->report(Loc, diag::ERR_SemaInvalidBoolForUnaryOperands);
    return nullptr;
  }

  //rewrite:
  //  +Val to Val
  //  -Val to 0 - Val
  //  ~intVal to intVal ^ -1
  //  ++id to id = id + 1 
  //  --id to id = id - 1
  //  !Val to Val == 0

  ExprAST* result = this;

  // Check original unary operator and convert it to binary expression
  switch (Op) {
    // + not affect result value at all, simply remove it
    case tok::Plus: result = Val; break;

    case tok::Minus: 
      // Convert to 0 - Val
      if (Val->ExprType->isFloat()) {
        result = new BinaryExprAST(Val->Loc, tok::Minus, new FloatExprAST(Val->Loc, 0), Val);
      } else {
        result = new BinaryExprAST(Val->Loc, tok::Minus, new IntExprAST(Val->Loc, 0), Val);
      }
      break;

    case tok::Tilda:
      // ~ operator only available for int type
      if (!Val->ExprType->isInt()) {
        scope->report(Loc, diag::ERR_SemaInvalidOperandForComplemet);
        return nullptr;
      } else {
        // Convert to Val ^ -1
        result = new BinaryExprAST(Val->Loc, tok::BitXor, Val, new IntExprAST(Val->Loc, -1));
        break;
      }

    case tok::PlusPlus:
    case tok::MinusMinus:
      // We allow ++ or -- only for IdExprAST or IndexExprAST as operand
      // Note: ++ ++ id not allowed too
      if ((!Val->ExprType->isInt() && !isa<PointerTypeAST>(Val->ExprType) && 
        !Val->ExprType->isString()) || !Val->isLValue()) {
        scope->report(Loc, diag::ERR_SemaInvalidPostfixPrefixOperand);
        return nullptr;
      } else {
        // We have special case for IndexExprAST and MemberAccessAST operand
        if (isa<IndexExprAST>(Val) || isa<MemberAccessExprAST>(Val) || Val->ExprType->isString()) {
          ExprType = Val->ExprType;
          return this;
        }

        if (isa<PointerTypeAST>(Val->ExprType)) {
          // We need to convert ++ ptr or -- ptr to ptr = &ptr[1] or ptr = &ptr[-1]
          ExprAST* val = Val;
          ExprAST* valCopy = Val->clone();
          result = new BinaryExprAST(Val->Loc, tok::Assign,
            val,
            new AddressOfExprAST(Val->Loc,
              new IndexExprAST(Val->Loc, valCopy, 
                new IntExprAST(Val->Loc, (Op == tok::PlusPlus) ? 1 : -1))));
        } else {
          // We need to convert ++ id or -- id to id = id + 1 or id = id + -1
          ExprAST* val = Val;
          ExprAST* valCopy = Val->clone();
          result = new BinaryExprAST(Val->Loc, tok::Assign, 
            val,
            new BinaryExprAST(Val->Loc, tok::Plus,
              valCopy, 
             new IntExprAST(Val->Loc, (Op == tok::PlusPlus) ? 1 : -1)));
        }
      }
      break;

    case tok::Not:
      // We should convert it to Val == 0
      result = new BinaryExprAST(Val->Loc, tok::Equal, Val, new IntExprAST(Val->Loc, 
        0));
      break;

    default:
      // Should never happen
      assert(0 && "Invalid unary expression");
      return nullptr;
  }

  if (result != this) {
    // Because old unary expression was replaced we should delete this and
    // perform semantic on new expression
    Val = nullptr;
    delete this;
    return result->semantic(scope);
  }

  return result;
}

ExprAST* UnaryExprAST::clone() {
  return new UnaryExprAST(Loc, Op, Val->clone());
}

// BinaryExprAST implementation
ExprAST* BinaryExprAST::semantic(Scope* scope) {
  if (ExprType) {
    return this;
  }

  // Perform semantic on left operand of an expression
  LeftExpr = LeftExpr->semantic(scope);

  // Check for ++ or -- because it's special case binary operator with only
  // 1 operand
  if (Op == tok::PlusPlus || Op == tok::MinusMinus) {
    // We allow only id of int type as operand of ++/-- operator
    if (!LeftExpr->isLValue() || (!LeftExpr->ExprType->isInt() && 
      !isa<PointerTypeAST>(LeftExpr->ExprType) && !LeftExpr->ExprType->isString())) {
      scope->report(Loc, diag::ERR_SemaInvalidPostfixPrefixOperand);
      return nullptr;
    }

    // Set resulted type and return this
    ExprType = LeftExpr->ExprType;
    return this;
  }

  // Perform semantic on right operand of an expression
  RightExpr = RightExpr->semantic(scope);

  // Check for validity of operands
  if (!LeftExpr->ExprType || !RightExpr->ExprType) {
    scope->report(Loc, diag::ERR_SemaUntypedBinaryExpressionOperands);
    return nullptr;
  }

  // Disallow aggregates in binary expressions
  if (LeftExpr->ExprType->isAggregate() || RightExpr->ExprType->isAggregate()) {
    scope->report(Loc, diag::ERR_SemaAggregateAsExpression);
    return nullptr;
  }

  // Operator , is special case. Resulted type is always type of right operand
  if (Op == tok::Comma) {
    // Set resulted type and return this
    ExprType = RightExpr->ExprType;
    return this;
  }

  // Disable binary operator with void operand
  if (LeftExpr->ExprType->isVoid() || RightExpr->ExprType->isVoid()) {
    scope->report(Loc, diag::ERR_SemaOperandIsVoid);
    return nullptr;
  }

  // We should check binary operator for special case compare operators
  // which have bool result type
  switch (Op) {
    case tok::Less:
    case tok::Greater:
    case tok::LessEqual:
    case tok::GreaterEqual:
    case tok::Equal:
    case tok::NotEqual:
      // Disable string, pointers and arrays in comparisons
      if (LeftExpr->ExprType->isString() || isa<PointerTypeAST>(LeftExpr->ExprType) || 
        isa<ArrayTypeAST>(LeftExpr->ExprType) || RightExpr->ExprType->isString() ||
        isa<PointerTypeAST>(RightExpr->ExprType) || isa<ArrayTypeAST>(RightExpr->ExprType)) {
        scope->report(Loc, diag::ERR_SemaInvalidTypeForComparison);
        return nullptr;
      }

      // If left operand is bool then we should convert it to an int
      if (LeftExpr->ExprType->isBool()) {
        LeftExpr = new CastExprAST(LeftExpr->Loc, LeftExpr, 
          BuiltinTypeAST::get(TypeAST::TI_Int));
        LeftExpr = LeftExpr->semantic(scope);
      }

      // If left operand is char then we should convert it to an int
      if (LeftExpr->ExprType->isChar()) {
        LeftExpr = new CastExprAST(LeftExpr->Loc, LeftExpr, 
          BuiltinTypeAST::get(TypeAST::TI_Int));
        LeftExpr = LeftExpr->semantic(scope);
      }

      // <, <=, >, >=, == and != should have both operand of the same type,
      // perform conversion if needed
      if (!LeftExpr->ExprType->equal(RightExpr->ExprType)) {
        // Perform conversion
        RightExpr = new CastExprAST(RightExpr->Loc, RightExpr, LeftExpr->ExprType);
        RightExpr = RightExpr->semantic(scope);
      }
      
      // Set resulted type to bool and return this
      ExprType = BuiltinTypeAST::get(TypeAST::TI_Bool);
      return this;

    case tok::LogOr:
    case tok::LogAnd:
      // Check for conversion of left and right expressions to boolean
      if (!LeftExpr->ExprType->implicitConvertTo(BuiltinTypeAST::get(TypeAST::TI_Bool)) ||
        !RightExpr->ExprType->implicitConvertTo(BuiltinTypeAST::get(TypeAST::TI_Bool))) {
        scope->report(Loc, diag::ERR_SemaCantConvertToBoolean);
        return nullptr;
      }

      // Set resulted type to bool and return this
      ExprType = BuiltinTypeAST::get(TypeAST::TI_Bool);
      return this;

    default:
      // Check other cases
      break;
  }

  // If left operand is bool then we should convert it to an int
  if (LeftExpr->ExprType == BuiltinTypeAST::get(TypeAST::TI_Bool)) {
    LeftExpr = new CastExprAST(LeftExpr->Loc, LeftExpr, 
      BuiltinTypeAST::get(TypeAST::TI_Int));
    LeftExpr = LeftExpr->semantic(scope);
  }

  // Resulted type is type of the left operand
  ExprType = LeftExpr->ExprType;

  // = operator is special case with IdExprAST as left part. Check it
  if (Op == tok::Assign) {
    // if resulted type is pointer then check right operand
    if (isa<PointerTypeAST>(ExprType)) {
      // Check for constant integral 0 value
      if (RightExpr->isIntConst() && ((IntExprAST*)RightExpr)->Val != 0) {
        scope->report(Loc, diag::ERR_SemaPointerInitialization);
        return nullptr;
      // Check for right operand compatibility
      } else if (!RightExpr->ExprType->implicitConvertTo(ExprType)) {
        scope->report(Loc, diag::ERR_SemaPointerInitialization);
        return nullptr;
      } else {
        // Generate cast
        RightExpr = new CastExprAST(RightExpr->Loc, RightExpr, LeftExpr->ExprType);
        RightExpr = RightExpr->semantic(scope);
      }
    } else {
      // Perform cast on right operand if needed
      if (!LeftExpr->ExprType->equal(RightExpr->ExprType)) {
        RightExpr = new CastExprAST(RightExpr->Loc, RightExpr, LeftExpr->ExprType);
        RightExpr = RightExpr->semantic(scope);
      }
    }

    // Check for lvalue of left operand
    if (!LeftExpr->isLValue()) {
      scope->report(Loc, diag::ERR_SemaMissingLValueInAssignment);
      return nullptr;
    }

    // It's valid expression. Return this
    return this;
  }

  // Disable string, arrays and pointers from usage in expressions
  if (isa<ArrayTypeAST>(LeftExpr->ExprType) || isa<ArrayTypeAST>(RightExpr->ExprType)) {
    scope->report(Loc, diag::ERR_SemaCantUseArrayInBinaryExpression);
    return nullptr;
  }

  if (isa<PointerTypeAST>(LeftExpr->ExprType) || LeftExpr->ExprType->isString() || 
    isa<PointerTypeAST>(RightExpr->ExprType) || RightExpr->ExprType->isString()) {
    if (Op != tok::Plus && Op != tok::Minus) {
      scope->report(Loc, diag::ERR_SemaPointerOrStringInBinaryExpression);
      return nullptr;
    }

    if (isa<PointerTypeAST>(LeftExpr->ExprType) || LeftExpr->ExprType->isString()) {
      if (!RightExpr->ExprType->isInt()) {
        scope->report(Loc, diag::ERR_SemaPointerArithmeticsForNonInt);
        return nullptr;
      }

      ExprType = LeftExpr->ExprType;
      return this;
    } else {
      scope->report(Loc, diag::ERR_SemaPointerArithmeticsForNonInt);
      return nullptr;
    }
  }

  // If left operand is char convert it to an int
  if (LeftExpr->ExprType->isChar()) {
    LeftExpr = new CastExprAST(LeftExpr->Loc, LeftExpr, 
      BuiltinTypeAST::get(TypeAST::TI_Int));
    LeftExpr = LeftExpr->semantic(scope);
    ExprType = LeftExpr->ExprType;
  }

  // Perform cast on right operand if needed
  if (!LeftExpr->ExprType->equal(RightExpr->ExprType)) {
    // If right operand is float convert all expression to float
    if (RightExpr->ExprType->isFloat()) {
      ExprType = RightExpr->ExprType;
      LeftExpr = new CastExprAST(LeftExpr->Loc, LeftExpr, RightExpr->ExprType);
      LeftExpr = LeftExpr->semantic(scope);
    } else {
      // Convert right expression's type to left expression's type
      RightExpr = new CastExprAST(RightExpr->Loc, RightExpr, LeftExpr->ExprType);
      RightExpr = RightExpr->semantic(scope);
    }
  }

  // We have different sets of valid binary operators for int and float types
  if (ExprType == BuiltinTypeAST::get(TypeAST::TI_Int)) {
    // Check valid int binary operators
    switch (Op) {
      case tok::Plus:
      case tok::Minus:
      case tok::Mul:
      case tok::Div:
      case tok::Mod:
      case tok::BitOr:
      case tok::BitAnd:
      case tok::BitXor:
      case tok::LShift:
      case tok::RShift:
        return this;

      default:
        // The only way we had this error is invalid operator set in parser
        assert(0 && "Invalid integral binary operator"); 
        return nullptr;
    }
  } else {
    // Check valid floating point binary operators
    switch (Op) {
      case tok::Plus: 
      case tok::Minus: 
      case tok::Mul: 
      case tok::Div: 
      case tok::Mod: 
      case tok::Less: 
        return this;

      default:
        // It's invalid floating point binary operator
        scope->report(Loc, diag::ERR_SemaInvalidBinaryExpressionForFloatingPoint);
        return nullptr;
    }
  }
}

ExprAST* BinaryExprAST::clone() {
  return new BinaryExprAST(Loc, Op, LeftExpr->clone(), 
    RightExpr ? RightExpr->clone() : nullptr);
}

// CondExprAST implementation
bool CondExprAST::isLValue() {
  return IfExpr->isLValue() && ElseExpr->isLValue();
}

ExprAST* CondExprAST::semantic(Scope* scope) {
  if (SemaDone) {
    return this;
  }

  // Perform semantic on condition, then and else parts of an expression
  Cond = Cond->semantic(scope);
  IfExpr = IfExpr->semantic(scope);
  ElseExpr = ElseExpr->semantic(scope);

  // Conditional part shouldn't have void type
  if (Cond->ExprType == nullptr || Cond->ExprType->isVoid()) {
    scope->report(Loc, diag::ERR_SemaConditionIsVoid);
    return nullptr;
  }

  // Check that condition can be converted to boolean
  if (!Cond->ExprType->implicitConvertTo(BuiltinTypeAST::get(TypeAST::TI_Bool))) {
    scope->report(Loc, diag::ERR_SemaCantConvertToBoolean);
    return nullptr;
  }

  // Resulted type is type of left part
  ExprType = IfExpr->ExprType;

  // If both parts have same types then we are done
  if (IfExpr->ExprType->equal(ElseExpr->ExprType)) {
    SemaDone = true;
    return this;
  }

  // We disallow cases when one of operands have void type (both void are allowed)
  if (!IfExpr->ExprType || !ElseExpr->ExprType ||
    IfExpr->ExprType->isVoid() || ElseExpr->ExprType->isVoid()) {
    scope->report(Loc, diag::ERR_SemaOperandIsVoid);
    return nullptr;
  }

  // Convert else part to type of the then part
  ElseExpr = new CastExprAST(ElseExpr->Loc, ElseExpr, ExprType);
  ElseExpr = ElseExpr->semantic(scope);
  SemaDone = true;

  return this;
}

ExprAST* CondExprAST::clone() {
  return new CondExprAST(Loc, Cond->clone(), IfExpr->clone(), ElseExpr->clone());
}

// CallExprAST implementation

/// Resolve function \c func call with \c arg as arguments
/// \param[in] func - function or overload set to call
/// \param[in] args - arguments to call with
/// \return Function which should be called
static SymbolAST* resolveFunctionCall(Scope *scope, SymbolAST* func, CallExprAST* args) {
  // We have simple case for only 1 function
  if (isa<FuncDeclAST>(func)) {
    FuncDeclAST* fnc = static_cast< FuncDeclAST* >(func);
    FuncTypeAST* type = static_cast< FuncTypeAST* >(fnc->ThisType);

    // Check number of arguments
    if (args->Args.size() != type->Params.size()) {
      scope->report(args->Loc, diag::ERR_SemaInvalidNumberOfArgumentsInCall);
      return nullptr;
    }

    ExprList::iterator arg = args->Args.begin();
    ParameterList::iterator it = type->Params.begin();

    // Check all arguments
    for (ParameterList::iterator end = type->Params.end(); it != end; ++it, ++arg) {
      // Check can argument be passed to function or not. If not then we signal
      // about error
      if (!(*arg)->ExprType->implicitConvertTo((*it)->Param)) {
        scope->report(args->Loc, diag::ERR_SemaInvalidTypesOfArgumentsInCall);
        return nullptr;
      }
    }

    // Function can be called with this arguments
    return func;
  }

    return nullptr;
}

ExprAST* CallExprAST::semantic(Scope* scope) {
  if (ExprType) {
    return this;
  }

  // Perform semantic on callee
  Callee = Callee->semantic(scope);

  // We allow only IdExprAST as callee
  if (isa<IdExprAST>(Callee)) {
    SymbolAST* sym = ((IdExprAST*)Callee)->ThisSym;

    // Val should point on function
    if (isa<FuncDeclAST>(sym)) {
      TypeAST* returnType = nullptr;

      for (ExprList::iterator arg = Args.begin(), end = Args.end(); arg != end; 
        ++arg) {
        // Check semantic of argument
        *arg = (*arg)->semantic(scope);
      }
      
      // Try to resolve function's call
      if (SymbolAST* newSym = resolveFunctionCall(scope, sym, this)) {
        FuncDeclAST* fnc = static_cast< FuncDeclAST* >(newSym);
        FuncTypeAST* type = static_cast< FuncTypeAST* >(fnc->ThisType);

        // Done check all arguments
        ExprList::iterator arg = Args.begin();
        ParameterList::iterator it = type->Params.begin();

        // Check all parameters of functions and perform 1 to 1 mapping of 
        // function's parameters to callee arguments
        for (ParameterList::iterator end = type->Params.end(); it != end; 
          ++it, ++arg) {
          // Check types of argument and function's parameter and make cast if 
          // needed
          if (!(*arg)->ExprType->equal((*it)->Param)) {
            ExprAST* oldArg = (*arg);
            *arg = new CastExprAST(oldArg->Loc, oldArg->clone(), (*it)->Param);
            *arg = (*arg)->semantic(scope);
            delete oldArg;
          }
        }

        // Set return type
        if (!returnType) {
          ExprType = ((FuncDeclAST*)newSym)->ReturnType;
        } else {
          ExprType = returnType;
        }

        // Set symbol of function's to call
        CallFunc = newSym;
        return this;
      }
    }
  }

  scope->report(Loc, diag::ERR_SemaInvalidArgumentsForCall);
  return nullptr;
}

ExprAST* CallExprAST::clone() {
  ExprList exprs;
  ExprList::iterator it = Args.begin();
  ExprList::iterator end = Args.end();

  for ( ; it != end; ++it) {
    exprs.push_back((*it)->clone());
  }

  return new CallExprAST(Loc, Callee->clone(), exprs);
}

} // namespace simple