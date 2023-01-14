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

// IntExprAST implementation
bool IntExprAST::isTrue() {
  return true;
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

// IdExprAST implementation
bool IdExprAST::isLValue() {
  return true;
}

ExprAST* IdExprAST::semantic(Scope* scope) {
  // if ExprType isn't nullptr then semantic pass was already passed for this
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

// CastExprAST implementation
ExprAST* CastExprAST::semantic(Scope* scope) {
  if (SemaDone) {
    return this;
  }

  // Make sure that we set type for cast
  assert(ExprType != 0 && "Type for cast not set"); 
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
    case tok::MinusMinus: {
        // We allow ++ or -- only for IdExprAST as operand
        // Note: ++ ++ id not allowed too
        if (!Val->ExprType->isInt() || !Val->isLValue()) {
          scope->report(Loc, diag::ERR_SemaInvalidPostfixPrefixOperand);
          return nullptr;
        }
        
        // We need to convert ++ id or -- id to id = id + 1 or id = id + -1
        ExprAST* val = Val;
        ExprAST* valCopy = Val->clone();
        result = new BinaryExprAST(Val->Loc, tok::Assign, 
          val,
          new BinaryExprAST(Val->Loc, tok::Plus,
            valCopy, 
            new IntExprAST(Val->Loc, (Op == tok::PlusPlus) ? 1 : -1)));
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
    if (!LeftExpr->isLValue() || !LeftExpr->ExprType->isInt()) {
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
      // If left operand is bool then we should convert it to an int
      if (LeftExpr->ExprType->isBool()) {
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
    // Perform cast on right operand if needed
    if (!LeftExpr->ExprType->equal(RightExpr->ExprType)) {
      RightExpr = new CastExprAST(RightExpr->Loc, RightExpr, LeftExpr->ExprType);
      RightExpr = RightExpr->semantic(scope);
    }

    // Check for lvalue of left operand
    if (!LeftExpr->isLValue()) {
      scope->report(Loc, diag::ERR_SemaMissingLValueInAssignment);
      return nullptr;
    }

    // It's valid expression. Return this
    return this;
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
  if (Cond->ExprType == 0 || Cond->ExprType->isVoid()) {
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
  if (IfExpr->ExprType == ElseExpr->ExprType) {
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

/// Check is \c func1 is more specialized than \c func2
/// \param[in] func1 - 1st function
/// \param[in] func2 - 2nd function
static bool moreSpecialized(SymbolAST* func1, SymbolAST* func2) {
  FuncTypeAST* type1 = (FuncTypeAST*)((FuncDeclAST*)func1)->ThisType;
  FuncTypeAST* type2 = (FuncTypeAST*)((FuncDeclAST*)func2)->ThisType;

  ParameterList::iterator it1 = type1->Params.begin();
  ParameterList::iterator it2 = type2->Params.begin();
  ParameterList::iterator end = type1->Params.end();

  // Check all arguments
  for ( ; it1 != end; ++it1, ++it2) {
    // If argument of 1st function can't convert to argument of the 2nd function
    // then func1 function is more specialized than func2
    if (!((*it1)->Param->implicitConvertTo((*it2)->Param))) {
      return true;
    }
  }

  // func1 is not more specialized than func2
  return false;
}

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

  // We allow only IdExprAST or MemberAccessAST as callee
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