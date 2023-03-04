#ifndef SIMPLE_AST_AST_H
#define SIMPLE_AST_AST_H

#include <map>
#include <vector>

#include "simple/Basic/Diagnostic.h"
#include "simple/Basic/Name.h"
#include "simple/Basic/TokenKinds.h"
#include "simple/Driver/Driver.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/SMLoc.h"

namespace simple {

struct Scope;
struct SymbolAST;

struct TypeAST {
  /// Type's identifiers
  enum TypeId {
    TI_Void,     ///< void type
    TI_Bool,     ///< boolean type
    TI_Int,      ///< integral type
    TI_Float,    ///< floating point type
    TI_Function  ///< function type
  };

  /// Constructor
  /// \param[in] typeKind - one of TypeId values
  TypeAST(int typeKind)
    : TypeKind(typeKind),
      ThisType(nullptr),
      MangleName(nullptr, 0) {
    registerTypeForDeletion(this);
  }
  
  /// Destructor
  virtual ~TypeAST() {
  }

  /// Check is it integral type
  bool isInt() {
    return TypeKind == TI_Int;
  }
  /// Check is it floating point type
  bool isFloat() {
    return TypeKind == TI_Float;
  }
  /// Check is it boolean type
  bool isBool() {
    return TypeKind == TI_Bool;
  }
  /// Check is it void type
  bool isVoid() {
    return TypeKind == TI_Void;
  }

  /// Perform semantic analysis on type
  /// \param[in] scope - current scope
  /// \return Resulted type (probably new)
  virtual TypeAST* semantic(Scope* scope) = 0;
  /// Check can this type can be implicit converted to \c newType
  /// \param[in] newType - new type
  virtual bool implicitConvertTo(TypeAST* newType) = 0;
  /// Check equivalence of this type with \c otherType
  bool equal(TypeAST* otherType);
  /// Write type info to mangle buffer (unique decorated name of this type, if
  ///   two types have same decoration name then they are same)
  /// \param[in] output - resulted buffer
  virtual void toMangleBuffer(llvm::raw_ostream& output) = 0;
  /// Calculate mangle name for this type
  void calcMangle();

  /// Get LLVM's type associated with this type
  virtual llvm::Type* getType() = 0;

  static void registerTypeForDeletion(TypeAST* thisType);
  static void clearAllTypes();

  int TypeKind; ///< Type's kind
  llvm::Type* ThisType; ///< LLVM's type generated for this type
  llvm::StringRef MangleName; ///< Mangle name for this type

  static llvm::StringSet<> TypesTable; ///< Set of used types
};

/// Base class for expression AST node
struct ExprAST {
  /// Expression's identifiers
  enum ExprId {
    EI_Int,           ///< integral constant
    EI_Float,         ///< floating point constant
    EI_Id,            ///< identifier expression
    EI_Cast,          ///< cast expression
    EI_Unary,         ///< unary expression
    EI_Binary,        ///< binary expression
    EI_Call,          ///< call expression
    EI_Cond           ///< conditional expression
  };

  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] exprKind - one of ExprId values
  /// \param[in] type - type of this expression
  ExprAST(llvm::SMLoc loc, int exprKind, TypeAST *type = nullptr)
    : Loc(loc),
      ExprKind(exprKind),
      ExprType(type) {
  }

  /// Destructor
  virtual ~ExprAST() {
  }

  /// Check is it integral constant
  bool isIntConst() { return ExprKind == EI_Int; }
  /// Check is it constant expression (integral, floating point)
  bool isConst() {
    return ExprKind == EI_Int || ExprKind == EI_Float;
  }

  /// Check is constant expression has true value
  virtual bool isTrue();
  /// Check is it lvalue expression
  virtual bool isLValue();
  /// Perform semantic analysis on expression
  /// \param[in] scope - current scope
  /// \return Expression after semantic analysis (probably new expression)
  virtual ExprAST *semantic(Scope *scope) = 0;

  /// Generate code for lvalue expression
  /// \param[in] Context - code generation's context
  virtual llvm::Value *getLValue(SLContext &Context);
  /// Generate code for rvalue expression
  /// \param[in] Context - code generation's context
  virtual llvm::Value *getRValue(SLContext &Context) = 0;

  /// Create copy of this expression
  virtual ExprAST *clone() = 0;

  llvm::SMLoc Loc; ///< Location in the source file
  int ExprKind;       ///< Expression's kind
  TypeAST *ExprType;  ///< Expression's type
};

/// List of expressions
typedef llvm::SmallVector<ExprAST*, 4> ExprList;

/// Base class for statement AST node
struct StmtAST {
  /// Statement's identifier
  enum StmtId {
    SI_Expr, ///< expression statement
    SI_Var, ///< declaration stateement
    SI_For, ///< for loop statment
    SI_While, ///< while loop statement
    SI_If, ///< if statement
    SI_Return, ///< return statement
    SI_Continue, ///< continue statement
    SI_Break, ///< break statement
    SI_Block ///< block statement
  };

  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] stmtKind - one of StmtId values
  StmtAST(llvm::SMLoc loc, int stmtKind)
    : Loc(loc),
      StmtKind(stmtKind),
      SemaState(0) {
  }

  /// Destructor
  virtual ~StmtAST() {
  }

  /// Check is it jump instruction (break, continue or return)
  bool isJump() {
    return StmtKind == SI_Break || StmtKind == SI_Continue || StmtKind == SI_Return;
  }

  /// Check has statement return statement or not
  virtual bool hasReturn();
  /// Check has statement jump statement or not
  virtual bool hasJump();
  /// Perform semantic analysis on statement
  /// \param[in] scope - current scope
  /// \return Statement after semantic analysis (can be new statement)
  /// \remark Old statement's variable should be replaced with value returned
  ///   by this function
  StmtAST* semantic(Scope* scope);
  /// Perform semantic analysis on statement
  /// \param[in] scope - current scope
  /// \return Statement after semantic analysis (can be new statement)
  /// \note This is working function which should be implemented in derived
  ///   classes (should never be called directly except in semantic function)
  virtual StmtAST* doSemantic(Scope* scope);
  
  /// Generate code for statement
  /// \param[in] Context - code generation's context
  virtual llvm::Value* generateCode(SLContext& Context);

  llvm::SMLoc Loc; ///< Location in the source file
  int StmtKind; ///< Statements kind
  int SemaState; ///< Stage of semantic analysis
};

/// List of statements
typedef llvm::SmallVector< StmtAST*, 4 > StmtList;

/// Base class for symbol AST node
struct SymbolAST {
  /// Symbol's identifier
  enum SymbolId {
    SI_Variable,    ///< variable declaration
    SI_Function,    ///< function declaration
    SI_Module,      ///< module declaration
    SI_Parameter,   ///< function's parameter declaration
    SI_Block        ///< scope declaration
  };

  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] symbolKind - one of SymbolId values
  /// \param[in] id - symbol's name
  SymbolAST(llvm::SMLoc loc, int symbolKind, Name *id)
    : Loc(loc),
      SymbolKind(symbolKind),
      Id(id),
      Parent(nullptr),
      SemaState(0) {
  }

  /// Destructor
  virtual ~SymbolAST() {}
  
  /// Get symbol's type
  virtual TypeAST *getType();
  /// Perform 1st stage of semantic analysis (resolve symbol names)
  /// \param[in] scope - current scope
  void semantic(Scope *scope);
  /// Perform 2nd stage of semantic analysis (resolve types of base classes)
  /// \param[in] scope - current scope
  void semantic2(Scope *scope);
  /// Perform 3rd stage of semantic analysis (resolve aggregate members)
  /// \param[in] scope - current scope
  void semantic3(Scope *scope);
  /// Perform 4th stage of semantic analysis (resolve virtual functions tables,
  ///   constructors and destructor)
  /// \param[in] scope - current scope
  void semantic4(Scope *scope);
  /// Perform 5th stage of semantic analysis (resolve function's body)
  /// \param[in] scope - current scope
  void semantic5(Scope *scope);

  /// Perform 1st stage of semantic analysis (resolve symbol names)
  /// \param[in] scope - current scope
  /// \note This is working function which should be implemented in derived
  ///   classes (should never be called directly except in semantic function)
  virtual void doSemantic(Scope *scope);
  /// Perform 2nd stage of semantic analysis (resolve types of base classes)
  /// \param[in] scope - current scope
  /// \note This is working function which should be implemented in derived
  ///   classes (should never be called directly except in semantic2 function)
  virtual void doSemantic2(Scope *scope);
  /// Perform 3rd stage of semantic analysis (resolve aggregate members)
  /// \param[in] scope - current scope
  /// \note This is working function which should be implemented in derived
  ///   classes (should never be called directly except in semantic3 function)
  virtual void doSemantic3(Scope *scope);
  /// Perform 4th stage of semantic analysis (resolve virtual functions tables,
  ///   constructors and destructor)
  /// \param[in] scope - current scope
  /// \note This is working function which should be implemented in derived
  ///   classes (should never be called directly except in semantic4 function)
  virtual void doSemantic4(Scope *scope);
  /// Perform 5th stage of semantic analysis (resolve function's body)
  /// \param[in] scope - current scope
  /// \note This is working function which should be implemented in derived
  ///   classes (should never be called directly except in semantic5 function)
  virtual void doSemantic5(Scope *scope);

  /// Generate code for symbol's value
  /// \param[in] Context - code generation's context
  virtual llvm::Value *getValue(SLContext &Context);
  /// Generate code for this symbol
  /// \param[in] Context - code generation's context
  virtual llvm::Value *generateCode(SLContext &Context);

  /// Get member symbol by name
  /// \param[in] id - name of a symbol to search
  /// \param[in] flags - search flags (bit 1 is set if members of base class
  ///   should be skipped)
  virtual SymbolAST *find(Name *id, int flags = 0);

  llvm::SMLoc Loc;    ///< Location in the source file
  int SymbolKind;     ///< Symbol's kind
  Name *Id;           ///< Symbol's name
  SymbolAST *Parent;  ///< Enclosed scope
  int SemaState;      ///< Current semantic analysis stage for this symbol
};

/// List of symbols
typedef llvm::SmallVector<SymbolAST *, 4> SymbolList;

/// AST node built-in type
struct BuiltinTypeAST : TypeAST {
  /// Get instance of built-in type
  /// \param[in] type - one of TypeAST::TypeId values
  static TypeAST *get(int type);

  TypeAST *semantic(Scope *scope);
  bool implicitConvertTo(TypeAST *newType);
  void toMangleBuffer(llvm::raw_ostream &output);
  
  llvm::Type *getType();

  static bool classof(const TypeAST *T) {
    return T->TypeKind == TI_Void ||
      T->TypeKind == TI_Bool ||
      T->TypeKind == TI_Int ||
      T->TypeKind == TI_Float;
  }
  
private:
  BuiltinTypeAST(int type)
    : TypeAST(type) {
  }
};

void mangleAggregateName(llvm::raw_ostream& output, SymbolAST* thisSym);
llvm::Value* promoteToBool(llvm::Value* val, TypeAST* type, llvm::IRBuilder< >& builder);
llvm::ConstantInt* getConstInt(uint64_t value);

/// AST node for function parameter
struct ParameterAST {
  /// Constructor
  /// \param[in] type - type of function parameter
  /// \param[in] id - name of function parameter (can be 0)
  ParameterAST(TypeAST* type, Name* id)
    : Param(type),
      Id(id),
      CodeValue(nullptr) {
  }

  ~ParameterAST() {
  }

  TypeAST* Param; ///< Type of function parameter
  Name* Id; ///< Name of function parameter

  /// Generated code for variable which hold copy of function parameter
  llvm::Value* CodeValue;
};

/// List of function parameters
typedef llvm::SmallVector< ParameterAST*, 4 > ParameterList;

/// AST node for function type
struct FuncTypeAST : TypeAST {
  /// Constructor
  /// \param[in] returnType - type of function's return value
  /// \param[in] params - list of function's parameters
  FuncTypeAST(TypeAST* returnType, const ParameterList& params)
    : TypeAST(TI_Function),
      ReturnType(returnType),
      Params(params) {
  }

  ~FuncTypeAST() {
    for (ParameterList::iterator it = Params.begin(), end = Params.end();
      it != end; ++it) {
      delete *it;
    }
  }

  TypeAST* semantic(Scope* scope);
  bool implicitConvertTo(TypeAST* newType);
  void toMangleBuffer(llvm::raw_ostream& output);

  llvm::Type* getType();

  static bool classof(const TypeAST *T) {
    return T->TypeKind == TI_Function;
  }

  TypeAST* ReturnType; ///< Type of function's return value (nullptr for void)
  ParameterList Params; ///< List of function's parameters
};

/// AST node for integral constant
struct IntExprAST : ExprAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] value - stored value
  IntExprAST(llvm::SMLoc loc, int value)
    : ExprAST(loc, EI_Int, BuiltinTypeAST::get(TypeAST::TI_Int)),
      Val(value) {
  }

  bool isTrue();
  ExprAST* semantic(Scope* scope);
  ExprAST* clone();

  llvm::Value* getRValue(SLContext& Context);

  static bool classof(const ExprAST *E) {
    return E->ExprKind == EI_Int;
  }

  int Val; ///< Stored value
};

/// AST node for floating point constant
struct FloatExprAST : ExprAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] value - stored value
  FloatExprAST(llvm::SMLoc loc, double value)
    : ExprAST(loc, EI_Float, BuiltinTypeAST::get(TypeAST::TI_Float)),
      Val(value) {
  }

  bool isTrue();
  ExprAST* semantic(Scope* scope);
  ExprAST* clone();

  llvm::Value* getRValue(SLContext& Context);

  static bool classof(const ExprAST *E) {
    return E->ExprKind == EI_Float;
  }

  double Val; ///< Stored value
};

/// AST type for identifier expression
struct IdExprAST : ExprAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] name - identifier's name
  IdExprAST(llvm::SMLoc loc, Name *name)
    : ExprAST(loc, EI_Id),
      Val(name),
      ThisSym(nullptr) {
  }

  bool isLValue();
  ExprAST* semantic(Scope* scope);
  ExprAST* clone();

  llvm::Value* getLValue(SLContext& Context);
  llvm::Value* getRValue(SLContext& Context);

  static bool classof(const ExprAST *E) {
    return E->ExprKind == EI_Id;
  }

  Name* Val; ///< Identifier's name
  SymbolAST* ThisSym; ///< Symbol for identifier (only valid after semantic pass)
};

/// AST node for cast expression
struct CastExprAST : ExprAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] expr - expression for cast
  /// \param[in] type - type to cast
  CastExprAST(llvm::SMLoc loc, ExprAST *expr, TypeAST *type)
    : ExprAST(loc, EI_Cast, type),
      Val(expr),
      SemaDone(false) {
  }

  /// Destructor
  ~CastExprAST() {
    delete Val;
  }

  ExprAST* semantic(Scope* scope);
  ExprAST* clone();

  llvm::Value* getRValue(SLContext& Context);

  static bool classof(const ExprAST *E) {
    return E->ExprKind == EI_Cast;
  }

  ExprAST* Val; ///< Expression for cast
  bool SemaDone; ///< true - if semantic is done
};

/// AST node for unary expression
struct UnaryExprAST : ExprAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] op - unary operator
  /// \param[in] value - expression's operand
  UnaryExprAST(llvm::SMLoc loc, int op, ExprAST *value)
    : ExprAST(loc, EI_Unary),
      Op(op),
      Val(value) {
  }

  /// Destructor
  ~UnaryExprAST() {
    delete Val;
  }

  ExprAST* semantic(Scope* scope);
  ExprAST* clone();

  llvm::Value* getRValue(SLContext& Context);

  static bool classof(const ExprAST *E) {
    return E->ExprKind == EI_Unary;
  }

  int Op; ///< Unary operator
  ExprAST* Val; ///< Expression's operand
};

/// AST node for binary expression
struct BinaryExprAST : ExprAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] op - binary operator
  /// \param[in] lhs - left operand
  /// \param[in] rhs - right operand
  BinaryExprAST(llvm::SMLoc loc, int op, ExprAST *lhs, ExprAST *rhs)
    : ExprAST(loc, EI_Binary),
      Op(op),
      LeftExpr(lhs),
      RightExpr(rhs) {
  }

  /// Destructor
  ~BinaryExprAST() {
    delete LeftExpr;
    delete RightExpr;
  }

  ExprAST* semantic(Scope* scope);
  ExprAST* clone();

  llvm::Value* getRValue(SLContext& Context);

  static bool classof(const ExprAST *E) {
    return E->ExprKind == EI_Binary;
  }

  int Op; ///< Binary operator
  ExprAST* LeftExpr; ///< Left operand
  ExprAST* RightExpr; ///< Right operand
};

/// AST node for conditional expression
struct CondExprAST : ExprAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] cond - condition part of expression
  /// \param[in] ifExpr - then part of expression
  /// \param[in] elseExpr - else part of expression
  CondExprAST(llvm::SMLoc loc, ExprAST *cond, ExprAST *ifExpr,
    ExprAST* elseExpr)
    : ExprAST(loc, EI_Cond),
      Cond(cond),
      IfExpr(ifExpr),
      ElseExpr(elseExpr),
      SemaDone(false) {
  }

  /// Destructor
  ~CondExprAST() {
    delete Cond;
    delete IfExpr;
    delete ElseExpr;
  }

  bool isLValue();
  ExprAST* semantic(Scope* scope);
  ExprAST* clone();

  llvm::Value* getLValue(SLContext& Context);
  llvm::Value* getRValue(SLContext& Context);

  static bool classof(const ExprAST *E) {
    return E->ExprKind == EI_Cond;
  }

  ExprAST* Cond; ///< Condition part of expression
  ExprAST* IfExpr; ///< Then part of expression
  ExprAST* ElseExpr; ///< Else part of expression
  bool SemaDone; ///< true - if semantic is done
};

/// AST node for function call expression
struct CallExprAST : ExprAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] callee - function to call
  /// \param[in] args - list of function arguments
  CallExprAST(llvm::SMLoc loc, ExprAST *callee, const ExprList &args)
    : ExprAST(loc, EI_Call),
      Callee(callee),
      Args(args),
      CallFunc(nullptr) {
  }

  /// Destructor
  ~CallExprAST() {
    delete Callee;

    for (ExprList::iterator it = Args.begin(), end = Args.end(); it != end; ++it) {
      delete (*it);
    }
  }

  ExprAST* semantic(Scope* scope);
  ExprAST* clone();

  llvm::Value* getRValue(SLContext& Context);

  static bool classof(const ExprAST *E) {
    return E->ExprKind == EI_Call;
  }

  ExprAST* Callee; ///< Function to call
  ExprList Args; ///< List of function arguments
  SymbolAST* CallFunc; ///< Symbol for function to call (valid after semantic)
};

/// AST node for expression statement
struct ExprStmtAST : StmtAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] expr - stored expression (can be nullptr for empty statement)
  ExprStmtAST(llvm::SMLoc loc, ExprAST *expr)
    : StmtAST(loc, SI_Expr),
      Expr(expr) {
  }

  /// Destructor
  ~ExprStmtAST() {
    delete Expr;
  }

  StmtAST* doSemantic(Scope* scope);

  llvm::Value* generateCode(SLContext& Context);

  static bool classof(const StmtAST *S) {
    return S->StmtKind == SI_Expr;
  }

  ExprAST* Expr; ///< Stored expression (can be nullptr for empty statement)
};

/// AST node for landing pad
/// \note Hold information on cleanup stack
struct LandingPadAST {
  /// Constructor
  /// \param[in] prev - previous landing location
  LandingPadAST(LandingPadAST* prev)
    : Prev(prev),
      ReturnValue(nullptr),
      OwnerBlock(nullptr),
      BreakLoc(nullptr),
      ContinueLoc(nullptr),
      ReturnLoc(nullptr),
      FallthroughLoc(nullptr),
      Breaks(0),
      Returns(0),
      Continues(0),
      IsLoop(false) {
  }

  /// Constructor
  LandingPadAST()
    : Prev(nullptr),
      ReturnValue(nullptr),
      OwnerBlock(nullptr),
      BreakLoc(nullptr),
      ContinueLoc(nullptr),
      ReturnLoc(nullptr),
      FallthroughLoc(nullptr),
      Breaks(0),
      Returns(0),
      Continues(0),
      IsLoop(false) {
  }

  /// Get return value
  llvm::Value* getReturnValue();

  LandingPadAST* Prev; ///< Previous landing location
  llvm::Value* ReturnValue; ///< Return value (you should use getReturnValue)
  StmtAST* OwnerBlock; ///< Owner block statement
  llvm::BasicBlock* BreakLoc; ///< Location for break statement
  llvm::BasicBlock* ContinueLoc; ///< Location for continue statement
  llvm::BasicBlock* ReturnLoc; ///< Location for return statement
  llvm::BasicBlock* FallthroughLoc; ///< Location for fallthrough
  int Breaks; ///< Number of break statements which can lead to this landing pad
  int Returns; ///< Number of return statments which can lead to this landing pad
  /// Number of continue statements which can lead to this landing pad
  int Continues;
  bool IsLoop; ///< It's landing pad for loop
};

/// AST node for if statement
struct IfStmtAST : StmtAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] cond - if condition
  /// \param[in] thenBody - body of then part
  /// \param[in] elseBody - body of else part (can be nullptr)
  IfStmtAST(llvm::SMLoc loc,
    ExprAST *cond,
    StmtAST *thenBody,
    StmtAST* elseBody)
    : StmtAST(loc, SI_If),
      Cond(cond),
      ThenBody(thenBody),
      ElseBody(elseBody),
      LandingPad(nullptr) {
  }

  /// Destructor
  ~IfStmtAST() {
    delete Cond;
    delete ThenBody;
    delete ElseBody;
    delete LandingPad;
  }

  bool hasReturn();
  bool hasJump();
  StmtAST* doSemantic(Scope* scope);

  llvm::Value* generateCode(SLContext& Context);

  static bool classof(const StmtAST *S) {
    return S->StmtKind == SI_If;
  }

  ExprAST* Cond; ///< if condition
  StmtAST* ThenBody; ///< Body of then part
  StmtAST* ElseBody; ///< Body of else part (can be nullptr)

  LandingPadAST* LandingPad; ///< Landing pad
};

/// AST node for while loop statement
struct WhileStmtAST : StmtAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] cond - while condition
  /// \param[in] body - loop's body
  WhileStmtAST(llvm::SMLoc loc, ExprAST *cond, StmtAST *body)
    : StmtAST(loc, SI_While),
      Cond(cond),
      Body(body),
      PostExpr(nullptr),
      LandingPad(nullptr) {
  }

  /// Destructor
  ~WhileStmtAST() {
    delete Cond;
    delete Body;
    delete PostExpr;
    delete LandingPad;
  }

  bool hasReturn();
  StmtAST* doSemantic(Scope* scope);

  llvm::Value* generateCode(SLContext& Context);

  static bool classof(const StmtAST *S) {
    return S->StmtKind == SI_While;
  }

  ExprAST* Cond; ///< while condition
  StmtAST* Body; ///< Loop's body

  /// Post expression (should be used only for conversion 'for' loop into
  /// 'while' loop)
  ExprAST* PostExpr;
  LandingPadAST* LandingPad; ///< Landing pad
};

/// AST node for break statement
struct BreakStmtAST : StmtAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  BreakStmtAST(llvm::SMLoc loc)
    : StmtAST(loc, SI_Break),
      BreakLoc(nullptr) { }

  bool hasJump();
  StmtAST* doSemantic(Scope* scope);

  llvm::Value* generateCode(SLContext& Context);

  static bool classof(const StmtAST *S) {
    return S->StmtKind == SI_Break;
  }

  LandingPadAST* BreakLoc; ///< Break location (only valid after semantic)
};

/// AST node for continue statement
struct ContinueStmtAST : StmtAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  ContinueStmtAST(llvm::SMLoc loc)
    : StmtAST(loc, SI_Continue),
      ContinueLoc(nullptr) { }

  bool hasJump();
  StmtAST* doSemantic(Scope* scope);

  llvm::Value* generateCode(SLContext& Context);

  static bool classof(const StmtAST *S) {
    return S->StmtKind == SI_Continue;
  }

  LandingPadAST* ContinueLoc; ///< Continue location (only valid after semantic)
};

/// AST node for return statement
struct ReturnStmtAST : StmtAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] expr - value to return (nullptr for return from void function)
  ReturnStmtAST(llvm::SMLoc loc, ExprAST *expr)
    : StmtAST(loc, SI_Return),
      Expr(expr),
      ReturnLoc(nullptr) {
  }

  ~ReturnStmtAST() {
    delete Expr;
  }

  bool hasReturn();
  bool hasJump();
  StmtAST* doSemantic(Scope* scope);

  llvm::Value* generateCode(SLContext& Context);

  static bool classof(const StmtAST *S) {
    return S->StmtKind == SI_Return;
  }

  ExprAST* Expr; ///< Value to return (nullptr for return from void function)
  LandingPadAST* ReturnLoc; ///< Return location (only valid after semantic)
};

/// AST type for block statement
struct BlockStmtAST : StmtAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] body - list of inner statements
  BlockStmtAST(llvm::SMLoc loc, const StmtList &body)
    : StmtAST(loc, SI_Block),
      HasReturn(false),
      HasJump(false),
      Body(body),
      ThisBlock(nullptr),
      LandingPad(nullptr),
      IsPromoted(false) {
  }

  /// Destructor
  ~BlockStmtAST() {
    for (StmtList::iterator it = Body.begin(), end = Body.end(); it != end; ++it) {
      delete *it;
    }

    delete ThisBlock;
    delete LandingPad;
  }

  bool hasReturn();
  bool hasJump();
  StmtAST* doSemantic(Scope* scope);

  llvm::Value* generateCode(SLContext& Context);
  /// Generate code for part of nested statements
  /// \param[in] Context - code generation context
  /// \param[in] it - first statement
  /// \param[in] end - last statement
  /// \note We need that function because constructors and destructor have
  ///   special compiler generated code which should be handled differently
  llvm::Value* generatePartialCode(SLContext& Context, StmtList::iterator it,
    StmtList::iterator end);

  static bool classof(const StmtAST *S) {
    return S->StmtKind == SI_Block;
  }

  bool HasReturn; ///< true - if \c Body has return statement
  bool HasJump; ///< true - if \c Body has any jump statement
  StmtList Body; ///< List of inner statements
  SymbolAST* ThisBlock; ///< Symbol for inner variables
  LandingPadAST* LandingPad; ///< Landing pad
  bool IsPromoted; ///< true - if this block is promoted from variable declaration
};

/// AST node for declaration statement
struct DeclStmtAST : StmtAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] decls - list of all declared variables
  DeclStmtAST(llvm::SMLoc loc, const SymbolList &decls)
    : StmtAST(loc, SI_Var),
      Decls(decls) {
  }
  /// Destructor
  ~DeclStmtAST() {
    for (SymbolList::iterator it = Decls.begin(), end = Decls.end(); it != end; ++it) {
      delete *it;
    }
  }

  StmtAST* doSemantic(Scope* scope);

  llvm::Value* generateCode(SLContext& Context);

  static bool classof(const StmtAST *S) {
    return S->StmtKind == SI_Var;
  }

  SymbolList Decls; ///< List of all declared variables
};

/// AST node for for loop statement
struct ForStmtAST : StmtAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] initExpr - initialization expression of the loop (can be nullptr)
  /// \param[in] decls - list of initialization variables (can have 0 size)
  /// \param[in] cond - for condition (can be nullptr)
  /// \param[in] post - post expression (can be nullptr)
  /// \param[in] body - loop's body
  ForStmtAST(llvm::SMLoc loc,
    ExprAST *initExpr,
    const SymbolList &decls,
    ExprAST* cond,
    ExprAST* post,
    StmtAST* body)
    : StmtAST(loc, SI_For),
      InitExpr(initExpr),
      InitDecls(decls),
      Cond(cond),
      Post(post),
      Body(body) {
  }

  /// Destructor
  ~ForStmtAST() {
    delete InitExpr;

    for (SymbolList::iterator it = InitDecls.begin(), end = InitDecls.end();
      it != end; ++it) {
      delete *it;
    }

    delete Cond;
    delete Post;
    delete Body;
  }

  StmtAST* doSemantic(Scope* scope);
  
  llvm::Value* generateCode(SLContext& Context);
  
  static bool classof(const StmtAST *S) {
    return S->StmtKind == SI_For;
  }
  
  ExprAST* InitExpr; ///< Initialization expression of the loop (can be nullptr)
  SymbolList InitDecls; ///< List of initialization variables (can have 0 size)
  ExprAST* Cond; ///< For condition (can be nullptr)
  ExprAST* Post; ///< Post expression (can be nullptr)
  StmtAST* Body; ///< Loop's body
};

/// AST for variable symbol
struct VarDeclAST : SymbolAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] varType - symbol's type
  /// \param[in] id - symbol's name
  /// \param[in] value - symbol's initialization value (can be nullptr)
  VarDeclAST(llvm::SMLoc loc, TypeAST *varType, Name *id,
    ExprAST* value)
    : SymbolAST(loc, SI_Variable, id),
      ThisType(varType),
      Val(value),
      CodeValue(nullptr) {
  }

  /// Destructor
  ~VarDeclAST() {
    delete Val;
  }

  TypeAST* getType();
  void doSemantic(Scope* scope);
  void doSemantic3(Scope* scope);

  llvm::Value* getValue(SLContext& Context);
  llvm::Value* generateCode(SLContext& Context);

  static bool classof(const SymbolAST *T) {
    return T->SymbolKind == SI_Variable;
  }

  TypeAST* ThisType; ///< Symbol's type
  ExprAST* Val; ///< Symbol's initialization value (can be nullptr)

  /// LLVM value for this symbol (only valid during code generation pass)
  llvm::Value* CodeValue;
};

/// AST node for symbol with own scope
struct ScopeSymbol : SymbolAST {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] symbolKind - one of SymbolAST::SymbolId values
  /// \param[in] id - symbol's name
  ScopeSymbol(llvm::SMLoc loc, int symbolKind, Name *id)
    : SymbolAST(loc, symbolKind, id) {
  }

  /// Destructor
  ~ScopeSymbol();

  SymbolAST* find(Name* id, int flags = 0);

  /// Map of variables declared in this scope
  typedef std::map< Name*, SymbolAST* > SymbolMap;
  SymbolMap Decls; ///< Declared variables
};

/// AST node for function parameter symbol
struct ParameterSymbolAST : SymbolAST {
  /// Constructor
  /// \param[in] param - function's parameter
  ParameterSymbolAST(ParameterAST* param)
    : SymbolAST(llvm::SMLoc(), SI_Parameter, param->Id),
      Param(param) {
  }

  ~ParameterSymbolAST() {
  }

  TypeAST* getType();
  void doSemantic(Scope* scope);

  llvm::Value* getValue(SLContext& Context);
  llvm::Value* generateCode(SLContext& Context);

  SymbolAST* find(Name* id, int flags = 0);

  static bool classof(const SymbolAST *T) {
    return T->SymbolKind == SI_Parameter;
  }

  ParameterAST* Param; ///< Function's parameter
};

/// AST node for function declaration
struct FuncDeclAST : ScopeSymbol {
  /// Constructor
  /// \param[in] loc - location in the source file
  /// \param[in] funcType - function's type
  /// \param[in] id - function's name
  /// \param[in] body - function's body
  /// \param[in] tok - one of TK_Def, TK_Virtual or TK_Overload
  FuncDeclAST(llvm::SMLoc loc, TypeAST *funcType, Name *id,
    StmtAST* body, int tok = tok::Def)
    : ScopeSymbol(loc, SI_Function, id),
      ThisType(funcType),
      ReturnType(nullptr),
      Body(body),
      Tok(tok),
      LandingPad(nullptr),
      CodeValue(nullptr),
      Compiled(false) {
  }

  /// Destructor
  ~FuncDeclAST() {
    delete Body;
    delete LandingPad;

    for (std::vector< SymbolAST* >::iterator it = FuncVars.begin(),
      end = FuncVars.end(); it != end; ++it) {
      if (isa<ParameterSymbolAST>(*it)) {
        delete *it;
      }
    }
  }
  
  TypeAST* getType();
  llvm::Value* getValue(SLContext& Context);
  llvm::Value* generateCode(SLContext& Context);
  void doSemantic(Scope* scope);
  void doSemantic5(Scope* scope);

  static bool classof(const SymbolAST *T) {
    return T->SymbolKind == SI_Function;
  }

  TypeAST* ThisType; ///< Functions's type
  /// Type of function's return value (Valid after semantic pass)
  TypeAST* ReturnType;
  StmtAST* Body; ///< Function's body
  int Tok; ///< One of TK_Def
  LandingPadAST* LandingPad; ///< Landing location

  /// List of all variables declared in the function
  /// \note Contain all variables declared in the function's body even not
  ///   accessible in current function scope
  std::vector< SymbolAST* > FuncVars;
  /// Code generated for function's declaration
  llvm::Function* CodeValue;
  bool Compiled; ///< true - if function is compiled (or runtime function)
};

/// AST node for module declaration
struct ModuleDeclAST : ScopeSymbol {
  /// Constructor
  /// \param[in] decls - list of all module declarations
  ModuleDeclAST(DiagnosticsEngine &D, const SymbolList& decls)
    : ScopeSymbol(llvm::SMLoc(), SI_Module, nullptr),
      Members(decls),
      MainPtr(nullptr),
      Diag(D) {
  }

  /// Destructor
  ~ModuleDeclAST() {
    for (SymbolList::iterator it = Members.begin(), end = Members.end();
      it != end; ++it) {
      delete *it;
    }
  }

  /// Semantic analyze
  /// \note We have own version and ignore Symbol class version, because
  ///   module is the root node for semantic
  void semantic();

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Woverloaded-virtual"
  /// Generate code
  /// \note We have own version and ignore Symbol class version, because
  ///   module is the root node for code generation
  void generateCode();
#pragma clang diagnostic pop

  /// Load module from file
  /// \param[in] fileName - name of the file
  /// \return Loaded module
  static ModuleDeclAST* load(
    llvm::SourceMgr &SrcMgr,
    DiagnosticsEngine &Diags,
    llvm::StringRef fileName
  );

  static bool classof(const SymbolAST *T) {
    return T->SymbolKind == SI_Module;
  }

  SymbolList Members; ///< Declarations of module symbols
  double (*MainPtr)(); ///< Address of the main function
  DiagnosticsEngine &Diag; ///< Diagnostic engine for error handling
};

/// Class for symbol scope management
struct Scope {
  /// Constructor
  Scope(DiagnosticsEngine &D)
    : Enclosed(nullptr),
      ThisModule(nullptr),
      CurScope(nullptr),
      EnclosedFunc(nullptr),
      BreakLoc(nullptr),
      ContinueLoc(nullptr),
      LandingPad(nullptr),
      Diag(D) {
  }

  /// Constructor
  /// \param[in] enclosed - enclosed scope
  Scope(Scope* enclosed)
    : Enclosed(enclosed),
      ThisModule(enclosed->ThisModule),
      CurScope(enclosed->CurScope),
      EnclosedFunc(enclosed->EnclosedFunc),
      BreakLoc(enclosed->BreakLoc),
      ContinueLoc(enclosed->ContinueLoc),
      LandingPad(enclosed->LandingPad),
      Diag(enclosed->Diag) {
  }

  /// Constructor
  /// \param[in] thisModule - main module
  Scope(ModuleDeclAST* thisModule)
    : Enclosed(nullptr),
      ThisModule(thisModule),
      CurScope(thisModule),
      EnclosedFunc(nullptr),
      BreakLoc(nullptr),
      ContinueLoc(nullptr),
      LandingPad(nullptr),
      Diag(thisModule->Diag) {
  }

  /// Find symbol with name
  /// \param[in] id - name of searched symbol
  SymbolAST* find(Name* id);
  /// Find member of current symbol
  /// \param[in] id - name of searched symbol
  /// \param[in] flags - search flags (bit 1 is set if members of base class
  ///   should be skipped)
  SymbolAST* findMember(Name* id, int flags = 0);
  /// Create new nested scope
  Scope* push();
  /// Create new nested scope with new scope symbol
  Scope* push(ScopeSymbol* sym);
  /// Remove top scope
  Scope* pop();

  /// Recreate list of scopes for \c sym
  /// \param[in] scope - current scope
  /// \param[in] sym - symbol for which scope should be recreated
  static Scope* recreateScope(Scope* scope, SymbolAST* sym);
  /// Clear all scopes chain except module's scope
  /// \param[in] scope - scope to clear
  static void clearAllButModule(Scope* scope);

  template <typename... Args>
  void report(SMLoc Loc, unsigned DiagID, Args &&...Arguments) {
    Diag.report(Loc, DiagID, std::forward<Args>(Arguments)...);
  }

  Scope* Enclosed; ///< Enclosed scope
  ModuleDeclAST* ThisModule; ///< Enclosed module
  SymbolAST* CurScope; ///< Current symbol with scope
  FuncDeclAST* EnclosedFunc; ///< Escaped function
  StmtAST* BreakLoc; ///< Statement with break location
  StmtAST* ContinueLoc; ///< Statement with continue location
  LandingPadAST* LandingPad; ///< Current landing pad
  DiagnosticsEngine &Diag; ///< Diagnostic engine for error handling
};

} // namespace simple

#endif