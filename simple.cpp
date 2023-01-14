#include "simple/AST/AST.h"
#include "simple/Lexer/Lexer.h"
#include "simple/Parser/Parser.h"
#include "simple/Basic/Diagnostic.h"
#include "simple/Driver/Driver.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/WithColor.h"

static llvm::cl::opt<std::string> InputFilename(
  llvm::cl::Positional,
  llvm::cl::Required,
  llvm::cl::desc("<input file>")
);

static const char *Head = "simple - Simple language compiler";

void printVersion(llvm::raw_ostream &OS) {
  OS << "  Default target: "
     << llvm::sys::getDefaultTargetTriple() << "\n";
  std::string CPU(llvm::sys::getHostCPUName());
  OS << "  Host CPU: " << CPU << "\n";
  OS << "\n";
  OS.flush();
  exit(EXIT_SUCCESS);
}

int main(int Argc, const char **Argv) {
  llvm::InitLLVM X(Argc, Argv);

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  llvm::cl::SetVersionPrinter(&printVersion);
  llvm::cl::ParseCommandLineOptions(Argc, Argv, Head);

  simple::initJIT();

  llvm::SourceMgr SrcMgr;
  simple::DiagnosticsEngine Diags(SrcMgr);

  std::unique_ptr<simple::ModuleDeclAST> Mod(
    simple::ModuleDeclAST::load(SrcMgr, Diags, InputFilename)
  );

  if (!Mod) {
    return -1;
  }

  Mod->semantic();
  Mod->generateCode();

  if (Mod->MainPtr) {
    llvm::outs() << "Main return: " << Mod->MainPtr() << "\n";
    llvm::outs().flush();
  }

  simple::TypeAST::clearAllTypes();

  return 0;
}