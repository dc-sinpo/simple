#include "simple/Driver/Driver.h"

using namespace llvm;

llvm::cl::opt<OptLevel> OptimizationLevel(llvm::cl::desc("Choose optimization level:"),
  llvm::cl::values(
    clEnumValN(NoOptimizations, "O0", "No optimizations"),
    clEnumVal(O1, "Enable trivial optimizations"),
    clEnumVal(O2, "Enable default optimizations"),
    clEnumVal(O3, "Enable expensive optimizations")
  )
);

namespace llvm {
namespace orc {
Expected<ThreadSafeModule> SimpleJIT::optimizeModule(ThreadSafeModule TSM, const MaterializationResponsibility &R) {
    TSM.withModuleDo([](Module &M) {
      // Create a function pass manager.
      auto FPM = std::make_unique<legacy::FunctionPassManager>(&M);

      // if (OptimizationLevel > NoOptimizations) {
      //   // Add some optimizations.
      //   FPM->add(createInstructionCombiningPass());
      //   FPM->add(createReassociatePass());
      //   FPM->add(createGVNPass());
      //   FPM->add(createCFGSimplificationPass());
      // }

      FPM->doInitialization();

      // Run the optimizations over all functions in the module being added to
      // the JIT.
      for (auto &F : M) {
        FPM->run(F);
      }
    });

    return std::move(TSM);
  }
} // namespace orc
} // namespace llvm

namespace simple {

static std::unique_ptr<llvm::orc::SimpleJIT> TheJIT;
static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<IRBuilder<>> Builder;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<SLContext> TheSLContext;
static ExitOnError ExitOnErr;

LLVMContext& getGlobalContext() {
  return *TheContext.get();
}

SLContext& getSLContext() {
  return *TheSLContext.get();
}

llvm::orc::SimpleJIT& getJIT() {
  return *TheJIT.get();
}

void initJIT() {
  TheJIT = ExitOnErr(llvm::orc::SimpleJIT::Create());

  // Open a new context and module.
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("simple", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());

  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);

  TheSLContext = std::make_unique<SLContext>();

  TheSLContext->TheTarget = &TheModule->getDataLayout();
  TheSLContext->TheModule = TheModule.get();
  TheSLContext->TheBuilder = Builder.get();
}

llvm::JITTargetAddress getJITMain() {
  auto TSM = llvm::orc::ThreadSafeModule(std::move(TheModule), std::move(TheContext));
  auto H = TheJIT->addModule(std::move(TSM));

  // Get the main function's JITSymbol.
  auto Sym = ExitOnErr(getJIT().lookup("main"));

  // Get the symbol's address and cast it to the right type (takes no
  // arguments, returns a double) so we can call it as a native function.
  return Sym.getAddress();
}

} // namespace simple