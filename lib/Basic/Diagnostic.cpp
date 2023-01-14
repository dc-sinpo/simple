#include "simple/Basic/Diagnostic.h"

using namespace simple;

namespace {
const char *DiagnosticText[] = {
#define DIAG(ID, Level, Msg) Msg,
#include "simple/Basic/Diagnostic.def"
};
SourceMgr::DiagKind DiagnosticKind[] = {
#define DIAG(ID, Level, Msg) SourceMgr::DK_##Level,
#include "simple/Basic/Diagnostic.def"
};
} // namespace

const char *DiagnosticsEngine::getDiagnosticText(unsigned DiagID) {
  return DiagnosticText[DiagID];
}

SourceMgr::DiagKind DiagnosticsEngine::getDiagnosticKind(unsigned DiagID) {
  return DiagnosticKind[DiagID];
}