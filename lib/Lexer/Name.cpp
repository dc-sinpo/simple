#include "simple/Basic/Name.h"

namespace simple {

Name *Name::Super = nullptr;
Name *Name::This = nullptr;
Name *Name::New = nullptr;
Name *Name::Delete = nullptr;
Name *Name::Ctor = nullptr;
Name *Name::Dtor = nullptr;

} // namespace simple