#include <Rts.h>
#include "stub/MentatInterface_stub.h"

    
__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
  char *args[] = {
    "mentat-wasm.wasm",
    "+RTS",
    "--nonmoving-gc",
    "-H64m",
    "-RTS",
  };
    int argc = sizeof(args) / sizeof(args[0]);
    char **argv = args;
    hs_init_with_rtsopts(&argc, &argv);
    hs_perform_gc();
    hs_perform_gc();
    rts_clearMemory();
}

