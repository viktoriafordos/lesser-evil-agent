-module(lea_os).

-define(API, [memory_data/0]).
-ignore_xref(?API).
-export(?API).

memory_data() -> memsup:get_system_memory_data().
