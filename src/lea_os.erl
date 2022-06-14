-module(lea_os).

-define(API, [memory_data/0, cpu_data/0]).
-ignore_xref(?API).
-export(?API).

memory_data() ->
    %% http://erlang.org/doc/man/memsup.html#get_system_memory_data-0
    HostTotalMemory = proplists:get_value(system_total_memory,
                                          memsup:get_system_memory_data()),
    %% https://erlang.org/doc/man/erlang.html#memory-0
    #{erlang_vm_total_memory => erlang:memory(total),
      host_total_memory => HostTotalMemory}.

cpu_data() ->
    #{avg1 => cpu_sup:avg1()/256,
      avg5 => cpu_sup:avg5()/256,
      avg15 => cpu_sup:avg15()/256}.
