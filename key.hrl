-record( key, {
           type,
           key,
           value,
           safe_writes = true,
           
           last_access = 0,
           last_update = 0,
           
           expires_after = -1,
           flushed_after = -1
           }).
           
-record( keyPid, {
    write_queue = [],
    write_ref = nil, 
    waiting_client_pid = nil,
    listeners = [],
    blocked_commands = []
}).

