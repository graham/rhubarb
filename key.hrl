-record( keyData, {
           type,
           key,
           value,
           safe_writes = true,
           last_access = 0,
           last_update = 0,
           
           expires_at = -1,
           flushed_at = -1
           }).
           
