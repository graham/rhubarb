-record( keyData, {
           type,
           key,
           value,
           safe_writes = true,
           
           noisy_on_write = false,
           
           last_access = 0,
           last_update = 0,
           
           expires_after = -1,
           flushed_after = -1
           }).
           
