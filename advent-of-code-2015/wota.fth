: snort ( c-addr1 u1 -- c-addr2 u2 )
  r/o open-file throw dup
  file-size 2drop
  dup allot ( fileid u2 -- )
  swap here swap rot ( c-addr2 fileid u2 -- )
  2 pick swap 2 pick ( c-addr2 fileid c-addr2 u2 fileid -- )
  read-file throw ( c-addr2 fileid u2 -- )
  swap
  close-file throw ;
