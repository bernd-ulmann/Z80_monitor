;
; 12-MAY-2013, B. Ulmann
;        To build a Forth interpreter which is loadable from disk, just
;       assemble this file, it includes everything else which is necessary
;       and sets the control variable accordingly.
;
LOADABLE        EQU     1
#include        "../monitor/mondef.asm"
#include        "CAMEL80.S"
