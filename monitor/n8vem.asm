N8VEM           equ     1               ; If set to 1, the monitor is built
                                        ; for the N8VEM, otherwise, the
                                        ; old homebrew Z80 computer is targeted.
FEATURE_BASIC   equ     1               ; If set to 0, the BASIC interpreter
                                        ; will not be included.
LOADABLE_MONITOR    equ 0               ; Generate a ROM- and not a disk-image.
#include        "monitor/monitor.asm"
