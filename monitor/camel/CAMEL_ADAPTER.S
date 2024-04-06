;
;  Extensions to CAMEL Forth that allow access to the simple Z80 monitor 
; system calls.
;  This file is for the Forth interpreter that runs in RAM and is not part
; of the monitor!
;
; 10-JUN-2012   B. Ulmann
; 12-MAY-2012   B. Ulmann       Introduced the LOADABLE flag
;
;
;MOUNT: Mount a FAT formatted disk - otherwise no input/output.
                defw    link217
                defb    0
link218         defb    5, "mount"
MOUNT           
#if LOADABLE = 1
                push    ix
                ld      ix, _fatmount
                rst     08
                pop     ix
#else
                call    fatmount
#endif
;
                ex de,hl                        ; next
                ld e,(hl)
                inc hl
                ld d,(hl)
                inc hl
                ex de,hl
                jp (hl)
;GETS: Like ACCEPT but returns the address of a 0-terminated string.
                defw    link218
                defb    0
link219         defb    4, "gets"
GETS            call    docolon
;
                dw      PAD, LIT, 80, ACCEPT, PAD, PLUS, LIT, 0
                dw      SWOP, CSTORE, PAD, EXIT
;FOPEN: Expects the address of a 0-terminated string on the TOS, and opens
;       the file specified. It returns the first cluster number on the TOS.
;       If the file can not be opened, this 1st cluster number is $0000.
                defw    link219
                defb    0
link220         defb    5, "fopen"
FOPEN           
#if LOADABLE = 1
                push    ix
#endif
                push    iy
                push    hl
                push    de
                ld      hl, bc                  ; The string's address is on TOS
                ld      de, string_12_bfr       ; Get string buffer address
                ld      iy, fcb                 ; Get FCB address
#if LOADABLE = 1
                ld      ix, _fopen              ; What system call?
                rst     08                      ; Execute system call
#else
                call    fopen
#endif
                ld      iy, fcb                 ; Get the first cluster number
                ld      b, (iy + fcb_first_cluster)
                ld      c, (iy + fcb_first_cluster + 1)
                pop     de
                pop     hl
                pop     iy
#if LOADABLE = 1
                pop     ix
#endif
;
                ex de,hl                        ; next
                ld e,(hl)
                inc hl
                ld d,(hl)
                inc hl
                ex de,hl
                jp (hl)
;FGETC: Read a character from the file currently opened and return it on the TOS
;       - if EOF is reached, a 0 will be returned.
                defw    link220
                defb    0
link221         defb    5, "fgetc"
FGETC           push    bc                      ; Make room for a new TOS
#if LOADABLE = 1
                push    ix
#endif
                push    iy
                ld      iy, fcb                 ; Get the FCB address
#if LOADABLE = 1
                ld      ix, _fgetc              ; What system call?
                rst     08                      ; Execute the system call
#else
                call    fgetc
#endif
                jr      nc, FGETC_OK            ; End of file not yet reached?
                xor     a                       ; EOF -> Return 0 byte
FGETC_OK        ld      b, 0                    ; Copy character read to new TOS
                ld      c, a
                pop     iy
#if LOADABLE = 1
                pop     ix
#endif
;
                ex de,hl                        ; next
                ld e,(hl)
                inc hl
                ld d,(hl)
                inc hl
                ex de,hl
                jp (hl)
;STRLEN: Return the length of a 0-terminated string whose address is found
;        on the TOS.
                defw    link221
                defb    0
link222         defb     6, "strlen"
STRLEN          push    hl
                ld      hl, bc                  ; Copy pointer to string to HL
                ld      bc, 0                   ; Counter for string length
STRLEN_LOOP     ld      a, (hl)
                cp      eos
                jr      z, STRLEN_EXIT          ; Terminator found
                inc     bc
                inc     hl
                jr      STRLEN_LOOP
STRLEN_EXIT     pop     hl
;
                ex de,hl                        ; next
                ld e,(hl)
                inc hl
                ld d,(hl)
                inc hl
                ex de,hl
                jp (hl)
;FGETS: Get a record from a file (delimited by CR/LF). Expects address and 
;       maximum length on stack, returns actual length.
;Side effect: This routine makes use of the 12 character string buffer to
;             store the buffer address temporarily!
                defw    link222
                defb    0
link223         defb    5, "fgets"
FGETS           ld      (scratch_area), bc      ; Remember maximum length
                pop     bc                      ; Get address of string buffer
#if LOADABLE = 1
                push    ix
#endif
                push    iy
                push    hl
                push    de
                ld      hl, bc                  ; HL now points to the buffer
                ld      bc, (scratch_area)      ; Get buffer length
                ld      b, c
                ld      de, hl                  ; Remember start of string
FGETS_LOOP      
                ld      iy, fcb                 ; Pointer to FCB
#if LOADABLE = 1
                ld      ix, _fgetc
                rst     08
#else
                call    fgetc
#endif
                jr      c, FGETS_DONE           ; EOF
                cp      cr                      ; Terminator CR found?
                jr      z, FGETS_DONE
                cp      lf                      ; Terminator LF found?
                jr      z, FGETS_DONE
                ld      (hl), a                 ; Store character
                inc     hl
                djnz    FGETS_LOOP              ; Get next char. if enough space
FGETS_DONE      ld      (hl), eos               ; Write terminating byte
                and     a                       ; Clear carry
                sbc     hl, de                  ; Determine string length
                ld      bc, hl                  ; String length -> TOS
                pop     de                      ; Restore register contents
                pop     hl
                pop     iy
#if LOADABLE = 1
                pop     ix
#endif
;
                ex de,hl                        ; next
                ld e,(hl)
                inc hl
                ld d,(hl)
                inc hl
                ex de,hl
                jp (hl)
;LOAD: Ask for a filename and load the corresponding file.
;  S" ENTER FILENAME: " TYPE
;  GETS
;  FOPEN 0= IF
;    ." UNABLE TO OPEN FILE!"
;  ELSE
;    BEGIN
;      TIB DUP TIBSIZE FGETS ?DUP 0 <>
;    WHILE
;      INTERPRET
;    REPEAT
;  THEN
;
                defw    link223
                defb    0
link224         defb    4, "load"
LOAD            call    docolon
                dw      XSQUOTE
                db      16, "Enter Filename: "
                dw      TYPE, GETS, FOPEN, LIT, 0, NOTEQUAL, QBRANCH, LOAD_ERR
LOAD_BEGIN      dw      TIB, DUP, TIBSIZE, FGETS
                dw      QDUP, LIT, 0, NOTEQUAL, QBRANCH, LOAD_EXIT
                dw      INTERPRET, BRANCH, LOAD_BEGIN
                dw      BRANCH, LOAD_EXIT
LOAD_ERR        dw      XSQUOTE
                db      24, 0ah, 0dh, "Unable to open file!", 0ah, 0dh
                dw      TYPE
LOAD_EXIT       dw      EXIT

