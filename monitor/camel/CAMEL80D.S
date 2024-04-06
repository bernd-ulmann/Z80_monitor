; LISTING 3.
;
; ===============================================
; CamelForth for the Zilog Z80
; (c) 1994 Bradford J. Rodriguez
; Permission is granted to freely copy, modify,
; and distribute this program for personal or
; educational use.  Commercial inquiries should
; be directed to the author at 221 King St. E.,
; #32, Hamilton, Ontario L8N 1B5 Canada
;
; CAMEL80D.S: CPU and Model Dependencies
;   Source code is for the Zilog Macro Assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
;
; Direct-Threaded Forth model for Zilog Z80
;   cell size is   16 bits (2 bytes)
;   char size is    8 bits (1 byte)
;   address unit is 8 bits (1 byte), i.e.,
;       addresses are byte-aligned.
; ===============================================

; ALIGNMENT AND PORTABILITY OPERATORS ===========
; Many of these are synonyms for other words,
; and so are defined as CODE words.

;C ALIGN    --                         align HERE
;    head ALIGN,5,"ALIGN",docode
        defw    link74
        defb    0
link75  defb    5, "ALIGN"
ALIGN
;
noop:   ;next
        ex de,hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ex de,hl
        jp (hl)

;C ALIGNED  addr -- a-addr       align given addr
;    head ALIGNED,7,"ALIGNED",docode
        defw    link75
        defb    0
link76  defb    7, "ALIGNED"
ALIGNED
;
        jr noop

;Z CELL     -- n                 size of one cell
;    head CELL,4,"CELL",docon
        defw    link76
        defb    0
link77  defb    4, "CELL"
CELL    call    docon
;
        dw 2

;C CELL+    a-addr1 -- a-addr2      add cell size
;   2 + ;
;    head CELLPLUS,5,"CELL+",docode
        defw    link77
        defb    0
link78  defb    5, "CELL+"
CELLPLUS
;
        inc bc
        inc bc
        ;next
        ex de,hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ex de,hl
        jp (hl)

;C CELLS    n1 -- n2            cells->adrs units
;    head CELLS,5,"CELLS",docode
        defw    link78
        defb    0
link79  defb    5, "CELLS"
CELLS
;
        jp TWOSTAR

;C CHAR+    c-addr1 -- c-addr2   add char size
;    head CHARPLUS,5,"CHAR+",docode
        defw    link79
        defb    0
link80  defb    5, "CHAR+"
CHARPLUS
;
        jp ONEPLUS

;C CHARS    n1 -- n2            chars->adrs units
;    head CHARS,5,"CHARS",docode
        defw    link80
        defb    0
link81  defb    5, "CHARS"
CHARS
;
        jr noop

;C >BODY    xt -- a-addr      adrs of param field
;   3 + ;                     Z80 (3 byte CALL)
;    head TOBODY,5,">BODY",docolon
        defw    link81
        defb    0
link82  defb    5, ">BODY"
TOBODY  call    docolon
;
        DW LIT,3,PLUS,EXIT

;X COMPILE,  xt --         append execution token
; I called this word ,XT before I discovered that
; it is defined in the ANSI standard as COMPILE,.
; On a DTC Forth this simply appends xt (like , )
; but on an STC Forth this must append 'CALL xt'.
;    head COMMAXT,8,"COMPILE,",docode
        defw    link82
        defb    0
link83  defb    7, "COMPILE"                    ; CHANGED: Was 8?!?!?
COMMAXT
;
        jp      COMMA

;Z !CF    adrs cfa --   set code action of a word
;   0CD OVER C!         store 'CALL adrs' instr
;   1+ ! ;              Z80 VERSION
; Depending on the implementation this could
; append CALL adrs or JUMP adrs.
;    head STORECF,3,"!CF",docolon
        defw    link83
        defb    0
link84  defb    3, "!CF"
STORECF call    docolon
;
        DW LIT,0CDH,OVER,CSTORE
        DW ONEPLUS,STORE,EXIT

;Z ,CF    adrs --       append a code field
;   HERE !CF 3 ALLOT ;  Z80 VERSION (3 bytes)
;    head COMMACF,3,",CF",docolon
        defw    link84
        defb    0
link85  defb    3, ",CF"
COMMACF call    docolon
;
        DW HERE,STORECF,LIT,3,ALLOT,EXIT

;Z !COLON   --      change code field to docolon
;   -3 ALLOT docolon-adrs ,CF ;
; This should be used immediately after CREATE.
; This is made a distinct word, because on an STC
; Forth, colon definitions have no code field.
;    head STORCOLON,6,"!COLON",docolon
        defw    link85
        defb    0
link86  defb    6, "!COLON"
STORCOLON       call    docolon
;
        DW LIT,-3,ALLOT
        DW LIT,docolon,COMMACF,EXIT

;Z ,EXIT    --      append hi-level EXIT action
;   ['] EXIT ,XT ;
; This is made a distinct word, because on an STC
; Forth, it appends a RET instruction, not an xt.
;    head CEXIT,5,",EXIT",docolon
        defw    link86
        defb    0
link87  defb    5, ",EXIT"
CEXIT   call    docolon
;
        DW LIT,EXIT,COMMAXT,EXIT

; CONTROL STRUCTURES ============================
; These words allow Forth control structure words
; to be defined portably.

;Z ,BRANCH   xt --    append a branch instruction
; xt is the branch operator to use, e.g. qbranch
; or (loop).  It does NOT append the destination
; address.  On the Z80 this is equivalent to ,XT.
;    head COMMABRANCH,7,",BRANCH",docode
        defw    link87
        defb    0
link88  defb    7, ",BRANCH"
COMMABRANCH
;
        jp COMMA

;Z ,DEST   dest --        append a branch address
; This appends the given destination address to
; the branch instruction.  On the Z80 this is ','
; ...other CPUs may use relative addressing.
;    head COMMADEST,5,",DEST",docode
        defw    link88
        defb    0
link89  defb    5, ",DEST"
COMMADEST
;
        jp COMMA

;Z !DEST   dest adrs --    change a branch dest'n
; Changes the destination address found at 'adrs'
; to the given 'dest'.  On the Z80 this is '!'
; ...other CPUs may need relative addressing.
;    head STOREDEST,5,"!DEST",docode
        defw    link89
        defb    0
link90  defb    5, "!DEST"
STOREDEST
;
        jp STORE

; HEADER STRUCTURE ==============================
; The structure of the Forth dictionary headers
; (name, link, immediate flag, and "smudge" bit)
; does not necessarily differ across CPUs.  This
; structure is not easily factored into distinct
; "portable" words; instead, it is implicit in
; the definitions of FIND and CREATE, and also in
; NFA>LFA, NFA>CFA, IMMED?, IMMEDIATE, HIDE, and
; REVEAL.  These words must be (substantially)
; rewritten if either the header structure or its
; inherent assumptions are changed.


