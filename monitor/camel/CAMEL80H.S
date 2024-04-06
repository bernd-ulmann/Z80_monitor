; LISTING 2.
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
; CAMEL80H.S: High Level Words
;   Source code is for the Zilog Macro Assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
; ===============================================

; SYSTEM VARIABLES & CONSTANTS ==================

;C BL      -- char            an ASCII space
;    head BL,2,"BL",docon
        defw    link90
        defb    0
link91  defb    2, "bl"
BL      call    docon
;
        dw 20h

;Z tibsize  -- n         size of TIB
;    head TIBSIZE,7,"TIBSIZE",docon
        defw    link91
        defb    0
link92  defb    7, "tibsize"
TIBSIZE call    docon
;
        dw 124          ; 2 chars safety zone

;X tib     -- a-addr     Terminal Input Buffer
;  HEX 82 CONSTANT TIB   CP/M systems: 126 bytes
;  HEX -80 USER TIB      others: below user area
;    head TIB,3,"TIB",docon
        defw    link92
        defb    0
link93  defb    3, "tib"
TIB     call    douser
;
        dw -80h

;Z u0      -- a-addr       current user area adrs
;  0 USER U0
;    head U0,2,"U0",douser
        defw    link93
        defb    0
link94  defb    2, "u0"
U0      call    douser
;
        dw 0

;C >IN     -- a-addr        holds offset into TIB
;  2 USER >IN
;    head TOIN,3,">IN",douser
        defw    link94
        defb    0
link95  defb    3, ">in"
TOIN    call    douser
;
        dw 2

;C BASE    -- a-addr       holds conversion radix
;  4 USER BASE
;    head BASE,4,"BASE",douser
        defw    link95
        defb    0
link96  defb    4, "base"
BASE    call    douser
;
        dw 4

;C STATE   -- a-addr       holds compiler state
;  6 USER STATE
;    head STATE,5,"STATE",douser
        defw    link96
        defb    0
link97  defb    5, "state"
STATE   call    douser
;
        dw 6

;Z dp      -- a-addr       holds dictionary ptr
;  8 USER DP
;    head DP,2,"DP",douser
        defw    link97
        defb    0
link98  defb    2, "dp"
DP      call    douser
;
        dw 8

;Z 'source  -- a-addr      two cells: len, adrs
; 10 USER 'SOURCE
;;    head TICKSOURCE,7,"'SOURCE",douser
;        DW link                 ; must expand
;        DB 0                    ; manually
;link    .SET $                  ; because of
;        DB 7,27h,"SOURCE"       ; tick character
        defw    link98
        defb    0
link99  defb    7, $27, "source"
;
TICKSOURCE: call douser         ; in name!
        dw 10

;Z latest    -- a-addr     last word in dict.
;   14 USER LATEST
;    head LATEST,6,"LATEST",douser
        defw    link99
        defb    0
link100 defb    6, "latest"
LATEST  call    douser
;
        dw 14

;Z hp       -- a-addr     HOLD pointer
;   16 USER HP
;    head HP,2,"HP",douser
        defw    link100
        defb    0
link101 defb    2, "hp"
HP      call    douser
;
        dw 16

;Z LP       -- a-addr     Leave-stack pointer
;   18 USER LP
;    head LP,2,"LP",douser
        defw    link101
        defb    0
link102 defb    2, "lp"
LP      call    douser
;
        dw 18

;Z s0       -- a-addr     end of parameter stack
;    head S0,2,"S0",douser
        defw    link102
        defb    0
link103 defb    2, "s0"
S0      call    douser
;
        dw 100h

;X PAD       -- a-addr    user PAD buffer
;                         = end of hold area!
;    head PAD,3,"PAD",douser
        defw    link103
        defb    0
link104 defb    3, "pad"
PAD     call    douser
;
        dw 128h

;Z l0       -- a-addr     bottom of Leave stack
;    head L0,2,"L0",douser
        defw    link104
        defb    0
link105 defb    2, "l0"
L0      call    douser
;
        dw 180h

;Z r0       -- a-addr     end of return stack
;    head R0,2,"R0",douser
        defw    link105
        defb    0
link106 defb    2, "r0"
R0      call    douser
;
        dw 200h

;Z uinit    -- addr  initial values for user area
;    head UINIT,5,"UINIT",docreate
        defw    link106
        defb    0
link107 defb    5, "uinit"
UINIT   call    docreate
;
        DW 0,0,10,0     ; reserved,>IN,BASE,STATE
        DW enddict      ; DP
        DW 0,0          ; SOURCE init'd elsewhere
        DW lastword     ; LATEST
        DW 0            ; HP init'd elsewhere

;Z #init    -- n    #bytes of user area init data
;    head NINIT,5,"#INIT",docon
        defw    link107
        defb    0
link108 defb    5, "#init"
NINIT   call    docon
;
        DW 18

; ARITHMETIC OPERATORS ==========================

;C S>D    n -- d          single -> double prec.
;   DUP 0< ;
;    head STOD,3,"S>D",docolon
        defw    link108
        defb    0
link109 defb    3, "s>d"
STOD    call    docolon
;
        dw DUP,ZEROLESS,EXIT

;Z ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;        ...a common factor
;    head QNEGATE,7,"?NEGATE",docolon
        defw    link109
        defb    0
link110 defb    7, "?negate"
QNEGATE call    docolon
;
        DW ZEROLESS,QBRANCH,QNEG1,NEGATE
QNEG1:  DW EXIT

;C ABS     n1 -- +n2     absolute value
;   DUP ?NEGATE ;
;    head ABS,3,"ABS",docolon
        defw    link110
        defb    0
link111 defb    3, "abs"
ABS     call    docolon
;
        DW DUP,QNEGATE,EXIT

;X DNEGATE   d1 -- d2     negate double precision
;   SWAP INVERT SWAP INVERT 1 M+ ;
;    head DNEGATE,7,"DNEGATE",docolon
        defw    link111
        defb    0
link112 defb    7, "dnegate"
DNEGATE call    docolon
;
        DW SWOP,INVERT,SWOP,INVERT,LIT,1,MPLUS
        DW EXIT

;Z ?DNEGATE  d1 n -- d2   negate d1 if n negative
;   0< IF DNEGATE THEN ;       ...a common factor
;    head QDNEGATE,8,"?DNEGATE",docolon
        defw    link112
        defb    0
link113 defb    8, "?dnegate"
QDNEGATE        call    docolon
;
        DW ZEROLESS,QBRANCH,DNEG1,DNEGATE
DNEG1:  DW EXIT

;X DABS     d1 -- +d2    absolute value dbl.prec.
;   DUP ?DNEGATE ;
;    head DABS,4,"DABS",docolon
        defw    link113
        defb    0
link114 defb    4, "dabs"
DABS    call    docolon
;
        DW DUP,QDNEGATE,EXIT

;C M*     n1 n2 -- d    signed 16*16->32 multiply
;   2DUP XOR >R        carries sign of the result
;   SWAP ABS SWAP ABS UM*
;   R> ?DNEGATE ;
;    head MSTAR,2,"M*",docolon
        defw    link114
        defb    0
link115 defb    2, "m*"
MSTAR   call    docolon
;
        DW TWODUP,XOR,TOR
        DW SWOP,ABS,SWOP,ABS,UMSTAR
        DW RFROM,QDNEGATE,EXIT

;C SM/REM   d1 n1 -- n2 n3   symmetric signed div
;   2DUP XOR >R              sign of quotient
;   OVER >R                  sign of remainder
;   ABS >R DABS R> UM/MOD
;   SWAP R> ?NEGATE
;   SWAP R> ?NEGATE ;
; Ref. dpANS-6 section 3.2.2.1.
;    head SMSLASHREM,6,"SM/REM",docolon
        defw    link115
        defb    0
link116 defb    6, "sm/rem"
SMSLASHREM      call    docolon
;
        DW TWODUP,XOR,TOR,OVER,TOR
        DW ABS,TOR,DABS,RFROM,UMSLASHMOD
        DW SWOP,RFROM,QNEGATE,SWOP,RFROM,QNEGATE
        DW EXIT

;C FM/MOD   d1 n1 -- n2 n3   floored signed div'n
;   DUP >R              save divisor
;   SM/REM
;   DUP 0< IF           if quotient negative,
;       SWAP R> +         add divisor to rem'dr
;       SWAP 1-           decrement quotient
;   ELSE R> DROP THEN ;
; Ref. dpANS-6 section 3.2.2.1.
;    head FMSLASHMOD,6,"FM/MOD",docolon
        defw    link116
        defb    0
link117 defb    6, "fm/mod"
FMSLASHMOD      call    docolon
;
        DW DUP,TOR,SMSLASHREM
        DW DUP,ZEROLESS,QBRANCH,FMMOD1
        DW SWOP,RFROM,PLUS,SWOP,ONEMINUS
        DW BRANCH,FMMOD2
FMMOD1: DW RFROM,DROP
FMMOD2: DW EXIT

;C *      n1 n2 -- n3       signed multiply
;   M* DROP ;
;    head STAR,1,'*',docolon
        defw    link117
        defb    0
link118 defb    1, "*"
STAR    call    docolon
;
        dw MSTAR,DROP,EXIT

;C /MOD   n1 n2 -- n3 n4    signed divide/rem'dr
;   >R S>D R> FM/MOD ;
;    head SLASHMOD,4,"/MOD",docolon
        defw    link118
        defb    0
link119 defb    4, "/mod"
SLASHMOD        call    docolon
;
        dw TOR,STOD,RFROM,FMSLASHMOD,EXIT

;C /      n1 n2 -- n3       signed divide
;   /MOD nip ;
;    head SLASH,1,'/',docolon
        defw    link119
        defb    0
link120 defb    1, "/"
SLASH   call    docolon
;
        dw SLASHMOD,NIP,EXIT

;C MOD    n1 n2 -- n3       signed remainder
;   /MOD DROP ;
;    head MOD,3,"MOD",docolon
        defw    link120
        defb    0
link121 defb    3, "mod"
MOD     call    docolon
;
        dw SLASHMOD,DROP,EXIT

;C */MOD  n1 n2 n3 -- n4 n5    n1*n2/n3, rem&quot
;   >R M* R> FM/MOD ;
;    head SSMOD,5,"*/MOD",docolon
        defw    link121
        defb    0
link122 defb    5, "*/mod"
SSMOD   call    docolon
;
        dw TOR,MSTAR,RFROM,FMSLASHMOD,EXIT

;C */     n1 n2 n3 -- n4        n1*n2/n3
;   */MOD nip ;
;    head STARSLASH,2,"*/",docolon
        defw    link122
        defb    0
link123 defb    2, "*/"
STARSLASH       call    docolon
;
        dw SSMOD,NIP,EXIT

;C MAX    n1 n2 -- n3       signed maximum
;   2DUP < IF SWAP THEN DROP ;
;    head MAX,3,"MAX",docolon
        defw    link123
        defb    0
link124 defb    3, "max"
MAX     call    docolon
;
        dw TWODUP,LESS,QBRANCH,MAX1,SWOP
MAX1:   dw DROP,EXIT

;C MIN    n1 n2 -- n3       signed minimum
;   2DUP > IF SWAP THEN DROP ;
;    head MIN,3,"MIN",docolon
        defw    link124
        defb    0
link125 defb    3, "min"
MIN     call    docolon
;
        dw TWODUP,GREATER,QBRANCH,MIN1,SWOP
MIN1:   dw DROP,EXIT

; DOUBLE OPERATORS ==============================

;C 2@    a-addr -- x1 x2    fetch 2 cells
;   DUP CELL+ @ SWAP @ ;
;   the lower address will appear on top of stack
;    head TWOFETCH,2,"2@",docolon
        defw    link125
        defb    0
link126 defb    2, "2@"
TWOFETCH        call    docolon
;
        dw DUP,CELLPLUS,FETCH,SWOP,FETCH,EXIT

;C 2!    x1 x2 a-addr --    store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
;    head TWOSTORE,2,"2!",docolon
        defw    link126
        defb    0
link127 defb    2, "2!"
TWOSTORE        call    docolon
;
        dw SWOP,OVER,STORE,CELLPLUS,STORE,EXIT

;C 2DROP  x1 x2 --          drop 2 cells
;   DROP DROP ;
;    head TWODROP,5,"2DROP",docolon
        defw    link127
        defb    0
link128 defb    5, "2drop"
TWODROP call    docolon
;
        dw DROP,DROP,EXIT

;C 2DUP   x1 x2 -- x1 x2 x1 x2   dup top 2 cells
;   OVER OVER ;
;    head TWODUP,4,"2DUP",docolon
        defw    link128
        defb    0
link129 defb    4, "2dup"
TWODUP  call    docolon
;
        dw OVER,OVER,EXIT

;C 2SWAP  x1 x2 x3 x4 -- x3 x4 x1 x2  per diagram
;   ROT >R ROT R> ;
;    head TWOSWAP,5,"2SWAP",docolon
        defw    link129
        defb    0
link130 defb    5, "2swap"
TWOSWAP call    docolon
;
        dw ROT,TOR,ROT,RFROM,EXIT

;C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
;   >R >R 2DUP R> R> 2SWAP ;
;    head TWOOVER,5,"2OVER",docolon
        defw    link130
        defb    0
link131 defb    5, "2over"
TWOOVER call    docolon
;
        dw TOR,TOR,TWODUP,RFROM,RFROM
        dw TWOSWAP,EXIT

; INPUT/OUTPUT ==================================

;C COUNT   c-addr1 -- c-addr2 u  counted->adr/len
;   DUP CHAR+ SWAP C@ ;
;    head COUNT,5,"COUNT",docolon
        defw    link131
        defb    0
link132 defb    5, "count"
COUNT   call    docolon
;
        dw DUP,CHARPLUS,SWOP,CFETCH,EXIT

;C CR      --               output newline
;   0D EMIT 0A EMIT ;
;    head CR,2,"CR",docolon
        defw    link132
        defb    0
link133 defb    2, "cr"
CR      call    docolon
;
        dw LIT,0dh,EMIT,LIT,0ah,EMIT,EXIT

;C SPACE   --               output a space
;   BL EMIT ;
;    head SPACE,5,"SPACE",docolon
        defw    link133
        defb    0
link134 defb    5, "space"
SPACE   call    docolon
;
        dw BL,EMIT,EXIT

;C SPACES   n --            output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
;    head SPACES,6,"SPACES",docolon
        defw    link134
        defb    0
link135 defb    6, "spaces"
SPACES  call    docolon
;
SPCS1:  DW DUP,QBRANCH,SPCS2
        DW SPACE,ONEMINUS,BRANCH,SPCS1
SPCS2:  DW DROP,EXIT

;Z umin     u1 u2 -- u      unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
;    head UMIN,4,"UMIN",docolon
        defw    link135
        defb    0
link136 defb    4, "umin"
UMIN    call    docolon
;
        DW TWODUP,UGREATER,QBRANCH,UMIN1,SWOP
UMIN1:  DW DROP,EXIT

;Z umax    u1 u2 -- u       unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
;    head UMAX,4,"UMAX",docolon
        defw    link136
        defb    0
link137 defb    4, "umax"
UMAX    call    docolon
;
        DW TWODUP,ULESS,QBRANCH,UMAX1,SWOP
UMAX1:  DW DROP,EXIT

;C ACCEPT  c-addr +n -- +n'  get line from term'l
;   OVER + 1- OVER      -- sa ea a
;   BEGIN KEY           -- sa ea a c
;   DUP 0D <> WHILE
;       DUP EMIT        -- sa ea a c
;       DUP 8 = IF  DROP 1-    >R OVER R> UMAX
;             ELSE  OVER C! 1+ OVER UMIN
;       THEN            -- sa ea a
;   REPEAT              -- sa ea a c
;   DROP NIP SWAP - ;
;    head ACCEPT,6,"ACCEPT",docolon
                defw    link137
                defb    0
link138         defb    6, "accept"
ACCEPT          ld      (scratch_area), hl      ; Save HL
                ld      (scratch_area + 2), de  ; ...and DE
                ld      b, c                    ; Get buffer length
                pop     hl                      ; Get buffer address
#if LOADABLE = 1
                push    ix
                ld      ix, _gets
                rst     08
                pop     ix
#else
                call    gets
#endif
                ld      de, hl
ACCEPT_LOOP     ld      a, (hl)
                cp      0
                jr      z, ACCEPT_EXIT
                inc     hl
                jr      ACCEPT_LOOP
ACCEPT_EXIT     and     a                       ; Clear carry, just in case
                sbc     hl, de                  ; HL now contains the strlen
                ld      bc, hl                  ; Strlen -> TOS
                ld      de, (scratch_area + 2)  ; Restore DE
                ld      hl, (scratch_area)      ; ...and H
        ;
                ex de,hl                        ; next
                ld e,(hl)
                inc hl
                ld d,(hl)
                inc hl
                ex de,hl
                jp (hl)
;;
;        DW OVER,PLUS,ONEMINUS,OVER
;ACC1:   DW KEY,DUP,LIT,0DH,NOTEQUAL,QBRANCH,ACC5
;        DW DUP,EMIT
;;*** If the character is a LF, skip it!
;        DW DUP,LIT,0ah,EQUAL,QBRANCH,ACC1_1
;        DW DROP,BRANCH,ACC1
;;***
;ACC1_1: DW DUP,LIT,8,EQUAL,QBRANCH,ACC3
;        DW DROP,ONEMINUS,TOR,OVER,RFROM,UMAX
;
;        dw LIT, '\', EMIT
;
;        DW BRANCH,ACC4
;ACC3:   DW OVER,CSTORE,ONEPLUS,OVER,UMIN
;ACC4:   DW BRANCH,ACC1
;ACC5:   DW DROP,NIP,SWOP,MINUS,EXIT

;C TYPE    c-addr +n --     type line to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN ;
;    head TYPE,4,"TYPE",docolon
        defw    link138
        defb    0
link139 defb    4, "type"
TYPE    call    docolon
;
        DW QDUP,QBRANCH,TYP4
        DW OVER,PLUS,SWOP,XDO
TYP3:   DW II,CFETCH,EMIT,XLOOP,TYP3
        DW BRANCH,TYP5
TYP4:   DW DROP
TYP5:   DW EXIT

;Z (S")     -- c-addr u   run-time code for S"
;   R> COUNT 2DUP + ALIGNED >R  ;
;    head XSQUOTE,4,"(S\")",docolon
        defw    link139
        defb    0
link140 defb    4, "(S", $22, ")"
XSQUOTE call    docolon
;
        DW RFROM,COUNT,TWODUP,PLUS,ALIGNED,TOR
        DW EXIT

;C S"       --         compile in-line string
;   COMPILE (S")  [ HEX ]
;   22 WORD C@ 1+ ALIGNED ALLOT ; IMMEDIATE
;    immed SQUOTE,2,"S\"",docolon
        defw    link140
        defb    1
link141 defb    2, "s", $22
SQUOTE  call    docolon
;
        DW LIT,XSQUOTE,COMMAXT
        DW LIT,22H,WORD,CFETCH,ONEPLUS
        DW ALIGNED,ALLOT,EXIT

;C ."       --         compile string to print
;   POSTPONE S"  POSTPONE TYPE ; IMMEDIATE
;    immed DOTQUOTE,2,".\"",docolon
        defw    link141
        defb    1
link142 defb    2, ".", $22
DOTQUOTE        call    docolon
;
        DW SQUOTE
        DW LIT,TYPE,COMMAXT
        DW EXIT

; NUMERIC OUTPUT ================================
; Numeric conversion is done l.s.digit first, so
; the output buffer is built backwards in memory.

; Some double-precision arithmetic operators are
; needed to implement ANSI numeric conversion.

;Z UD/MOD   ud1 u2 -- u3 ud4   32/16->32 divide
;   >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
;    head UDSLASHMOD,6,"UD/MOD",docolon
        defw    link142
        defb    0
link143 defb    6, "ud/mod"
UDSLASHMOD      call    docolon
;
        DW TOR,LIT,0,RFETCH,UMSLASHMOD,ROT,ROT
        DW RFROM,UMSLASHMOD,ROT,EXIT

;Z UD*      ud1 d2 -- ud3      32*16->32 multiply
;   DUP >R UM* DROP  SWAP R> UM* ROT + ;
;    head UDSTAR,3,"UD*",docolon
        defw    link143
        defb    0
link144 defb    3, "ud*"
UDSTAR  call    docolon
;
        DW DUP,TOR,UMSTAR,DROP
        DW SWOP,RFROM,UMSTAR,ROT,PLUS,EXIT

;C HOLD  char --        add char to output string
;   -1 HP +!  HP @ C! ;
;    head HOLD,4,"HOLD",docolon
        defw    link144
        defb    0
link145 defb    4, "hold"
HOLD    call    docolon
;
        DW LIT,-1,HP,PLUSSTORE
        DW HP,FETCH,CSTORE,EXIT

;C <#    --             begin numeric conversion
;   PAD HP ! ;          (initialize Hold Pointer)
;    head LESSNUM,2,"<#",docolon
        defw    link145
        defb    0
link146 defb    2, "<#"
LESSNUM call    docolon
;
        DW PAD,HP,STORE,EXIT

;Z >digit   n -- c      convert to 0..9A..Z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
;    head TODIGIT,6,">DIGIT",docolon
        defw    link146
        defb    0
link147 defb    6, ">digit"
TODIGIT call    docolon
;
        DW DUP,LIT,9,GREATER,LIT,7,AND,PLUS
        DW LIT,30H,PLUS,EXIT

;C #     ud1 -- ud2     convert 1 digit of output
;   BASE @ UD/MOD ROT >digit HOLD ;
;    head NUM,1,'#',docolon
        defw    link147
        defb    0
link148 defb    1, "#"
NUM     call    docolon
;
        DW BASE,FETCH,UDSLASHMOD,ROT,TODIGIT
        DW HOLD,EXIT

;C #S    ud1 -- ud2     convert remaining digits
;   BEGIN # 2DUP OR 0= UNTIL ;
;    head NUMS,2,"#S",docolon
        defw    link148
        defb    0
link149 defb    2, "#s"
NUMS    call    docolon
;
NUMS1:  DW NUM,TWODUP,OR,ZEROEQUAL,QBRANCH,NUMS1
        DW EXIT

;C #>    ud1 -- c-addr u    end conv., get string
;   2DROP HP @ PAD OVER - ;
;    head NUMGREATER,2,"#>",docolon
        defw    link149
        defb    0
link150 defb    2, "#>"
NUMGREATER      call    docolon
;
        DW TWODROP,HP,FETCH,PAD,OVER,MINUS,EXIT

;C SIGN  n --           add minus sign if n<0
;   0< IF 2D HOLD THEN ;
;    head SIGN,4,"SIGN",docolon
        defw    link150
        defb    0
link151 defb    4, "sign"
SIGN    call    docolon
;
        DW ZEROLESS,QBRANCH,SIGN1,LIT,2DH,HOLD
SIGN1:  DW EXIT

;C U.    u --           display u unsigned
;   <# 0 #S #> TYPE SPACE ;
;    head UDOT,2,"U.",docolon
        defw    link151
        defb    0
link152 defb    2, "u."
UDOT    call    docolon
;
        DW LESSNUM,LIT,0,NUMS,NUMGREATER,TYPE
        DW SPACE,EXIT

;C .     n --           display n signed
;   <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
;    head DOT,1,'.',docolon
        defw    link152
        defb    0
link153 defb    1, "."
DOT     call    docolon
;
        DW LESSNUM,DUP,ABS,LIT,0,NUMS
        DW ROT,SIGN,NUMGREATER,TYPE,SPACE,EXIT

;C DECIMAL  --      set number base to decimal
;   10 BASE ! ;
;    head DECIMAL,7,"DECIMAL",docolon
        defw    link153
        defb    0
link154 defb    7, "decimal"
DECIMAL call    docolon
;
        DW LIT,10,BASE,STORE,EXIT

;X HEX     --       set number base to hex
;   16 BASE ! ;
;    head HEX,3,"HEX",docolon
        defw    link154
        defb    0
link155 defb    3, "hex"
HEX     call    docolon
;
        DW LIT,16,BASE,STORE,EXIT

; DICTIONARY MANAGEMENT =========================

;C HERE    -- addr      returns dictionary ptr
;   DP @ ;
;    head HERE,4,"HERE",docolon
        defw    link155
        defb    0
link156 defb    4, "here"
HERE    call    docolon
;
        dw DP,FETCH,EXIT

;C ALLOT   n --         allocate n bytes in dict
;   DP +! ;
;    head ALLOT,5,"ALLOT",docolon
        defw    link156
        defb    0
link157 defb    5, "allot"
ALLOT   call    docolon
;
        dw DP,PLUSSTORE,EXIT

; Note: , and C, are only valid for combined
; Code and Data spaces.

;C ,    x --           append cell to dict
;   HERE ! 1 CELLS ALLOT ;
;    head COMMA,1,2CH,docolon
        defw    link157
        defb    0
link158 defb    1, ","
COMMA   call    docolon
;
        dw HERE,STORE,LIT,1,CELLS,ALLOT,EXIT

;C C,   char --        append char to dict
;   HERE C! 1 CHARS ALLOT ;
;    head CCOMMA,2,"C,",docolon
        defw    link158
        defb    0
link159 defb    2, "c,"
CCOMMA  call    docolon
;
        dw HERE,CSTORE,LIT,1,CHARS,ALLOT,EXIT

; INTERPRETER ===================================
; Note that NFA>LFA, NFA>CFA, IMMED?, and FIND
; are dependent on the structure of the Forth
; header.  This may be common across many CPUs,
; or it may be different.

;C SOURCE   -- adr n    current input buffer
;   'SOURCE 2@ ;        length is at lower adrs
;    head SOURCE,6,"SOURCE",docolon
        defw    link159
        defb    0
link160 defb    6, "source"
SOURCE  call    docolon
;
        DW TICKSOURCE,TWOFETCH,EXIT

;X /STRING  a u n -- a+n u-n   trim string
;   ROT OVER + ROT ROT - ;
;    head SLASHSTRING,7,"/STRING",docolon
        defw    link160
        defb    0
link161 defb    7, "/string"
SLASHSTRING     call    docolon
;
        DW ROT,OVER,PLUS,ROT,ROT,MINUS,EXIT

;Z >counted  src n dst --     copy to counted str
;   2DUP C! CHAR+ SWAP CMOVE ;
;    head TOCOUNTED,8,">COUNTED",docolon
        defw    link161
        defb    0
link162 defb    8, ">counted"
TOCOUNTED       call    docolon
;
        DW TWODUP,CSTORE,CHARPLUS,SWOP,CMOVE,EXIT

;C WORD   char -- c-addr n   word delim'd by char
;   DUP  SOURCE >IN @ /STRING   -- c c adr n
;   DUP >R   ROT SKIP           -- c adr' n'
;   OVER >R  ROT SCAN           -- adr" n"
;   DUP IF CHAR- THEN        skip trailing delim.
;   R> R> ROT -   >IN +!        update >IN offset
;   TUCK -                      -- adr' N
;   HERE >counted               --
;   HERE                        -- a
;   BL OVER COUNT + C! ;    append trailing blank
;    head WORD,4,"WORD",docolon
        defw    link162
        defb    0
link163 defb    4, "word"
WORD    call    docolon
;
        DW DUP,SOURCE,TOIN,FETCH,SLASHSTRING
        DW DUP,TOR,ROT,SKIP
        DW OVER,TOR,ROT,SCAN
        DW DUP,QBRANCH,WORD1,ONEMINUS  ; char-
WORD1:  DW RFROM,RFROM,ROT,MINUS,TOIN,PLUSSTORE
        DW TUCK,MINUS
        DW HERE,TOCOUNTED,HERE
        DW BL,OVER,COUNT,PLUS,CSTORE,EXIT

;Z NFA>LFA   nfa -- lfa    name adr -> link field
;   3 - ;
;    head NFATOLFA,7,"NFA>LFA",docolon
        defw    link163
        defb    0
link164 defb    7, "nfa>lfa"
NFATOLFA        call    docolon
;
        DW LIT,3,MINUS,EXIT

;Z NFA>CFA   nfa -- cfa    name adr -> code field
;   COUNT 7F AND + ;       mask off 'smudge' bit
;    head NFATOCFA,7,"NFA>CFA",docolon
        defw    link164
        defb    0
link165 defb    7, "nfa>cfa"
NFATOCFA        call    docolon
;
        DW COUNT,LIT,07FH,AND,PLUS,EXIT

;Z IMMED?    nfa -- f      fetch immediate flag
;   1- C@ ;                     nonzero if immed
;    head IMMEDQ,6,"IMMED?",docolon
        defw    link165
        defb    0
link166 defb    6, "immed?"
IMMEDQ  call    docolon
;
        DW ONEMINUS,CFETCH,EXIT

;C FIND   c-addr -- c-addr 0   if not found
;C                  xt  1      if immediate
;C                  xt -1      if "normal"
;   LATEST @ BEGIN             -- a nfa
;       2DUP OVER C@ CHAR+     -- a nfa a nfa n+1
;       S=                     -- a nfa f
;       DUP IF
;           DROP
;           NFA>LFA @ DUP      -- a link link
;       THEN
;   0= UNTIL                   -- a nfa  OR  a 0
;   DUP IF
;       NIP DUP NFA>CFA        -- nfa xt
;       SWAP IMMED?            -- xt iflag
;       0= 1 OR                -- xt 1/-1
;   THEN ;
;    head FIND,4,"FIND",docolon
        defw    link166
        defb    0
link167 defb    4, "find"
FIND    call    docolon
;
        DW LATEST,FETCH
FIND1:  DW TWODUP,OVER,CFETCH,CHARPLUS
        DW SEQUAL,DUP,QBRANCH,FIND2
        DW DROP,NFATOLFA,FETCH,DUP
FIND2:  DW ZEROEQUAL,QBRANCH,FIND1
        DW DUP,QBRANCH,FIND3
        DW NIP,DUP,NFATOCFA
        DW SWOP,IMMEDQ,ZEROEQUAL,LIT,1,OR
FIND3:  DW EXIT

;C LITERAL  x --        append numeric literal
;   STATE @ IF ['] LIT ,XT , THEN ; IMMEDIATE
; This tests STATE so that it can also be used
; interpretively.  (ANSI doesn't require this.)
;    immed LITERAL,7,"LITERAL",docolon
        defw    link167
        defb    1
link168 defb    7, "literal"
LITERAL call    docolon
;
        DW STATE,FETCH,QBRANCH,LITER1
        DW LIT,LIT,COMMAXT,COMMA
LITER1: DW EXIT

;Z DIGIT?   c -- n -1   if c is a valid digit
;Z            -- x  0   otherwise
;   [ HEX ] DUP 39 > 100 AND +     silly looking
;   DUP 140 > 107 AND -   30 -     but it works!
;   DUP BASE @ U< ;
;    head DIGITQ,6,"DIGIT?",docolon
        defw    link168
        defb    0
link169 defb    6, "digit?"
DIGITQ  call    docolon
;
        DW DUP,LIT,39H,GREATER,LIT,100H,AND,PLUS
        DW DUP,LIT,140H,GREATER,LIT,107H,AND
        DW MINUS,LIT,30H,MINUS
        DW DUP,BASE,FETCH,ULESS,EXIT

;Z ?SIGN   adr n -- adr' n' f  get optional sign
;Z  advance adr/n if sign; return NZ if negative
;   OVER C@                 -- adr n c
;   2C - DUP ABS 1 = AND    -- +=-1, -=+1, else 0
;   DUP IF 1+               -- +=0, -=+2
;       >R 1 /STRING R>     -- adr' n' f
;   THEN ;
;    head QSIGN,5,"?SIGN",docolon
        defw    link169
        defb    0
link170 defb    5, "?sign"
QSIGN   call    docolon
;
        DW OVER,CFETCH,LIT,2CH,MINUS,DUP,ABS
        DW LIT,1,EQUAL,AND,DUP,QBRANCH,QSIGN1
        DW ONEPLUS,TOR,LIT,1,SLASHSTRING,RFROM
QSIGN1: DW EXIT

;C >NUMBER  ud adr u -- ud' adr' u'
;C                      convert string to number
;   BEGIN
;   DUP WHILE
;       OVER C@ DIGIT?
;       0= IF DROP EXIT THEN
;       >R 2SWAP BASE @ UD*
;       R> M+ 2SWAP
;       1 /STRING
;   REPEAT ;
;    head TONUMBER,7,">NUMBER",docolon
        defw    link170
        defb    0
link171 defb    7, ">number"
TONUMBER        call    docolon
;
TONUM1: DW DUP,QBRANCH,TONUM3
        DW OVER,CFETCH,DIGITQ
        DW ZEROEQUAL,QBRANCH,TONUM2,DROP,EXIT
TONUM2: DW TOR,TWOSWAP,BASE,FETCH,UDSTAR
        DW RFROM,MPLUS,TWOSWAP
        DW LIT,1,SLASHSTRING,BRANCH,TONUM1
TONUM3: DW EXIT

;Z ?NUMBER  c-addr -- n -1      string->number
;Z                 -- c-addr 0  if convert error
;   DUP  0 0 ROT COUNT      -- ca ud adr n
;   ?SIGN >R  >NUMBER       -- ca ud adr' n'
;   IF   R> 2DROP 2DROP 0   -- ca 0   (error)
;   ELSE 2DROP NIP R>
;       IF NEGATE THEN  -1  -- n -1   (ok)
;   THEN ;
;    head QNUMBER,7,"?NUMBER",docolon
        defw    link171
        defb    0
link172 defb    7, "?number"
QNUMBER call    docolon
;
        DW DUP,LIT,0,DUP,ROT,COUNT
        DW QSIGN,TOR,TONUMBER,QBRANCH,QNUM1
        DW RFROM,TWODROP,TWODROP,LIT,0
        DW BRANCH,QNUM3
QNUM1:  DW TWODROP,NIP,RFROM,QBRANCH,QNUM2,NEGATE
QNUM2:  DW LIT,-1
QNUM3:  DW EXIT

;Z INTERPRET    i*x c-addr u -- j*x
;Z                      interpret given buffer
; This is a common factor of EVALUATE and QUIT.
; ref. dpANS-6, 3.4 The Forth Text Interpreter
;   'SOURCE 2!  0 >IN !
;   BEGIN
;   BL WORD DUP C@ WHILE        -- textadr
;       FIND                    -- a 0/1/-1
;       ?DUP IF                 -- xt 1/-1
;           1+ STATE @ 0= OR    immed or interp?
;           IF EXECUTE ELSE ,XT THEN
;       ELSE                    -- textadr
;           ?NUMBER
;           IF POSTPONE LITERAL     converted ok
;           ELSE COUNT TYPE 3F EMIT CR ABORT  err
;           THEN
;       THEN
;   REPEAT DROP ;
;    head INTERPRET,9,"INTERPRET",docolon
        defw    link172
        defb    0
link173 defb    9, "interpret"
INTERPRET       call    docolon
;
        DW TICKSOURCE,TWOSTORE,LIT,0,TOIN,STORE
INTER1: DW BL,WORD,DUP,CFETCH,QBRANCH,INTER9
        DW FIND,QDUP,QBRANCH,INTER4
        DW ONEPLUS,STATE,FETCH,ZEROEQUAL,OR
        DW QBRANCH,INTER2
        DW EXECUTE,BRANCH,INTER3
INTER2: DW COMMAXT
INTER3: DW BRANCH,INTER8
INTER4: DW QNUMBER,QBRANCH,INTER5
        DW LITERAL,BRANCH,INTER6
INTER5: DW COUNT,TYPE,LIT,3FH,EMIT,CR,ABORT
INTER6:
INTER8: DW BRANCH,INTER1
INTER9: DW DROP,EXIT

;C EVALUATE  i*x c-addr u -- j*x  interprt string
;   'SOURCE 2@ >R >R  >IN @ >R
;   INTERPRET
;   R> >IN !  R> R> 'SOURCE 2! ;
;    head EVALUATE,8,"EVALUATE",docolon
        defw    link173
        defb    0
link174 defb    8, "evaluate"
EVALUATE        call    docolon
;
        DW TICKSOURCE,TWOFETCH,TOR,TOR
        DW TOIN,FETCH,TOR,INTERPRET
        DW RFROM,TOIN,STORE,RFROM,RFROM
        DW TICKSOURCE,TWOSTORE,EXIT

;C QUIT     --    R: i*x --    interpret from kbd
;   L0 LP !  R0 RP!   0 STATE !
;   BEGIN
;       TIB DUP TIBSIZE ACCEPT 
;       INTERPRET
;       STATE @ 0= IF ." OK" CR THEN
;   AGAIN ;
;    head QUIT,4,"QUIT",docolon
        defw    link174
        defb    0
link175 defb    4, "quit"
QUIT    call    docolon
;
        DW L0,LP,STORE
        DW R0,RPSTORE,LIT,0,STATE,STORE
QUIT1:  DW TIB,DUP,TIBSIZE,ACCEPT
        DW INTERPRET
        DW STATE,FETCH,ZEROEQUAL,QBRANCH,QUIT2
        DW XSQUOTE
        DB 3," OK"
        DW TYPE, CR
QUIT2:  DW BRANCH,QUIT1

;C ABORT    i*x --   R: j*x --   clear stk & QUIT
;   S0 SP!  QUIT ;
;    head ABORT,5,"ABORT",docolon
        defw    link175
        defb    0
link176 defb    5, "abort"
ABORT   call    docolon
;
        DW S0,SPSTORE,QUIT   ; QUIT never returns

;Z ?ABORT   f c-addr u --      abort & print msg
;   ROT IF TYPE ABORT THEN 2DROP ;
;    head QABORT,6,"?ABORT",docolon
        defw    link176
        defb    0
link177 defb    6, "?abort"
QABORT  call    docolon
;
        DW ROT,QBRANCH,QABO1,TYPE,ABORT
QABO1:  DW TWODROP,EXIT

;C ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
;C         i*x x1 --       R: j*x --      x1<>0
;   POSTPONE S" POSTPONE ?ABORT ; IMMEDIATE
;    immed ABORTQUOTE,6,"ABORT\"",docolon
        defw    link177
        defb    1
link178 defb    6, "ABORT", $22
ABORTQUOTE      call    docolon
;
        DW SQUOTE
        DW LIT,QABORT,COMMAXT
        DW EXIT

;C '    -- xt           find word in dictionary
;   BL WORD FIND
;   0= ABORT" ?" ;
;    head TICK,1,'\'',docolon
;        DW link                 ; must expand
;        DB 0                    ; manually
;link    .SET $                  ; because of
;        DB 1,27h                ; tick character
        defw    link178
        defb    0
link179 defb    1, $27
;
TICK:   call docolon
        DW BL,WORD,FIND,ZEROEQUAL,XSQUOTE
        DB 1,'?'
        DW QABORT,EXIT

;C CHAR   -- char           parse ASCII character
;   BL WORD 1+ C@ ;
;    head CHAR,4,"CHAR",docolon
        defw    link179
        defb    0
link180 defb    4, "char"
CHAR    call    docolon
;
        DW BL,WORD,ONEPLUS,CFETCH,EXIT

;C [CHAR]   --          compile character literal
;   CHAR  ['] LIT ,XT  , ; IMMEDIATE
;    immed BRACCHAR,6,"[CHAR]",docolon
        defw    link180
        defb    1
link181 defb    6, "[char]"
BRACHAR call    docolon
;
        DW CHAR
        DW LIT,LIT,COMMAXT
        DW COMMA,EXIT

;C (    --                     skip input until )
;   [ HEX ] 29 WORD DROP ; IMMEDIATE
;    immed PAREN,1,'(',docolon
        defw    link181
        defb    1
link182 defb    1, "("
PAREN   call    docolon
;
        DW LIT,29H,WORD,DROP,EXIT

; COMPILER ======================================

;C CREATE   --      create an empty definition
;   LATEST @ , 0 C,         link & immed field
;   HERE LATEST !           new "latest" link
;   BL WORD C@ 1+ ALLOT         name field
;   docreate ,CF                code field
;    head CREATE,6,"CREATE",docolon
        defw    link182
        defb    0
link183 defb    6, "create"
CREATE  call    docolon
;
        DW LATEST,FETCH,COMMA,LIT,0,CCOMMA
        DW HERE,LATEST,STORE
        DW BL,WORD,CFETCH,ONEPLUS,ALLOT
        DW LIT,docreate,COMMACF,EXIT

;Z (DOES>)  --      run-time action of DOES>
;   R>              adrs of headless DOES> def'n
;   LATEST @ NFA>CFA    code field to fix up
;   !CF ;
;    head XDOES,7,"(DOES>)",docolon
        defw    link183
        defb    0
link184 defb    7, "(does>)"
XDOES   call    docolon
;
        DW RFROM,LATEST,FETCH,NFATOCFA,STORECF
        DW EXIT

;C DOES>    --      change action of latest def'n
;   COMPILE (DOES>)
;   dodoes ,CF ; IMMEDIATE
;    immed DOES,5,"DOES>",docolon
        defw    link184
        defb    1
link185 defb    5, "does>"
DOES    call    docolon
;
        DW LIT,XDOES,COMMAXT
        DW LIT,dodoes,COMMACF,EXIT

;C RECURSE  --      recurse current definition
;   LATEST @ NFA>CFA ,XT ; IMMEDIATE
;    immed RECURSE,7,"RECURSE",docolon
        defw    link185
        defb    1
link186 defb    7, "recurse"
RECURSE call    docolon
;
        DW LATEST,FETCH,NFATOCFA,COMMAXT,EXIT

;C [        --      enter interpretive state
;   0 STATE ! ; IMMEDIATE
;    immed LEFTBRACKET,1,'[',docolon
        defw    link186
        defb    1
link187 defb    1, "["
LEFTBRACKET     call    docolon
;
        DW LIT,0,STATE,STORE,EXIT

;C ]        --      enter compiling state
;   -1 STATE ! ;
;    head RIGHTBRACKET,1,']',docolon
        defw    link187
        defb    0
link188 defb    1, "]"
RIGHTBRACKET    call    docolon
;
        DW LIT,-1,STATE,STORE,EXIT

;Z HIDE     --      "hide" latest definition
;   LATEST @ DUP C@ 80 OR SWAP C! ;
;    head HIDE,4,"HIDE",docolon
        defw    link188
        defb    0
link189 defb    4, "hide"
HIDE    call    docolon
;
        DW LATEST,FETCH,DUP,CFETCH,LIT,80H,OR
        DW SWOP,CSTORE,EXIT

;Z REVEAL   --      "reveal" latest definition
;   LATEST @ DUP C@ 7F AND SWAP C! ;
;    head REVEAL,6,"REVEAL",docolon
        defw    link189
        defb    0
link190 defb    6, "reveal"
REVEAL  call    docolon
;
        DW LATEST,FETCH,DUP,CFETCH,LIT,7FH,AND
        DW SWOP,CSTORE,EXIT

;C IMMEDIATE   --   make last def'n immediate
;   1 LATEST @ 1- C! ;   set immediate flag
;    head IMMEDIATE,9,"IMMEDIATE",docolon
        defw    link190
        defb    0
link191 defb    9, "immediate"
IMMEDIATE       call    docolon
;
        DW LIT,1,LATEST,FETCH,ONEMINUS,CSTORE
        DW EXIT

;C :        --      begin a colon definition
;   CREATE HIDE ] !COLON ;
;    head COLON,1,':',docode
        defw    link191
        defb    0
link192 defb    1, ':'
COLON   
;
        CALL docolon    ; code fwd ref explicitly
        DW CREATE,HIDE,RIGHTBRACKET,STORCOLON
        DW EXIT

;C ;
;   REVEAL  ,EXIT
;   POSTPONE [  ; IMMEDIATE
;    immed SEMICOLON,1,';',docolon
        defw    link192
        defb    1
link193 defb    1, ';'
SEMICOLON       call    docolon
;
        DW REVEAL,CEXIT
        DW LEFTBRACKET,EXIT

;C [']  --         find word & compile as literal
;   '  ['] LIT ,XT  , ; IMMEDIATE
; When encountered in a colon definition, the
; phrase  ['] xxx  will cause   LIT,xxt  to be
; compiled into the colon definition (where
; (where xxt is the execution token of word xxx).
; When the colon definition executes, xxt will
; be put on the stack.  (All xt's are one cell.)
;    immed BRACTICK,3,['],docolon
;        DW link                 ; must expand
;        DB 1                    ; manually
;link    .SET $                  ; because of
;        DB 3,5Bh,27h,5Dh        ; tick character
        defw    link193
        defb    1
link194 defb    3, "[", $27, "]"
;
BRACTICK: call docolon
        DW TICK               ; get xt of 'xxx'
        DW LIT,LIT,COMMAXT    ; append LIT action
        DW COMMA,EXIT         ; append xt literal

;C POSTPONE  --   postpone compile action of word
;   BL WORD FIND
;   DUP 0= ABORT" ?"
;   0< IF   -- xt  non immed: add code to current
;                  def'n to compile xt later.
;       ['] LIT ,XT  ,      add "LIT,xt,COMMAXT"
;       ['] ,XT ,XT         to current definition
;   ELSE  ,XT      immed: compile into cur. def'n
;   THEN ; IMMEDIATE
;    immed POSTPONE,8,"POSTPONE",docolon
        defw    link194
        defb    1
link195 defb    8, "postpone"
POSTPONE        call    docolon
;
        DW BL,WORD,FIND,DUP,ZEROEQUAL,XSQUOTE
        DB 1,'?'
        DW QABORT,ZEROLESS,QBRANCH,POST1
        DW LIT,LIT,COMMAXT,COMMA
        DW LIT,COMMAXT,COMMAXT,BRANCH,POST2
POST1:  DW COMMAXT
POST2:  DW EXIT

;Z COMPILE   --   append inline execution token
;   R> DUP CELL+ >R @ ,XT ;
; The phrase ['] xxx ,XT appears so often that
; this word was created to combine the actions
; of LIT and ,XT.  It takes an inline literal
; execution token and appends it to the dict.
;    head COMPILE,7,"COMPILE",docolon
;        DW RFROM,DUP,CELLPLUS,TOR
;        DW FETCH,COMMAXT,EXIT
; N.B.: not used in the current implementation

; CONTROL STRUCTURES ============================

;C IF       -- adrs    conditional forward branch
;   ['] qbranch ,BRANCH  HERE DUP ,DEST ;
;   IMMEDIATE
;    immed IF,2,"IF",docolon
        defw    link195
        defb    1
link196 defb    2, "if"
IF      call    docolon
;
        DW LIT,QBRANCH,COMMABRANCH
        DW HERE,DUP,COMMADEST,EXIT

;C THEN     adrs --        resolve forward branch
;   HERE SWAP !DEST ; IMMEDIATE
;    immed THEN,4,"THEN",docolon
        defw    link196
        defb    1
link197 defb    4, "then"
THEN    call    docolon
;
        DW HERE,SWOP,STOREDEST,EXIT

;C ELSE     adrs1 -- adrs2    branch for IF..ELSE
;   ['] branch ,BRANCH  HERE DUP ,DEST
;   SWAP  POSTPONE THEN ; IMMEDIATE
;    immed ELSE,4,"ELSE",docolon
        defw    link197
        defb    1
link198 defb    4, "else"
ELSE    call    docolon
;
        DW LIT,BRANCH,COMMABRANCH
        DW HERE,DUP,COMMADEST
        DW SWOP,THEN,EXIT

;C BEGIN    -- adrs        target for bwd. branch
;   HERE ; IMMEDIATE
;    immed BEGIN,5,"BEGIN",docode
        defw    link198
        defb    1
link199 defb    5, "begin"
BEGIN
;
        jp HERE

;C UNTIL    adrs --   conditional backward branch
;   ['] qbranch ,BRANCH  ,DEST ; IMMEDIATE
;   conditional backward branch
;    immed UNTIL,5,"UNTIL",docolon
        defw    link199
        defb    1
link200 defb    5, "until"
UNTIL   call    docolon
;
        DW LIT,QBRANCH,COMMABRANCH
        DW COMMADEST,EXIT

;X AGAIN    adrs --      uncond'l backward branch
;   ['] branch ,BRANCH  ,DEST ; IMMEDIATE
;   unconditional backward branch
;    immed AGAIN,5,"AGAIN",docolon
        defw    link200
        defb    1
link201 defb    5, "again"
AGAIN   call    docolon
;
        DW LIT,BRANCH,COMMABRANCH
        DW COMMADEST,EXIT

;C WHILE    -- adrs         branch for WHILE loop
;   POSTPONE IF ; IMMEDIATE
;    immed WHILE,5,"WHILE",docode
        defw    link201
        defb    1
link202 defb    5, "while"
WHILE  
;
        jp IF

;C REPEAT   adrs1 adrs2 --     resolve WHILE loop
;   SWAP POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
;    immed REPEAT,6,"REPEAT",docolon
        defw    link202
        defb    1
link203 defb    6, "repeat"
REPEAT  call    docolon
;
        DW SWOP,AGAIN,THEN,EXIT

;Z >L   x --   L: -- x        move to leave stack
;   CELL LP +!  LP @ ! ;      (L stack grows up)
;    head TOL,2,">L",docolon
        defw    link203
        defb    0
link204 defb    2, ">l"
TOL     call    docolon
;
        DW CELL,LP,PLUSSTORE,LP,FETCH,STORE,EXIT

;Z L>   -- x   L: x --      move from leave stack
;   LP @ @  CELL NEGATE LP +! ;
;    head LFROM,2,"L>",docolon
        defw    link204
        defb    0
link205 defb    2, "l>"
LFROM   call    docolon
;
        DW LP,FETCH,FETCH
        DW CELL,NEGATE,LP,PLUSSTORE,EXIT

;C DO       -- adrs   L: -- 0
;   ['] xdo ,XT   HERE     target for bwd branch
;   0 >L ; IMMEDIATE           marker for LEAVEs
;    immed DO,2,"DO",docolon
        defw    link205
        defb    1
link206 defb    2, "do"
DO      call    docolon
;
        DW LIT,XDO,COMMAXT,HERE
        DW LIT,0,TOL,EXIT

;Z ENDLOOP   adrs xt --   L: 0 a1 a2 .. aN --
;   ,BRANCH  ,DEST                backward loop
;   BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT ;
;                                 resolve LEAVEs
; This is a common factor of LOOP and +LOOP.
;    head ENDLOOP,7,"ENDLOOP",docolon
        defw    link206
        defb    0
link207 defb    7, "endloop"
ENDLOOP call    docolon
;
        DW COMMABRANCH,COMMADEST
LOOP1:  DW LFROM,QDUP,QBRANCH,LOOP2
        DW THEN,BRANCH,LOOP1
LOOP2:  DW EXIT

;C LOOP    adrs --   L: 0 a1 a2 .. aN --
;   ['] xloop ENDLOOP ;  IMMEDIATE
;    immed LOOP,4,"LOOP",docolon
        defw    link207
        defb    1
link208 defb    4, "loop"
LOOP    call    docolon
;
        DW LIT,XLOOP,ENDLOOP,EXIT

;C +LOOP   adrs --   L: 0 a1 a2 .. aN --
;   ['] xplusloop ENDLOOP ;  IMMEDIATE
;    immed PLUSLOOP,5,"+LOOP",docolon
        defw    link208
        defb    1
link209 defb    5, "+loop"
PLUSLOOP        call    docolon
;
        DW LIT,XPLUSLOOP,ENDLOOP,EXIT

;C LEAVE    --    L: -- adrs
;   ['] UNLOOP ,XT
;   ['] branch ,BRANCH   HERE DUP ,DEST  >L
;   ; IMMEDIATE      unconditional forward branch
;    immed LEAVE,5,"LEAVE",docolon
        defw    link209
        defb    1
link210 defb    5, "leave"
LEAVE   call    docolon
;
        DW LIT,UNLOOP,COMMAXT
        DW LIT,BRANCH,COMMABRANCH
        DW HERE,DUP,COMMADEST,TOL,EXIT

; OTHER OPERATIONS ==============================

;X WITHIN   n1|u1 n2|u2 n3|u3 -- f   n2<=n1<n3?
;  OVER - >R - R> U< ;          per ANS document
;    head WITHIN,6,"WITHIN",docolon
        defw    link210
        defb    0
link211 defb    6, "within"
WITHIN  call    docolon
;
        DW OVER,MINUS,TOR,MINUS,RFROM,ULESS,EXIT

;C MOVE    addr1 addr2 u --     smart move
;             VERSION FOR 1 ADDRESS UNIT = 1 CHAR
;  >R 2DUP SWAP DUP R@ +     -- ... dst src src+n
;  WITHIN IF  R> CMOVE>        src <= dst < src+n
;       ELSE  R> CMOVE  THEN ;          otherwise
;    head MOVE,4,"MOVE",docolon
        defw    link211
        defb    0
link212 defb    4, "move"
MOVE    call    docolon
;
        DW TOR,TWODUP,SWOP,DUP,RFETCH,PLUS
        DW WITHIN,QBRANCH,MOVE1
        DW RFROM,CMOVEUP,BRANCH,MOVE2
MOVE1:  DW RFROM,CMOVE
MOVE2:  DW EXIT

;C DEPTH    -- +n        number of items on stack
;   SP@ S0 SWAP - 2/ ;   16-BIT VERSION!
;    head DEPTH,5,"DEPTH",docolon
        defw    link212
        defb    0
link213 defb    5, "depth"
DEPTH   call    docolon
;
        DW SPFETCH,S0,SWOP,MINUS,TWOSLASH,EXIT

;C ENVIRONMENT?  c-addr u -- false   system query
;                         -- i*x true
;   2DROP 0 ;       the minimal definition!
;    head ENVIRONMENTQ,12,"ENVIRONMENT?",docolon
        defw    link213
        defb    0
link214 defb    12, "environment?"
ENVIRONMENTQ    call    docolon
;
        DW TWODROP,LIT,0,EXIT

; UTILITY WORDS AND STARTUP =====================

;X WORDS    --          list all words in dict.
;   LATEST @ BEGIN
;       DUP COUNT TYPE SPACE
;       NFA>LFA @
;   DUP 0= UNTIL
;   DROP ;
;    head WORDS,5,"WORDS",docolon
        defw    link214
        defb    0
link215 defb    5, "words"
WORDS   call    docolon
;
        DW LATEST,FETCH
WDS1:   DW DUP,COUNT,TYPE,SPACE,NFATOLFA,FETCH
        DW DUP,ZEROEQUAL,QBRANCH,WDS1
        DW DROP,EXIT

;X .S      --           print stack contents
;   SP@ S0 - IF
;       SP@ S0 2 - DO I @ U. -2 +LOOP
;   THEN ;
;    head DOTS,2,".S",docolon
        defw    link215
        defb    0
link216 defb    2, ".s"
DOTS    call    docolon
;
        DW SPFETCH,S0,MINUS,QBRANCH,DOTS2
        DW SPFETCH,S0,LIT,2,MINUS,XDO
DOTS1:  DW II,FETCH,UDOT,LIT,-2,XPLUSLOOP,DOTS1
DOTS2:  DW EXIT

;Z COLD     --      cold start Forth system
;   UINIT U0 #INIT CMOVE      init user area
;   80 COUNT INTERPRET       interpret CP/M cmd
;   ." Z80 CamelForth etc."
;   ABORT ;
;    head COLD,4,"COLD",docolon
        defw    link216
        defb    0
link217 defb    4, "cold"
COLD    call    docolon
;
        DW UINIT,U0,NINIT,CMOVE
;        DW LIT,80h,COUNT,INTERPRET                             CHANGED
        DW XSQUOTE
        DB 112,0dh,0ah
        DB "Z80 CamelForth v1.01  25 Jan 1995", 0dh, 0ah
        DB "  06-JUN-2012: Initial N8VEM port", 0dh, 0ah
        DB "  12-MAY-2013: Lower case conversion", 0dh, 0ah
        DB 0dh,0ah
        DW TYPE,ABORT       ; ABORT never returns
