;
;  Definitions used by the monitor and all programs based on this monitor.
;
; 01-JUN-2013   B. Ulmann   1st implementation
; 26-AUG-2014   B. Ulmann   Added basic video routine call numbers
;
rom_start       equ     $0
rom_end         equ     $7fff
ram_start       equ     $8000
ram_end         equ     $ffff
buffer          equ     ram_end - $1ff  ; 512 byte IDE general purpose buffer
last_user_ram   equ     $f9ff
;
; Define the FAT control block memory addresses:
;
datastart       equ     buffer - 4      ; Data area start vector
rootstart       equ     datastart - 4   ; Root directory start vector
fat1start       equ     rootstart - 4   ; Start vector to first FAT
psiz            equ     fat1start - 4   ; Size of partition (in sectors)
pstart          equ     psiz - 4        ; First sector of partition
rootlen         equ     pstart - 2      ; Maximum number of entries in directory
fatsec          equ     rootlen - 2     ; FAT size in sectors
ressec          equ     fatsec - 2      ; Number of reserved sectors
clusiz          equ     ressec - 1      ; Size of a cluster (in sectors)
fatname         equ     clusiz - 9      ; Name of the FAT (null terminated)
fatcb           equ     fatname         ; Start of the FATCB
;
; Define a file control block (FCB) memory addresses and displacements:
;
file_buffer     equ     fatcb - $200            ; 512 byte sector buffer
cluster_sector  equ     file_buffer - 1         ; Current sector in cluster
current_sector  equ     cluster_sector - 4      ; Current sector address
current_cluster equ     current_sector - 2      ; Current cluster number
file_pointer    equ     current_cluster - 4     ; Pointer for file position
file_type       equ     file_pointer - 1        ; 0 -> not found, else OK
first_cluster   equ     file_type - 2           ; First cluster of file
file_size       equ     first_cluster - 4       ; Size of file
file_name       equ     file_size - 12          ; Canonical name of file
fcb             equ     file_name               ; Start of the FCB
;
fcb_filename            equ     0
fcb_file_size           equ     $c
fcb_first_cluster       equ     $10
fcb_file_type           equ     $12
fcb_file_pointer        equ     $13
fcb_current_cluster     equ     $17
fcb_current_sector      equ     $19
fcb_cluster_sector      equ     $1d
fcb_file_buffer         equ     $1e
;
; We also need some general purpose string buffers:
;
string_81_bfr   equ     fcb - 81
string_12_bfr   equ     string_81_bfr - 12
;
;  A number of routines need a bit of scratch RAM, too. Since these are 
; sometimes interdependent, each routine gets its own memory cells (only
; possible since the routines are not recursively called).
;
load_file_scrat equ     string_12_bfr - 2       ; Two bytes for load_file
str2filename_de equ     load_file_scrat - 2     ; Two bytes for str2filename
fopen_eob       equ     str2filename_de - 2     ; Eight bytes for fopen
fopen_rsc       equ     fopen_eob - 4
fopen_scr       equ     fopen_rsc - 2
dirlist_scratch equ     fopen_scr - 2           ; Eight bytes for fopen
dirlist_eob     equ     dirlist_scratch - 2
dirlist_rootsec equ     dirlist_eob - 4
scratch_area    equ     dirlist_rootsec - $1    ; Scratch memory (16 byte)
;
start_type      equ     scratch_area  - $10    ; Distinguish cold/warm start
;
;  System calls are implemented by rst08 which expects the number of the
; call to be executed in ix. The numbers of valid calls are defined here:
;
_cold_start     equ     $0
_is_hex         equ     $1
_is_print       equ     $2
_to_upper       equ     $3
_crlf           equ     $4
_getc           equ     $5
_putc           equ     $6
_puts           equ     $7
_strcmp         equ     $8
_gets           equ     $9
_fgetc          equ     $a
_dump_fcb       equ     $b
_fopen          equ     $c
_dirlist        equ     $d
_fatmount       equ     $e
_fatunmount     equ     $f
_strchr         equ     $10
_uart_status    equ     $11
_getc_nowait    equ     $12
_print_word     equ     $13
_print_byte     equ     $14
_stroup         equ     $15
_get_word       equ     $16
_vputc          equ     $17
_vputs          equ     $18
;
; Some useful ASCII (control) characters:
;
eos             equ     $00             ; End of string
cr              equ     $0d             ; Carriage return
lf              equ     $0a             ; Line feed
space           equ     $20             ; Space
tab             equ     $09             ; Tabulator
bs              equ     $08             ; Backspace
bel             equ     $07             ; Bell
ctrl_y          equ     25              ; CTRL-Y character
xon             equ     $11
xoff            equ     $13
del             equ     127
