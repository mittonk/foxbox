INCLUDE "hardware.inc"

SECTION "Header", ROM0[$100]

    jp EntryPoint

    ds $150 - @, 0 ; Make room for the header
EntryPoint:
    ; Do not turn the LCD off outside of VBlank
WaitVBlank:
    ld a, [rLY]
    cp 144
    jp c, WaitVBlank

    ; Turn the LCD off
    ld a, 0
    ld [rLCDC], a

    ; Copy the tile data
    ld de, Tiles
    ld hl, $9000
    ld bc, TilesEnd - Tiles
    call Memcopy

    ; Copy the tilemap
    ld de, Tilemap
    ld hl, $9800
    ld bc, TilemapEnd - Tilemap
    call Memcopy

    ; Copy the player and crate tiles
    ld de, Objects
    ld hl, $8000
    ld bc, ObjectsEnd - Objects
    call Memcopy

    ; Clear object storage
    ld a, 0
    ld b, 160
    ld hl, _OAMRAM
ClearOam:
    ld [hli], a
    dec b
    jp nz, ClearOam

    ; Init player object
    ld hl, _OAMRAM
    ld a, 96 + 16
    ld [hli], a
    ld a, 48 + 8
    ld [hli], a
    ld a, 0 ; Player
    ld [hli], a
    ld [hli], a

    ; Init crate object 1
    ld hl, _OAMRAM + 4
    ld a, 80 + 16
    ld [hli], a
    ld a, 56 + 8
    ld [hli], a
    ld a, 1  ; Crate
    ld [hli], a
    ld a, 0
    ld [hli], a

    ; Init crate object 2
    ld hl, _OAMRAM + 8 ; TODO (mittonk): Naming?
    ld a, 88 + 16
    ld [hli], a
    ld a, 56 + 8
    ld [hli], a
    ld a, 1  ; Crate
    ld [hli], a
    ld a, 0
    ld [hli], a

    ; Init crate object 3
    ld hl, _OAMRAM + 12
    ld a, 88 + 16
    ld [hli], a
    ld a, 64 + 8
    ld [hli], a
    ld a, 1  ; Crate
    ld [hli], a
    ld a, 0
    ld [hli], a

    ; Turn the LCD on
    ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON
    ld [rLCDC], a

    ; During the first (blank) frame, initialize display registers
    ld a, %11100100
    ld [rBGP], a
    ld a, %11100100
    ld [rOBP0], a
    ld a, %11100100
    ld [rOBP1], a

    ; Initialize global variables
    ld a, 0
    ld [wFrameCounter], a
    ld [wCurKeys], a
    ld [wNewKeys], a


Main:
    ; Wait until it's *not* VBlank
    ld a, [rLY]
    cp 144
    jp nc, Main
WaitVBlank2:
    ld a, [rLY]
    cp 144
    jp c, WaitVBlank2

    ; Game clock
    ld a, [wFrameCounter]
    inc a
    ld [wFrameCounter], a
    cp a, 15 ; Every 15 frames (a quarter of a second), run the following code
    jp nz, Main

    ; Reset the frame counter back to 0
    ld a, 0
    ld [wFrameCounter], a


    ; Check the current keys every frame and move left or right.
    call UpdateKeys

    ; First, check if the left button is pressed.
CheckLeft:
    ld a, [wCurKeys]
    and a, PADF_LEFT
    jp z, CheckRight
Left:
    ; Move the paddle one square to the left.
    ld a, [_OAMRAM + 1]
    sub a, 8
    ; If we've already hit the edge of the playfield, don't move.
    cp a, 8
    jp z, Main
    ld [_OAMRAM + 1], a
    jp Main

; Then check the right button.
CheckRight:
    ld a, [wCurKeys]
    and a, PADF_RIGHT
    jp z, CheckUp
Right:
    ; Move the paddle one square to the right.
    ld a, [_OAMRAM + 1]
    add a, 8
    ; If we've already hit the edge of the playfield, don't move.
    cp a, 160
    jp z, Main
    ld [_OAMRAM + 1], a
    jp Main


; Then check the up button.
CheckUp:
    ld a, [wCurKeys]
    and a, PADF_UP
    jp z, CheckDown
Up:
    ; Move the paddle one square up.
    ld a, [_OAMRAM]
    sub a, 8
    ; If we've already hit the edge of the playfield, don't move.
    cp a, 16
    jp z, Main
    ld [_OAMRAM], a
    jp Main


; Then check the down button.
CheckDown:
    ld a, [wCurKeys]
    and a, PADF_DOWN
    jp z, Main
Down:
    ; Move the paddle one square down.
    ld a, [_OAMRAM]
    add a, 8
    ; If we've already hit the edge of the playfield, don't move.
    cp a, 152
    jp z, Main
    ld [_OAMRAM], a
    jp Main


  ; https://gbdev.io/gb-asm-tutorial/part2/input.html
UpdateKeys:
  ; Poll half the controller
  ld a, P1F_GET_BTN
  call .onenibble
  ld b, a ; B7-4 = 1; B3-0 = unpressed buttons

  ; Poll the other half
  ld a, P1F_GET_DPAD
  call .onenibble
  swap a ; A3-0 = unpressed directions; A7-4 = 1
  xor a, b ; A = pressed buttons + directions
  ld b, a ; B = pressed buttons + directions

  ; And release the controller
  ld a, P1F_GET_NONE
  ldh [rP1], a

  ; Combine with previous wCurKeys to make wNewKeys
  ld a, [wCurKeys]
  xor a, b ; A = keys that changed state
  and a, b ; A = keys that changed to pressed
  ld [wNewKeys], a
  ld a, b
  ld [wCurKeys], a
  ret

.onenibble
  ldh [rP1], a ; switch the key matrix
  call .knownret ; burn 10 cycles calling a known ret
  ldh a, [rP1] ; ignore value while waiting for the key matrix to settle
  ldh a, [rP1]
  ldh a, [rP1] ; this read counts
  or a, $F0 ; A7-4 = 1; A3-0 = unpressed keys
.knownret
  ret

; Copy bytes from one area to another.
; @param de: Source
; @param hl: Destination
; @param bc: Length
Memcopy:
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, Memcopy
    ret

Tiles:
	; 00 Outside
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333

	; 01 Floor
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000

	; 02 Wall
	dw `12111111
	dw `11111121
	dw `11121111
	dw `11111111
	dw `11211111
	dw `11111211
	dw `21111111
	dw `11111111

	; 03 Target
	dw `00000000
	dw `00000000
	dw `00022000
	dw `00222200
	dw `00222200
	dw `00022000
	dw `00000000
	dw `00000000

	; 04 Border, left
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211

	; 05 Border, top
	dw `33333333
	dw `33333333
	dw `33333333
	dw `22222222
	dw `22222222
	dw `22222222
	dw `11111111
	dw `11111111

	; 06 Border, bottom
	dw `11111111
	dw `11111111
	dw `22222222
	dw `22222222
	dw `22222222
	dw `33333333
	dw `33333333
	dw `33333333

	; 07 Border, right
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333

	; 08 Border, top-left
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333222
	dw `33332222
	dw `33322222
	dw `33322221
	dw `33322211

	; 09 Border, top-right
	dw `33333333
	dw `33333333
	dw `33333333
	dw `22233333
	dw `22223333
	dw `22222333
	dw `12222333
	dw `11222333

	; 0a Border, bottom-left
	dw `33322211
	dw `33322221
	dw `33322222
	dw `33332222
	dw `33333222
	dw `33333333
	dw `33333333
	dw `33333333

	; 0b Border, bottom-right
	dw `11222333
	dw `12222333
	dw `22222333
	dw `22223333
	dw `22233333
	dw `33333333
	dw `33333333
	dw `33333333

TilesEnd:

Tilemap:
	db $08, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $05, $09, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0

	db $04, $00, $00, $00, $00, $00, $02, $02, $02, $02, $02, $00, $00, $00, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $00, $02, $01, $01, $01, $02, $02, $02, $02, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $00, $02, $01, $01, $01, $02, $01, $01, $02, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $00, $02, $02, $01, $01, $01, $01, $03, $02, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $02, $02, $02, $01, $02, $02, $02, $03, $02, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0

	db $04, $00, $00, $00, $00, $02, $01, $01, $01, $02, $00, $02, $03, $02, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $02, $01, $01, $01, $02, $00, $02, $02, $02, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $02, $01, $01, $01, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $02, $02, $02, $02, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0

	db $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07, 0,0,0,0,0,0,0,0,0,0,0,0
	db $0a, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $0b, 0,0,0,0,0,0,0,0,0,0,0,0

TilemapEnd:

Objects:
    ; 8000 Player
    dw `02000020
    dw `02122120
    dw `02311320
    dw `00211200
    dw `00122100
    dw `00211200
    dw `02111120
    dw `20202022

    ; 8008 Crate
    dw `33333333
    dw `33222233
    dw `32322323
    dw `32233223
    dw `32233223
    dw `32322323
    dw `33222233
    dw `33333333

ObjectsEnd:

SECTION "Counter", WRAM0
wFrameCounter: db

SECTION "Input Variables", WRAM0
wCurKeys: db
wNewKeys: db

