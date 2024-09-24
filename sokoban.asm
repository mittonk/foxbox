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
    ld [wDestY], a
    ld [wDestX], a
    ld [wFurtherY], a
    ld [wFurtherX], a
    ld [wPushingCrate], a


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


    ; Check for input.
    call UpdateKeys

    ; First, check if the left button is pressed.
CheckLeft:
    ld a, [wCurKeys]
    and a, PADF_LEFT
    jp z, CheckRight
Left:
    ; Move the player one square to the left.
    ld a, [_OAMRAM + 1]
    sub a, 8
    ; If we've already hit the edge of the playfield, don't move.
    cp a, 8
    jp z, Main

    ; If we hit a wall, don't move.
    ; Remember to offset the OAM position!
    ; (8, 16) in OAM coordinates is (0, 0) on the screen.
    ld d, a ; Save dest position
    ld a, [_OAMRAM]
    sub a, 16
    ld c, a
    ld a, [_OAMRAM + 1]
    sub a, 8 + 8
    ld b, a
    call GetTileByPixel ; Returns tile address in hl
    ld a, [hl]
    call IsWallTile
    jp z, Main

    ; All clear, move.
    ld a, d ; Recover dest
    ld [_OAMRAM + 1], a
    jp Main

; Then check the right button.
CheckRight:
    ld a, [wCurKeys]
    and a, PADF_RIGHT
    jp z, CheckUp
Right:
    ; Move the player one square to the right.
    ld a, [_OAMRAM + 1]
    add a, 8
    ; If we've already hit the edge of the playfield, don't move.
    cp a, 160
    jp z, Main


    ; If we hit a wall, don't move.
    ; Remember to offset the OAM position!
    ; (8, 16) in OAM coordinates is (0, 0) on the screen.
    ld d, a ; Save dest position
    ld a, [_OAMRAM]
    sub a, 16
    ld c, a
    ld a, [_OAMRAM + 1]
    sub a, 8 - 8
    ld b, a
    call GetTileByPixel ; Returns tile address in hl
    ld a, [hl]
    call IsWallTile
    jp z, Main

    ; All clear, move.
    ; TODO (mittonk): Crate collision.
    ld a, d ; Recover dest
    ld [_OAMRAM + 1], a
    jp Main


; Then check the up button.
CheckUp:
    ld a, [wCurKeys]
    and a, PADF_UP
    jp z, CheckDown
Up:
    ; Move the player one square up.

    ; Scope out our Dest and Further locations
    ; X locations all same
    ld a, [_OAMRAM+1]
    ld [wDestX], a
    ld [wFurtherX], a
    ; Y in a row
    ld a, [_OAMRAM]
    sub a, 8
    ld [wDestY], a
    sub a, 8
    ld [wFurtherY], a
    add a, 8  ; Leave Dest Y in a

    ; If we've already hit the edge of the playfield, don't move.
    cp a, 16
    jp z, Main

    ; If dest is a wall, don't move.
    ld a, [wDestY]
    ld c, a
    ld a, [wDestX]
    ld b, a
    call GetTileByOam ; Returns tile address in hl
    ld a, [hl]
    call IsWallTile
    jp z, Main

IsCrateUp:
    ; Is there a crate?
    ; b: dest x oam
    ; c: dest y oam
    ; hl: coordinate to modify
    ld a, [wDestY]
    ld c, a
    ld a, [wDestX]
    ld b, a
    ld e, 0  ; Crate number
    call IsCrate0
    jp z, CanCrateMoveUp
    ld e, 1  ; Crate number
    call IsCrate1
    jp z, CanCrateMoveUp
    ld e, 2  ; Crate number
    call IsCrate2
    jp z, CanCrateMoveUp
  
    ; No crate there.
    jp MoveUp

CanCrateMoveUp:
    ; Remember which crate we're pushing.
    ld a, e
    ld [wPushingCrate], a
    ; Can the crate move?
    ; Check that Further isn't the edge, isn't a wall, and doesn't have a crate.
    ; 
    ; If we've already hit the edge of the playfield, don't move.
    ld a, [wFurtherY]
    cp a, 16
    jp z, Main

    ; If Further is a wall, bail.
    ld a, [wFurtherY]
    ld c, a
    ld a, [wFurtherX]
    ld b, a
    call GetTileByOam ; Returns tile address in hl
    ld a, [hl]
    call IsWallTile
    jp z, Main

    ; Is there a crate?
    ; b: dest x oam
    ; c: dest y oam
    ld a, [wFurtherY]
    ld c, a
    ld a, [wFurtherX]
    ld b, a
    ld e, 0  ; Crate number
    call IsCrate0
    jp z, Main
    ld e, 1  ; Crate number
    call IsCrate1
    jp z, Main
    ld e, 2  ; Crate number
    call IsCrate2
    jp z, Main
    ; OK, so the crate can be pushed.  Do it.

MoveCrateUp:
    ; Yes: Move crate first.
    call PushingCrateY ; Active addr in hl
    ld a, [wFurtherY]
    ld [hl], a  ; Actually move crate

MoveUp:
    ; All clear, move.
    ld a, [wDestY]
    ld [_OAMRAM], a
    jp Main


; Then check the down button.
CheckDown:
    ld a, [wCurKeys]
    and a, PADF_DOWN
    jp z, Main
Down:
    ; Move the player one square down.
    ; Scope out our Dest and Further locations
    ; X locations all same
    ld a, [_OAMRAM+1]
    ld [wDestX], a
    ld [wFurtherX], a
    ; Y in a row
    ld a, [_OAMRAM]
    add a, 8
    ld [wDestY], a
    add a, 8
    ld [wFurtherY], a
    sub a, 8  ; Leave Dest Y in a

    ; If we've already hit the edge of the playfield, don't move.
    cp a, 152
    jp z, Main

    ; If dest is a wall, don't move.
    ld a, [wDestY]
    ld c, a
    ld a, [wDestX]
    ld b, a
    call GetTileByOam ; Returns tile address in hl
    ld a, [hl]
    call IsWallTile
    jp z, Main

IsCrateDown:
    ; Is there a crate?
    ; b: dest x oam
    ; c: dest y oam
    ; hl: coordinate to modify
    ld a, [wDestY]
    ld c, a
    ld a, [wDestX]
    ld b, a
    ld e, 0  ; Crate number
    call IsCrate0
    jp z, CanCrateMoveDown
    ld e, 1  ; Crate number
    call IsCrate1
    jp z, CanCrateMoveDown
    ld e, 2  ; Crate number
    call IsCrate2
    jp z, CanCrateMoveDown
  
    ; No crate there.
    jp MoveDown

CanCrateMoveDown:
    ; Remember which crate we're pushing.
    ld a, e
    ld [wPushingCrate], a
    ; Can the crate move?
    ; Check that Further isn't the edge, isn't a wall, and doesn't have a crate.
    ; 
    ; If we've already hit the edge of the playfield, don't move.
    ld a, [wFurtherY]
    cp a, 152
    jp z, Main

    ; If Further is a wall, bail.
    ld a, [wFurtherY]
    ld c, a
    ld a, [wFurtherX]
    ld b, a
    call GetTileByOam ; Returns tile address in hl
    ld a, [hl]
    call IsWallTile
    jp z, Main

    ; Is there a crate?
    ; b: dest x oam
    ; c: dest y oam
    ld a, [wFurtherY]
    ld c, a
    ld a, [wFurtherX]
    ld b, a
    ld e, 0  ; Crate number
    call IsCrate0
    jp z, Main
    ld e, 1  ; Crate number
    call IsCrate1
    jp z, Main
    ld e, 2  ; Crate number
    call IsCrate2
    jp z, Main
    ; OK, so the crate can be pushed.  Do it.

MoveCrateDown:
    ; Yes: Move crate first.
    call PushingCrateY ; Active addr in hl
    ld a, [wFurtherY]
    ld [hl], a  ; Actually move crate

MoveDown:
    ; All clear, move.
    ld a, [wDestY]
    ld [_OAMRAM], a
    jp Main




IsCrate0:
    ; Is there a crate?
    ; b: dest x oam
    ; c: dest y oam
    ld a, [_OAMRAM+4]
    cp a, c
    ret nz  ; Y doesn't match, bail.
    ld a, [_OAMRAM+4+1]
    cp a, b
    ret  ; Z=true means Y, X both match.

IsCrate1:
    ; Is there a crate?
    ; b: dest x oam
    ; c: dest y oam
    ld a, [_OAMRAM+8]
    cp a, c
    ret nz  ; Y doesn't match, bail.
    ld a, [_OAMRAM+8+1]
    cp a, b
    ret  ; Z=true means Y, X both match.

IsCrate2:
    ; Is there a crate?
    ; b: dest x oam
    ; c: dest y oam
    ld a, [_OAMRAM+12]
    cp a, c
    ret nz  ; Y doesn't match, bail.
    ld a, [_OAMRAM+12+1]
    cp a, b
    ret  ; Z=true means Y, X both match.

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

; Get the active Y axis for the crate we're pushing.
; @return hl: Y axis storage for active crate
PushingCrateY:
    ld a, [wPushingCrate]
    cp 0
    jp z, PushingCrateY0
    cp 1
    jp z, PushingCrateY1
    cp 2
    jp z, PushingCrateY2
    jp Main ; Shouldn't happen
PushingCrateY0:
    ld hl, _OAMRAM+4
    ret
PushingCrateY1:
    ld hl, _OAMRAM+8
    ret
PushingCrateY2:
    ld hl, _OAMRAM+12
    ret


; Convert a OAM position to a tilemap address
; hl = $9800 + pixelX + pixelY * 32
; @param b: X, oam
; @param c: Y, oam
; @return hl: tile address
GetTileByOam:
    ; (8, 16) in OAM coordinates is (0, 0) on the screen.
    ld a, c
    sub a, 16
    ld c, a
    ld a, b
    sub a, 8
    ld b, a
    call GetTileByPixel ; Returns tile address in hl
    ret

; Convert a pixel position to a tilemap address
; hl = $9800 + X + Y * 32
; @param b: X, pixel
; @param c: Y, pixel
; @return hl: tile address
GetTileByPixel:
    ; First, we need to divide by 8 to convert a pixel position to a tile
    ; position.
    ; After this we want to multiply the Y position by 32.
    ; These operations effectively cancel out so we only need to mask the Y
    ; value.
    ld a, c
    and a, %11111000
    ld l, a
    ld h, 0
    ; Now we have the position * 8 in hl
    add hl, hl ; position * 16
    add hl, hl ; position * 32
    ; Convert the X position to an offset.
    ld a, b
    srl a ; a / 2
    srl a ; a / 4
    srl a ; a / 8
    ; Add the two offsets together.
    add a, l
    ld l, a
    adc a, h
    sub a, l
    ld h, a
    ; Add the offset to the tilemap's base address, and we are done!
    ld bc, $9800
    add hl, bc
    ret

; @param a: tile ID
; @return z: set if a is a wall.
IsWallTile:
    cp a, $02
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

SECTION "Game Variables", WRAM0
wDestY: db
wDestX: db
wFurtherY: db
wFurtherX: db
wPushingCrate: db

