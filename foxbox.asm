INCLUDE "hardware.inc"

; Player direction enum
def SOUTH equ 0
def EAST equ 1
def NORTH equ 2
def WEST equ 3

SECTION "Header", ROM0[$100]

    jp EntryPoint

    ds $150 - @, 0 ; Make room for the header

SECTION "Code", ROM0
EntryPoint:
    ; Wait for VBlank
    ld a, [rLY]
    cp a, 144
    jr c, EntryPoint
    ; Do not turn the LCD off outside of VBlank

    ; Disable screen
    xor a, a
    ld [rLCDC], a

    ; Initialize Sprite Object Library.
    call InitSprObjLib
    
    ; Copy the tile data
    ld de, Tiles
    ld hl, $9000
    ld bc, TilesEnd - Tiles
    call Memcopy

    ; Copy the tilemap
    ; TODO (kmitton): FSM to handle title screen vs gameplay.
    ; ld de, TilemapLevel0
    ; ld hl, $9800
    ; ld bc, TilemapLevel0End - TilemapLevel0
    ; call Memcopy
    ld de, TilemapTitle
    ld hl, $9800
    ld bc, TilemapTitleEnd - TilemapTitle
    call Memcopy

    ; Copy the player and crate tiles
    ld de, Objects
    ld hl, $8000
    ld bc, ObjectsEnd - Objects
    call Memcopy

    ; Reset hardware OAM
    xor a, a
    ld b, 160
    ld hl, _OAMRAM
.resetOAM
    ld [hli], a
    dec b
    jr nz, .resetOAM
  
    ; Turn the LCD on
    ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON | LCDCF_OBJ16
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
    ld [wAnimCounter], a
    ld [wCurKeys], a
    ld [wNewKeys], a
    ld [wDestY], a
    ld [wDestX], a
    ld [wFurtherY], a
    ld [wFurtherX], a
    ld [wPushingCrate], a
    ld [wPlayerDir], a

    ; Place Player
    ; Level 0
    ; ld a, 112 + OAM_Y_OFS
    ; ld [wPlayerY], a
    ; ld a, 16 + OAM_X_OFS
    ; ld [wPlayerX], a

    ; Title screen
    ld a, $50 + OAM_Y_OFS
    ld [wPlayerY], a
    ld a, $50 + OAM_X_OFS
    ld [wPlayerX], a

    ; Place Crates
    ld a, 80 + OAM_Y_OFS
    ld [wCrate0Y], a
    ld a, 32 + OAM_X_OFS
    ld [wCrate0X], a

    ld a, 96 + OAM_Y_OFS
    ld [wCrate1Y], a
    ld a, 32 + OAM_X_OFS
    ld [wCrate1X], a

    ld a, 96 + OAM_Y_OFS
    ld [wCrate2Y], a
    ld a, 48 + OAM_X_OFS
    ld [wCrate2X], a

;SECTION "Main", ROM0

; Main game loop.
Main::
    call ResetShadowOAM

    ; Blit player
    ld a, [wPlayerDir]
    cp a, 0
    jp z, .south
    dec a
    jp z, .east
    dec a
    jp z, .north
    dec a
    jp .west


    ; TODO (mittonk): Fancy indexing, or a table in ROM, or similar?
.south:
    ld a, [wAnimCounter]
    cp a, 0
    jp nz, .south2
    ld hl, PlayerMetaspriteSouth
    jp .playerPos
.south2:
    ld hl, PlayerMetaspriteSouth2
    jp .playerPos
.east:
    ld a, [wAnimCounter]
    cp a, 0
    jp nz, .east2
    ld hl, PlayerMetaspriteEast
    jp .playerPos
.east2:
    ld hl, PlayerMetaspriteEast2
    jp .playerPos
.north:
    ld a, [wAnimCounter]
    cp a, 0
    jp nz, .north2
    ld hl, PlayerMetaspriteNorth
    jp .playerPos
.north2:
    ld hl, PlayerMetaspriteNorth2
    jp .playerPos
.west:
    ld a, [wAnimCounter]
    cp a, 0
    jp nz, .west2
    ld hl, PlayerMetaspriteWest
    jp .playerPos
.west2:
    ld hl, PlayerMetaspriteWest2
    jp .playerPos

.playerPos:
    ld a, [wPlayerY]
    ld b, a
    ld a, [wPlayerX]
    ld c, a
    call RenderMetaspriteUnscaled

    ; Blit crates
    ld a, [wCrate0Y]
    ld b, a
    ld a, [wCrate0X]
    ld c, a
    ld hl, CrateMetasprite
    call RenderMetaspriteUnscaled

    ld a, [wCrate1Y]
    ld b, a
    ld a, [wCrate1X]
    ld c, a
    ld hl, CrateMetasprite
    call RenderMetaspriteUnscaled

    ld a, [wCrate2Y]
    ld b, a
    ld a, [wCrate2X]
    ld c, a
    ld hl, CrateMetasprite
    call RenderMetaspriteUnscaled




WaitVBlank:
    ; Wait until it's *not* VBlank
    ld a, [rLY]
    cp SCRN_Y
    jp nc, Main
WaitVBlank2:
    ld a, [rLY]
    cp SCRN_Y
    jp c, WaitVBlank2

    ; Push sprites to OAM
    ld a, HIGH(wShadowOAM)
    call hOAMDMA

    ; Game clock
    ld a, [wFrameCounter]
    inc a
    ld [wFrameCounter], a
    cp a, 15 ; Every 15 frames (a quarter of a second), run the following code
    jp nz, Main

    ; Reset the frame counter back to 0
    ld a, 0
    ld [wFrameCounter], a

    ; Flip the animation counter
    ld a, [wAnimCounter]
    xor a, 1
    ld [wAnimCounter], a

    ; Check for input.
    call UpdateKeys

; First, check if the left button is pressed.
CheckLeft:
    ld a, [wCurKeys]
    and a, PADF_LEFT
    jp z, CheckRight
.left:
    ; Move the player one square left.

    ; Face accordingly, even if we don't end up moving.
    ld a, WEST
    ld [wPlayerDir], a

    ; Scope out our Dest and Further locations
    ; Y locations all same
    ld a, [wPlayerY]
    ld [wDestY], a
    ld [wFurtherY], a
    ; X in a row
    ld a, [wPlayerX]
    sub a, 16
    ld [wDestX], a
    sub a, 16
    ld [wFurtherX], a
    add a, 16  ; Leave Dest X in a

    ; If we've already hit the edge of the playfield, don't move.
    cp a, OAM_X_OFS
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

.isCrateLeft:
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
    jp z, .canCrateMoveLeft
    ld e, 1  ; Crate number
    call IsCrate1
    jp z, .canCrateMoveLeft
    ld e, 2  ; Crate number
    call IsCrate2
    jp z, .canCrateMoveLeft

    ; No crate there.
    jp .moveLeft

.canCrateMoveLeft:
    ; Remember which crate we're pushing.
    ld a, e
    ld [wPushingCrate], a
    ; Can the crate move?
    ; Check that Further isn't the edge, isn't a wall, and doesn't have a crate.
    ; 
    ; If we've already hit the edge of the playfield, don't move.
    ld a, [wFurtherX]
    cp a, OAM_X_OFS
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
    call IsCrate0
    jp z, Main
    call IsCrate1
    jp z, Main
    call IsCrate2
    jp z, Main
    ; OK, so the crate can be pushed.  Do it.

.moveCrateLeft:
    ; Yes: Move crate first.
    call PushingCrateX ; Active addr in hl
    ld a, [wFurtherX]
    ld [hl], a  ; Actually move crate

.moveLeft:
    ; All clear, move.
    ld a, [wDestX]
    ld [wPlayerX], a
    jp Main


; Then check the right button.
CheckRight:
    ld a, [wCurKeys]
    and a, PADF_RIGHT
    jp z, CheckUp
Right:
    ; Move the player one square right.

    ; Face accordingly, even if we don't end up moving.
    ld a, EAST
    ld [wPlayerDir], a

    ; Scope out our Dest and Further locations
    ; Y locations all same
    ld a, [wPlayerY]
    ld [wDestY], a
    ld [wFurtherY], a
    ; X in a row
    ld a, [wPlayerX]
    add a, 16
    ld [wDestX], a
    add a, 16
    ld [wFurtherX], a
    sub a, 16  ; Leave Dest X in a

    ; If we've already hit the edge of the playfield, don't move.
    cp a, SCRN_X
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

IsCrateRight:
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
    jp z, CanCrateMoveRight
    ld e, 1  ; Crate number
    call IsCrate1
    jp z, CanCrateMoveRight
    ld e, 2  ; Crate number
    call IsCrate2
    jp z, CanCrateMoveRight

    ; No crate there.
    jp MoveRight

CanCrateMoveRight:
    ; Remember which crate we're pushing.
    ld a, e
    ld [wPushingCrate], a
    ; Can the crate move?
    ; Check that Further isn't the edge, isn't a wall, and doesn't have a crate.
    ; 
    ; If we've already hit the edge of the playfield, don't move.
    ld a, [wFurtherX]
    cp a, SCRN_X
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
    call IsCrate0
    jp z, Main
    call IsCrate1
    jp z, Main
    call IsCrate2
    jp z, Main
    ; OK, so the crate can be pushed.  Do it.

MoveCrateRight:
    ; Yes: Move crate first.
    call PushingCrateX ; Active addr in hl
    ld a, [wFurtherX]
    ld [hl], a  ; Actually move crate

MoveRight:
    ; All clear, move.
    ld a, [wDestX]
    ld [wPlayerX], a
    jp Main


; Then check the up button.
CheckUp:
    ld a, [wCurKeys]
    and a, PADF_UP
    jp z, CheckDown
Up:
    ; Move the player one square up.

    ; Face accordingly, even if we don't end up moving.
    ld a, NORTH
    ld [wPlayerDir], a

    ; Scope out our Dest and Further locations
    ; X locations all same
    ld a, [wPlayerX]
    ld [wDestX], a
    ld [wFurtherX], a
    ; Y in a row
    ld a, [wPlayerY]
    sub a, 16
    ld [wDestY], a
    sub a, 16
    ld [wFurtherY], a
    add a, 16  ; Leave Dest Y in a

    ; If we've already hit the edge of the playfield, don't move.
    cp a, OAM_Y_OFS
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
    cp a, OAM_Y_OFS
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
    call IsCrate0
    jp z, Main
    call IsCrate1
    jp z, Main
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
    ld [wPlayerY], a
    jp Main


; Then check the down button.
CheckDown:
    ld a, [wCurKeys]
    and a, PADF_DOWN
    jp z, Main
Down:
    ; Move the player one square down.

    ; Face accordingly, even if we don't end up moving.
    ld a, SOUTH
    ld [wPlayerDir], a

    ; Scope out our Dest and Further locations
    ; X locations all same
    ld a, [wPlayerX]
    ld [wDestX], a
    ld [wFurtherX], a
    ; Y in a row
    ld a, [wPlayerY]
    add a, 16
    ld [wDestY], a
    add a, 16
    ld [wFurtherY], a
    sub a, 16  ; Leave Dest Y in a

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
    call IsCrate0
    jp z, Main
    call IsCrate1
    jp z, Main
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
    ld [wPlayerY], a
    jp Main




IsCrate0:
    ; Is there a crate?
    ; b: dest x oam
    ; c: dest y oam
    ld a, [wCrate0Y]
    cp a, c
    ret nz  ; Y doesn't match, bail.
    ld a, [wCrate0X]
    cp a, b
    ret  ; Z=true means Y, X both match.

IsCrate1:
    ; Is there a crate?
    ; b: dest x oam
    ; c: dest y oam
    ld a, [wCrate1Y]
    cp a, c
    ret nz  ; Y doesn't match, bail.
    ld a, [wCrate1X]
    cp a, b
    ret  ; Z=true means Y, X both match.

IsCrate2:
    ; Is there a crate?
    ; b: dest x oam
    ; c: dest y oam
    ld a, [wCrate2Y]
    cp a, c
    ret nz  ; Y doesn't match, bail.
    ld a, [wCrate2X]
    cp a, b
    ret  ; Z=true means Y, X both match.


SECTION "UpdateKeys", ROM0

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

SECTION "Utilities", ROM0
; Get the active X axis for the crate we're pushing.
; @return hl: X axis storage for active crate
PushingCrateX:
    ld a, [wPushingCrate]
    cp 0
    jp z, .pushingCrateX0
    cp 1
    jp z, .pushingCrateX1
    cp 2
    jp z, .pushingCrateX2
    jp Main ; Shouldn't happen
.pushingCrateX0:
    ld hl, wCrate0X
    ret
.pushingCrateX1:
    ld hl, wCrate1X
    ret
.pushingCrateX2:
    ld hl, wCrate2X
    ret


; Get the active Y axis for the crate we're pushing.
; @return hl: Y axis storage for active crate
PushingCrateY:
    ld a, [wPushingCrate]
    cp 0
    jp z, .pushingCrateY0
    cp 1
    jp z, .pushingCrateY1
    cp 2
    jp z, .pushingCrateY2
    jp Main ; Shouldn't happen
.pushingCrateY0:
    ld hl, wCrate0Y
    ret
.pushingCrateY1:
    ld hl, wCrate1Y
    ret
.pushingCrateY2:
    ld hl, wCrate2Y
    ret


; Convert a OAM position to a tilemap address
; hl = $9800 + pixelX + pixelY * 32
; @param b: X, oam
; @param c: Y, oam
; @return hl: tile address
GetTileByOam:
    ; (8, 16) in OAM coordinates is (0, 0) on the screen.
    ld a, c
    sub a, OAM_Y_OFS
    ld c, a
    ld a, b
    sub a, OAM_X_OFS
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
    cp a, $08  ; Top-left tile of a wall
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

;SECTION "Tiles", ROM0
Tiles:
	; 00 Outside A
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333

	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333

	; 02 Outside B
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333

	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333

	; 04 Floor A
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000

	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000

	; 06 Floor B
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000

	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000

	; 08 Wall A
	dw `11111111
	dw `11111121
	dw `11121111
	dw `11111111
	dw `11211111
	dw `11111211
	dw `21111111
	dw `11111111

	dw `12111111
	dw `11111121
	dw `11121111
	dw `11111111
	dw `11211111
	dw `11111211
	dw `12111111
	dw `11111111

	; 0a Wall B
	dw `12111111
	dw `11111121
	dw `11121111
	dw `11111111
	dw `11211111
	dw `11111211
	dw `21111111
	dw `11111111

	dw `12111111
	dw `11111121
	dw `11121111
	dw `11111111
	dw `11211111
	dw `11111211
	dw `21111111
	dw `11111111

	; 0c Target A
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000022
	dw `00000222
	dw `00002222
	dw `00002222

	dw `00002222
	dw `00002222
	dw `00000222
	dw `00000022
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000

	; 0e Target B
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `22000000
	dw `22200000
	dw `22220000
	dw `22220000

	dw `22220000
	dw `22220000
	dw `22200000
	dw `22000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000

TitleTileData: INCBIN "assets/title.2bpp"

TilesEnd:

;SECTION "Tilemap", ROM0
TilemapLevel0:
	db $00, $00, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $00, $00, $00, $00, $00, $00, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $00, $00, $00, $00, $00, $00, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $08, $0a, $04, $06, $04, $06, $04, $06, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $09, $0b, $05, $07, $05, $07, $05, $07, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $08, $0a, $04, $06, $04, $06, $04, $06, $08, $0a, $04, $06, $04, $06, $08, $0a, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0

	db $00, $00, $09, $0b, $05, $07, $05, $07, $05, $07, $09, $0b, $05, $07, $05, $07, $09, $0b, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $08, $0a, $08, $0a, $04, $06, $04, $06, $04, $06, $04, $06, $0c, $0e, $08, $0a, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $09, $0b, $09, $0b, $05, $07, $05, $07, $05, $07, $05, $07, $0d, $0f, $09, $0b, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $08, $0a, $08, $0a, $08, $0a, $04, $06, $08, $0a, $08, $0a, $08, $0a, $0c, $0e, $08, $0a, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $09, $0b, $09, $0b, $09, $0b, $05, $07, $09, $0b, $09, $0b, $09, $0b, $0d, $0f, $09, $0b, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0

	db $08, $0a, $04, $06, $04, $06, $04, $06, $08, $0a, $00, $00, $08, $0a, $0c, $0e, $08, $0a, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $09, $0b, $05, $07, $05, $07, $05, $07, $09, $0b, $00, $00, $09, $0b, $0d, $0f, $09, $0b, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $08, $0a, $04, $06, $04, $06, $04, $06, $08, $0a, $00, $00, $08, $0a, $08, $0a, $08, $0a, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $09, $0b, $05, $07, $05, $07, $05, $07, $09, $0b, $00, $00, $09, $0b, $09, $0b, $09, $0b, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $08, $0a, $04, $06, $04, $06, $04, $06, $08, $0a, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0

	db $09, $0b, $05, $07, $05, $07, $05, $07, $09, $0b, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $08, $0a, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $09, $0b, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0

TilemapLevel0End:

TilemapTitle:
	db $00, $00, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $08, $0a, $08, $0a, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $08, $0a, $08, $0a, 0,0,0,0,0,0,0,0,0,0,0,0
	db $09, $0b, $09, $0b, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $09, $0b, $09, $0b, 0,0,0,0,0,0,0,0,0,0,0,0
	db $08, $0a, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $08, $0a, 0,0,0,0,0,0,0,0,0,0,0,0

	db $09, $0b, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $09, $0b, 0,0,0,0,0,0,0,0,0,0,0,0
	db $08, $0a, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $08, $0a, 0,0,0,0,0,0,0,0,0,0,0,0
	db $09, $0b, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $09, $0b, 0,0,0,0,0,0,0,0,0,0,0,0
	db $08, $0a, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $08, $0a, 0,0,0,0,0,0,0,0,0,0,0,0
	db $09, $0b, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $09, $0b, 0,0,0,0,0,0,0,0,0,0,0,0

	db $08, $0a, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $08, $0a, 0,0,0,0,0,0,0,0,0,0,0,0
	db $09, $0b, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $09, $0b, 0,0,0,0,0,0,0,0,0,0,0,0
	db $08, $0a, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $08, $0a, 0,0,0,0,0,0,0,0,0,0,0,0
	db $09, $0b, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $09, $0b, 0,0,0,0,0,0,0,0,0,0,0,0
	db $08, $0a, $08, $0a, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $04, $06, $08, $0a, $08, $0a, 0,0,0,0,0,0,0,0,0,0,0,0

	db $09, $0b, $09, $0b, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $05, $07, $09, $0b, $09, $0b, 0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $08, $0a, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0
	db $00, $00, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $09, $0b, $00, $00, 0,0,0,0,0,0,0,0,0,0,0,0

TilemapTitleEnd:

;SECTION "Objects", ROM0
Objects:
    ; 00, 02 Player South
PlayerSouthTileData: INCBIN "assets/player_south.2bpp"

    ; 04, 06 Player East
PlayerEastTileData: INCBIN "assets/player_east.2bpp"

    ; 08, 0a Player East2
PlayerEast2TileData: INCBIN "assets/player_east2.2bpp"

    ; 0c, 0e Player North
PlayerNorthTileData: INCBIN "assets/player_north.2bpp"

    ; 10 Crate A
    dw `33333333
    dw `33222222
    dw `32333333
    dw `32332222
    dw `32323222
    dw `32322322
    dw `32322232
    dw `32322223

    dw `32322223
    dw `32322232
    dw `32322322
    dw `32323222
    dw `32332222
    dw `32333333
    dw `33222222
    dw `33333333

    ; 12 Crate B
    dw `33333333
    dw `22222233
    dw `33333323
    dw `22223323
    dw `22232323
    dw `22322323
    dw `23222323
    dw `32222323

    dw `32222323
    dw `23222323
    dw `22322323
    dw `22232323
    dw `22223323
    dw `33333323
    dw `22222233
    dw `33333333

ObjectsEnd:

PlayerMetaspriteSouth:
    .metasprite1    db 0,0,0,0
    .metasprite2    db 0,8,2,0
    .metaspriteEnd  db 128

PlayerMetaspriteSouth2:  ; Mirror horizontally
    .metasprite1    db 0,0,2,OAMF_XFLIP
    .metasprite2    db 0,8,0,OAMF_XFLIP
    .metaspriteEnd  db 128

PlayerMetaspriteEast:
    .metasprite1    db 0,0,4,0
    .metasprite2    db 0,8,6,0
    .metaspriteEnd  db 128

PlayerMetaspriteEast2:  ; Alternate set of tiles
    .metasprite1    db 0,0,$8,0
    .metasprite2    db 0,8,$a,0
    .metaspriteEnd  db 128

PlayerMetaspriteNorth:
    .metasprite1    db 0,0,$c,0
    .metasprite2    db 0,8,$e,0
    .metaspriteEnd  db 128

PlayerMetaspriteNorth2:  ; Mirror horizontally
    .metasprite1    db 0,0,$e,OAMF_XFLIP
    .metasprite2    db 0,8,$c,OAMF_XFLIP
    .metaspriteEnd  db 128

PlayerMetaspriteWest:  ; Reuse East tiles with some flipping and shuffling
    .metasprite1    db 0,0,6,OAMF_XFLIP
    .metasprite2    db 0,8,4,OAMF_XFLIP
    .metaspriteEnd  db 128

PlayerMetaspriteWest2:  ; Reuse East2 tiles with some flipping and shuffling
    .metasprite1    db 0,0,$a,OAMF_XFLIP
    .metasprite2    db 0,8,$8,OAMF_XFLIP
    .metaspriteEnd  db 128

CrateMetasprite:
    .metasprite1    db 0,0,$10,0
    .metasprite2    db 0,8,$12,0
    .metaspriteEnd  db 128

SECTION "Counter", WRAM0
wFrameCounter: db
wAnimCounter: db

SECTION "Input Variables", WRAM0
wCurKeys: db
wNewKeys: db
wLastKeys: db

SECTION "Game Variables", WRAM0
wPlayerY: db
wPlayerX: db
wPlayerDir: db
wCrate0Y: db
wCrate0X: db
wCrate1Y: db
wCrate1X: db
wCrate2Y: db
wCrate2X: db
wDestY: db
wDestX: db
wFurtherY: db
wFurtherX: db
wPushingCrate: db
wGameState: db
