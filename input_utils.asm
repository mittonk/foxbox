INCLUDE "hardware.inc"

SECTION "InputUtilsVariables", WRAM0

mWaitKey:: db

SECTION "UpdateKeys", ROM0

    ; https://gbdev.io/gb-asm-tutorial/part2/input.html
UpdateKeys::
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

SECTION "InputUtils", ROM0

WaitForKeyFunction::

    ; Save our original value
    push bc

    
WaitForKeyFunction_Loop:

    ; save the keys last frame
    ld a, [wCurKeys]
    ld [wLastKeys], a
    
    ; This is in input.asm
    ; It's straight from: https://gbdev.io/gb-asm-tutorial/part2/input.html
    ; In their words (paraphrased): reading player input for gameboy is NOT a
    ; trivial task
    ; So it's best to use some tested code
    call UpdateKeys

    
    ld a, [mWaitKey]
    ld b, a
    ld a, [wCurKeys]
    and b
    jp z, WaitForKeyFunction_NotPressed
    
    ld a, [wLastKeys]
    and b
    jp nz, WaitForKeyFunction_NotPressed

    ; restore our original value
    pop bc

    ret


WaitForKeyFunction_NotPressed:

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Wait a small amount of time
    ; Save our count in this variable
    ld a, 1
    ld [wVBlankCount], a

    ; Call our function that performs the code
    call WaitForVBlankFunction
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    jp WaitForKeyFunction_Loop

