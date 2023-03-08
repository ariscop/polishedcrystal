Random::
	; just like the stock RNG, this exits with the value in [hRandomSub]
	; it also stores a random value in [hRandomAdd]
	push hl
	push de
	push bc
	call UpdateDividerCounters
	ld hl, wRNGState
	ld a, [hli]
	ld b, a
	ld a, [hli]
	ld c, a
	ld a, [hli]
	ld d, a
	ld e, [hl]
	ld a, e
	add a
	xor b
	ld b, a
	ld a, d
	rla
	ld l, c
	rl l
	ld h, b
	rl h
	sbc a
	and 1
	xor c
	ld c, a
	ld a, h
	xor d
	ld d, a
	ld a, l
	xor e
	ld e, a
	ld h, b
	ld l, c
	push hl
	ld h, d
	ld a, e
rept 2
	sla e
	rl d
	rl c
	rl b
endr
	xor e
	ld e, a
	ld a, h
	xor d
	ld d, a
	pop hl
	ld a, l
	xor c
	ld c, a
	ld a, h
	xor b
	ld hl, wRNGState
	ld [hli], a
	ld a, c
	ld [hli], a
	ld a, d
	ld [hli], a
	ld [hl], e
	ldh a, [rDIV]
	add [hl]
	ldh [hRandomAdd], a
	ld a, [hli]
	inc hl
	inc hl
	sub [hl]
	ldh [hRandomSub], a
	jmp PopBCDEHL

UpdateDividerCounters::
	ldh a, [rDIV]
	ld hl, wRNGCumulativeDividerMinus
	sbc [hl]
	ld [hld], a
	ldh a, [rDIV]
	adc [hl]
	ld [hld], a
	ret nc
	inc [hl]
	ret

AdvanceRNGState::
	ld hl, wRNGState
	ld a, [hli]
	ld b, a ; wRNGState[0]
	ld a, [hli]
	ld c, a ; wRNGState[1]
	ld a, [hli]
	ld d, a ; wRNGState[2]
	ld a, [hli]
	ld e, a ; wRNGState[3]
	ld a, [hli]
	ld l, [hl] ; wRNGCumulativeDividerPlus[1]
	ld h, a ; wRNGCumulativeDividerPlus[0]
	ldh a, [rDIV]
	rra
	jr nc, .try_upper
.try_lower
	ld a, h
	cp d
	ld a, l
	jr nz, .lower
	cp e
	jr nz, .lower
.upper
	xor c
	ld c, a
	ld a, h
	xor b
	jr .done
.try_upper
	ld a, h
	cp b
	ld a, l
	jr nz, .upper
	cp c
	jr nz, .upper
.lower
	xor e
	ld e, a
	ld a, h
	xor d
	ld d, a
	ld a, b
.done
	ld hl, wRNGState
	ld [hli], a ; wRNGState[0]
	ld a, c
	ld [hli], a ; wRNGState[1]
	ld a, d
	ld [hli], a ; wRNGState[2]
	ld [hl], e ; wRNGState[3]
	ret

RandomRange::
; Return a random number between 0 and a (non-inclusive).

	push bc
	ld c, a

	; b = $100 % c
	xor a
	sub c
.mod
	sub c
	jr nc, .mod
	add c
	ld b, a

	; Get a random number
	; from 0 to $ff - b.
	push bc
.loop
	call Random
	ldh a, [hRandomAdd]
	ld c, a
	add b
	jr c, .loop
	ld a, c
	pop bc

	call SimpleDivide

	pop bc
	ret

RandomRange16::
; Return a random number between 0 and bc (non-inclusive)
	ld a, b
	and a
	jr z, .8bit
	push hl
	push de
	cpl
	ld d, a
	ld a, c
	cpl
	ld e, a
	inc de

; de = $10000 % bc
	ld hl, 0
	add hl, de
.mod
	add hl, de
	jr c, .mod
	add hl, bc
	ld d, h
	ld e, l
	push bc

	; get a random number
	; from 0 to $ffff - de
.loop
	call Random
	ldh a, [hRandomAdd]
	ld h, a
	ldh a, [hRandomSub]
	ld l, a
	add hl, de
	jr c, .loop

	pop de
	ld b, h
	ld c, l
	call Divide16
	pop de
	pop hl
	ret

.8bit
	ld a, c
	call RandomRange
	ld c, a
	ret

; Handles all RNG calls in the battle engine
BattleRandom::
; If the normal RNG is used in a link battle it'll desync.
; To circumvent this a shared PRNG is used instead.

; But if we're in a non-link battle we're safe to use it
	ld a, [wLinkMode]
	and a
	jmp z, Random

; The PRNG operates in streams of 10 values.

; Which value are we trying to pull?
	push hl
	push bc
	ld a, [wLinkBattleRNCount]
	ld c, a
	ld b, 0
	ld hl, wLinkBattleRNs
	add hl, bc
	inc a
	ld [wLinkBattleRNCount], a

; If we haven't hit the end yet, we're good
	cp 10 - 1 ; Exclude last value. See the closing comment
	ld a, [hl]
	pop bc
	pop hl
	ret c

; If we have, we have to generate new pseudorandom data
; Instead of having multiple PRNGs, ten seeds are used
	push hl
	push bc
	push af

; Reset count to 0
	xor a
	ld [wLinkBattleRNCount], a
	ld hl, wLinkBattleRNs
	ld b, 10 ; number of seeds

; Generate next number in the sequence for each seed
; The algorithm takes the form *5 + 1 % 256
.loop
	; get last #
	ld a, [hl]

	; a * 5 + 1
	ld c, a
	add a
	add a
	add c
	inc a

	; update #
	ld [hli], a
	dec b
	jr nz, .loop

; This has the side effect of pulling the last value first,
; then wrapping around. As a result, when we check to see if
; we've reached the end, we check the one before it.

	pop af
	pop bc
	pop hl
	ret

BattleRandomRange::
; battle friendly RandomRange
	push bc
	ld b, a

	; ensure even distribution by cutting off the top
.loop
	add b
	jr nc, .loop
	sub b
	ld c, a
.loop2
	call BattleRandom
	cp c
	jr nc, .loop2

	; now we have a random number without the uneven top, get mod of it
.loop3
	sub b
	jr nc, .loop3
	add b

	; return the result
	pop bc
	ret
