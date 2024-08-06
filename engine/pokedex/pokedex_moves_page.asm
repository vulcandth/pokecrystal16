; started as 5 before UI overhaul, now is 7
; would decrease to 3 if we wanted to use additional line for attack info
DEF MAX_NUM_MOVES EQU 7

Field_Moves_List: ; PK16 expanded moves, moves are now 2 bytes each!!
	dw TELEPORT, SOFTBOILED, MILK_DRINK, \
		HEADBUTT, ROCK_SMASH, SWEET_SCENT, DIG,\
		CUT, FLY, SURF, STRENGTH, FLASH, WATERFALL, WHIRLPOOL
Field_Moves_Method_List: ; PK16 THIS BRANCH ISNT USING EXPANDED ITEMS, but when we do, remember to switch from db to dw
	db 0, 0, 0, TM01 + 1, TM01 + 7, TM01 + 11, TM01 + 27, HM01, \
	HM01 + 1, HM01 + 2, HM01 + 3, HM01 + 4, HM01 + 5, HM01 + 6

; the category box is 2 tiles high, 12 tiles wide
; must fill in spaces to erase other text already printed
; top string goes in 8, 6, bottom in 8, 7
String_LVL_text:
	db "LVL-UP     @"
String_FIELD_text:
	db "FIELD      @"
String_EGG_text:
	db "EGG        @"
String_MOVES_text:
	db " MOVES     @"
String_TECH_text:
	db "TECHNICAL  @"
String_MACHINES_text:
	db " MACHINES  @"
String_MOVE_text:
	db "MOVE       @"
String_TUTOR_text:
	db " TUTOR     @"

Pokedex_SkipEvolutions: ; ripped straight from engine\pokemon\evolve.asm
	ld a, b
	call GetFarByte
	inc hl
	and a
	ret z
	cp EVOLVE_STAT
	jr nz, .no_extra_skip
	inc hl
.no_extra_skip
	inc hl
	inc hl
	inc hl
	jr Pokedex_SkipEvolutions

Pokedex_GetNextEvoAttackByte: ; ripped straight from engine\pokemon\evolve.asm
	ldh a, [hTemp]
	call GetFarByte
	inc hl
	ret

Print_Category_MOVES_text:
	ld hl, String_MOVES_text
	jp Print_Category_text

DisplayDexMonMoves::
	ld a, [wTempSpecies]
	ld [wCurSpecies], a
	ld [wCurPartySpecies], a
	call Pokedex_Clearbox

	; the byte flag that tells us which type of table we're currently on
	; 0 = Info, 1 = Stats, 2 = LVL UP, 3 = FIELD, 4 =  EGG, 5 = TMs, 6 = HMs, 7 = MTs
	
	ld a, [wPokedexEntryType]
	cp DEXENTRY_LVLUP
	jr z, .LvlUpLearnset
	cp DEXENTRY_EGG
	jr z, .EggMoves
	cp DEXENTRY_FIELD
	jr z, .Field_Moves
	cp DEXENTRY_TMS
	jr z, .TMs
	; bit DEXENTRY_HMS, a
	; jr nz, .HMs
	cp DEXENTRY_MTS
	jr z, .MTs
.LvlUpLearnset_new
	ld a, DEXENTRY_LVLUP
	call DexEntry_NextCategory
.LvlUpLearnset
; place category name
	ld de, String_LVL_text
	call Print_Category_MOVES_text

	ld a, DEXENTRY_LVLUP
	ld [wPokedexEntryType], a
	call Pokedex_Calc_LvlMovesPtr
	call Pokedex_Print_NextLvlMoves
	ret
.EggMoves
; place category name
	ld de, String_EGG_text
	call Print_Category_MOVES_text

	ld a, DEXENTRY_EGG
	ld [wPokedexEntryType], a
	call Pokedex_Calc_EggMovesPtr
	ret z
	call Pokedex_Print_Egg_moves
	ret
.Field_Moves
; place category name
	ld de, String_FIELD_text
	call Print_Category_MOVES_text

	ld a, DEXENTRY_FIELD
	ld [wPokedexEntryType], a
	call Pokedex_PrintFieldMoves
	ret
.TMs
; place category name
	ld de, String_TECH_text
	ld hl, String_MACHINES_text
	call Print_Category_text

	ld a, DEXENTRY_TMS
	ld [wPokedexEntryType], a
	call Pokedex_PrintTMs
	ret
; .HMs
; 	ld a, DEXENTRY_HMS
; 	ld [wPokedexEntryType], a
; 	call Pokedex_PrintHMs
; 	ret
.MTs
; place category name
	ld de, String_MOVE_text
	ld hl, String_TUTOR_text
	call Print_Category_text

	ld a, DEXENTRY_MTS
	ld [wPokedexEntryType], a
	call Pokedex_PrintMTs
	ret

Pokedex_Calc_LvlMovesPtr:
	ld a, [wTempSpecies]
	call GetPokemonIndexFromID
	ld b, h
	ld c, l
	ld hl, EvosAttacksPointers
	ld a, BANK(EvosAttacksPointers)
	call LoadDoubleIndirectPointer
	ldh [hTemp], a ; BANK("Evolutions and Attacks Pointers")
	ld b, a
	call Pokedex_SkipEvolutions ; ripped straight from Vulcandth's, would rather copy than farcall lol
; .CalcPageoffset
	call Pokedex_PrintPageNum ; page num is also returned in a
	ld c, MAX_NUM_MOVES
	call SimpleMultiply 
	; double this num and add to first byte after Evo's 0
	; for p16, triple the num
	ld b, 0
	ld c, a
	; PK16 expanded moves: triple, since each move entry is now 3 bytes instead of 2
	add hl, bc
	add hl, bc
	add hl, bc
	ret

Pokedex_Print_NextLvlMoves:
; Print No more than MAX_NUM_MOVES moves
	ld b, 0
	ld c, 0 ; our move counter, max of MAX_NUM_MOVES
.learnset_loop
	; ldh a, [hTemp] ; BANK("Evolutions and Attacks Pointers")
	; call GetFarByte ; lvl to learn move, or 0 (delimiter)
	call Pokedex_GetNextEvoAttackByte ; hl now points to second byte of move
	and a
	jr z, .FoundEnd
	push hl ; ; ptr to lvl to learn move, or 0 (delimiter)
	ld [wTextDecimalByte], a
	hlcoord 2, 9
	call DexEntry_adjusthlcoord
	ld [hl], "<DEX_LV>"
	hlcoord 3, 9
	call DexEntry_adjusthlcoord
	ld de, wTextDecimalByte
	push bc ; move counter
	lb bc, PRINTNUM_LEFTALIGN | 1, 2
	call PrintNum
	pop bc ; move counter
	pop hl ; hl now points to second byte of move ; ptr to lvl to learn move, or 0 (delimiter)
	; inc hl ; points to first byte of move
	push hl ; hl now points to second byte of move ; points to first byte of move
	ldh a, [hTemp] ; BANK("Evolutions and Attacks Pointers")
	call GetFarWord
	call GetMoveIDFromIndex
	ld [wNamedObjectIndex], a
	call GetMoveName
	hlcoord 7, 9
	call DexEntry_adjusthlcoord
	push bc ; move counter
	call PlaceString
	pop bc ; move counter
	pop hl ; points to first byte of move
	inc hl ; points to second byte of move
	inc hl ; points to next move entry's lvl learned
	inc bc ; increment move counter
	ld a, MAX_NUM_MOVES
	cp c
	jr nz, .learnset_loop
	jr .MaxedPage
.MaxedPage ; Printed MAX_NUM_MOVES moves. Moves are still left. Inc the Page counter
	; check to see if really any moves left, we dont want a blank page
	ldh a, [hTemp] ; BANK("Evolutions and Attacks Pointers")
	call GetFarByte ; lvl to learn move, or 0 (delimiter)
	and a
	jr z, .FoundEnd
	call DexEntry_IncPageNum
	ret
.FoundEnd
	ld a, DEXENTRY_FIELD
	call DexEntry_NextCategory
	ret

Pokedex_PrintFieldMoves:
; CheckLvlUpMoves, 1 for fail, 0 for yes, in c
	call Pokedex_PrintPageNum ; page num is also returned in a
	ld a, [wPokedexStatus] ; field moves index
	ld b, a
	ld c, 0 ; current line
.fm_loop
	push bc
	ld c, b
	ld b, 0
	ld hl, Field_Moves_List
	add hl, bc ; since moves are 2 bytes
	add hl, bc ; we need to add the index twice
	ld e, [hl]
	inc hl
	ld d, [hl]
	ld h, d
	ld l, e
	call GetMoveIDFromIndex
	ld d, a
	call Pokedex_CheckLvlUpMoves
	ld a, c ; c has lvl we learn move
	ld e, c
	pop bc
	and a
	jr nz, .print_move_name
.check_machines
; check TM/HM
	push bc
	ld c, b 
	ld b, 0
	ld hl, Field_Moves_List
	add hl, bc ; since moves are 2 bytes
	add hl, bc ; we need to add the index twice
	push de
	ld e, [hl]
	inc hl
	ld d, [hl]
	ld h, d
	ld l, e
	pop de
	call GetMoveIDFromIndex
	ld d, a
	ld [wPutativeTMHMMove], a
	farcall CanLearnTMHMMove
	ld a, c
	pop bc
	and a
	jr z, .notcompatible
; check TM/HM done
.print_move_name
	push de
	ld a, d
	ld [wNamedObjectIndex], a
	call GetMoveName
	push bc ; our count is in c
	hlcoord 7, 9
	call DexEntry_adjusthlcoord
	call PlaceString
	pop bc
; print TM/HM num
	ld d, 0
	ld e, b
	ld hl, Field_Moves_Method_List
	add hl, de
	ld a, [hl]
	pop de
	and a
	jr nz, .tm_or_hm
.printlvlupmove	
	push bc
	hlcoord 3, 9
	call DexEntry_adjusthlcoord
	ld [hl], "<DEX_LV>"
	inc hl
	lb bc, PRINTNUM_LEFTALIGN | 1, 2
	ld a, e
	ld [wTextDecimalByte], a
	ld de, wTextDecimalByte
	call PrintNum
	pop bc
	jr .inc_line_count  
.tm_or_hm
	; a has the item id of the tm/hm/mt
	push af
	ld a, e ; lvl at which they learn move?
	and a
	jr nz, .BothLvlUpandTMHMMT
	pop af
	ld [wNamedObjectIndex], a
	call GetItemName
	push bc
	hlcoord 2, 9
	call DexEntry_adjusthlcoord
	call PlaceString
	pop bc
; print TM/HM num done
.inc_line_count
	inc c ; since we printed a line
	ld a, MAX_NUM_MOVES
	cp c
	jr nz, .notcompatible ; not yet printed all MAX_NUM_MOVES slots
	; We've printed all MAX_NUM_MOVES slots
	; check if we need to move to next category or if there are moves left
	call Pokedex_anymoreFieldMoves
	jr z, .done ; there are no moves left
	; moves left
	call DexEntry_IncPageNum
	ret
.notcompatible
	ld a, NUM_FIELD_MOVES - 1
	cp b
	jr z, .done
	inc b
	ld a, b
	ld [wPokedexStatus], a ; moves machines index
	jp .fm_loop
.done
	ld a, DEXENTRY_EGG
	call DexEntry_NextCategory
	ld a, c
	and a
	ret nz ; we've had at least one HM Move
	hlcoord 4, 9
	ld de, DexEntry_NONE_text
	call PlaceString
	ret
.both_no_more_room
	pop af
	jr .inc_line_count	
.BothLvlUpandTMHMMT
	; we're print number regardless
	push bc
	push de
	hlcoord 3, 9
	call DexEntry_adjusthlcoord
	ld [hl], "<DEX_LV>"
	inc hl
	lb bc, PRINTNUM_LEFTALIGN | 1, 2
	ld a, e
	ld [wTextDecimalByte], a
	ld de, wTextDecimalByte
	call PrintNum
	pop de
	pop bc

	ld a, c
	dec b ; so it will do dig again on next page
	cp MAX_NUM_MOVES - 1
	jr z, .both_no_more_room ; we only have 1 space left, so prioritize lvl up
	inc b ; we have room, so fix the field move counter
	; print name again
	inc c ; line counter
	push bc
	push de
	ld a, d
	ld [wNamedObjectIndex], a
	call GetMoveName
	; push bc ; our count is in c
	hlcoord 7, 9
	call DexEntry_adjusthlcoord
	call PlaceString
	pop de
	pop bc

	xor a ; so we dont get flagged as both again
	ld e, a
	pop af ; tm/hm/mt item id
	jp .tm_or_hm
	; a needs to be the TM/HM/MT item id	
	; do not increment index, so it will print both on next page

Pokedex_anymoreFieldMoves:
	ld a, NUM_FIELD_MOVES - 1
	cp b
	jr z, .none	
	; b has the current machine index
	inc b
.fmloop
	push bc
	ld d, 0
	ld e, b 
	ld hl, Field_Moves_List
	add hl, de
	add hl, de
	call GetMoveIDFromIndex
	ld d, a

	call Pokedex_CheckLvlUpMoves
	ld a, c ; c has lvl we learn move
	ld d, a ; to preserve it for later
	pop bc
	and a
	jr nz, .yes
	push bc
	ld d, 0
	ld e, b
	ld hl, Field_Moves_Method_List
	add hl, de
	ld a, [hl]
	and a
	jr z, .not_tm_or_hm
	ld [wCurItem], a
	farcall GetTMHMItemMove
	ld a, [wTempTMHM]	
	ld [wPutativeTMHMMove], a
	farcall CanLearnTMHMMove
	ld a, c
	pop bc
	and a
	jr nz, .yes
.not_tm_or_hm
	ld a, NUM_FIELD_MOVES - 1
	cp b
	jr z, .none
	inc b
	jr .fmloop	
.yes
	ld a, b
	ld [wPokedexStatus], a ; so we can start off at the next learnable machine
	ld a, 1
	and a
	ret
.none
	xor a
	ret

Pokedex_CheckLvlUpMoves: ; used by pokedex field moves
; move looking for in 'd'
	ld a, [wCurPartySpecies]
	; ld a, [wTempSpecies]
	call GetPokemonIndexFromID
	ld b, h
	ld c, l
	ld hl, EvosAttacksPointers
	ld a, BANK(EvosAttacksPointers)
	call LoadDoubleIndirectPointer
	ldh [hTemp], a ; BANK("Evolutions and Attacks Pointers")
	ld b, a
	call Pokedex_SkipEvolutions ; ripped straight from Vulcandth's, would rather copy than farcall lol
.find_move
	call Pokedex_GetNextEvoAttackByte ; inc's hl too, now pointint to first move byte
	and a
	jr z, .notfound ; end of mon's lvl up learnset, 0 is the delimiter
	ld c, a ; the lvl we learn move
	push hl
	ldh a, [hTemp]
	call GetFarWord
	call GetMoveIDFromIndex
	pop hl
	inc hl ; pointing to second move byte
	inc hl ; pointing to lvl of next move entry, or 0 if we're at the end
	cp d ; the move we're looking for, 'd' is not clobbered in any of the used funcs or farcalls
	jr z, .found
	jr .find_move
.found
	; lvl learned move in c
	ret ; move is in lvl up learnset
.notfound
	xor a
	ld c, a
	ret

Pokedex_Calc_EggMovesPtr:
	ld a, DEXENTRY_EGG
	ld [wPokedexEntryType], a
	call Pokedex_PrintPageNum ; page num is also returned in a
	ld a, [wPokedexEntryPageNum]
	ld c, MAX_NUM_MOVES ; we can print MAX_NUM_MOVES Egg moves per page
	call SimpleMultiply ; double this num and add to first byte after Evo's 0
	ld b, 0
	ld c, a ; num of egg moves we've already printed on previous pages
	push bc ; num of egg moves we've already printed on previous pages

	ld a, [wCurPartySpecies]
	callfar GetLowestEvolutionStage ; uses wCurPartySpecies
	ld a, [wCurPartySpecies]
	callfar GetLowestEvolutionStage ; uses wCurPartySpecies
	ld a, [wCurPartySpecies]
	ld [wTempSpecies], a
	ld [wCurSpecies], a
	ld [wTempMonSpecies], a

	call GetPokemonIndexFromID
	ld b, h
	ld c, l
	ld hl, EggMovePointers
	ld a, BANK(EggMovePointers)	
	call LoadDoubleIndirectPointer
	ldh [hTemp], a ; BANK("Egg Moves")
	push hl
	call GetFarWord ; a will be -1 in both bytes if no egg moves
	ld d, h
	ld e, l
	pop hl
	pop bc ; num of egg moves we've already printed on previous pages
	add hl, bc
	add hl, bc ; add twice since moves are now 2 bytes in PK16 expanded moves
	ld a, d
	cp -1
	ret nz
	ld a, e
	cp -1
	ret nz
	; if we reach here, the mon has no egg moves at all
	; will not call Pokedex_Print_Egg_moves
	; increment to next category
	ld a, DEXENTRY_TMS
	call DexEntry_NextCategory
	;print NONE
	hlcoord 3, 9
	ld de, DexEntry_NONE_text
	call PlaceString
	ret

Pokedex_Print_Egg_moves:
; Print No more than MAX_NUM_MOVES moves
	ld b, 0
	ld c, 0 ; our move counter, max of MAX_NUM_MOVES - 1 for MAX_NUM_MOVES moves
	; our adjusted pointer based on page num is in hl
.Egg_loop
	ldh a, [hTemp]; BANK("Egg Moves")
	call GetFarByte ; EGG Move, or -1 for end
	cp -1
	jr z, .FoundEnd
	push hl
	ldh a, [hTemp]; BANK("Egg Moves")
	call GetFarWord ; EGG Move, or -1 for end
	call GetMoveIDFromIndex	
	pop hl
	inc hl ; Moves HL to next Byte, 2nd byte of move
	inc hl ; moves to first byte of next move, or -1
	push hl
	ld [wNamedObjectIndex], a ; all the "Name" Funcs use this 
	call GetMoveName ; returns the string pointer in de
	hlcoord 3, 9
	call DexEntry_adjusthlcoord
	push bc
	call PlaceString ; places Move Name
	pop bc
	pop hl
	ld a, MAX_NUM_MOVES - 1 ; means we just printed last move on page
	cp c
	jr z, .MaxedPage
	inc c
	jr .Egg_loop
.MaxedPage ; Printed MAX_NUM_MOVES moves. Moves are still left. Inc the Page counter
; CheckNextByte, we dont want blank screen if we just printed last move in slot 5
	ldh a, [hTemp]; BANK("Egg Moves")
	call GetFarByte; Move # returned in "a"
	cp -1
	jr z, .FoundEnd
	call DexEntry_IncPageNum
	ld a, [wCurPartySpecies]
	ld [wCurSpecies], a
	ld [wTempSpecies], a
	ld [wTempMonSpecies], a
	ret
.FoundEnd
	ld a, DEXENTRY_TMS
	call DexEntry_NextCategory
	ld a, [wCurPartySpecies]
	ld [wCurSpecies], a
	ld [wTempSpecies], a
	ld [wTempMonSpecies], a
	ret

Pokedex_PrintTMs:
	call Pokedex_PrintPageNum ; page num is also returned in a
	ld a, [wPokedexStatus] ; machine moves index
	ld b, a
	ld c, 0 ; current line
.tm_loop
	push bc
	ld a, TM01
	add b ; machine moves index
	ld [wCurItem], a ; machine moves index
	farcall GetTMHMItemMove
	ld a, [wTempTMHM]
	ld [wPutativeTMHMMove], a
	farcall CanLearnTMHMMove
	ld a, c
	pop bc
	and a
	jr z, .notcompatible
	call GetMoveName
	push bc ; our count is in c
	hlcoord 7, 9
	call DexEntry_adjusthlcoord
	call PlaceString
	pop bc
	ld a, [wPokedexStatus]
	ld b, a
	ld a, TM01
	add b ; machine moves index
	ld [wNamedObjectIndex], a
	call GetItemName
	push bc
	hlcoord 2, 9
	call DexEntry_adjusthlcoord
	call PlaceString
	pop bc
	inc c ; since we printed a line
	ld a, MAX_NUM_MOVES
	cp c
	jr nz, .notcompatible ; not yet printed all 5 slots
	; We've printed all MAX_NUM_MOVES slots
	; check if we need to move to next category or if there are moves left
	call Pokedex_anymoreTMs
	jr z, .done ; there are no moves left
	; moves left
	call DexEntry_IncPageNum
	ret
.notcompatible
	ld a, NUM_TMS - 1
	cp b
	jr z, .done
	inc b
	ld a, b
	ld [wPokedexStatus], a ; moves machines index
	jr .tm_loop
.done
	ld a, DEXENTRY_MTS
	call DexEntry_NextCategory
	ld a, c
	and a
	ret nz ; we've had at least one HM Move
	hlcoord 4, 9
	ld de, DexEntry_NONE_text
	call PlaceString
	ret

Pokedex_anymoreTMs:
	; b has the current HM index
	ld a, NUM_TMS - 1
	cp b
	jr z, .none
	inc b
.tmloop
	push bc
	ld a, TM01
	add b
	ld [wCurItem], a
	farcall GetTMHMItemMove
	ld a, [wTempTMHM]	
	ld [wPutativeTMHMMove], a
	farcall CanLearnTMHMMove
	ld a, c
	pop bc
	and a
	jr nz, .yes
	ld a, NUM_TMS - 1
	cp b
	jr z, .none
	inc b
	jr .tmloop	
.yes
	ld a, b
	ld [wPokedexStatus], a ; so we can start off at the next learnable machine
	ld a, 1
	and a
	ret
.none
	xor a
	ld [wPokedexStatus], a
	ret

Pokedex_PrintMTs:
	call Pokedex_PrintPageNum ; page num is also returned in a
	ld a, [wPokedexStatus] ; moves machines index
	ld b, a ; result of simple multiply in a
	ld c, 0 ; current line
.mt_loop
	push bc
	ld a, MT01
	add b
	ld [wCurItem], a
	farcall GetTMHMItemMove
	ld a, [wTempTMHM]
	ld [wPutativeTMHMMove], a
	farcall CanLearnTMHMMove
	ld a, c
	pop bc
	and a
	jr z, .notcompatible
	call GetMoveName
	push bc ; our count is in c
	hlcoord 3, 9
	call DexEntry_adjusthlcoord
	call PlaceString
	pop bc
	inc c ; since we printed a line
	ld a, MAX_NUM_MOVES
	cp c
	jr nz, .notcompatible
	; We've printed all MAX_NUM_MOVES slots
	; check if we need to move to next category or if there are moves left
	call Pokedex_anymoreMTs
	jr z, .done ; there are no moves left
	; moves left
	call DexEntry_IncPageNum
	ret
.notcompatible
	ld a, NUM_TUTORS - 1
	cp b
	jr z, .done
	inc b
	ld a, b
	ld [wPokedexStatus], a ; moves machines index
	jr .mt_loop
.done
	ld a, DEXENTRY_LVLUP
	call DexEntry_NextCategory
	ld a, c
	and a
	ret nz ; we've had at least one HM Move
	hlcoord 4, 9
	ld de, DexEntry_NONE_text
	call PlaceString
	ret

Pokedex_anymoreMTs:
	ld a, NUM_TUTORS - 1
	cp b
	jr z, .none
	; b has the current HM index
	inc b
.mtloop
	push bc
	ld a, MT01
	add b
	ld [wCurItem], a
	farcall GetTMHMItemMove
	ld a, [wTempTMHM]	
	ld [wPutativeTMHMMove], a
	farcall CanLearnTMHMMove
	ld a, c
	pop bc
	and a
	jr nz, .yes
	ld a, NUM_TUTORS - 1
	cp b
	jr z, .none
	inc b
	jr .mtloop	
.yes
	ld a, 1
	and a
	ret
.none
	xor a
	ret

; Pokedex_PrintHMs:
; 	hlcoord 2, 9
; 	ld de, .dex_HM_text
; 	call PlaceString
; 	call Pokedex_PrintPageNum ; page num is also returned in a
; 	ld c, MAX_NUM_MOVES
; 	ld a, [wPokedexStatus]
; 	ld b, a
; 	ld c, 0 ; current line
; .hm_loop
; 	push bc
; 	ld a, HM01
; 	add b
; 	ld [wCurItem], a
; 	farcall GetTMHMItemMove
; 	ld a, [wTempTMHM]
; 	ld [wPutativeTMHMMove], a
; 	farcall CanLearnTMHMMove
; 	ld a, c
; 	pop bc
; 	and a
; 	jr z, .notcompatible
; 	call GetMoveName
; 	push bc ; our count is in c
; 	hlcoord 7, 9
; 	call DexEntry_adjusthlcoord
; 	call PlaceString
; 	pop bc
; 	ld a, HM01
; 	add b
; 	ld [wNamedObjectIndex], a
; 	call GetItemName
; 	push bc
; 	hlcoord 2, 9
; 	call DexEntry_adjusthlcoord
; 	call PlaceString
; 	pop bc
; 	inc c ; since we printed a line
; 	ld a, MAX_NUM_MOVES
; 	cp c
; 	jr nz, .notcompatible
; 	call Pokedex_anymoreHMs
; 	jr z, .done
; 	call DexEntry_IncPageNum
; 	ret
; .notcompatible
; 	ld a, NUM_HMS - 1
; 	cp b
; 	jr z, .done
; 	inc b
; 	jr .hm_loop
; .done
; 	ld a, DEXENTRY_MTS
; 	call DexEntry_NextCategory
; 	ld a, c
; 	and a
; 	ret nz ; we've had at least one HM Move
; 	hlcoord 4, 9
; 	ld de, DexEntry_NONE_text
; 	call PlaceString
; 	ret
; .dex_HM_text:
; 	db "HIDDEN MACHINES@"

; Pokedex_anymoreHMs:
; 	ld a, NUM_HMS - 1
; 	cp b
; 	jr z, .none
; 	; b has the current HM index
; 	inc b
; .hmloop
; 	push bc
; 	ld a, HM01
; 	add b
; 	ld [wCurItem], a
; 	farcall GetTMHMItemMove
; 	ld a, [wTempTMHM]	
; 	ld [wPutativeTMHMMove], a
; 	farcall CanLearnTMHMMove
; 	ld a, c
; 	pop bc
; 	and a
; 	jr nz, .yes
; 	ld a, NUM_HMS - 1
; 	cp b
; 	jr z, .none
; 	inc b
; 	jr .hmloop	
; .yes
; 	ld a, 1
; 	and a
; 	ret
; .none
; 	xor a
; 	ld [wPokedexStatus], a
; 	ret

DexEntry_NONE_text:
	db "NONE@"
