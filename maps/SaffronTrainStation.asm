const_value set 2
	const SAFFRONTRAINSTATION_OFFICER
	const SAFFRONTRAINSTATION_GYM_GUY
	const SAFFRONTRAINSTATION_TEACHER
	const SAFFRONTRAINSTATION_LASS

SaffronTrainStation_MapScriptHeader:
.MapTriggers:
	db 0

.MapCallbacks:
	db 0

OfficerScript_0x18a81e:
	faceplayer
	opentext
	checkevent EVENT_RESTORED_POWER_TO_KANTO
	iftrue .MagnetTrainToGoldenrod
	writetext UnknownText_0x18a8a9
	waitbutton
	closetext
	end

.MagnetTrainToGoldenrod:
	writetext UnknownText_0x18a8dd
	yesorno
	iffalse .DecidedNotToRide
	checkitem PASS
	iffalse .PassNotInBag
	writetext UnknownText_0x18a917
	waitbutton
	closetext
	applymovement SAFFRONTRAINSTATION_OFFICER, MovementData_0x18a88f
	applymovement PLAYER, MovementData_0x18a898
	writebyte $1
	special Special_MagnetTrain
	warpcheck
	newloadmap MAPSETUP_TRAIN
	applymovement PLAYER, .MovementBoardTheTrain
	wait $14
	end

.MovementBoardTheTrain:
	turn_head_down
	step_end

.PassNotInBag:
	writetext UnknownText_0x18a956
	waitbutton
	closetext
	end

.DecidedNotToRide:
	writetext UnknownText_0x18a978
	waitbutton
	closetext
	end

Script_ArriveFromGoldenrod:
	applymovement SAFFRONTRAINSTATION_OFFICER, MovementData_0x18a88f
	applymovement PLAYER, MovementData_0x18a8a1
	applymovement SAFFRONTRAINSTATION_OFFICER, MovementData_0x18a894
	opentext
	writetext UnknownText_0x18a993
	waitbutton
	closetext
	end

GymGuyScript_0x18a875:
	faceplayer
	opentext
	checkevent EVENT_RETURNED_MACHINE_PART
	iftrue UnknownScript_0x18a883
	writetext UnknownText_0x18a9ca
	waitbutton
	closetext
	end

UnknownScript_0x18a883:
	writetext UnknownText_0x18aa61
	waitbutton
	closetext
	end

TeacherScript_0x18a889:
	jumptextfaceplayer UnknownText_0x18aaab

LassScript_0x18a88c:
	jumptextfaceplayer UnknownText_0x18ab20

MovementData_0x18a88f:
	step_up
	step_up
	step_right
	turn_head_left
	step_end

MovementData_0x18a894:
	step_left
	step_down
	step_down
	step_end

MovementData_0x18a898:
	step_up
	step_up
	step_up
	step_left
	step_left
	step_left
	step_up
	step_up
	step_end

MovementData_0x18a8a1:
	step_left
	step_left
	step_down
	step_down
	step_down
	step_down
	turn_head_up
	step_end

UnknownText_0x18a8a9:
	text "I'm sorry, but the"
	line "Magnet Train isn't"
	cont "operating now."
	done

UnknownText_0x18a8dd:
	text "We'll soon depart"
	line "for Goldenrod."

	para "Are you coming on"
	line "board?"
	done

UnknownText_0x18a917:
	text "May I see your"
	line "rail pass, please?"

	para "OK. Right this"
	line "way, please."
	done

UnknownText_0x18a956:
	text "Sorry, but you"
	line "don't have a pass."
	done

UnknownText_0x18a978:
	text "We hope to see you"
	line "again."
	done

UnknownText_0x18a993:
	text "We have arrived in"
	line "Saffron."

	para "We hope to see you"
	line "again."
	done

UnknownText_0x18a9ca:
	text "The Magnet Train"
	line "is a super-modern"

	para "rail liner that"
	line "uses electricity"

	para "and magnets to"
	line "attain incredible"
	cont "speed."

	para "However, if there"
	line "isn't any elec-"
	cont "tricity…"
	done

UnknownText_0x18aa61:
	text "Whew…"

	para "How many times"
	line "have I gone back"

	para "and forth between"
	line "Kanto and Johto?"
	done

UnknownText_0x18aaab:
	text "Before the Magnet"
	line "Train Station was"

	para "built, there was a"
	line "house there."

	para "A little girl"
	line "named Copycat used"
	cont "to live there."
	done

UnknownText_0x18ab20:
	text "Hi. Do you have a"
	line "rail pass? I have"

	para "one. All the peo-"
	line "ple in Saffron who"

	para "ride the Magnet"
	line "Train have passes."
	done

SaffronTrainStation_MapEventHeader:
.Warps:
	db 4
	warp_def $11, $8, 6, SAFFRON_CITY
	warp_def $11, $9, 6, SAFFRON_CITY
	warp_def $5, $6, 4, GOLDENROD_MAGNET_TRAIN_STATION
	warp_def $5, $b, 3, GOLDENROD_MAGNET_TRAIN_STATION

.XYTriggers:
	db 1
	xy_trigger 0, $6, $b, Script_ArriveFromGoldenrod

.Signposts:
	db 0

.PersonEvents:
	db 4
	person_event SPRITE_OFFICER, 9, 9, SPRITEMOVEDATA_STANDING_DOWN, 0, 0, -1, -1, 0, PERSONTYPE_SCRIPT, 0, OfficerScript_0x18a81e, -1
	person_event SPRITE_GYM_GUY, 14, 10, SPRITEMOVEDATA_WANDER, 1, 1, -1, -1, 0, PERSONTYPE_SCRIPT, 0, GymGuyScript_0x18a875, -1
	person_event SPRITE_TEACHER, 11, 6, SPRITEMOVEDATA_STANDING_RIGHT, 0, 0, -1, -1, 0, PERSONTYPE_SCRIPT, 0, TeacherScript_0x18a889, EVENT_SAFFRON_TRAIN_STATION_POPULATION
	person_event SPRITE_LASS, 10, 6, SPRITEMOVEDATA_STANDING_UP, 0, 0, -1, -1, (1 << 3) | PAL_OW_GREEN, PERSONTYPE_SCRIPT, 0, LassScript_0x18a88c, EVENT_SAFFRON_TRAIN_STATION_POPULATION
