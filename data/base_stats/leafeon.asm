	db LEAFEON ; 252

	db  65, 110, 130,  95,  60,  65
	;   hp  atk  def  spd  sat  sdf

	db GRASS, GRASS
	db 45 ; catch rate
	db 196 ; base exp
	db NO_ITEM ; item 1
	db NO_ITEM ; item 2
	db 31 ; gender
	db 35 ; step cycles to hatch
	dn 7, 7 ; frontpic dimensions
	db LEAF_GUARD ; ability 1
if DEF(FAITHFUL)
	db LEAF_GUARD ; ability 2
else
	db NATURAL_CURE ; ability 2
endc
	db CHLOROPHYLL ; hidden ability
	db MEDIUM_FAST ; growth rate
	dn FIELD, FIELD ; egg groups

	; ev_yield
	ev_yield   0,   0,   2,   0,   0,   0
	;         hp, atk, def, spd, sat, sdf

	; tmhm
	tmhm ROAR, TOXIC, SWORDS_DANCE, HIDDEN_POWER, SUNNY_DAY, HYPER_BEAM, PROTECT, RAIN_DANCE, GIGA_DRAIN, SOLAR_BEAM, IRON_TAIL, RETURN, DIG, SHADOW_BALL, MUD_SLAP, DOUBLE_TEAM, SWIFT, AERIAL_ACE, FACADE, REST, ATTRACT, FURY_CUTTER, ROCK_SMASH, ENERGY_BALL, X_SCISSOR, ENDURE, GIGA_IMPACT, FLASH, STRENGTH, HEADBUTT, HYPER_VOICE, SEED_BOMB, SLEEP_TALK, SUBSTITUTE, SWAGGER
	; end
