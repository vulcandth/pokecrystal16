; AI_SMART encourages Sunny Day if it also knows these moves.

SunnyDayMoves:
; BUG: "Smart" AI does not encourage Sunny Day when it knows Solar Beam, Flame Wheel, or Moonlight (see docs/bugs_and_glitches.md)
	dw FIRE_PUNCH
	dw EMBER
	dw FLAMETHROWER
	dw FIRE_SPIN
	dw FIRE_BLAST
	dw SACRED_FIRE
	dw MORNING_SUN
	dw SYNTHESIS
	dw -1 ; end
