package register

// This register reflects the state of various functions inside the PPU. It is often used for determining timing.
//To determine when the PPU has reached a given pixel of the screen, put an opaque (non-transparent) pixel of
//sprite 0 there.
// 7  bit  0
// ---- ----
// VSO. ....
// |||| ||||
// |||+-++++- Least significant bits previously written into a PPU register
// |||        (due to register not being updated for this address)
// ||+------- Sprite overflow. The intent was for this flag to be set
// ||         whenever more than eight sprites appear on a scanline, but a
// ||         hardware bug causes the actual behavior to be more complicated
// ||         and generate false positives as well as false negatives; see
// ||         PPU sprite evaluation. This flag is set during sprite
// ||         evaluation and cleared at dot 1 (the second dot) of the
// ||         pre-render line.
// |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
// |          a nonzero background pixel; cleared at dot 1 of the pre-render
// |          line.  Used for raster timing.
// +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
//
//	Set at dot 1 of line 241 (the line *after* the post-render
//	line); cleared after reading $2002 and at dot 1 of the
//	pre-render line.

const (
	NOTUSED         uint8 = 0b0000_0001
	NOTUSED2        uint8 = 0b0000_0010
	NOTUSED3        uint8 = 0b0000_0100
	NOTUSED4        uint8 = 0b0000_1000
	NOTUSED5        uint8 = 0b0001_0000
	SPRITE_OVERFLOW uint8 = 0b0010_0000
	SPRITE_ZERO_HIT uint8 = 0b0100_0000
	VBLANK_STARTED  uint8 = 0b1000_0000
)

type StatusRegister struct {
	Status uint8
}

func (sr *StatusRegister) SetVBlankStatus(status bool) {
	if status {
		sr.Status = sr.Status | VBLANK_STARTED
	} else {
		sr.Status = sr.Status & ^VBLANK_STARTED
	}
}

func (sr *StatusRegister) ReSetVBlankStatus() {
	sr.SetVBlankStatus(false)
}

func (sr *StatusRegister) IsInVBlank() bool {
	return sr.Status&VBLANK_STARTED != 0
}

func (sr *StatusRegister) SetSpriteZeroHit(status bool) {
	if status {
		sr.Status = sr.Status | SPRITE_ZERO_HIT
	} else {
		sr.Status = sr.Status & ^SPRITE_ZERO_HIT
	}
}

func (sr *StatusRegister) SetSpriteOverflow(status bool) {
	if status {
		sr.Status = sr.Status | SPRITE_OVERFLOW
	} else {
		sr.Status = sr.Status & ^SPRITE_OVERFLOW
	}
}

func (sr *StatusRegister) GetStatus() uint8 {
	return sr.Status
}
