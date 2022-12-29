package register

// This register controls the rendering of sprites and backgrounds, as well as colour effects.
// 7  bit  0
// ---- ----
// BGRs bMmG
// |||| ||||
// |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
// |||| +---- 1: Show background
// |||+------ 1: Show sprites
// ||+------- Emphasize red (green on PAL/Dendy)
// |+-------- Emphasize green (red on PAL/Dendy)
// +--------- Emphasize blue
const (
	GREYSCALE                uint8 = 0b0000_0001
	LEFTMOST_8PXL_BACKGROUND uint8 = 0b0000_0010
	LEFTMOST_8PXL_SPRITE     uint8 = 0b0000_0100
	SHOW_BACKGROUND          uint8 = 0b0000_1000
	SHOW_SPRITES             uint8 = 0b0001_0000
	EMPHASISE_RED            uint8 = 0b0010_0000
	EMPHASISE_GREEN          uint8 = 0b0100_0000
	EMPHASISE_BLUE           uint8 = 0b1000_0000
)

type MaskRegister struct {
	Status uint8
}

func (mr *MaskRegister) IsGreyScale() bool {
	return mr.Status&GREYSCALE == 1
}

func (mr *MaskRegister) IsShowBGInLeftMost8P() bool {
	return mr.Status&LEFTMOST_8PXL_BACKGROUND == 1
}

func (mr *MaskRegister) IsShowSPInLeftMost8P() bool {
	return mr.Status&LEFTMOST_8PXL_SPRITE == 1
}

func (mr *MaskRegister) IsShowBG() bool {
	return mr.Status&SHOW_BACKGROUND == 1
}

func (mr *MaskRegister) IsShowSP() bool {
	return mr.Status&SHOW_SPRITES == 1
}

func (mr *MaskRegister) IsEmphasiseRed() bool {
	return mr.Status&EMPHASISE_RED == 1
}

func (mr *MaskRegister) IsEmphasiseGreen() bool {
	return mr.Status&EMPHASISE_GREEN == 1
}

func (mr *MaskRegister) IsEmphasiseBLUE() bool {
	return mr.Status&EMPHASISE_BLUE == 1
}

func (mr *MaskRegister) Update(value uint8) {
	mr.Status = value
}
