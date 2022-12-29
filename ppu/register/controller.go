package register

type ControllerStatus uint8

// 7  bit  0
// ---- ----
// VPHB SINN
// |||| ||||
// |||| ||++- Base nametable address
// |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
// |||| |+--- VRAM address increment per CPU read/write of PPUDATA
// |||| |     (0: add 1, going across; 1: add 32, going down)
// |||| +---- Sprite pattern table address for 8x8 sprites
// ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
// |||+------ Background pattern table address (0: $0000; 1: $1000)
// ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
// |+-------- PPU master/slave select
// |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
// +--------- Generate an NMI at the start of the vertical blanking interval (0: off; 1: on)

const (
	N ControllerStatus = 0b0000_0011
	I ControllerStatus = 0b0000_0100
	S ControllerStatus = 0b0000_1000
	B ControllerStatus = 0b0001_0000
	H ControllerStatus = 0b0010_0000
	P ControllerStatus = 0b0100_0000
	V ControllerStatus = 0b1000_0000
)

type ControllerRegister struct {
	Status uint8
}

func (cr *ControllerRegister) UpdateController(value uint8) {
	cr.Status = value
}

func (cr *ControllerRegister) GetBaseNameTableAddr() uint16 {
	switch cr.Status & uint8(N) {
	case 0:
		return 0x2000
	case 1:
		return 0x2400
	case 2:
		return 0x2800
	case 3:
		return 0x2C00
	default:
		panic("wrong nameTable")
	}
}

func (cr *ControllerRegister) VRAMAddressIncrement() uint8 {
	if cr.Status&uint8(I) == 0x0000 {
		return 1
	} else {
		return 32
	}
}

func (cr *ControllerRegister) GetSpritePatternAddress() uint16 {
	if cr.Status&uint8(S) == 0x0000 {
		return 0x0000
	} else {
		return 0x1000
	}
}

func (cr *ControllerRegister) GetBackgroundPatternAddress() uint16 {
	if cr.Status&uint8(B) == 0x0000 {
		return 0x0000
	} else {
		return 0x1000
	}
}

func (cr *ControllerRegister) GerSpriteSize() uint8 {
	if cr.Status&uint8(H) == 0x0000 {
		return 8
	} else {
		return 16
	}
}

func (cr *ControllerRegister) GetMasterSlaveSelect() uint8 {
	return cr.Status & uint8(P)
}

func (cr *ControllerRegister) GenerateVBlankNMI() bool {
	if cr.Status&uint8(V) == 1 {
		return true
	} else {
		return false
	}
}
