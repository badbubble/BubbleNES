package bus

const (
	RamSize            uint16 = 0x0800
	RamMirrorsStart    uint16 = 0x0000
	RamMirrorsEnd      uint16 = 0x1FFF
	RamPPUMirrorsStart uint16 = 0x2000
	RamPPUMirrorsEnd   uint16 = 0x3FFF
)

type Bus struct {
	CPURam [RamSize]uint8
}

func (bus *Bus) MemRead(addr uint16) uint8 {
	switch {
	case addr <= RamMirrorsEnd:
		MirrorsDownAddr := addr & 0b0000_0111_1111_1111
		return bus.CPURam[MirrorsDownAddr]
	case addr >= RamPPUMirrorsStart && addr <= RamPPUMirrorsEnd:
		MirrorsDownAddr := addr & 0b0010_0000_0000_0111
		//TODO implement PPU
		return bus.CPURam[MirrorsDownAddr]
	}
	return 0
}

func (bus *Bus) MemWrite(addr uint16, data uint8) {
	switch {
	case addr <= RamMirrorsEnd:
		MirrorsDownAddr := addr & 0b0000_0111_1111_1111
		bus.CPURam[MirrorsDownAddr] = data
	case addr >= RamPPUMirrorsStart && addr <= RamPPUMirrorsEnd:
		MirrorsDownAddr := addr & 0b0010_0000_0000_0111
		//TODO implement PPU
		bus.CPURam[MirrorsDownAddr] = data
	}
}

func New() *Bus {
	return &Bus{}
}
