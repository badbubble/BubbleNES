package bus

import "Nes/cartridge"

//  _______________ $10000  _______________
// | PRG-ROM       |       |               |
// | Upper Bank    |       |               |
// |_ _ _ _ _ _ _ _| $C000 | PRG-ROM       |
// | PRG-ROM       |       |               |
// | Lower Bank    |       |               |
// |_______________| $8000 |_______________|
// | SRAM          |       | SRAM          |
// |_______________| $6000 |_______________|
// | Expansion ROM |       | Expansion ROM |
// |_______________| $4020 |_______________|
// | I/O Registers |       |               |
// |_ _ _ _ _ _ _ _| $4000 |               |
// | Mirrors       |       | I/O Registers |
// | $2000-$2007   |       |               |
// |_ _ _ _ _ _ _ _| $2008 |               |
// | I/O Registers |       |               |
// |_______________| $2000 |_______________|
// | Mirrors       |       |               |
// | $0000-$07FF   |       |               |
// |_ _ _ _ _ _ _ _| $0800 |               |
// | RAM           |       | RAM           |
// |_ _ _ _ _ _ _ _| $0200 |               |
// | Stack         |       |               |
// |_ _ _ _ _ _ _ _| $0100 |               |
// | Zero Page     |       |               |
// |_______________| $0000 |_______________|

const (
	// CPURamSize 2KB
	CPURamSize         uint16 = 0x0800
	CPURamMirrorsStart uint16 = 0x0000
	CPUMaxRam          uint16 = 0x07FF
	// CPURamMirrorsEnd mirrored three times
	CPURamMirrorsEnd   uint16 = 0x1FFF
	PPURamMirrorsStart uint16 = 0x2000
	PPURamMirrorsEnd   uint16 = 0x3FFF
	PPUMaxRam          uint16 = 0x2007
)

type Bus struct {
	// Nes cpu only address 2kb
	CPURam [CPURamSize]uint8
	Rom    *cartridge.Rom
}

func (bus *Bus) MemRead(addr uint16) uint8 {
	switch {
	case addr >= CPURamMirrorsStart && addr <= CPURamMirrorsEnd:
		return bus.CPURam[addr&CPUMaxRam] // 0x07ff
	case addr >= PPURamMirrorsStart && addr <= PPURamMirrorsEnd:
		// MirrorsDownAddr := addr & MaxPPURam //
		//TODO implement PPU
		return 0

	case addr >= 0x8000:
		return bus.ReadPRGRom(addr)
	}
	return 0
}

func (bus *Bus) MemWrite(addr uint16, data uint8) {
	switch {
	case addr <= CPURamMirrorsEnd:
		bus.CPURam[addr&CPUMaxRam] = data
	case addr >= PPURamMirrorsStart && addr <= PPURamMirrorsEnd:
		//TODO implement PPU
		bus.CPURam[addr&PPUMaxRam] = data
		//
	case addr >= 0x8000:
		panic("Attempt to write to Cartridge ROM space")
	}
}

func (bus *Bus) ReadPRGRom(addr uint16) uint8 {
	addr -= 0x8000
	// 0x8000-0x10000 PRG Rom Size might be 16 KiB or 32 KiB. Because [0x8000 … 0x10000] mapped region is 32 KiB of
	//addressable space, the upper 16 KiB needs to be mapped to the lower 16 KiB (if a game has only 16 KiB of PRG ROM)
	if len(bus.Rom.PRGRom) == 0x4000 && addr >= 0x4000 {
		addr = addr % 0x4000
	}
	return bus.Rom.PRGRom[uint(addr)]
}

func New(rom *cartridge.Rom) *Bus {
	return &Bus{
		CPURam: [2048]uint8{},
		Rom:    rom,
	}
}
