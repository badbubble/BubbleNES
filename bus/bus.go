package bus

import (
	"Nes/cartridge"
	"Nes/ppu"
	"fmt"
	"log"
)

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
	PPURamSize         uint16 = 0x3FFF
	PPURamMirrorsStart uint16 = 0x2000
	PPURamMirrorsEnd   uint16 = 0x3FFF
	PPUMaxRam          uint16 = 0x2007
)

type Bus struct {
	// Nes cpu only address 2kb
	CPURam [CPURamSize]uint8
	PPURam [PPURamSize]uint8
	Rom    *cartridge.Rom
	PPU    *ppu.PPU
	Cycles uint
}

func (b *Bus) Tick(cycles uint) {
	b.Cycles += cycles
	b.PPU.Tick(cycles * 3)
}

func (b *Bus) CPUMemRead(addr uint16) uint8 {
	switch {
	case addr >= CPURamMirrorsStart && addr <= CPURamMirrorsEnd:
		return b.CPURam[addr&CPUMaxRam] // 0x07ff
	case addr >= PPURamMirrorsStart && addr <= PPURamMirrorsEnd:
		MirrorsDownAddr := addr & PPUMaxRam
		switch MirrorsDownAddr {
		case 0x2000, 0x2001, 0x2003, 0x2005, 0x2006, 0x401:
			log.Fatalf("try to read from write-only ppu address")
		case 0x2002:
			return b.PPU.ReadStatus()
		case 0x2004:
			return b.PPU.ReadOMAData()
		case 0x2007:
			return b.PPU.ReadData()
		}
	case addr >= 0x8000:
		return b.ReadPRGRom(addr)
	default:
		fmt.Printf("Ignoring mem access at %d\n", addr)
		return 0
	}
	return 0
}

func (b *Bus) CPUMemWrite(addr uint16, value uint8) {
	switch {
	case addr <= CPURamMirrorsEnd:
		b.CPURam[addr&CPUMaxRam] = value
	case addr >= PPURamMirrorsStart && addr <= PPURamMirrorsEnd:
		MirrorsDownAddr := addr & PPUMaxRam
		switch MirrorsDownAddr {
		case 0x2000:
			b.PPU.WriteToController(value)
		case 0x2001:
			b.PPU.WriteToMask(value)
		case 0x2002:
			log.Fatalf("try to write to ppu status register")
		case 0x2003:
			b.PPU.WriteToOAMAddr(value)
		case 0x2004:
			b.PPU.WriteToOAMData(value)
		case 0x2005:
			b.PPU.WriteToScroll(value)
		case 0x2006:
			b.PPU.WriteToPPUAddr(value)
		case 0x2007:
			b.PPU.WriteToData(value)
		}
		//
	case addr >= 0x8000:
		panic("Attempt to write to Cartridge ROM space")
	default:
		fmt.Printf("Ignoring mem write-access at %d\n", addr)
	}
}

func (b *Bus) ReadPRGRom(addr uint16) uint8 {
	addr -= 0x8000
	// 0x8000-0x10000 PRG Rom Size might be 16 KiB or 32 KiB. Because [0x8000 … 0x10000] mapped region is 32 KiB of
	//addressable space, the upper 16 KiB needs to be mapped to the lower 16 KiB (if a game has only 16 KiB of PRG ROM)
	if len(b.Rom.PRGRom) == 0x4000 && addr >= 0x4000 {
		addr = addr % 0x4000
	}
	return b.Rom.PRGRom[uint(addr)]
}

func New(rom *cartridge.Rom) *Bus {
	p := ppu.New(rom.CHRRom, rom.ScreenMirroring)
	return &Bus{
		CPURam: [2048]uint8{},
		Rom:    rom,
		PPU:    p,
	}
}
