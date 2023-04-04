package bus

import (
	"Nes/cartridge"
	"Nes/ppu"
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
	PPUMaxRam          uint16 = 0x0007
	ControllerStart    uint16 = 0x4016
	ControllerEnd      uint16 = 0x4017
	DAMAddress         uint16 = 0x4014
)

type Bus struct {
	// Nes cpu only address 2kb
	CPURam          [CPURamSize]uint8
	PPURam          [PPURamSize]uint8
	Cart            *cartridge.Cartridge
	PPU             *ppu.PPU
	Controllers     [2]uint8
	ControllerState [2]uint8
	DMAPage         uint8
	DMAAddr         uint8
	DMAData         uint8
	IsDMATransfer   bool
	IsDMADummy      bool
}

func (b *Bus) CPURead(addr uint16, isTrace bool) uint8 {
	if isCart, data := b.Cart.CPURead(addr); isCart {
		return data
	} else if addr <= CPURamMirrorsEnd {
		return b.CPURam[addr&CPUMaxRam]
	} else if addr >= PPURamMirrorsStart && addr <= PPURamMirrorsEnd {
		return b.PPU.CPURead(addr&PPUMaxRam, isTrace)
	} else if addr >= ControllerStart && addr <= ControllerEnd {
		if b.ControllerState[addr&0x0001]&0x80 != 0 {
			data = 0x01
		} else {
			data = 0x00
		}
		b.ControllerState[addr&0x0001] <<= 1
		return data
	}
	return 0
}

func (b *Bus) CPUWrite(addr uint16, value uint8) {
	if isCart := b.Cart.CPUWrite(addr, value); isCart {
		return
	} else if addr <= CPURamMirrorsEnd {
		b.CPURam[addr&CPUMaxRam] = value
	} else if addr >= PPURamMirrorsStart && addr <= PPURamMirrorsEnd {
		b.PPU.CPUWrite(addr&PPUMaxRam, value)
	} else if addr == DAMAddress {
		b.DMAPage = value
		b.DMAAddr = 0x00
		b.IsDMATransfer = true
	} else if addr >= ControllerStart && addr <= ControllerEnd {
		b.ControllerState[addr&0x1000] = b.Controllers[addr&0x1000]
	}
}

func (b *Bus) Reset() {
	b.PPURam = [PPURamSize]uint8{}
	b.CPURam = [CPURamSize]uint8{}
}

func New(cart *cartridge.Cartridge, ppu *ppu.PPU) *Bus {

	return &Bus{
		CPURam:          [2048]uint8{},
		Cart:            cart,
		PPU:             ppu,
		Controllers:     [2]uint8{},
		ControllerState: [2]uint8{},
		IsDMADummy:      true,
		IsDMATransfer:   false,
		DMAPage:         0x00,
		DMAData:         0x00,
		DMAAddr:         0x00,
	}
}
