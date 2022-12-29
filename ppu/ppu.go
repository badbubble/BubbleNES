package ppu

import (
	"Nes/cartridge"
	"Nes/ppu/register"
	"log"
)

const (
	PaletteTableSize = 32
	PPURamSize       = 2048
	OMADataSize      = 256
)

type PPU struct {
	CHRRom             []uint8
	PaletteTable       [PaletteTableSize]uint8
	VRam               [PPURamSize]uint8 // video ram 0x2000~0x2ffff Mirror to 0x3000~0x3ffff
	OMAData            [OMADataSize]uint8
	OMAAddr            uint8
	InternalDataBuffer uint8
	// Registers
	Address    register.AddressRegister
	Controller register.ControllerRegister
	Mask       register.MaskRegister
	Status     register.StatusRegister
	Scroll     register.ScrollRegister

	Mirroring cartridge.MirroringType
	Scanline  uint16
	Cycles    uint
	IsNMI     bool
}

func (p *PPU) Tick(cycles uint) bool {
	p.Cycles += cycles
	if p.Cycles >= 341 {
		p.Cycles -= 341
		p.Scanline += 1
		if p.Scanline == 241 {
			if p.Controller.GenerateVBlankNMI() {
				p.Status.SetVBlankStatus(true)
			}
		}
		if p.Scanline >= 262 {
			p.Scanline = 0
			p.Status.ReSetVBlankStatus()
			return true
		}
	}
	return false
}

func (p *PPU) WriteToController(value uint8) {
	p.Controller.UpdateController(value)
}

func (p *PPU) WriteToMask(value uint8) {
	p.Mask.Update(value)
}

func (p *PPU) ReadStatus() uint8 {
	data := p.Status.GetStatus()
	p.Status.ReSetVBlankStatus()
	p.Address.ResetLatch()
	p.Scroll.ResetLatch()
	return data
}

func (p *PPU) WriteToOAMAddr(value uint8) {
	p.OMAAddr = value
}

func (p *PPU) WriteToOAMData(value uint8) {
	p.OMAData[uint(p.OMAAddr)] = value
	p.OMAAddr += 1
}

func (p *PPU) WriteToScroll(value uint8) {
	p.Scroll.Write(value)
}

func (p *PPU) WriteToPPUAddr(value uint8) {
	p.Address.Update(value)
}

func (p *PPU) IncrementVRAMAddr() {
	p.Address.Increment(p.Controller.VRAMAddressIncrement())
}

func (p *PPU) MirrorVRAMAddr(addr uint16) uint16 {
	// Horizontal:
	//   [ A ] [ a ]
	//   [ B ] [ b ]

	// Vertical:
	//   [ A ] [ B ]
	//   [ a ] [ b ]
	mirroredAddr := addr & 0x2FFF
	VRamIdx := mirroredAddr - 0x2000
	nameTable := VRamIdx / 0x400
	if p.Mirroring == cartridge.Horizontal {
		switch nameTable {
		case 0:
			return VRamIdx
		case 1:
			return VRamIdx - 0x0400
		case 2:
			return VRamIdx - 0x0400
		case 3:
			return VRamIdx - 0x0800
		}

	} else if p.Mirroring == cartridge.Vertical {
		switch nameTable {
		case 0:
			return VRamIdx
		case 1:
			return VRamIdx
		case 2:
			return VRamIdx - 0x0800
		case 3:
			return VRamIdx - 0x0800
		}
	}
	return VRamIdx
}

func (p *PPU) ReadOMAData() uint8 {
	return p.OMAData[uint(p.OMAAddr)]
}

func (p *PPU) ReadData() uint8 {
	addr := p.Address.Get()
	p.IncrementVRAMAddr()
	var result uint8
	// Mirroring all PPU address space
	addr = addr & 0x3fff
	if addr <= 0x1FFF {
		result = p.InternalDataBuffer
		p.InternalDataBuffer = p.CHRRom[uint(addr)]
		return result
	} else if 0x2000 <= addr && addr <= 0x3eff {
		// Mirroring VRAM address space
		addr = addr & 0x2fff
		result = p.InternalDataBuffer
		p.InternalDataBuffer = p.VRam[uint(p.MirrorVRAMAddr(addr))]
		return result
	} else if 0x3F00 <= addr && addr <= 0x3FFF {
		addr = addr & 0x3F1F
		result = p.PaletteTable[uint(addr-0x3FF)]
		return result
	}
	return 0x0000
}
func (p *PPU) WriteToData(value uint8) {
	addr := p.Address.Get()
	// Mirroring all PPU address space
	addr = addr & 0x3fff
	if addr <= 0x1FFF {
		log.Fatalf("Try to write to CHR Rom Space")
	} else if 0x2000 <= addr && addr <= 0x3eff {
		// Mirroring VRAM address space
		addr = addr & 0x2fff
		p.VRam[uint(p.MirrorVRAMAddr(addr))] = value
	} else if 0x3F00 <= addr && addr <= 0x3FFF {
		addr = addr & 0x3F1F
		p.PaletteTable[uint(addr-0x3FF)] = value
	}
	p.IncrementVRAMAddr()
}

func New(chrRom []uint8, mirroring cartridge.MirroringType) *PPU {
	p := &PPU{
		CHRRom:       chrRom,
		PaletteTable: [32]uint8{},
		VRam:         [2048]uint8{},
		OMAData:      [256]uint8{},
		Mirroring:    mirroring,
		Address: register.AddressRegister{
			Value:      [2]uint8{},
			IsHighByte: true,
		},
	}
	return p
}
