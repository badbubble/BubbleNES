package ppu

import (
	"Nes/cartridge"
	"Nes/ppu/register"
	"Nes/ppu/render"
	"math/rand"
)

const (
	PaletteTableSize = 32
	VRamSize         = 2048
	OMADataSize      = 256
	FrameWidth       = 256
	FrameHeight      = 240
)

type PPU struct {
	PatternTable      [2][4096]uint8          //Pattern Tables, Address: 0x0000~0x1FFF
	NameTable         [2][1024]uint8          // Name Tables, Address: 0x2000~0x3EFF
	PaletteTable      [PaletteTableSize]uint8 // Palettes, Address: 0x3F00~0x3FFF
	Frame             [FrameWidth][FrameHeight][3]uint8
	PatternTableImage [2][128][128][3]uint8
	NameTableFrame    []uint8
	PatternTableFrame []uint8

	InternalDataBuffer uint8
	// Registers
	Controller register.ControllerRegister // 0x2000
	Mask       register.MaskRegister       // 0x2001
	Status     register.StatusRegister     // 0x2002
	OMAAddr    uint8                       // 0x2003
	OMAData    [OMADataSize]uint8          // 0x2004
	Scroll     register.ScrollRegister     // 0x2005
	Address    register.AddressRegister    // 0x2006

	Cart *cartridge.Cartridge

	Scanline        int
	Cycles          int
	IsFrameComplete bool
	NMI             bool

	// loopy registers
	VRamAddr register.Loopy
	TRamAddr register.Loopy

	AddressLatch bool
	FineX        uint8

	BGNextTileId     uint8
	BGNextTileAttrib uint8
	BGNextTileLSB    uint8
	BGNextTileMSB    uint8
}

func (p *PPU) SetFrame(x, y int, rgb [3]uint8) {
	if x >= 0 && x < FrameWidth && y >= 0 && y < FrameHeight {
		p.Frame[x][y] = rgb
	}
}

func (p *PPU) Clock() {
	//if p.Scanline >= -1 && p.Scanline < 240 {
	//	if p.Scanline == -1 && p.Cycles == 1 {
	//		p.Status.SetVBlankStatus(false)
	//	}
	//	if (p.Cycles >= 2 && p.Cycles <= 258) || (p.Cycles >= 321 && p.Cycles < 338) {
	//		switch (p.Cycles - 1) % 8 {
	//		case 0:
	//			p.BGNextTileId = p.PPURead(0x2000 | (p.VRamAddr.Data & 0x0FFF))
	//		case 2:
	//			//p.BGNextTileAttrib = p.PPURead(0x23C0 | (p.VRamAddr.NameTableY << 11))
	//		case 4:
	//		case 6:
	//		case 7:
	//
	//		}
	//	}
	//	if p.Cycles == 256 {
	//
	//	}
	//}
	//
	//if p.Scanline == 241 && p.Cycles == 1 {
	//	p.Status.SetVBlankStatus(true)
	//	if p.Controller.GenerateVBlankNMI() {
	//		p.NMI = true
	//	}
	//}
	if rand.Int()%2 == 0 {
		p.SetFrame(p.Cycles-1, p.Scanline, render.SystemPalette[0x3F])
	} else {
		p.SetFrame(p.Cycles-1, p.Scanline, render.SystemPalette[0x30])
	}
	p.Cycles += 1
	if p.Cycles >= 340 {
		p.Cycles = 0
		p.Scanline += 1
		if p.Scanline >= 260 {
			p.Scanline = 0
			p.IsFrameComplete = true
		}
	}

}

func (p *PPU) WriteToController(value uint8) {
	//isNMI := p.Controller.GenerateVBlankNMI()
	p.Controller.Update(value)
	//if !isNMI && p.Controller.GenerateVBlankNMI() && p.Status.IsInVBlank() {
	//	p.NMI = 1
	//}
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

func (p *PPU) ReadOMAData() uint8 {
	return p.OMAData[uint(p.OMAAddr)]
}

func (p *PPU) ReadData() uint8 {
	//addr := p.Address.Get()
	//p.IncrementVRAMAddr()
	//var result uint8
	//// Mirroring all PPU address space
	//addr = addr & 0x3fff
	//// TODO: there is a bug.
	//if addr <= 0x1FFF {
	//	result = p.InternalDataBuffer
	//	p.InternalDataBuffer = p.CHRRom[uint(addr)]
	//	return result
	//} else if 0x2000 <= addr && addr <= 0x2fff {
	//	// Mirroring VRAM address space
	//	addr = addr & 0x2fff
	//	result = p.InternalDataBuffer
	//	p.InternalDataBuffer = p.VRam[uint(p.MirrorVRAMAddr(addr))]
	//	return result
	//} else if 0x3F00 <= addr && addr <= 0x3FFF {
	//	addr = addr & 0x3F1F
	//	result = p.PaletteTable[uint(addr-0x3FF)]
	//	return result
	//}
	return 0x0000
}
func (p *PPU) WriteToData(value uint8) {
	//addr := p.Address.Get()
	//// Mirroring all PPU address space
	//addr = addr & 0x3fff
	//if addr <= 0x1FFF {
	//	log.Fatalf("Try to write to CHR Rom Space")
	//} else if 0x2000 <= addr && addr <= 0x3eff {
	//	// Mirroring VRAM address space
	//	addr = addr & 0x2fff
	//	p.R[uint(p.MirrorVRAMAddr(addr))] = value
	//} else if 0x3F00 <= addr && addr <= 0x3FFF {
	//	addr = addr & 0x3F1F
	//	p.PaletteTable[uint(addr-0x3FF)] = value
	//}
	p.IncrementVRAMAddr()
}

// GetPatternTable extract pattern table image from pattern table memory
func (p *PPU) GetPatternTable(i, palette int) {
	for nTileY := 0; nTileY < 16; nTileY++ {
		for nTileX := 0; nTileX < 16; nTileX++ {
			// oneline = 8(a tile) * 16(tiles) * 2(LSB, MSB) = 256
			// onetile = 8(a tile) * 2(LSB, MSB) = 16
			nOffset := nTileY*256 + nTileX*16
			for row := 0; row < 8; row++ {
				tileLSB := p.PPURead(uint16(i*0x1000 + nOffset + row + 0x0000))
				tileMSB := p.PPURead(uint16(i*0x1000 + nOffset + row + 0x0008))
				for col := 0; col < 8; col++ {
					pixel := ((tileLSB & 0x01) << 1) | (tileMSB & 0x01)
					tileMSB >>= 1
					tileLSB >>= 1
					p.PatternTableImage[i][nTileX*8+(7-col)][nTileY*8+row] = p.GetColorFromPalette(palette, pixel)
				}
			}
		}
	}
}

func (p *PPU) GetColorFromPalette(palette int, pixel uint8) [3]uint8 {
	return render.SystemPalette[p.PPURead(0x3F00+(uint16(palette)<<2)+uint16(pixel))]
}

// PPURead PPU read its own address map
func (p *PPU) PPURead(addr uint16) uint8 {
	// the max ppu memory map address is 0x3FFF
	addr &= 0x3FFF
	// cartridge map the address
	if isPPU, data := p.Cart.PPURead(addr); isPPU {
		return data
	} else if addr <= 0x1FFF { // Pattern Mem
		// There are two Pattern Tables
		//  0 from 0x0000 to 0x0FFF 4 KiB
		//  1 from 0x1000 to 0x1FFF 4 KiB

		// find which pattern table to use.
		whichPatternTable := (addr & 0x1000) >> 12
		// get relative address
		relativePatternTableAddress := addr & 0x0FFF
		return p.PatternTable[whichPatternTable][relativePatternTableAddress]

	} else if addr >= 0x2000 && addr <= 0x3EFF { // Name Table
		// There are four name tables
		// 0 from 0x2000 to 0x23FF 1KiB
		// 1 from 0x2400 to 0x27FF 1 KiB
		// 2 from 0x2800 to 0x2BFF 1 KiB
		// 3 from 0x2C00 to 0x2FFF 1 KiB
		// 0x3000~0x3EFF mirroring from 0x2000 to 0x2EFF

		// the size of NameTable is 4KiB
		addr &= 0x0FFF

		switch p.Cart.ScreenMirroring {
		case cartridge.Vertical:
			// Vertical:
			//   [ A ] [ B ]
			//   [ a ] [ b ]
			if addr <= 0x03FF {
				return p.NameTable[0][addr&0x03FF /*relative address in one name table*/]
			} else if addr >= 0x0400 && addr <= 0x07FF {
				return p.NameTable[1][addr&0x03FF]
			} else if addr >= 0x800 && addr <= 0x0BFF {
				return p.NameTable[0][addr&0x03FF]
			} else if addr >= 0x0C00 && addr <= 0x03FF {
				return p.NameTable[1][addr&0x03FF]
			}
		case cartridge.Horizontal:
			// Horizontal:
			//   [ A ] [ a ]
			//   [ B ] [ b ]

			if addr <= 0x03FF {
				return p.NameTable[0][addr&0x03FF /*relative address in one name table*/]
			} else if addr >= 0x0400 && addr <= 0x07FF {
				return p.NameTable[0][addr&0x03FF]
			} else if addr >= 0x800 && addr <= 0x0BFF {
				return p.NameTable[1][addr&0x03FF]
			} else if addr >= 0x0C00 && addr <= 0x03FF {
				return p.NameTable[1][addr&0x03FF]
			}
		}

	} else if addr >= 0x3F00 && addr <= 0x3FFF { // palette
		// BG
		// 0: 0x3F00
		// 1: 0x3F01 0x3F02 0x3F03 0x3F04[0x3F00]
		// 2: 0x3F05 0x3F06 0x3F07 0x3F08[0X3F00]
		// 3: 0x3F09 0x3F0A 0x3F0B 0x3F0C[0x3F00]
		// FG
		// 4: 0x3F0D 0x3F0E 0x3F0F 0x3F10[0x3F00]
		// 5: 0x3F11 0x3F12 0x3F13 0x3F14[0x3F00]
		// 6: 0x3F15 0x3F16 0x3F17 0x3F18[0x3F00]
		// 7: 0x3F19 0x3F1A 0x3F1B 0x3F1C[0x3F00]
		/*
			    There are a total of 28 bytes of palette memory (each byte being 6 bits wide).
			    Addresses $3F10, $3F14, $3F18, and $3F1C do not contain distinct data; instead,
				they access the data at $3F00, $3F04, $3F08, and $3F0C.
		*/
		addr &= 0x001F
		if addr == 0x0010 {
			addr = 0x0000
		}
		if addr == 0x0014 {
			addr = 0x0004
		}
		if addr == 0x0018 {
			addr = 0x0008
		}
		if addr == 0x001C {
			addr = 0x000C
		}
		return p.PaletteTable[addr]
	}
	return 0x0000
}

// PPUWrite PPU Writes to its own memory map
func (p *PPU) PPUWrite(addr uint16, data uint8) {
	// the max ppu memory map address is 0x3FFF
	addr &= 0x3FFF
	// cartridge map the address
	if isPPU := p.Cart.PPUWrite(addr, data); isPPU {
	} else if addr <= 0x1FFF { // Pattern Mem
		// There are two Pattern Tables
		//  0 from 0x0000 to 0x0FFF 4 KiB
		//  1 from 0x1000 to 0x1FFF 4 KiB

		// find which pattern table to use.
		whichPatternTable := (addr & 0x1000) >> 12
		// get relative address
		relativePatternTableAddress := addr & 0x0FFF
		p.PatternTable[whichPatternTable][relativePatternTableAddress] = data

	} else if addr >= 0x2000 && addr <= 0x3EFF { // Name Table
		// There are four name tables
		// 0 from 0x2000 to 0x23FF 1KiB
		// 1 from 0x2400 to 0x27FF 1 KiB
		// 2 from 0x2800 to 0x2BFF 1 KiB
		// 3 from 0x2C00 to 0x2FFF 1 KiB
		// 0x3000~0x3EFF mirroring from 0x2000 to 0x2EFF

		// the size of NameTable is 4KiB
		addr &= 0x0FFF

		switch p.Cart.ScreenMirroring {
		case cartridge.Vertical:
			// Vertical:
			//   [ A ] [ B ]
			//   [ a ] [ b ]
			if addr <= 0x03FF {
				p.NameTable[0][addr&0x03FF /*relative address in one name table*/] = data
			} else if addr >= 0x0400 && addr <= 0x07FF {
				p.NameTable[1][addr&0x03FF] = data
			} else if addr >= 0x800 && addr <= 0x0BFF {
				p.NameTable[0][addr&0x03FF] = data
			} else if addr >= 0x0C00 && addr <= 0x03FF {
				p.NameTable[1][addr&0x03FF] = data
			}
		case cartridge.Horizontal:
			// Horizontal:
			//   [ A ] [ a ]
			//   [ B ] [ b ]

			if addr <= 0x03FF {
				p.NameTable[0][addr&0x03FF /*relative address in one name table*/] = data
			} else if addr >= 0x0400 && addr <= 0x07FF {
				p.NameTable[0][addr&0x03FF] = data
			} else if addr >= 0x800 && addr <= 0x0BFF {
				p.NameTable[1][addr&0x03FF] = data
			} else if addr >= 0x0C00 && addr <= 0x03FF {
				p.NameTable[1][addr&0x03FF] = data
			}
		}

	} else if addr >= 0x3F00 && addr <= 0x3FFF { // palette
		// BG
		// 0: 0x3F00
		// 1: 0x3F01 0x3F02 0x3F03 0x3F04[0x3F00]
		// 2: 0x3F05 0x3F06 0x3F07 0x3F08[0X3F00]
		// 3: 0x3F09 0x3F0A 0x3F0B 0x3F0C[0x3F00]
		// FG
		// 4: 0x3F0D 0x3F0E 0x3F0F 0x3F10[0x3F00]
		// 5: 0x3F11 0x3F12 0x3F13 0x3F14[0x3F00]
		// 6: 0x3F15 0x3F16 0x3F17 0x3F18[0x3F00]
		// 7: 0x3F19 0x3F1A 0x3F1B 0x3F1C[0x3F00]

		addr &= 0x001F
		if addr == 0x0010 {
			addr = 0x0000
		}
		if addr == 0x0014 {
			addr = 0x0004
		}
		if addr == 0x0018 {
			addr = 0x0008
		}
		if addr == 0x001C {
			addr = 0x000C
		}
		p.PaletteTable[addr] = data
	}
}

func (p *PPU) CPUWrite(addr uint16, data uint8) {
	switch addr {
	case 0x0000: // Control
		p.WriteToController(data)
		p.TRamAddr.NameTableX = p.Controller.GetNameTableX()
		p.TRamAddr.NameTableY = p.Controller.GetNameTableY()
		break
	case 0x0001: // Mask
		p.WriteToMask(data)
	case 0x0002: // Status
		break
	case 0x0003: // OAM Address
		break
	case 0x0004: // OAM Data
		break
	case 0x0005: // Scroll
		if !p.AddressLatch {
			p.FineX = data & 0x07
			p.TRamAddr.CoarseX = data >> 3
			p.AddressLatch = true
		} else {
			p.TRamAddr.FineY = data & 0x07
			p.TRamAddr.CoarseY = data >> 3
			p.AddressLatch = false
		}
		break
	case 0x0006: // PPU Address
		if !p.AddressLatch {
			p.TRamAddr.Data = (p.TRamAddr.Data & 0x00FF) | (uint16(data) << 8)
			p.AddressLatch = true
		} else {
			p.TRamAddr.Data = (p.TRamAddr.Data & 0xFF00) | uint16(data)
			p.VRamAddr.Data = p.TRamAddr.Data
			p.AddressLatch = false
		}
	case 0x0007: // PPU Data
		p.PPUWrite(p.VRamAddr.Data, data)
		p.VRamAddr.Data += uint16(p.Controller.VRAMAddressIncrement())
	}

}
func (p *PPU) CPURead(addr uint16) uint8 {
	var data uint8
	switch addr {
	case 0x0000: // Control
		data = p.Controller.Status
	case 0x0001: // Mask
		data = p.Mask.Status
		p.Status.SetVBlankStatus(false)
		p.AddressLatch = false
	case 0x0002: // Status
		break
	case 0x0003: // OAM Address
		break
	case 0x0004: // OAM Data
		break
	case 0x0005: // Scroll
		break
	case 0x0006: // PPu Address
		break
	case 0x0007: // PPU Data
		data = p.InternalDataBuffer
		p.InternalDataBuffer = p.PPURead(p.VRamAddr.Data)
		if p.VRamAddr.Data >= 0x3F00 {
			data = p.InternalDataBuffer
		}

		p.VRamAddr.Data += uint16(p.Controller.VRAMAddressIncrement())
	}
	return data
}

func New(cart *cartridge.Cartridge) *PPU {
	p := &PPU{
		PatternTable: [2][4096]uint8{},
		PaletteTable: [32]uint8{},
		NameTable:    [2][1024]uint8{},
		OMAData:      [256]uint8{},
		Address: register.AddressRegister{
			Value:      [2]uint8{},
			IsHighByte: true,
		},
		Cart: cart,
	}
	return p
}
