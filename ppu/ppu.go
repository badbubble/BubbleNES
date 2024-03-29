package ppu

import (
	"Nes/cartridge"
	"Nes/ppu/register"
	"fmt"
	"image"
	"image/color"
	"math"
)

const (
	PaletteTableSize = 32
	OMADataSize      = 256
	FrameWidth       = 256
	FrameHeight      = 240
	OneKiB           = 1024
)

var SystemPalette = [64]color.RGBA{
	{84, 84, 84, 255},
	{0, 30, 116, 255},
	{8, 16, 144, 255},
	{48, 0, 136, 255},
	{68, 0, 100, 255},
	{92, 0, 48, 255},
	{84, 4, 0, 255},
	{60, 24, 0, 255},
	{32, 42, 0, 255},
	{8, 58, 0, 255},
	{0, 64, 0, 255},
	{0, 60, 0, 255},
	{0, 50, 60, 255},
	{0, 0, 0, 255},
	{0, 0, 0, 255},
	{0, 0, 0, 255},
	{152, 150, 152, 255},
	{8, 76, 196, 255},
	{48, 50, 236, 255},
	{92, 30, 228, 255},
	{136, 20, 176, 255},
	{160, 20, 100, 255},
	{152, 34, 32, 255},
	{120, 60, 0, 255},
	{84, 90, 0, 255},
	{40, 114, 0, 255},
	{8, 124, 0, 255},
	{0, 118, 40, 255},
	{0, 102, 120, 255},
	{0, 0, 0, 255},
	{0, 0, 0, 255},
	{0, 0, 0, 255},
	{236, 238, 236, 255},
	{76, 154, 236, 255},
	{120, 124, 236, 255},
	{176, 98, 236, 255},
	{228, 84, 236, 255},
	{236, 88, 180, 255},
	{236, 106, 100, 255},
	{212, 136, 32, 255},
	{160, 170, 0, 255},
	{116, 196, 0, 255},
	{76, 208, 32, 255},
	{56, 204, 108, 255},
	{56, 180, 204, 255},
	{60, 60, 60, 255},
	{0, 0, 0, 255},
	{0, 0, 0, 255},
	{236, 238, 236, 255},
	{168, 204, 236, 255},
	{188, 188, 236, 255},
	{212, 178, 236, 255},
	{236, 174, 236, 255},
	{236, 174, 212, 255},
	{236, 180, 176, 255},
	{228, 196, 144, 255},
	{204, 210, 120, 255},
	{180, 222, 120, 255},
	{168, 226, 144, 255},
	{152, 226, 180, 255},
	{160, 214, 228, 255},
	{160, 162, 160, 255},
	{0, 0, 0, 255},
	{0, 0, 0, 255},
}

type PPU struct {
	PatternTable       [2][OneKiB * 4]uint8    //Pattern Tables, Address: 0x0000~0x1FFF
	NameTable          [2][OneKiB]uint8        // Name Tables, Address: 0x2000~0x3EFF
	PaletteTable       [PaletteTableSize]uint8 // Palettes, Address: 0x3F00~0x3FFF
	Frame              *image.RGBA             //[FrameWidth][FrameHeight][3]uint8
	PatternTableImage  [2]*image.RGBA
	InternalDataBuffer uint8
	// Registers
	Controller     register.ControllerRegister // 0x2000
	Mask           register.MaskRegister       // 0x2001
	Status         register.StatusRegister     // 0x2002
	OMAAddr        uint8                       // 0x2003
	OMAData        [OMADataSize]uint8          // 0x2004
	Scroll         register.ScrollRegister     // 0x2005
	Address        register.AddressRegister    // 0x2006
	Oam            [64]register.OAM
	OamAddr        uint8
	SpriteScanline [8]register.OAM
	SpriteCount    int

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

	BGShifterPatternLow    uint16
	BGShifterPatternHigh   uint16
	BGShifterAttributeLow  uint16
	BGShifterAttributeHigh uint16

	SpriteShifterPatternLow  [8]uint8
	SpriteShifterPatternHigh [8]uint8
	IsSpriteZeroHitPossible  bool
	IsSpriteZeroBeingRender  bool

	Debug bool
}

func (p *PPU) Clock() {
	if p.Scanline >= -1 && p.Scanline < 240 {
		if p.Scanline == 0 && p.Cycles == 0 {
			p.Cycles = 1
		}
		if p.Scanline == -1 && p.Cycles == 1 {

			p.Status.SetVBlankStatus(false)
			p.Status.SetSpriteOverflow(false)
			p.Status.SetSpriteZeroHit(false)

			for i := 0; i < 8; i++ {
				p.SpriteShifterPatternHigh[i] = 0x00
				p.SpriteShifterPatternLow[i] = 0x00
			}
		}
		if (p.Cycles >= 2 && p.Cycles < 258) || (p.Cycles >= 321 && p.Cycles < 338) {
			p.UpdateShifters()
			switch (p.Cycles - 1) % 8 {

			case 0:
				p.LoadBackgroundShifters()
				// fetch next tile id from nametables
				// nametable_y(1) + nametable_x(1) + coarse_y(5) + coarse_x(5)
				p.BGNextTileId = p.PPURead(0x2000 | (p.VRamAddr.Data & 0x0FFF))
				break
			case 2:
				// fetch next tile attribute
				// 2x32 = 64 attribute
				// 32 x 32 split into 8 x 8 x ( 4 x 4 ), which means that 4x4 tiles share one attribute byte
				// 32 x 32 / 4 = 8 x 8
				// attribute memory starts at 0x23C0
				// The attribute byte is assembled thus: BR(76) BL(54) TR(32) TL(10)
				//
				// +----+----+			    +----+----+
				// | TL | TR |			    | ID | ID |
				// +----+----+ where TL =   +----+----+
				// | BL | BR |			    | ID | ID |
				// +----+----+			    +----+----+
				p.BGNextTileAttrib = p.PPURead(0x23C0 | (uint16(p.VRamAddr.GetNameTableY()) << 11) |
					uint16(p.VRamAddr.GetNameTableX())<<10 |
					(uint16(p.VRamAddr.GetCoarseY())>>2)<<3 |
					uint16(p.VRamAddr.GetCoarseX())>>2)
				if p.VRamAddr.GetCoarseY()&0x02 != 0 {
					p.BGNextTileAttrib >>= 4
				}
				if p.VRamAddr.GetCoarseX()&0x02 != 0 {
					p.BGNextTileAttrib >>= 2
				}
				p.BGNextTileAttrib &= 0x03
				break
			case 4:
				// Fetch the next background tile LSB bit plane from the pattern memory
				p.BGNextTileLSB = p.PPURead(p.Controller.GetBackgroundPatternAddress() + uint16(p.BGNextTileId)<<4 +
					uint16(p.VRamAddr.GetFineY()) + 0)
				break
			case 6:
				// Fetch the next background tile MSB bit plane from the pattern memory
				p.BGNextTileMSB = p.PPURead(p.Controller.GetBackgroundPatternAddress() + uint16(p.BGNextTileId)<<4 +
					uint16(p.VRamAddr.GetFineY()) + 8)
				break
			case 7:
				p.IncrementScrollX()
				break
			}
		}
		if p.Cycles == 256 {
			p.IncrementScrollY()
		}
		if p.Cycles == 257 {
			p.LoadBackgroundShifters()
			p.TransferAddressX()
		}
		if p.Cycles == 338 || p.Cycles == 340 {
			p.BGNextTileId = p.PPURead(0x2000 | (p.VRamAddr.Data & 0x0FFF))
		}

		if p.Scanline == -1 && p.Cycles >= 280 && p.Cycles < 305 {
			p.TransferAddressY()
		}

		//fmt.Printf("C:%d S:%d\n", p.Cycles, p.Scanline)

		if p.Cycles == 257 && p.Scanline >= 0 {
			for i := 0; i < 8; i++ {
				p.SpriteScanline[i] = register.OAM{
					X:         0xFF,
					Y:         0xFF,
					TileID:    0xFF,
					Attribute: 0xFF,
				}
			}

			p.SpriteCount = 0
			NSprite := 0
			p.IsSpriteZeroHitPossible = false

			for NSprite < 64 && p.SpriteCount < 9 {
				diff := int16(p.Scanline) - int16(uint(p.Oam[NSprite].Y))
				//fmt.Printf("Cycle:%d Scanline:%d Y:%d Diff:%d NS:%d SC:%d\n", p.Cycles, p.Scanline, p.Oam[NSprite].Y, diff, NSprite, p.SpriteCount)

				if diff >= 0 && diff < int16(p.Controller.GerSpriteSize()) {
					if p.SpriteCount < 8 {
						if NSprite == 0 {
							p.IsSpriteZeroHitPossible = true
						}
						p.SpriteScanline[p.SpriteCount] = p.Oam[NSprite]
						//fmt.Printf("Cycle:%d Scanline:%d N:%d SC:%d X:%d Y:%d A:%d ID:%d\n", p.Cycles, p.Scanline, NSprite, p.SpriteCount, p.SpriteScanline[p.SpriteCount].X, p.SpriteScanline[p.SpriteCount].Y, p.SpriteScanline[p.SpriteCount].Attribute, p.SpriteScanline[p.SpriteCount].TileID)
						//for i := 0; i < 64; i++ {
						//	fmt.Printf("(%X|%X|%X|%X) ", p.Oam[i].Y, p.Oam[i].TileID, p.Oam[i].Attribute, p.Oam[i].X)
						//}
						//fmt.Println()
						p.SpriteCount += 1
					}
				}
				NSprite += 1
			}

			//fmt.Printf("Cycle:%d Scanline:%d SC:%d N:%d\n", p.Cycles, p.Scanline, p.SpriteCount, NSprite)
			//for i := 0; i < p.SpriteCount; i++ {
			//	fmt.Printf("(%X|%X|%X|%X) ", p.SpriteScanline[i].Y, p.SpriteScanline[i].TileID, p.SpriteScanline[i].Attribute, p.SpriteScanline[i].X)
			//}
			//fmt.Println()
			// overflow check
			if p.SpriteCount > 8 {
				p.Status.SetSpriteOverflow(true)
			}
		}

		if p.Cycles == 340 {
			for i := 0; i < p.SpriteCount; i++ {
				var highBits uint8
				var lowBits uint8
				var lowAddr uint16
				var highAddr uint16
				//fmt.Printf("(1) i:%d Cycle:%d Scanline:%d X:%d Y:%d A:%d ID:%d\n", i, p.Cycles, p.Scanline, p.SpriteScanline[i].X, p.SpriteScanline[i].Y, p.SpriteScanline[i].Attribute, p.SpriteScanline[i].TileID)

				if p.Controller.GerSpriteSize() == 0x08 {
					// 8 x 8
					if p.SpriteScanline[i].Attribute&0x80 == 0x00 {
						lowAddr = p.Controller.GetSpritePatternAddress() |
							(uint16(p.SpriteScanline[i].TileID) << 4) |
							uint16(p.Scanline-int(p.SpriteScanline[i].Y))
					} else {
						lowAddr = p.Controller.GetSpritePatternAddress() |
							(uint16(p.SpriteScanline[i].TileID) << 4) |
							7 - uint16(p.Scanline-int(p.SpriteScanline[i].Y))
					}
				} else {
					// 8 x 16 up
					// first
					if p.Scanline-int(p.SpriteScanline[i].Y) < 8 {
						// normal
						if p.SpriteScanline[i].Attribute&0x80 == 0x00 {
							lowAddr = (uint16(p.SpriteScanline[i].TileID&0x01) << 12) |
								(uint16(p.SpriteScanline[i].TileID&0xFE) << 4) |
								(uint16(p.Scanline-int(p.SpriteScanline[i].Y)) & 0x07)
						} else {
							// convert
							lowAddr = (uint16(p.SpriteScanline[i].TileID&0x01) << 12) |
								(uint16(p.SpriteScanline[i].TileID&0xFE) << 4) |
								(7 - (uint16(p.Scanline-int(p.SpriteScanline[i].Y)) & 0x07))
						}
					} else {
						// 8 x 16 down
						if p.SpriteScanline[i].Attribute&0x80 == 0x00 {
							lowAddr = (uint16(p.SpriteScanline[i].TileID&0x01) << 12) |
								(uint16((p.SpriteScanline[i].TileID&0xFE)+1) << 4) |
								(uint16(p.Scanline-int(p.SpriteScanline[i].Y)) & 0x07)
						} else {
							lowAddr = (uint16(p.SpriteScanline[i].TileID&0x01) << 12) |
								(uint16((p.SpriteScanline[i].TileID&0xFE)+1) << 4) |
								(7 - (uint16(p.Scanline-int(p.SpriteScanline[i].Y)) & 0x07))
						}

					}
				}
				highAddr = lowAddr + 8
				lowBits = p.PPURead(lowAddr)
				highBits = p.PPURead(highAddr)

				if p.SpriteScanline[i].Attribute&0x40 != 0 {
					lowBits = p.FlipByte(lowBits)
					highBits = p.FlipByte(highBits)
				}
				p.SpriteShifterPatternLow[i] = lowBits
				p.SpriteShifterPatternHigh[i] = highBits
				//fmt.Printf("(2) i:%d Cycle:%d Scanline:%d HB:%d LB:%d\n", i, p.Cycles, p.Scanline, highBits, lowBits)

				//fmt.Printf("(3) i:%d Cycle:%d Scanline:%d X:%d Y:%d A:%d ID:%d\n", i, p.Cycles, p.Scanline, p.SpriteScanline[i].X, p.SpriteScanline[i].Y, p.SpriteScanline[i].Attribute, p.SpriteScanline[i].TileID)

			}
		}
	}
	//if p.SpriteCount > 0 {
	//	fmt.Printf("(4) ")
	//	for i := 0; i < p.SpriteCount; i++ {
	//		fmt.Printf("(%X|%X|%X|%X) ", p.SpriteScanline[i].Y, p.SpriteScanline[i].TileID, p.SpriteScanline[i].Attribute, p.SpriteScanline[i].X)
	//	}
	//	fmt.Println()
	//}

	if p.Scanline == 240 {

	}

	if p.Scanline >= 241 && p.Scanline < 261 {
		if p.Scanline == 241 && p.Cycles == 1 {
			p.Status.SetVBlankStatus(true)
			if p.Controller.GenerateVBlankNMI() {
				p.NMI = true
			}
		}
	}

	var bgPixel uint8
	var bgPalette uint8

	if p.Mask.IsShowBG() {
		bitMux := uint16(0x8000 >> p.FineX)
		var p0Pixel uint8
		var p1Pixel uint8

		if p.BGShifterPatternLow&bitMux > 0 {
			p0Pixel = 1
		} else {
			p0Pixel = 0
		}

		if p.BGShifterPatternHigh&bitMux > 0 {
			p1Pixel = 1
		} else {
			p1Pixel = 0
		}
		bgPixel = (p1Pixel << 1) | p0Pixel

		var p0Pal uint8
		var p1Pal uint8

		if p.BGShifterAttributeLow&bitMux > 0 {
			p0Pal = 1
		} else {
			p0Pal = 0
		}

		if p.BGShifterAttributeHigh&bitMux > 0 {
			p1Pal = 1
		} else {
			p1Pal = 0
		}
		bgPalette = (p1Pal << 1) | p0Pal
	}

	var FGPixel uint8 = 0x00
	var FGPalette uint8 = 0x00
	var FGPriority uint8 = 0x00
	//fmt.Printf("C:%X Pixel:%X Palette:%X\n", p.Cycles, FGPixel, FGPalette)

	if p.Mask.IsShowSP() {

		p.IsSpriteZeroBeingRender = false
		for i := 0; i < p.SpriteCount; i++ {
			//fmt.Printf("(4) i:%d Cycle:%d Scanline:%d X:%d Y:%d A:%d ID:%d\n", i, p.Cycles, p.Scanline, p.SpriteScanline[i].X, p.SpriteScanline[i].Y, p.SpriteScanline[i].Attribute, p.SpriteScanline[i].TileID)
			if p.SpriteScanline[i].X == 0 {
				var FGPixelHigh uint8
				var FGPixelLow uint8
				if p.SpriteShifterPatternHigh[i]&0x80 != 0 {
					FGPixelHigh = 1
				}
				if p.SpriteShifterPatternLow[i]&0x80 != 0 {
					FGPixelLow = 1
				}

				FGPixel = FGPixelHigh<<1 | FGPixelLow
				FGPalette = p.SpriteScanline[i].Attribute&0x03 + 0x04
				if p.SpriteScanline[i].Attribute&0x20 == 0x00 {
					FGPriority = 0x01
				} else {
					FGPriority = 0x00
				}

				if FGPixel != 0x00 {
					if i == 0 {
						p.IsSpriteZeroBeingRender = true
					}
					break
				}
			}
		}
	}
	var Pixel uint8 = 0x00
	var Palette uint8 = 0x00

	if bgPixel == 0x00 && FGPixel == 0x00 {
		Pixel = 0x00
		Palette = 0x00
	} else if bgPixel == 0x00 && FGPixel > 0 {
		Pixel = FGPixel
		Palette = FGPalette
	} else if bgPixel > 0x00 && FGPixel == 0x00 {
		Pixel = bgPixel
		Palette = bgPalette
	} else {
		if FGPriority == 0x01 {
			Pixel = FGPixel
			Palette = FGPalette
		} else {
			Pixel = bgPixel
			Palette = bgPalette
		}

		if p.IsSpriteZeroBeingRender && p.IsSpriteZeroHitPossible {
			if !(p.Mask.IsShowBGInLeftMost8P() || p.Mask.IsShowSPInLeftMost8P()) {
				if p.Cycles >= 9 && p.Cycles < 258 {
					p.Status.SetSpriteZeroHit(true)
				}
			} else {
				if p.Cycles >= 1 && p.Cycles < 258 {
					p.Status.SetSpriteZeroHit(true)
				}
			}
		}
	}

	p.Frame.SetRGBA(p.Cycles-1,
		p.Scanline,
		SystemPalette[p.PPURead(0x3F00+(uint16(Palette)<<2)+uint16(Pixel))])
	if p.Debug {
		fmt.Printf("Scanline:%d Cycles:%d NTID:%d NTAID:%d R:%X G:%X B:%X\n", p.Scanline, p.Cycles, p.BGNextTileId, p.BGNextTileAttrib,
			SystemPalette[p.PPURead(0x3F00+(uint16(bgPalette)<<2)+uint16(bgPixel))&0x3F].R,
			SystemPalette[p.PPURead(0x3F00+(uint16(bgPalette)<<2)+uint16(bgPixel))&0x3F].G,
			SystemPalette[p.PPURead(0x3F00+(uint16(bgPalette)<<2)+uint16(bgPixel))&0x3F].B)
	}

	p.Cycles += 1

	if p.Cycles >= 341 {
		p.Cycles = 0
		p.Scanline += 1
		if p.Scanline >= 261 {
			p.Scanline = -1
			p.IsFrameComplete = true
		}
	}
}

func (p *PPU) WriteToController(value uint8) {
	//isNMI := p.Controller.GenerateVBlankNMI()
	p.Controller.Update(value)
	//if !isNMI && p.Controller.GenerateVBlankNMI() && p.Status.IsInVBlank() {
	//	p.NMI = 1
	//}p
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
					//pixel := tileMSB&0x01 + tileLSB&0x01
					tileMSB >>= 1
					tileLSB >>= 1
					p.PatternTableImage[i].SetRGBA(nTileX*8+(7-col),
						nTileY*8+row,
						SystemPalette[p.PPURead(0x3F00+(uint16(palette)<<2)+uint16(pixel))],
					)
					// fmt.Printf("x:%d y:%d offset:%d msb:%X lsb:%X palette:%d pixel:%d r:%X g:%X b:%X address:%X Resp:%X\n", nTileX*8+(7-col), nTileY*8+row, nOffset, tileMSB, tileLSB, palette, pixel,
					//	p.GetColorFromPalette(palette, pixel).R, p.GetColorFromPalette(palette, pixel).G, p.GetColorFromPalette(palette, pixel).B, 0x3F00+(uint16(palette)<<2)+uint16(pixel), p.PPURead(0x3F00+(uint16(palette)<<2)+uint16(pixel))&0x3F)

				}
			}
		}
	}
}

//func (p *PPU) GetColorFromPalette(palette int, pixel uint8) *color.RGBA {
//	return SystemPalette[p.PPURead(0x3F00+(uint16(palette)<<2)+uint16(pixel))&0x3F]
//}

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

func (p *PPU) WriteToOAM(addr uint8, data uint8) {

	idx := uint8(math.Floor(float64(addr / 4)))

	switch addr % 4 {
	case 0:
		p.Oam[idx].Y = data
	case 1:
		p.Oam[idx].TileID = data
	case 2:
		p.Oam[idx].Attribute = data
	case 3:
		p.Oam[idx].X = data
	}
}

func (p *PPU) ReadFromOAM(addr uint8) (data uint8) {

	idx := uint8(math.Floor(float64(addr / 4)))

	//fmt.Printf("Addr:%X tmpAddr:%X inAddr:%X\n", p.OamAddr, tmpAddr, tmpAddr%4)
	switch addr % 4 {
	case 0:
		data = p.Oam[idx].Y
	case 1:
		data = p.Oam[idx].TileID
	case 2:
		data = p.Oam[idx].Attribute
	case 3:
		data = p.Oam[idx].X
	}
	return
}

func (p *PPU) CPUWrite(addr uint16, data uint8) {
	switch addr {
	case 0x0000: // Control
		p.WriteToController(data)
		p.TRamAddr.SetNameTableX(p.Controller.GetNameTableX())
		p.TRamAddr.SetNameTableY(p.Controller.GetNameTableY())
		break
	case 0x0001: // Mask
		p.WriteToMask(data)
	case 0x0002: // Status
		break
	case 0x0003: // OAM Address
		p.OamAddr = data
	case 0x0004: // OAM Data
		p.WriteToOAM(p.OamAddr, data)
		break
	case 0x0005: // Scroll
		if !p.AddressLatch {
			p.FineX = data & 0x07
			p.TRamAddr.SetCoarseX(data >> 3)
			p.AddressLatch = true
		} else {
			p.TRamAddr.SetFineY(data)
			p.TRamAddr.SetCoarseY(data >> 3)
			p.AddressLatch = false
		}
		break
	case 0x0006: // PPU Address
		if !p.AddressLatch {
			p.TRamAddr.Data = (uint16(data)&0x3F)<<8 | (p.TRamAddr.Data & 0x00FF)
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

func (p *PPU) CPURead(addr uint16, isTrace bool) uint8 {
	var data uint8
	if isTrace {
		switch addr {
		case 0x0000: // Control
			data = p.Controller.Status
			break
		case 0x0001: // Mask
			data = p.Mask.Status
			break
		case 0x0002: // Status
			data = p.Status.Status&0xE0 | p.InternalDataBuffer&0x1F
			break
		case 0x0003: // OAM Address
			break
		case 0x0004: // OAM Data
			data = p.ReadFromOAM(p.OamAddr)
			break
		case 0x0005: // Scroll
			break
		case 0x0006: // PPu Address
			break
		case 0x0007: // PPU Data
			data = p.InternalDataBuffer

		}

		return data
	}
	switch addr {
	case 0x0000: // Control
		data = p.Controller.Status
	case 0x0001: // Mask
		data = p.Mask.Status
		p.Status.SetVBlankStatus(false)
		p.AddressLatch = false
	case 0x0002: // Status
		data = p.Status.Status&0xE0 | p.InternalDataBuffer&0x1F
		p.Status.SetVBlankStatus(false)
		p.AddressLatch = false
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
		PatternTableImage: [2]*image.RGBA{image.NewRGBA(image.Rect(0, 0, 128, 128)),
			image.NewRGBA(image.Rect(0, 0, 128, 128))},
		Frame: image.NewRGBA(image.Rect(0, 0, FrameWidth, FrameHeight)),
	}
	return p
}

func (p *PPU) IncrementScrollX() {
	if p.Mask.IsShowBG() || p.Mask.IsShowSP() {
		// 32 * 30
		if p.VRamAddr.GetCoarseX() == 31 {
			p.VRamAddr.SetCoarseX(0x00)
			p.VRamAddr.SetNameTableX(^p.VRamAddr.GetNameTableX())
		} else {
			p.VRamAddr.SetCoarseX(p.VRamAddr.GetCoarseX() + 1)
		}
	}
}

func (p *PPU) IncrementScrollY() {
	if p.Mask.IsShowBG() || p.Mask.IsShowSP() {
		if p.VRamAddr.GetFineY() < 7 {
			p.VRamAddr.SetFineY(p.VRamAddr.GetFineY() + 1)
		} else {
			p.VRamAddr.SetFineY(0x00)
			if p.VRamAddr.GetCoarseY() == 29 {
				p.VRamAddr.SetCoarseY(0x00)
				p.VRamAddr.SetNameTableY(^p.VRamAddr.GetNameTableY())
			} else if p.VRamAddr.GetCoarseY() == 31 {
				p.VRamAddr.SetCoarseY(0x00)
			} else {
				p.VRamAddr.SetCoarseY(p.VRamAddr.GetCoarseY() + 1)
			}
		}
	}
}

func (p *PPU) TransferAddressX() {
	if p.Mask.IsShowBG() || p.Mask.IsShowSP() {
		p.VRamAddr.SetNameTableX(p.TRamAddr.GetNameTableX())
		p.VRamAddr.SetCoarseX(p.TRamAddr.GetCoarseX())
	}
}

func (p *PPU) TransferAddressY() {
	if p.Mask.IsShowBG() || p.Mask.IsShowSP() {
		p.VRamAddr.SetNameTableY(p.TRamAddr.GetNameTableY())
		p.VRamAddr.SetCoarseY(p.TRamAddr.GetCoarseY())
		p.VRamAddr.SetFineY(p.TRamAddr.GetFineY())

	}
}

func (p *PPU) LoadBackgroundShifters() {
	p.BGShifterPatternLow = (p.BGShifterPatternLow & 0xFF00) | uint16(p.BGNextTileLSB)
	p.BGShifterPatternHigh = (p.BGShifterPatternHigh & 0xFF00) | uint16(p.BGNextTileMSB)
	if p.BGNextTileAttrib&0b01 != 0 {
		p.BGShifterAttributeLow = (p.BGShifterAttributeLow & 0xFF00) | 0xFF
	} else {
		p.BGShifterAttributeLow = (p.BGShifterAttributeLow & 0xFF00) | 0x00
	}

	if p.BGNextTileAttrib&0b10 != 0 {
		p.BGShifterAttributeHigh = (p.BGShifterAttributeHigh & 0xFF00) | 0xFF
	} else {
		p.BGShifterAttributeHigh = (p.BGShifterAttributeHigh & 0xFF00) | 0x00
	}

}

func (p *PPU) UpdateShifters() {
	if p.Mask.IsShowBG() {
		p.BGShifterAttributeHigh <<= 1
		p.BGShifterAttributeLow <<= 1
		p.BGShifterPatternHigh <<= 1
		p.BGShifterPatternLow <<= 1
	}
	if p.Mask.IsShowSP() && p.Cycles >= 1 && p.Cycles < 258 {
		for i := 0; i < p.SpriteCount; i++ {
			if p.SpriteScanline[i].X > 0 {
				p.SpriteScanline[i].X -= 1
			} else {
				p.SpriteShifterPatternLow[i] <<= 1
				p.SpriteShifterPatternHigh[i] <<= 1
			}
		}
	}
}

func (p *PPU) FlipByte(data uint8) uint8 {
	data = (data&0xF0)>>4 | (data&0x0F)<<4
	data = (data&0xCC)>>2 | (data&0x33)<<2
	data = (data&0xAA)>>1 | (data&0x55)<<1
	return data
}

func (p *PPU) Reset() {
	p.FineX = 0x00
	p.AddressLatch = false
	p.InternalDataBuffer = 0x00
	p.Scanline = 0
	p.Cycles = 0
	p.BGNextTileId = 0x00
	p.BGNextTileAttrib = 0x00
	p.BGNextTileMSB = 0x00
	p.BGNextTileLSB = 0x00
	p.BGShifterAttributeLow = 0x0000
	p.BGShifterAttributeHigh = 0x0000
	p.BGShifterPatternLow = 0x0000
	p.BGShifterPatternHigh = 0x0000
	p.Status.Status = 0x00
	p.Mask.Status = 0x00
	p.Controller.Status = 0x00
	p.VRamAddr.Data = 0x0000
	p.TRamAddr.Data = 0x0000
}
