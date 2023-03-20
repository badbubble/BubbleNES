package cartridge

import (
	"Nes/mapper"
	"log"
)

type MirroringType string

var (
	NesFileHeader = []uint8{0x4E, 0x45, 0x53, 0x1A} // string used to recognize .NES files
)

const (
	// PRGRomPageSize 16KB
	PRGRomPageSize uint = 16384
	// CHRRomPageSize 8KB
	CHRRomPageSize uint = 8192
)

const (
	Vertical   MirroringType = "VERTICAL"
	Horizontal MirroringType = "HORIZONTAL"
	FourScreen MirroringType = "FOUR_SCREEN"
)

type Cartridge struct {
	// for code, connected to cpu
	PRGRom []uint8
	// for visual graphic, connect to ppu
	CHRRom []uint8
	// provide access to extended rom memory for both prg and chr
	MapperID        uint8
	ScreenMirroring MirroringType
	Mapper          mapper.Mapper
}

func (c *Cartridge) CPURead(addr uint16) (bool, uint8) {
	if isMapped, mappedAddr := c.Mapper.CPUMapRead(addr); isMapped {
		data := c.PRGRom[mappedAddr]
		return true, data
	} else {
		return false, 0x0000
	}
}

func (c *Cartridge) CPUWrite(addr uint16, data uint8) bool {
	if isMapped, mappedAddr := c.Mapper.CPUMapWrite(addr); isMapped {
		c.PRGRom[mappedAddr] = data
		return true
	} else {
		return false
	}
}

func (c *Cartridge) PPURead(addr uint16) (bool, uint8) {
	if isMapped, mappedAddr := c.Mapper.PPUMapRead(addr); isMapped {
		data := c.CHRRom[mappedAddr]
		return true, data
	} else {
		return false, 0x0000
	}

}
func (c *Cartridge) PPUWrite(addr uint16, data uint8) bool {
	if isMapped, mappedAddr := c.Mapper.PPUMapWrite(addr); isMapped {
		c.CHRRom[mappedAddr] = data
		return true
	} else {
		return false
	}
}

func New(raw []uint8) *Cartridge {
	for idx, b := range raw[0:4] {
		if b != NesFileHeader[idx] {
			log.Fatalf("File is not a iNES file format")
		}
	}

	// Byte 6 - ROM Control Byte 1
	// /  7 6 5 4 3 2 1 0
	// /  | | | | | | | +--- 1 for Vertical, 0 for Horizontal
	// /  | | | | | | +----- 1 for battery-backed RAM at $6000-$7FFFF
	// /  | | | | | +------- 1 for a 512-byte trainer at $7000-$71FF
	// /  | | | | +--------- 1 for FourScreen VRAM layout
	// /  | | | | ----------- Four Lower bits of ROM Mapper Type
	firstControlByte := raw[6]

	// Byte 7 - ROM Control Byte 2
	// /  7 6 5 4 3 2 1 0
	// /  | | | | | | | +--- 0 for iNES 1.0
	// /  | | | | | | +----- 0 for iNES 1.0
	// /  | | | | | +------- if bit(3,2) == 10 == iNES 2.0, if bit(3,2) == 00 == iNES 1.0
	// /  | | | | ----------- Four upper bits of ROM Mapper Type
	SecondControlByte := raw[7]

	// get mapper value
	mapperID := (SecondControlByte & 0b1111_0000) | (firstControlByte >> 4)
	if version := SecondControlByte >> 2 & 0b11; version != 0 {
		log.Fatalf("iNES2.0 is not support")
	}

	isFourScreen := firstControlByte&0b0000_1000 != 0
	isVerticalMirroring := firstControlByte&0b0000_0001 != 0
	var screenMirroring MirroringType
	if isFourScreen {
		screenMirroring = FourScreen
	} else if !isFourScreen && isVerticalMirroring {
		screenMirroring = Vertical
	} else {
		screenMirroring = Horizontal
	}

	prgRomSize := uint(raw[4]) * PRGRomPageSize
	chrRomSize := uint(raw[5]) * CHRRomPageSize

	var pMapper mapper.Mapper
	switch mapperID {
	case 0:
		pMapper = &mapper.Mapper000{
			NumPRGBanks: raw[4],
			NumCHRBanks: raw[5],
		}
	}

	isTrainer := raw[6]&0b0000_0100 != 0
	// header has 16bytes
	prgRomStart := 16
	if isTrainer {
		// trainer has 512bytes
		prgRomStart = 16 + 512
	}

	chrRomStart := uint(prgRomStart) + prgRomSize
	return &Cartridge{
		PRGRom:          raw[prgRomStart : uint(prgRomStart)+prgRomSize],
		CHRRom:          raw[chrRomStart : chrRomStart+chrRomSize],
		MapperID:        mapperID,
		Mapper:          pMapper,
		ScreenMirroring: screenMirroring,
	}
}
