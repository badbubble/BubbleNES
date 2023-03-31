package mapper

type Mapper000 struct {
	NumPRGBanks uint8
	NumCHRBanks uint8
}

func (m *Mapper000) CPUMapRead(addr uint16) (bool, uint16) {
	var mappedAddr uint16
	if addr >= 0x8000 && addr <= 0xFFFF {
		if m.NumPRGBanks > 1 {
			mappedAddr = addr & 0x7FFF
		} else {
			mappedAddr = addr & 0x3FFF
		}
		return true, mappedAddr
	}
	return false, 0x0000
}

func (m *Mapper000) CPUMapWrite(addr uint16) (bool, uint16) {
	var mappedAddr uint16

	if addr >= 0x8000 && addr <= 0xFFFF {
		if m.NumPRGBanks > 1 {
			mappedAddr = addr & 0x7FFF
		} else {
			mappedAddr = addr & 0x3FFF
		}
		return true, mappedAddr
	}
	return false, 0x0000
}

func (m *Mapper000) PPUMapRead(addr uint16) (bool, uint16) {
	var mappedAddr uint16
	if addr <= 0x1FFF {
		mappedAddr = addr
		return true, mappedAddr
	}
	return false, mappedAddr
}

func (m *Mapper000) PPUMapWrite(addr uint16) (bool, uint16) {
	var mappedAddr uint16

	if addr >= 0x0000 && addr <= 0x1FFF {
		mappedAddr = addr
		return true, mappedAddr
	}
	return false, mappedAddr
}

func (m *Mapper000) Reset() {

}
