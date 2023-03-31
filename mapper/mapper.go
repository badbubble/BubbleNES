package mapper

type Mapper interface {
	PPUMapRead(addr uint16) (bool, uint16)
	PPUMapWrite(addr uint16) (bool, uint16)
	CPUMapRead(addr uint16) (bool, uint16)
	CPUMapWrite(addr uint16) (bool, uint16)
	Reset()
}
