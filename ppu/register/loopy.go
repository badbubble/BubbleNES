package register

type Loopy struct {
	Data uint16
}

func (l *Loopy) SetCoarseX(value uint8) {
	l.Data &= 0b1111_1111_1110_0000
	l.Data |= uint16(value & 0b0001_1111)
}

func (l *Loopy) GetCoarseX() uint8 {
	return uint8(l.Data & 0b0000_0000_0001_1111)
}

func (l *Loopy) SetCoarseY(value uint8) {
	l.Data &= 0b1111_1100_0001_1111
	l.Data |= uint16(value&0b0001_1111) << 5
}

func (l *Loopy) GetCoarseY() uint8 {
	return uint8((l.Data & 0b0000_0111_110_0000) >> 5)
}
func (l *Loopy) SetNameTableX(value uint8) {
	l.Data &= 0b1111_1011_1111_1111
	l.Data |= uint16(value&0x01) << 10
}

func (l *Loopy) GetNameTableX() uint8 {
	return uint8((l.Data & 0b0000_0100_0000_0000) >> 10)
}

func (l *Loopy) SetNameTableY(value uint8) {
	l.Data &= 0b1111_0111_1111_1111
	l.Data |= uint16(value&0x01) << 11
}

func (l *Loopy) GetNameTableY() uint8 {
	return uint8((l.Data & 0b0000_1000_0000_0000) >> 11)
}

func (l *Loopy) SetFineY(value uint8) {
	l.Data &= 0b1000_1111_1111_1111
	l.Data |= uint16(value&0b0000_0111) << 12
}
func (l *Loopy) GetFineY() uint8 {
	return uint8((l.Data & 0b0111_0000_0000_0000) >> 12)
}
