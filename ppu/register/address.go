package register

type AddressRegister struct {
	Value      [2]uint8
	IsHighByte bool
}

func (ar *AddressRegister) Set(data uint16) {
	hi := uint8(data >> 8)
	lo := uint8(data & 0x00FF)
	ar.Value[0] = hi
	ar.Value[1] = lo
}

func (ar *AddressRegister) Get() uint16 {
	return uint16(ar.Value[0])<<8 | uint16(ar.Value[1])
}

func (ar *AddressRegister) Update(data uint8) {
	if ar.IsHighByte {
		ar.Value[0] = data
	} else {
		ar.Value[1] = data
	}
	if ar.Get() > 0x3FFF {
		ar.Set(ar.Get() & 0x3FFF)
	}
	ar.IsHighByte = !ar.IsHighByte
}

func (ar *AddressRegister) Increment(inc uint8) {
	lo := ar.Value[1]
	ar.Value[1] += inc
	if lo > ar.Value[1] {
		ar.Value[0] += 1
	}
	if ar.Get() > 0x3FFF {
		ar.Set(ar.Get() & 0x3FFF)
	}
}

func (ar *AddressRegister) ResetLatch() {
	ar.IsHighByte = true
}
