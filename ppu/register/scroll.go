package register

type ScrollRegister struct {
	X     uint8
	Y     uint8
	Latch bool
}

func (sr *ScrollRegister) Write(data uint8) {
	if !sr.Latch {
		sr.X = data
	} else {
		sr.Y = data
	}
	sr.Latch = !sr.Latch
}

func (sr *ScrollRegister) ResetLatch() {
	sr.Latch = false
}
