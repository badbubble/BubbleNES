package cpu

func (cpu *CPU) NMI() {
	cpu.StackPushU16(cpu.PC)
	flag := cpu.Status
	cpu.SetFlag(B, false)
	cpu.SetFlag(U, true)
	cpu.StackPush(uint8(cpu.Status))
	cpu.Status = flag

	cpu.SetFlag(I, true)
	cpu.Bus.Tick(2)
	cpu.PC = 0xFFFA

}
