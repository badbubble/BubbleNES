package cpu

import "fmt"

// StatusFlag
// /  7 6 5 4 3 2 1 0
// /  N V _ B D I Z C
// /  | |   | | | | +--- Carry Flag
// /  | |   | | | +----- Zero Flag
// /  | |   | | +------- Interrupt Disable
// /  | |   | +--------- Decimal Mode (not used on NES)
// /  | |   +----------- Break Command
// /  | +--------------- Overflow Flag
// /  +----------------- Negative Flag
type StatusFlag uint8

const (
	C StatusFlag = 0b0000_0001
	Z StatusFlag = 0b0000_0010
	I StatusFlag = 0b0000_0100
	D StatusFlag = 0b0000_1000
	B StatusFlag = 0b0001_0000
	U StatusFlag = 0b0010_0000
	V StatusFlag = 0b0100_0000
	N StatusFlag = 0b1000_0000
)

const (
	StackOffset uint16 = 0x0100
	StackTop    uint8  = 0xfd
)

type Disassembly struct {
	addr        uint16
	Instruction InstructionName
	Mode        AddressMode
}

type CPU struct {
	X      uint8
	Y      uint8
	A      uint8
	Status StatusFlag
	PC     uint16
	SP     uint8

	Memory [0xFFFF]uint8

	Lookup  map[uint8]*Instruction
	Commons []Disassembly
}

func New() *CPU {
	cpu := &CPU{
		X:      0x00,
		Y:      0x00,
		A:      0x00,
		Status: 0x00,
		SP:     StackTop,
		PC:     0x00,
	}
	cpu.Lookup = make(map[uint8]*Instruction)
	cpu.addInstructions()
	return cpu

}
func (cpu *CPU) addDisassembly(name InstructionName, mode AddressMode, addr uint16) {
	fmt.Println(name, mode, addr)
	cpu.Commons = append(cpu.Commons, Disassembly{
		addr:        addr,
		Instruction: name,
		Mode:        mode,
	})
}

func (cpu *CPU) addInstructions() {
	cpu.Lookup[0x00] = NewInstruction(BRK, 1, 7, Implied)
	cpu.Lookup[0xea] = NewInstruction(NOP, 1, 2, Implied)

	/* Arithmetic */
	// ADC
	cpu.Lookup[0x69] = NewInstruction(ADC, 2, 2, Immediate)
	cpu.Lookup[0x65] = NewInstruction(ADC, 2, 3, ZeroPage)
	cpu.Lookup[0x75] = NewInstruction(ADC, 2, 4, ZeroPageX)
	cpu.Lookup[0x6d] = NewInstruction(ADC, 3, 4, Absolute)
	cpu.Lookup[0x7d] = NewInstruction(ADC, 3, 4 /*+1 if page crossed*/, AbsoluteX)
	cpu.Lookup[0x79] = NewInstruction(ADC, 3, 4 /*+1 if page crossed*/, AbsoluteY)
	cpu.Lookup[0x61] = NewInstruction(ADC, 2, 6, IndirectX)
	cpu.Lookup[0x71] = NewInstruction(ADC, 2, 5 /*+1 if page crossed*/, IndirectY)

	// SBC
	cpu.Lookup[0xe9] = NewInstruction(SBC, 2, 2, Immediate)
	cpu.Lookup[0xe5] = NewInstruction(SBC, 2, 3, ZeroPage)
	cpu.Lookup[0xf5] = NewInstruction(SBC, 2, 4, ZeroPageX)
	cpu.Lookup[0xed] = NewInstruction(SBC, 3, 4, Absolute)
	cpu.Lookup[0xfd] = NewInstruction(SBC, 3, 4 /*+1 if page crossed*/, AbsoluteX)
	cpu.Lookup[0xf9] = NewInstruction(SBC, 3, 4 /*+1 if page crossed*/, AbsoluteY)
	cpu.Lookup[0xe1] = NewInstruction(SBC, 2, 6, IndirectX)
	cpu.Lookup[0xf1] = NewInstruction(SBC, 2, 5 /*+1 if page crossed*/, IndirectY)

	// AND
	cpu.Lookup[0x29] = NewInstruction(AND, 2, 2, Immediate)
	cpu.Lookup[0x25] = NewInstruction(AND, 2, 3, ZeroPage)
	cpu.Lookup[0x35] = NewInstruction(AND, 2, 4, ZeroPageX)
	cpu.Lookup[0x2d] = NewInstruction(AND, 3, 4, Absolute)
	cpu.Lookup[0x3d] = NewInstruction(AND, 3, 4 /*+1 if page crossed*/, AbsoluteX)
	cpu.Lookup[0x39] = NewInstruction(AND, 3, 4 /*+1 if page crossed*/, AbsoluteY)
	cpu.Lookup[0x21] = NewInstruction(AND, 2, 6, IndirectX)
	cpu.Lookup[0x31] = NewInstruction(AND, 2, 5 /*+1 if page crossed*/, IndirectY)

	//EOR
	cpu.Lookup[0x49] = NewInstruction(EOR, 2, 2, Immediate)
	cpu.Lookup[0x45] = NewInstruction(EOR, 2, 3, ZeroPage)
	cpu.Lookup[0x55] = NewInstruction(EOR, 2, 4, ZeroPageX)
	cpu.Lookup[0x4d] = NewInstruction(EOR, 3, 4, Absolute)
	cpu.Lookup[0x5d] = NewInstruction(EOR, 3, 4 /*+1 if page crossed*/, AbsoluteX)
	cpu.Lookup[0x59] = NewInstruction(EOR, 3, 4 /*+1 if page crossed*/, AbsoluteY)
	cpu.Lookup[0x41] = NewInstruction(EOR, 2, 6, IndirectX)
	cpu.Lookup[0x51] = NewInstruction(EOR, 2, 5 /*+1 if page crossed*/, IndirectY)

	//ORA
	cpu.Lookup[0x09] = NewInstruction(ORA, 2, 2, Immediate)
	cpu.Lookup[0x05] = NewInstruction(ORA, 2, 3, ZeroPage)
	cpu.Lookup[0x15] = NewInstruction(ORA, 2, 4, ZeroPageX)
	cpu.Lookup[0x0d] = NewInstruction(ORA, 3, 4, Absolute)
	cpu.Lookup[0x1d] = NewInstruction(ORA, 3, 4 /*+1 if page crossed*/, AbsoluteX)
	cpu.Lookup[0x19] = NewInstruction(ORA, 3, 4 /*+1 if page crossed*/, AbsoluteY)
	cpu.Lookup[0x01] = NewInstruction(ORA, 2, 6, IndirectX)
	cpu.Lookup[0x11] = NewInstruction(ORA, 2, 5 /*+1 if page crossed*/, IndirectY)

	/* Shifts */
	// ASL
	cpu.Lookup[0x0a] = NewInstruction(ASL, 1, 2, Accumulator)
	cpu.Lookup[0x06] = NewInstruction(ASL, 2, 5, ZeroPage)
	cpu.Lookup[0x16] = NewInstruction(ASL, 2, 6, ZeroPageX)
	cpu.Lookup[0x0e] = NewInstruction(ASL, 3, 6, Absolute)
	cpu.Lookup[0x1e] = NewInstruction(ASL, 3, 7, AbsoluteX)

	// LSR
	cpu.Lookup[0x4a] = NewInstruction(LSR, 1, 2, Accumulator)
	cpu.Lookup[0x46] = NewInstruction(LSR, 2, 5, ZeroPage)
	cpu.Lookup[0x56] = NewInstruction(LSR, 2, 6, ZeroPageX)
	cpu.Lookup[0x4e] = NewInstruction(LSR, 3, 6, Absolute)
	cpu.Lookup[0x5e] = NewInstruction(LSR, 3, 7, AbsoluteX)

	// ROL
	cpu.Lookup[0x2a] = NewInstruction(ROL, 1, 2, Accumulator)
	cpu.Lookup[0x26] = NewInstruction(ROL, 2, 5, ZeroPage)
	cpu.Lookup[0x36] = NewInstruction(ROL, 2, 6, ZeroPageX)
	cpu.Lookup[0x2e] = NewInstruction(ROL, 3, 6, Absolute)
	cpu.Lookup[0x3e] = NewInstruction(ROL, 3, 7, AbsoluteX)

	// ROR
	cpu.Lookup[0x6a] = NewInstruction(ROR, 1, 2, Accumulator)
	cpu.Lookup[0x66] = NewInstruction(ROR, 2, 5, ZeroPage)
	cpu.Lookup[0x76] = NewInstruction(ROR, 2, 6, ZeroPageX)
	cpu.Lookup[0x6e] = NewInstruction(ROR, 3, 6, Absolute)
	cpu.Lookup[0x7e] = NewInstruction(ROR, 3, 7, AbsoluteX)

	// INC
	cpu.Lookup[0xe6] = NewInstruction(INC, 2, 5, ZeroPage)
	cpu.Lookup[0xf6] = NewInstruction(INC, 2, 6, ZeroPageX)
	cpu.Lookup[0xee] = NewInstruction(INC, 3, 6, Absolute)
	cpu.Lookup[0xfe] = NewInstruction(INC, 3, 7, AbsoluteX)

	// DEC
	cpu.Lookup[0xc6] = NewInstruction(DEC, 2, 5, ZeroPage)
	cpu.Lookup[0xd6] = NewInstruction(DEC, 2, 6, ZeroPageX)
	cpu.Lookup[0xce] = NewInstruction(DEC, 3, 6, Absolute)
	cpu.Lookup[0xde] = NewInstruction(DEC, 3, 7, AbsoluteX)

	// INX
	cpu.Lookup[0xe8] = NewInstruction(INX, 1, 2, Implied)
	// DEX
	cpu.Lookup[0xca] = NewInstruction(DEX, 1, 2, Implied)
	// INY
	cpu.Lookup[0xc8] = NewInstruction(INY, 1, 2, Implied)
	// DEY
	cpu.Lookup[0x88] = NewInstruction(DEY, 1, 2, Implied)

	//CMP
	cpu.Lookup[0xc9] = NewInstruction(CMP, 2, 2, Immediate)
	cpu.Lookup[0xc5] = NewInstruction(CMP, 2, 3, ZeroPage)
	cpu.Lookup[0xd5] = NewInstruction(CMP, 2, 4, ZeroPageX)
	cpu.Lookup[0xcd] = NewInstruction(CMP, 3, 4, Absolute)
	cpu.Lookup[0xdd] = NewInstruction(CMP, 3, 4 /*+1 if page crossed*/, AbsoluteX)
	cpu.Lookup[0xd9] = NewInstruction(CMP, 3, 4 /*+1 if page crossed*/, AbsoluteY)
	cpu.Lookup[0xc1] = NewInstruction(CMP, 2, 6, IndirectX)
	cpu.Lookup[0xd1] = NewInstruction(CMP, 2, 5 /*+1 if page crossed*/, IndirectY)
	// CPY
	cpu.Lookup[0xc0] = NewInstruction(CPY, 2, 2, Immediate)
	cpu.Lookup[0xc4] = NewInstruction(CPY, 2, 3, ZeroPage)
	cpu.Lookup[0xcc] = NewInstruction(CPY, 3, 4, Absolute)
	// CPX
	cpu.Lookup[0xe0] = NewInstruction(CPX, 2, 2, Immediate)
	cpu.Lookup[0xe4] = NewInstruction(CPX, 2, 3, ZeroPage)
	cpu.Lookup[0xec] = NewInstruction(CPX, 3, 4, Absolute)
	/* Branching */

	// JMP
	cpu.Lookup[0x4c] = NewInstruction(JMP, 3, 3, Immediate) //AddressingMode that acts as Immidiate
	cpu.Lookup[0x6c] = NewInstruction(JMP, 3, 5, Implied)   //AddressingMode:Indirect with 6502 bug

	//JSR
	cpu.Lookup[0x20] = NewInstruction(JSR, 3, 6, Absolute)

	// RTS
	cpu.Lookup[0x60] = NewInstruction(RTS, 1, 6, Implied)
	// RTI
	cpu.Lookup[0x40] = NewInstruction(RTI, 1, 6, Implied)
	// BNE
	cpu.Lookup[0xd0] = NewInstruction(BNE, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, Relative)
	// BVS
	cpu.Lookup[0x70] = NewInstruction(BVS, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, Relative)
	// BVC
	cpu.Lookup[0x50] = NewInstruction(BVC, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, Relative)
	// BMI
	cpu.Lookup[0x30] = NewInstruction(BMI, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, Relative)
	// BEQ
	cpu.Lookup[0xf0] = NewInstruction(BEQ, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, Relative)
	// BCS
	cpu.Lookup[0xb0] = NewInstruction(BCS, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, Relative)
	// BCC
	cpu.Lookup[0x90] = NewInstruction(BCC, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, Relative)
	// BPL
	cpu.Lookup[0x10] = NewInstruction(BPL, 2, 2 /*(+1 if branch succeeds +2 if to a new page)*/, Relative)
	// BIT
	cpu.Lookup[0x24] = NewInstruction(BIT, 2, 3, ZeroPage)
	cpu.Lookup[0x2c] = NewInstruction(BIT, 3, 4, Absolute)
	/* Stores, Loads */
	// LDA
	cpu.Lookup[0xa9] = NewInstruction(LDA, 2, 2, Immediate)
	cpu.Lookup[0xa5] = NewInstruction(LDA, 2, 3, ZeroPage)
	cpu.Lookup[0xb5] = NewInstruction(LDA, 2, 4, ZeroPageX)
	cpu.Lookup[0xad] = NewInstruction(LDA, 3, 4, Absolute)
	cpu.Lookup[0xbd] = NewInstruction(LDA, 3, 4 /*+1 if page crossed*/, AbsoluteX)
	cpu.Lookup[0xb9] = NewInstruction(LDA, 3, 4 /*+1 if page crossed*/, AbsoluteY)
	cpu.Lookup[0xa1] = NewInstruction(LDA, 2, 6, IndirectX)
	cpu.Lookup[0xb1] = NewInstruction(LDA, 2, 5 /*+1 if page crossed*/, IndirectY)

	// LDX
	cpu.Lookup[0xa2] = NewInstruction(LDX, 2, 2, Immediate)
	cpu.Lookup[0xa6] = NewInstruction(LDX, 2, 3, ZeroPage)
	cpu.Lookup[0xb6] = NewInstruction(LDX, 2, 4, ZeroPageY)
	cpu.Lookup[0xae] = NewInstruction(LDX, 3, 4, Absolute)
	cpu.Lookup[0xbe] = NewInstruction(LDX, 3, 4 /*+1 if page crossed*/, AbsoluteY)

	// LDY
	cpu.Lookup[0xa0] = NewInstruction(LDY, 2, 2, Immediate)
	cpu.Lookup[0xa4] = NewInstruction(LDY, 2, 3, ZeroPage)
	cpu.Lookup[0xb4] = NewInstruction(LDY, 2, 4, ZeroPageX)
	cpu.Lookup[0xac] = NewInstruction(LDY, 3, 4, Absolute)
	cpu.Lookup[0xbc] = NewInstruction(LDY, 3, 4 /*+1 if page crossed*/, AbsoluteX)
	// STA
	cpu.Lookup[0x85] = NewInstruction(STA, 2, 3, ZeroPage)
	cpu.Lookup[0x95] = NewInstruction(STA, 2, 4, ZeroPageX)
	cpu.Lookup[0x8d] = NewInstruction(STA, 3, 4, Absolute)
	cpu.Lookup[0x9d] = NewInstruction(STA, 3, 5, AbsoluteX)
	cpu.Lookup[0x99] = NewInstruction(STA, 3, 5, AbsoluteY)
	cpu.Lookup[0x81] = NewInstruction(STA, 2, 6, IndirectX)
	cpu.Lookup[0x91] = NewInstruction(STA, 2, 6, IndirectY)
	// STX
	cpu.Lookup[0x86] = NewInstruction(STX, 2, 3, ZeroPage)
	cpu.Lookup[0x96] = NewInstruction(STX, 2, 4, ZeroPageY)
	cpu.Lookup[0x8e] = NewInstruction(STX, 3, 4, Absolute)
	//STY
	cpu.Lookup[0x84] = NewInstruction(STY, 2, 3, ZeroPage)
	cpu.Lookup[0x94] = NewInstruction(STY, 2, 4, ZeroPageX)
	cpu.Lookup[0x8c] = NewInstruction(STY, 3, 4, Absolute)

	/* Flags clear */
	cpu.Lookup[0xd8] = NewInstruction(CLD, 1, 2, Implied)
	cpu.Lookup[0x58] = NewInstruction(CLI, 1, 2, Implied)
	cpu.Lookup[0xb8] = NewInstruction(CLV, 1, 2, Implied)
	cpu.Lookup[0x18] = NewInstruction(CLC, 1, 2, Implied)
	cpu.Lookup[0x38] = NewInstruction(SEC, 1, 2, Implied)
	cpu.Lookup[0x78] = NewInstruction(SEI, 1, 2, Implied)
	cpu.Lookup[0xf8] = NewInstruction(SED, 1, 2, Implied)

	cpu.Lookup[0xaa] = NewInstruction(TAX, 1, 2, Implied)
	cpu.Lookup[0xa8] = NewInstruction(TAY, 1, 2, Implied)
	cpu.Lookup[0xba] = NewInstruction(TSX, 1, 2, Implied)
	cpu.Lookup[0x8a] = NewInstruction(TXA, 1, 2, Implied)
	cpu.Lookup[0x9a] = NewInstruction(TXS, 1, 2, Implied)
	cpu.Lookup[0x98] = NewInstruction(TYA, 1, 2, Implied)
	/* Stack */
	cpu.Lookup[0x48] = NewInstruction(PHA, 1, 3, Implied)
	cpu.Lookup[0x68] = NewInstruction(PLA, 1, 4, Implied)
	cpu.Lookup[0x08] = NewInstruction(PHP, 1, 3, Implied)
	cpu.Lookup[0x28] = NewInstruction(PLP, 1, 4, Implied)
}

func (cpu *CPU) StackPush(data uint8) {
	cpu.write(StackOffset+uint16(cpu.SP), data)
	cpu.SP -= 1
}
func (cpu *CPU) StackPushU16(data uint16) {
	hi := uint8(data >> 8 & 0x00FF)
	lo := uint8(data & 0x00FF)

	cpu.StackPush(hi)
	cpu.StackPush(lo)
}

func (cpu *CPU) StackPop() uint8 {
	cpu.SP += 1
	return cpu.read(StackOffset + uint16(cpu.SP))
}

func (cpu *CPU) StackPopU16() uint16 {
	lo := cpu.StackPop()
	hi := cpu.StackPop()
	return uint16(hi)<<8 | uint16(lo)
}
func (cpu *CPU) SetFlag(flag StatusFlag, isSet bool) {
	if isSet {
		cpu.Status |= flag
	} else {
		cpu.Status &= ^flag
	}
}

func (cpu *CPU) GetFlag(flag StatusFlag) uint8 {
	if cpu.Status&flag != 0 {
		return 0x01
	}
	return 0x00
}

func (cpu *CPU) read(addr uint16) uint8 {
	return cpu.Memory[addr]
}

func (cpu *CPU) readU16(addr uint16) uint16 {
	lo := uint16(cpu.read(addr))
	hi := uint16(cpu.read(addr + 1))
	return (hi << 8) | lo
}

func (cpu *CPU) write16(addr uint16, data uint16) {
	hi := uint8(data >> 8)
	lo := uint8(data & 0x00FF)

	cpu.write(addr, lo)
	cpu.write(addr+1, hi)
}

func (cpu *CPU) write(addr uint16, data uint8) {
	cpu.Memory[addr] = data
}

func (cpu *CPU) UpdateZeroAndNegativeFlag(result uint8) {
	if result == 0x00 {
		cpu.SetFlag(Z, true)
	} else {
		cpu.SetFlag(Z, false)
	}

	if result&0b1000_0000 != 0 {
		cpu.SetFlag(N, true)
	} else {
		cpu.SetFlag(N, false)
	}
}

func (cpu *CPU) getOperandAddress(mode AddressMode) uint16 {
	switch mode {
	case Immediate:
		return cpu.Immediate()
	case Implied:
		return cpu.Implied()
	case Accumulator:
		return cpu.Accumulator()
	case ZeroPage:
		return cpu.ZeroPage()
	case ZeroPageX:
		return cpu.ZeroPageX()
	case ZeroPageY:
		return cpu.ZeroPageY()
	case Absolute:
		return cpu.Absolute()
	case AbsoluteX:
		return cpu.AbsoluteX()
	case AbsoluteY:
		return cpu.AbsoluteY()
	case Relative:
		return cpu.Relative()
	case Indirect:
		return cpu.Indirect()
	case IndirectX:
		return cpu.IndirectX()
	case IndirectY:
		return cpu.IndirectY()
	default:
		return 0x0000
	}
}

func (cpu *CPU) Reset() {
	cpu.A = 0x00
	cpu.X = 0x00
	cpu.Y = 0x00
	cpu.Status = 0x00
	cpu.PC = cpu.readU16(0xFFFC)
}

func (cpu *CPU) load(program []uint8) {
	if len(program) == 0 {
		return
	}
	addr := 0x8000
	for _, opcode := range program {
		cpu.write(uint16(addr), opcode)
		addr += 1
	}
	cpu.write16(0xFFFC, 0x8000)

}

func (cpu *CPU) run() {
	var opcode uint8
	var pcStatus uint16
	// CPU cycle
	for {
		opcode = cpu.read(cpu.PC)
		mode := cpu.Lookup[opcode].Mode
		cpu.PC += 1
		pcStatus = cpu.PC

		switch opcode {
		/* Arithmetic */
		// ADC
		case 0x69, 0x65, 0x75, 0x6d, 0x7d, 0x79, 0x61, 0x71:
			cpu.ADC(mode)
		// SBC
		case 0xe9, 0xe5, 0xf5, 0xed, 0xfd, 0xf9, 0xe1, 0xf1:
			cpu.SBC(mode)
		// CMP
		case 0xc9, 0xc5, 0xd5, 0xcd, 0xdd, 0xd9, 0xc1, 0xd1:
			cpu.CMP(mode)
		// CPX
		case 0xe0, 0xe4, 0xec:
			cpu.CPX(mode)
		// CPY
		case 0xc0, 0xc4, 0xcc:
			cpu.CPY(mode)

		/* Increments & Decrements */
		// INC
		case 0xe6, 0xf6, 0xee, 0xfe:
			cpu.INC(mode)
		// DEC
		case 0xc6, 0xd6, 0xce, 0xde:
			cpu.DEC(mode)
		// INX
		case 0xe8:
			cpu.INX(mode)
		// DEX
		case 0xca:
			cpu.DEX(mode)
		// INY
		case 0xc8:
			cpu.INY(mode)
		// DEY
		case 0x88:
			cpu.DEY(mode)

		/* Shifts */
		// ASL
		case 0x0a, 0x06, 0x16, 0x0e, 0x1e:
			cpu.ASL(mode)
		// LSR
		case 0x4A, 0x46, 0x56, 0x4e, 0x5e:
			cpu.LSR(mode)
		// ROL
		case 0x2a, 0x26, 0x36, 0x2e, 0x3e:
			cpu.ROL(mode)
		// ROR
		case 0x6a, 0x66, 0x76, 0x6e, 0x7e:
			cpu.ROR(mode)

		/* Logical */
		//AND
		case 0x29, 0x25, 0x35, 0x2d, 0x3d, 0x39, 0x21, 0x31:
			cpu.AND(mode)
		// EOR
		case 0x49, 0x45, 0x55, 0x4d, 0x5d, 0x59, 0x41, 0x51:
			cpu.EOR(mode)
		// ORA
		case 0x09, 0x05, 0x15, 0x0d, 0x1d, 0x19, 0x01, 0x11:
			cpu.ORA(mode)
		// BIT
		case 0x24, 0x2c:
			cpu.BIT(mode)
		/* Status Flag Changes */
		// CLC
		case 0x18:
			cpu.CLC(mode)
		// CLD
		case 0xd8:
			cpu.CLD(mode)
		// CLI
		case 0x58:
			cpu.CLI(mode)
		// CLV
		case 0xb8:
			cpu.CLV(mode)
		// SEC
		case 0x38:
			cpu.SEC(mode)
		// SEI
		case 0x78:
			cpu.SEI(mode)
		// SED
		case 0xf8:
			cpu.SED(mode)

		/* System Functions */
		// BRK
		case 0x00:
			cpu.BRK(mode)
			return

		/* Stack Operations */
		// TSX
		case 0xba:
			cpu.TSX(mode)
		// TXS
		case 0x9a:
			cpu.TXS(mode)
		// PHA
		case 0x48:
			cpu.PHA(mode)
		// PHP
		case 0x08:
			cpu.PHP(mode)
		// PLA
		case 0x68:
			cpu.PLA(mode)
		// PLP
		case 0x28:
			cpu.PLP(mode)

		/* Jumps & Calls */
		// JMP
		case 0x4c, 0x6c:
			cpu.JMP(mode)
		// JSR
		case 0x20:
			cpu.JSR(mode)
		// RTS
		case 0x60:
			cpu.RTS(mode)
		// RTI
		case 0x40:
			cpu.RTI(mode)

		/* Branches */
		// BCC
		case 0x90:
			cpu.BCC(mode)
		// BCS
		case 0xb0:
			cpu.BCS(mode)
		// BEQ
		case 0xf0:
			cpu.BCC(mode)
		// BMI
		case 0x30:
			cpu.BMI(mode)
		// BNE
		case 0xd0:
			cpu.BNE(mode)
		// BPL
		case 0x10:
			cpu.BPL(mode)
		/* Register Transfers */
		// TAX
		case 0xaa:
			cpu.TAX(mode)
		// TAY
		case 0xa8:
			cpu.TAY(mode)
		// TXA
		case 0x8a:
			cpu.TXA(mode)
		// TYA
		case 0x98:
			cpu.TYA(mode)
		// LDA
		/* Load and Store Operations */
		case 0xa9, 0xa5, 0xb5, 0xad, 0xbd, 0xb9, 0xa1, 0xb1:
			cpu.LDA(mode)
		// LDX
		case 0xa2, 0xa6, 0xb6, 0xae, 0xbe:
			cpu.LDX(mode)
		// LDY
		case 0xa0, 0xa4, 0xb4, 0xac, 0xbc:
			cpu.LDY(mode)
		// STA
		case 0x85, 0x95, 0x8d, 0x9d, 0x99, 0x81, 0x91:
			cpu.STA(mode)
		// STX
		case 0x86, 0x96, 0x8e:
			cpu.STX(mode)
		// STY
		case 0x84, 0x94, 0x8c:
			cpu.STY(mode)
		// NOP
		case 0xea:
			cpu.NOP(mode)
		}

		if pcStatus == cpu.PC {
			cpu.PC += cpu.Lookup[opcode].Length - 1
		}

	}
}

func (cpu *CPU) LoadAndRun(program []uint8) {
	cpu.load(program)
	cpu.Reset()
	cpu.run()
}
