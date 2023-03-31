package cpu

import (
	"Nes/bus"
	"Nes/ppu"
)

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

// CPU the 2A03 is different from the standard 6502 which have the ability to handle sound
type CPU struct {
	X      uint8
	Y      uint8
	A      uint8
	Status StatusFlag
	PC     uint16
	SP     uint8

	Lookup  map[uint8]*Instruction
	Bus     *bus.Bus
	Commons []Disassembly

	Cycle       uint
	CycleCount  uint
	SystemCycle uint
	PPU         *ppu.PPU
}

func New(bus *bus.Bus) *CPU {
	cpu := &CPU{
		X:      0x00,
		Y:      0x00,
		A:      0x00,
		Status: 0b0010_0100,
		SP:     StackTop,
		PC:     0x00,
		Bus:    bus,
		PPU:    bus.PPU,
		Cycle:  0,
	}
	cpu.Lookup = make(map[uint8]*Instruction)
	cpu.addInstructions()
	return cpu

}
func (cpu *CPU) addDisassembly(name InstructionName, mode AddressMode, addr uint16) {
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
	cpu.Lookup[0x4c] = NewInstruction(JMP, 3, 3, Absolute) // Absolute AddressingMode that acts as Immidiate
	cpu.Lookup[0x6c] = NewInstruction(JMP, 3, 5, Indirect) // Indirect AddressingMode:Indirect with 6502 bug

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
	/* unofficial */
	cpu.Lookup[0xc7] = NewInstruction(DCP, 2, 5, ZeroPage)
	cpu.Lookup[0xd7] = NewInstruction(DCP, 2, 6, ZeroPageX)
	cpu.Lookup[0xCF] = NewInstruction(DCP, 3, 6, Absolute)
	cpu.Lookup[0xdF] = NewInstruction(DCP, 3, 7, AbsoluteX)
	cpu.Lookup[0xdb] = NewInstruction(DCP, 3, 7, AbsoluteY)
	cpu.Lookup[0xd3] = NewInstruction(DCP, 2, 8, IndirectY)
	cpu.Lookup[0xc3] = NewInstruction(DCP, 2, 8, IndirectX)

	cpu.Lookup[0x27] = NewInstruction(RLA, 2, 5, ZeroPage)
	cpu.Lookup[0x37] = NewInstruction(RLA, 2, 6, ZeroPageX)
	cpu.Lookup[0x2F] = NewInstruction(RLA, 3, 6, Absolute)
	cpu.Lookup[0x3F] = NewInstruction(RLA, 3, 7, AbsoluteX)
	cpu.Lookup[0x3b] = NewInstruction(RLA, 3, 7, AbsoluteY)
	cpu.Lookup[0x33] = NewInstruction(RLA, 2, 8, IndirectY)
	cpu.Lookup[0x23] = NewInstruction(RLA, 2, 8, IndirectX)

	cpu.Lookup[0x07] = NewInstruction(SLO, 2, 5, ZeroPage)
	cpu.Lookup[0x17] = NewInstruction(SLO, 2, 6, ZeroPageX)
	cpu.Lookup[0x0F] = NewInstruction(SLO, 3, 6, Absolute)
	cpu.Lookup[0x1f] = NewInstruction(SLO, 3, 7, AbsoluteX)
	cpu.Lookup[0x1b] = NewInstruction(SLO, 3, 7, AbsoluteY)
	cpu.Lookup[0x03] = NewInstruction(SLO, 2, 8, IndirectX)
	cpu.Lookup[0x13] = NewInstruction(SLO, 2, 8, IndirectY)

	cpu.Lookup[0x47] = NewInstruction(SRE, 2, 5, ZeroPage)
	cpu.Lookup[0x57] = NewInstruction(SRE, 2, 6, ZeroPageX)
	cpu.Lookup[0x4F] = NewInstruction(SRE, 3, 6, Absolute)
	cpu.Lookup[0x5f] = NewInstruction(SRE, 3, 7, AbsoluteX)
	cpu.Lookup[0x5b] = NewInstruction(SRE, 3, 7, AbsoluteY)
	cpu.Lookup[0x43] = NewInstruction(SRE, 2, 8, IndirectX)
	cpu.Lookup[0x53] = NewInstruction(SRE, 2, 8, IndirectY)

	cpu.Lookup[0x80] = NewInstruction(NOPU, 2, 2, Immediate)
	cpu.Lookup[0x82] = NewInstruction(NOPU, 2, 2, Immediate)
	cpu.Lookup[0x89] = NewInstruction(NOPU, 2, 2, Immediate)
	cpu.Lookup[0xc2] = NewInstruction(NOPU, 2, 2, Immediate)
	cpu.Lookup[0xe2] = NewInstruction(NOPU, 2, 2, Immediate)
	cpu.Lookup[0x04] = NewInstruction(NOPU, 2, 3, ZeroPage)
	cpu.Lookup[0x44] = NewInstruction(NOPU, 2, 3, ZeroPage)
	cpu.Lookup[0x64] = NewInstruction(NOPU, 2, 3, ZeroPage)
	cpu.Lookup[0x14] = NewInstruction(NOPU, 2, 4, ZeroPageX)
	cpu.Lookup[0x34] = NewInstruction(NOPU, 2, 4, ZeroPageX)
	cpu.Lookup[0x54] = NewInstruction(NOPU, 2, 4, ZeroPageX)
	cpu.Lookup[0x74] = NewInstruction(NOPU, 2, 4, ZeroPageX)
	cpu.Lookup[0xd4] = NewInstruction(NOPU, 2, 4, ZeroPageX)
	cpu.Lookup[0xf4] = NewInstruction(NOPU, 2, 4, ZeroPageX)
	cpu.Lookup[0x0c] = NewInstruction(NOPU, 3, 4, Absolute)
	cpu.Lookup[0x1c] = NewInstruction(NOPU, 3, 4 /*or 5*/, AbsoluteX)
	cpu.Lookup[0x3c] = NewInstruction(NOPU, 3, 4 /*or 5*/, AbsoluteX)
	cpu.Lookup[0x5c] = NewInstruction(NOPU, 3, 4 /*or 5*/, AbsoluteX)
	cpu.Lookup[0x7c] = NewInstruction(NOPU, 3, 4 /*or 5*/, AbsoluteX)
	cpu.Lookup[0xdc] = NewInstruction(NOPU, 3, 4 /* or 5*/, AbsoluteX)
	cpu.Lookup[0xfc] = NewInstruction(NOPU, 3, 4 /* or 5*/, AbsoluteX)

	cpu.Lookup[0x02] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x12] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x22] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x32] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x42] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x52] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x62] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x72] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x92] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0xb2] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0xd2] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0xf2] = NewInstruction(NOPU, 1, 2, Implied)

	cpu.Lookup[0x1a] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x3a] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x5a] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0x7a] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0xda] = NewInstruction(NOPU, 1, 2, Implied)
	cpu.Lookup[0xfa] = NewInstruction(NOPU, 1, 2, Implied)

	cpu.Lookup[0xCB] = NewInstruction(AXS, 2, 2, Immediate)

	cpu.Lookup[0x6B] = NewInstruction(ARR, 2, 2, Immediate)

	cpu.Lookup[0xeb] = NewInstruction(SBCU, 2, 2, Immediate)

	cpu.Lookup[0x0b] = NewInstruction(ANC, 2, 2, Immediate)
	cpu.Lookup[0x2b] = NewInstruction(ANC, 2, 2, Immediate)

	cpu.Lookup[0x4b] = NewInstruction(ALR, 2, 2, Immediate)

	cpu.Lookup[0x67] = NewInstruction(RRA, 2, 5, ZeroPage)
	cpu.Lookup[0x77] = NewInstruction(RRA, 2, 6, ZeroPageX)
	cpu.Lookup[0x6f] = NewInstruction(RRA, 3, 6, Absolute)
	cpu.Lookup[0x7f] = NewInstruction(RRA, 3, 7, AbsoluteX)
	cpu.Lookup[0x7b] = NewInstruction(RRA, 3, 7, AbsoluteY)
	cpu.Lookup[0x63] = NewInstruction(RRA, 2, 8, IndirectX)
	cpu.Lookup[0x73] = NewInstruction(RRA, 2, 8, IndirectY)

	cpu.Lookup[0xe7] = NewInstruction(ISB, 2, 5, ZeroPage)
	cpu.Lookup[0xf7] = NewInstruction(ISB, 2, 6, ZeroPageX)
	cpu.Lookup[0xef] = NewInstruction(ISB, 3, 6, Absolute)
	cpu.Lookup[0xff] = NewInstruction(ISB, 3, 7, AbsoluteX)
	cpu.Lookup[0xfb] = NewInstruction(ISB, 3, 7, AbsoluteY)
	cpu.Lookup[0xe3] = NewInstruction(ISB, 2, 8, IndirectX)
	cpu.Lookup[0xf3] = NewInstruction(ISB, 2, 8, IndirectY)

	// highly unstable and not used
	cpu.Lookup[0xab] = NewInstruction(LXA, 2, 3, Immediate)
	cpu.Lookup[0x8b] = NewInstruction(XAA, 2, 3, Immediate)
	cpu.Lookup[0xbb] = NewInstruction(LAS, 3, 2, AbsoluteY)
	cpu.Lookup[0x9b] = NewInstruction(TAS, 3, 2, AbsoluteY)
	cpu.Lookup[0x93] = NewInstruction(AHX, 2 /* guess */, 8, IndirectY)
	cpu.Lookup[0x9f] = NewInstruction(AHX, 3 /* guess */, 4 /* or 5*/, AbsoluteY)
	cpu.Lookup[0x9e] = NewInstruction(SHX, 3 /* guess */, 4 /* or 5*/, AbsoluteY)
	cpu.Lookup[0x9c] = NewInstruction(SHY, 3 /* guess */, 4 /* or 5*/, AbsoluteX)

	cpu.Lookup[0xa7] = NewInstruction(LAX, 2, 3, ZeroPage)
	cpu.Lookup[0xb7] = NewInstruction(LAX, 2, 4, ZeroPageY)
	cpu.Lookup[0xaf] = NewInstruction(LAX, 3, 4, Absolute)
	cpu.Lookup[0xbf] = NewInstruction(LAX, 3, 4, AbsoluteY)
	cpu.Lookup[0xa3] = NewInstruction(LAX, 2, 6, IndirectX)
	cpu.Lookup[0xb3] = NewInstruction(LAX, 2, 5, IndirectY)

	cpu.Lookup[0x87] = NewInstruction(SAX, 2, 3, ZeroPage)
	cpu.Lookup[0x97] = NewInstruction(SAX, 2, 4, ZeroPageY)
	cpu.Lookup[0x8f] = NewInstruction(SAX, 3, 4, Absolute)
	cpu.Lookup[0x83] = NewInstruction(SAX, 2, 6, IndirectX)
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
	return cpu.Bus.CPURead(addr)
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
	if addr >= 0x2000 && addr <= 0x3FFF {
		//fmt.Printf("C:%X ADDR:%X DATA:%X\n", cpu.CycleCount, addr&0x0007, data)

	}

	cpu.Bus.CPUWrite(addr, data)
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

func (cpu *CPU) PageCross(source uint16, result uint16) {
	if source&0xFF00 != result&0xFF00 {
		cpu.Cycle += 1
	}
}

func (cpu *CPU) getAbsoluteAddress(mode AddressMode, addr uint16) uint16 {
	switch mode {
	case Immediate:
		return addr
	case Implied:
		return 0x0000
	case Accumulator:
		return 0x0000
	case ZeroPage:
		return uint16(cpu.read(addr))
	case ZeroPageX:
		zeroAddr := cpu.read(addr)
		zeroAddr += cpu.X
		// wraps around
		return uint16(zeroAddr) & 0x00FF
	case ZeroPageY:
		zeroAddr := cpu.read(addr)
		zeroAddr += cpu.Y
		// wraps around
		return uint16(zeroAddr) & 0x00FF
	case Absolute:
		return cpu.readU16(addr)
	case AbsoluteX:
		addr := cpu.readU16(addr)
		addr += uint16(cpu.X)
		return addr & 0xFFFF
	case AbsoluteY:
		addr := cpu.readU16(addr)
		addr += uint16(cpu.Y)
		return addr & 0xFFFF
	case Relative:
		offset := uint16(cpu.read(addr))
		if offset < 0x80 {
			return addr + 1 + offset
		} else {
			return addr + 1 - (offset | 0b0111_1111)
		}
	case Indirect:
		inAddr := cpu.readU16(addr)
		return cpu.readU16(inAddr)
	case IndirectX:
		baseAddr := uint16(cpu.read(addr))
		baseAddr += uint16(cpu.X)
		baseAddr &= 0x00FF
		lo := uint16(cpu.read(baseAddr))
		hi := uint16(cpu.read((baseAddr + 1) & 0x00FF))
		return (hi << 8) | lo
	case IndirectY:
		baseAddr := uint16(cpu.read(addr))
		lo := uint16(cpu.read(baseAddr))
		hi := uint16(cpu.read((baseAddr + 1) & 0x00FF))
		baseAddr = (hi << 8) | lo
		baseAddr += uint16(cpu.Y)
		return baseAddr
	default:
		return 0x0000
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
		return 0x000
	}
}

func (cpu *CPU) Reset() {
	cpu.A = 0x00
	cpu.X = 0x00
	cpu.Y = 0x00
	cpu.SP = 0xFD
	cpu.Status = 0x00
	cpu.SetFlag(U, true)
	cpu.PC = cpu.readU16(0xFFFC)
	cpu.Cycle = 8
}

func (cpu *CPU) Load(program []uint8) {
	if len(program) == 0 {
		return
	}
	addr := 0x0600
	for _, opcode := range program {
		cpu.write(uint16(addr), opcode)
		addr += 1
	}
	cpu.write16(0xFFFC, 0x0600)

}

func (cpu *CPU) ExecuteCode(opcode uint8, mode AddressMode) {

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
		cpu.BEQ(mode)
	// BMI
	case 0x30:
		cpu.BMI(mode)
	// BNE
	case 0xd0:
		cpu.BNE(mode)
	// BPL
	case 0x10:
		cpu.BPL(mode)
	// BVS
	case 0x70:
		cpu.BVS(mode)
	// BVC
	case 0x50:
		cpu.BVC(mode)
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
	// DCP
	case 0xc7, 0xd7, 0xCF, 0xdF, 0xdb, 0xd3, 0xc3:
		cpu.DCP(mode)
	// RLA
	case 0x27, 0x37, 0x2F, 0x3F, 0x3b, 0x33, 0x23:
		cpu.RLA(mode)
	// SLO
	case 0x07, 0x17, 0x0F, 0x1f, 0x1b, 0x03, 0x13:
		cpu.SLO(mode)
	// SRE
	case 0x47, 0x57, 0x4F, 0x5f, 0x5b, 0x43, 0x53:
		cpu.SRE(mode)
	// *NOP
	case 0x80, 0x82, 0x89, 0xc2, 0xe2, 0x04, 0x44, 0x64, 0x14, 0x34, 0x54, 0x74, 0xd4, 0xf4, 0x0c, 0x1c, 0x3c, 0x5c, 0x7c, 0xdc, 0xfc:
		cpu.NOPU(mode)
	case 0x02, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x92, 0xb2, 0xd2, 0xf2:
		cpu.NOPU(mode)
	case 0x1a, 0x3a, 0x5a, 0x7a, 0xda, 0xfa:
		cpu.NOPU(mode)

	// *AXS
	case 0xcb:
		cpu.AXS(mode)
	// *ARR
	case 0x6b:
		cpu.ARR(mode)
	// *SBC
	case 0xeb:
		cpu.SBC(mode)
	// *ALR
	case 0x4b:
		cpu.ALR(mode)

	// *RRA
	case 0x67, 0x77, 0x6f, 0x7f, 0x7b, 0x63, 0x73:
		cpu.RRA(mode)
	// ISB
	case 0xe7, 0xf7, 0xef, 0xff, 0xfb, 0xe3, 0xf3:
		cpu.ISB(mode)
	// LXA
	case 0xab:
		cpu.LXA(mode)
	// XAA
	case 0x8b:
		cpu.XAA(mode)
	// LAS
	case 0xbb:
		cpu.LAS(mode)
	// *TAS
	case 0x9b:
		cpu.TAS(mode)
	// SAX
	case 0x87, 0x97, 0x8f, 0x83:
		cpu.SAX(mode)
	// SHX
	case 0x9e:
		cpu.SHX(mode)
	// SHY
	case 0x9c:
		cpu.SHY(mode)
	// LAX
	case 0xa7, 0xb7, 0xaf, 0xbf, 0xa3, 0xb3:
		cpu.LAX(mode)
	// AHX
	case 0x93, 0x9f:
		cpu.AHX(mode)
	case 0xea:
		cpu.NOP(mode)
	}
}

func (cpu *CPU) SingleRun() {
	var opcode uint8
	var pcStatus uint16
	opcode = cpu.read(cpu.PC)
	mode := cpu.Lookup[opcode].Mode
	//tmpPC := cpu.PC
	cpu.PC += 1
	pcStatus = cpu.PC
	cpu.ExecuteCode(opcode, mode)
	cpu.Cycle += uint(cpu.Lookup[opcode].Cycles)

	//fmt.Println(cpu.Trace(tmpPC))

	if pcStatus == cpu.PC {
		cpu.PC += cpu.Lookup[opcode].Length - 1
	}
}

func (cpu *CPU) Clock() {
	if cpu.Cycle == 0 {
		cpu.SetFlag(U, true)
		//if cpu.CycleCount == 89083 {
		//	fmt.Printf("")
		//}
		//pc := cpu.PC
		cpu.SingleRun()
		cpu.SetFlag(U, true)
		// debug
		//var status []string
		//if cpu.GetFlag(N) == 1 {
		//	status = append(status, "N")
		//} else {
		//	status = append(status, ".")
		//}
		//if cpu.GetFlag(V) == 1 {
		//	status = append(status, "V")
		//} else {
		//	status = append(status, ".")
		//}
		//if cpu.GetFlag(U) == 1 {
		//	status = append(status, "U")
		//} else {
		//	status = append(status, ".")
		//}
		//if cpu.GetFlag(B) == 1 {
		//	status = append(status, "B")
		//} else {
		//	status = append(status, ".")
		//}
		//if cpu.GetFlag(D) == 1 {
		//	status = append(status, "D")
		//} else {
		//	status = append(status, ".")
		//}
		//if cpu.GetFlag(I) == 1 {
		//	status = append(status, "I")
		//} else {
		//	status = append(status, ".")
		//}
		//if cpu.GetFlag(Z) == 1 {
		//	status = append(status, "Z")
		//} else {
		//	status = append(status, ".")
		//}
		//if cpu.GetFlag(C) == 1 {
		//	status = append(status, "C")
		//} else {
		//	status = append(status, ".")
		//}

		//fmt.Printf("%d:%d PC:%X NPC:%X %s A:%X X:%X Y:%X %s STKP:%X\n", cpu.CycleCount, cpu.Cycle, pc, cpu.PC, "XXX", cpu.A, cpu.X, cpu.Y, strings.Join(status, ""), cpu.SP)
	}
	cpu.CycleCount += 1

	cpu.Cycle -= 1

}

//func (cpu *CPU) Run() {
//	var opcode uint8
//	var pcStatus uint16
//	// CPU cycle
//	for {
//		fmt.Println(cpu.Trace())
//		opcode = cpu.read(cpu.PC)
//		mode := cpu.Lookup[opcode].Mode
//		cpu.PC += 1
//		pcStatus = cpu.PC
//		cpu.ExecuteCode(opcode, mode)
//
//		cpu.Bus.Tick(uint(cpu.Lookup[opcode].Cycles))
//
//		if pcStatus == cpu.PC {
//			cpu.PC += cpu.Lookup[opcode].Length - 1
//		}
//	}
//}

//func (cpu *CPU) LoadAndRun(program []uint8) {
//	cpu.Load(program)
//	cpu.Reset()
//	cpu.Run()
//
//}
