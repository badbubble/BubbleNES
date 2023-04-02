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
	return cpu.Bus.CPURead(addr, false)
}
func (cpu *CPU) ReadTrace(addr uint16) uint8 {
	return cpu.Bus.CPURead(addr, true)
}

func (cpu *CPU) ReadU16Trace(addr uint16) uint16 {
	lo := uint16(cpu.ReadTrace(addr))
	hi := uint16(cpu.ReadTrace(addr + 1))
	return (hi << 8) | lo
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
		//if cpu.CycleCount == 10375601 {
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
