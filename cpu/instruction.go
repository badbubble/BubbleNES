package cpu

type InstructionName string

const (
	ADC  InstructionName = "ADC"
	AND  InstructionName = "AND"
	ASL  InstructionName = "ASL"
	BCC  InstructionName = "BCC"
	BCS  InstructionName = "BCS"
	BEQ  InstructionName = "BEQ"
	BIT  InstructionName = "BIT"
	BMI  InstructionName = "BMI"
	BNE  InstructionName = "BNE"
	BPL  InstructionName = "BPL"
	BRK  InstructionName = "BRK"
	BVC  InstructionName = "BVC"
	BVS  InstructionName = "BVS"
	CLC  InstructionName = "CLC"
	CLD  InstructionName = "CLD"
	CLI  InstructionName = "CLI"
	CLV  InstructionName = "CLV"
	CMP  InstructionName = "CMP"
	CPX  InstructionName = "CPX"
	CPY  InstructionName = "CPY"
	DEC  InstructionName = "DEC"
	DEX  InstructionName = "DEX"
	DEY  InstructionName = "DEY"
	EOR  InstructionName = "EOR"
	INC  InstructionName = "INC"
	INX  InstructionName = "INX"
	INY  InstructionName = "INY"
	JMP  InstructionName = "JMP"
	JSR  InstructionName = "JSR"
	LDA  InstructionName = "LDA"
	LDX  InstructionName = "LDX"
	LDY  InstructionName = "LDY"
	LSR  InstructionName = "LSR"
	NOP  InstructionName = "NOP"
	ORA  InstructionName = "ORA"
	PHA  InstructionName = "PHA"
	PHP  InstructionName = "PHP"
	PLA  InstructionName = "PLA"
	PLP  InstructionName = "PLP"
	ROL  InstructionName = "ROL"
	ROR  InstructionName = "ROR"
	RTI  InstructionName = "RTI"
	RTS  InstructionName = "RTS"
	SBC  InstructionName = "SBC"
	SEC  InstructionName = "SEC"
	SED  InstructionName = "SED"
	SEI  InstructionName = "SEI"
	STA  InstructionName = "STA"
	STX  InstructionName = "STX"
	STY  InstructionName = "STY"
	TAX  InstructionName = "TAX"
	TAY  InstructionName = "TAY"
	TSX  InstructionName = "TSX"
	TXA  InstructionName = "TXA"
	TXS  InstructionName = "TXS"
	TYA  InstructionName = "TYA"
	XXX  InstructionName = "XXX"
	DCP  InstructionName = "*DCP"
	RLA  InstructionName = "*RLA"
	SLO  InstructionName = "*SLO"
	SRE  InstructionName = "*SRE"
	NOPU InstructionName = "*NOP"
	AXS  InstructionName = "*AXS"
	ARR  InstructionName = "*ARR"
	SBCU InstructionName = "*SBC"
	ANC  InstructionName = "*ANC"
	ALR  InstructionName = "*ALR"
	RRA  InstructionName = "*RRA"
	ISB  InstructionName = "*ISB"
	LXA  InstructionName = "*LXA"
	XAA  InstructionName = "*XAA"
	LAS  InstructionName = "*LAS"
	TAS  InstructionName = "*TAS"
	AHX  InstructionName = "*AHX"
	SHX  InstructionName = "*SHX"
	SHY  InstructionName = "*SHY"
	LAX  InstructionName = "*LAX"
	SAX  InstructionName = "*SAX"
)

type Instruction struct {
	Name   InstructionName
	Mode   AddressMode
	Length uint16
	Cycles uint16
}

func NewInstruction(name InstructionName, len uint16, cycles uint16, mode AddressMode) *Instruction {
	return &Instruction{
		Name:   name,
		Mode:   mode,
		Length: len,
		Cycles: cycles,
	}
}

/* Arithmetic */
// The arithmetic operations perform addition and subtraction on the contents of the accumulator.
// The compare operations allow the comparison of the accumulator and X or Y with memory values.

///////////////////////////////////////////////////////////////////////////////
// INSTRUCTION IMPLEMENTATIONS

// Note: I have started with the two most complicated instructions to emulate, which
// ironically is addition and subtraction! I have tried to include a detailed
// explanation as to why they are so complex, yet so fundamental. I am also NOT
// going to do this through the explanation of 1 and 2's complement.

// Instruction: Add with Carry In
// Function:    A = A + M + C
// Flags Out:   C, V, N, Z
//
// Explanation:
// The purpose of this function is to add a value to the accumulator and a carry bit. If
// the result is > 255 there is an overflow setting the carry bit. Ths allows you to
// chain together ADC instructions to add numbers larger than 8-bits. This in itself is
// simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow.
//
// 10000100 = 128 + 4 = 132 in normal circumstances, we know this as unsigned, and it allows
// us to represent numbers between 0 and 255 (given 8 bits). The 6502 can also interpret
// this word as something else if we assume those 8 bits represent the range -128 to +127,
// i.e. it has become signed.
//
// Since 132 > 127, it effectively wraps around, through -128, to -124. This wraparound is
// called overflow, and this is a useful to know as it indicates that the calculation has
// gone outside the permissible range, and therefore no longer makes numeric sense.
//
// Note the implementation of ADD is the same in binary, this is just about how the numbers
// are represented, so the word 10000100 can be both -124 and 132 depending upon the
// context the programming is using it in. We can prove this!
//
//  10000100 =  132  or  -124
// +00010001 = + 17      + 17
//  ========    ===       ===     See, both are valid additions, but our interpretation of
//  10010101 =  149  or  -107     the context changes the value, not the hardware!
//
// In principle under the -128 to 127 range:
// 10000000 = -128, 11111111 = -1, 00000000 = 0, 00000001 = +1, 01111111 = +127
// therefore negative numbers have the most significant set, positive numbers do not
//
// To assist us, the 6502 can set the overflow flag, if the result of the addition has
// wrapped around. V <- ~(A^M) & A^(A+M+C) :D lol, let's work out why!
//
// Let's suppose we have A = 30, M = 10 and C = 0
//          A = 30 = 00011110
//          M = 10 = 00001010+
//     RESULT = 40 = 00101000
//
// Here we have not gone out of range. The resulting significant bit has not changed.
// So let's make a truth table to understand when overflow has occurred. Here I take
// the MSB of each component, where R is RESULT.
//
// A  M  R | V | A^R | A^M |~(A^M) |
// 0  0  0 | 0 |  0  |  0  |   1   |
// 0  0  1 | 1 |  1  |  0  |   1   |
// 0  1  0 | 0 |  0  |  1  |   0   |
// 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
// 1  0  0 | 0 |  1  |  1  |   0   |
// 1  0  1 | 0 |  0  |  1  |   0   |
// 1  1  0 | 1 |  1  |  0  |   1   |
// 1  1  1 | 0 |  0  |  0  |   1   |
//
// We can see how the above equation calculates V, based on A, M and R. V was chosen
// based on the following hypothesis:
//       Positive Number + Positive Number = Negative Result -> Overflow
//       Negative Number + Negative Number = Positive Result -> Overflow
//       Positive Number + Negative Number = Either Result -> Cannot Overflow
//       Positive Number + Positive Number = Positive Result -> OK! No Overflow
//       Negative Number + Negative Number = Negative Result -> OK! NO Overflow

// ADC instruction adds the contents of a memory location to the accumulator together with the carry bit.
// If overflow occurs the carry bit is set, this enables multiple byte addition to be performed.
func (cpu *CPU) ADC(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)

	sum := uint16(cpu.A) + uint16(value) + uint16(cpu.GetFlag(C))
	if sum > 0xff {
		cpu.SetFlag(C, true)
	} else {
		cpu.SetFlag(C, false)
	}
	if ^(uint16(cpu.A)^uint16(value))&(uint16(cpu.A)^sum)&0x0080 != 0 {
		cpu.SetFlag(V, true)
	} else {
		cpu.SetFlag(V, false)
	}

	cpu.A = uint8(sum)
	cpu.UpdateZeroAndNegativeFlag(cpu.A)
}

// SBC Subtraction with Borrow In
// Function:    A = A - M - (1 - C)
// Flags Out:   C, V, N, Z
// This instruction subtracts the contents of a memory location to the accumulator together with the not of
// the carry bit. If overflow occurs the carry bit is clear, this enables multiple byte subtraction to be performed.
// Explanation:
// Given the explanation for ADC above, we can reorganise our data
// to use the same computation for addition, for subtraction by multiplying
// the data by -1, i.e. make it negative
//
// A = A - M - (1 - C)  ->  A = A + -1 * (M - (1 - C))  ->  A = A + (-M + 1 + C)
//
// To make a signed positive number negative, we can invert the bits and add 1
// (OK, I lied, a bit of 1 and 2s complement :P)
//
//	5 = 00000101
//
// -5 = 11111010 + 00000001 = 11111011 (or 251 in our 0 to 255 range)
//
// The range is actually unimportant, because if I take the value 15, and add 251
// to it, given we wrap around at 256, the result is 10, so it has effectively
// subtracted 5, which was the original intention. (15 + 251) % 256 = 10
//
// Note that the equation above used (1-C), but this got converted to + 1 + C.
// This means we already have the +1, so all we need to do is invert the bits
// of M, the data(!) therefore we can simply add, exactly the same way we did
// before.
func (cpu *CPU) SBC(mode AddressMode) {

	addr := cpu.getOperandAddress(mode)
	value := ^cpu.read(addr)
	tmp := uint16(cpu.A) + uint16(value) + uint16(cpu.GetFlag(C))

	if tmp > 0xff {
		cpu.SetFlag(C, true)
	} else {
		cpu.SetFlag(C, false)
	}

	if (cpu.A^value)&0x80 == 0 && (cpu.A^uint8(tmp))&0x80 != 0 {
		cpu.SetFlag(V, true)
	} else {
		cpu.SetFlag(V, false)
	}

	cpu.A = uint8(tmp)
	cpu.UpdateZeroAndNegativeFlag(cpu.A)

}

// CMP Compare Accumulator
// This instruction compares the contents of the accumulator with another memory held value and sets
// the zero and carry flags as appropriate.
// Function:    C <- A >= M      Z <- (A - M) == 0
// Flags Out:   N, C, Z
func (cpu *CPU) CMP(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	cpu.SetFlag(C, cpu.A >= value)
	cpu.SetFlag(Z, (cpu.A-value)&0x00FF == 0)
	cpu.SetFlag(N, (cpu.A-value)&0x0080 != 0)

}

// CPX Compare X Register
// This instruction compares the contents of the X register with another memory held value and
// sets the zero and carry flags as appropriate.
// Function:    C <- X >= M      Z <- (X - M) == 0
// Flags Out:   N, C, Z
func (cpu *CPU) CPX(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)

	cpu.SetFlag(C, cpu.X >= value)
	cpu.SetFlag(Z, (cpu.X-value)&0x00FF == 0)
	cpu.SetFlag(N, (cpu.X-value)&0x0080 != 0)

}

// CPY Compare Y Register
// This instruction compares the contents of the Y register with another memory held value and
// sets the zero and carry flags as appropriate.
// Function:    C <- Y >= M      Z <- (Y - M) == 0
// Flags Out:   N, C, Z
func (cpu *CPU) CPY(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)

	cpu.SetFlag(C, cpu.Y >= value)
	cpu.SetFlag(Z, (cpu.Y-value)&0x00FF == 0)
	cpu.SetFlag(N, (cpu.Y-value)&0x0080 != 0)

}

/* Increments & Decrements */

// Increment or decrement a memory location or one of the X or Y registers by one
// setting the negative (N) and zero (Z) flags as appropriate.

// INC Increment Value at Memory Location
// Adds one to the value held at a specified memory location setting the zero and negative flags as appropriate.
// Function:    M = M + 1
// Flags Out:   N, Z
func (cpu *CPU) INC(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	value += 1
	cpu.write(addr, value)
	cpu.UpdateZeroAndNegativeFlag(value)
}

// DEC Decrement Value at Memory Location
// Subtracts one from the value held at a specified memory location setting the zero and negative flags as appropriate.
// Function:    M = M - 1
// Flags Out:   N, Z
func (cpu *CPU) DEC(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	value -= 1
	cpu.write(addr, value)
	cpu.UpdateZeroAndNegativeFlag(value)
}

// INX - Increment X Register
// Adds one to the X register setting the zero and negative flags as appropriate.
// Function:    X = X + 1
// Flags Out:   N, Z
func (cpu *CPU) INX(mode AddressMode) {
	cpu.X += 0x01
	cpu.UpdateZeroAndNegativeFlag(cpu.X)
}

// DEX - Decrement X Register
// Subtracts one from the X register setting the zero and negative flags as appropriate.
// Function:    X = X - 1
// Flags Out:   N, Z
func (cpu *CPU) DEX(mode AddressMode) {
	cpu.X -= 0x01
	cpu.UpdateZeroAndNegativeFlag(cpu.X)
}

// INY Increment Y Register
// Adds one to the Y register setting the zero and negative flags as appropriate.
// Function:    Y = Y + 1
// Flags Out:   N, Z
func (cpu *CPU) INY(mode AddressMode) {
	cpu.Y += 0x01
	cpu.UpdateZeroAndNegativeFlag(cpu.Y)
}

// DEY - Decrement Y Register
// Subtracts one from the Y register setting the zero and negative flags as appropriate.
// Function:    Y = Y - 1
// Flags Out:   N, Z
func (cpu *CPU) DEY(mode AddressMode) {
	cpu.Y -= 0x01
	cpu.UpdateZeroAndNegativeFlag(cpu.Y)
}

/* Shifts */
// Shift instructions cause the bits within either a memory location or the accumulator to be shifted by
// one bit position.
// To rotate instructions use the contents if the carry flag (C) to fill the vacant position generated by the shift and
// to catch the overflowing bit. The arithmetic and logical shifts shift in an appropriate 0 or 1 bit as appropriate
// but catch the overflow bit in the carry flag (C).

// ASL Arithmetic Shift Left
// This operation shifts all the bits of the accumulator or memory contents one bit left.
// Bit 0 is set to 0 and bit 7 is placed in the carry flag. The effect of this operation is to multiply
// the memory contents by 2 (ignoring 2's complement considerations), setting the carry if the result
// will not fit in 8 bits.
// Function:    A = C <- (A << 1) <- 0
// Flags Out:   N, Z, C
func (cpu *CPU) ASL(mode AddressMode) {
	if mode == Accumulator {
		value := uint16(cpu.A) << 1
		cpu.A = uint8(value)
		cpu.SetFlag(C, value&0xFF00 != 0)
		cpu.UpdateZeroAndNegativeFlag(cpu.A)
	} else {
		addr := cpu.getOperandAddress(mode)
		value := uint16(cpu.read(addr)) << 1
		cpu.write(addr, uint8(value))
		cpu.SetFlag(C, value&0xFF00 != 0)
		cpu.UpdateZeroAndNegativeFlag(uint8(value))
	}
}

// LSR Logical Shift Right
// Each of the bits in A or M is shift one place to the right.
// The bit that was in bit 0 is shifted into the carry flag. Bit 7 is set to zero.
func (cpu *CPU) LSR(mode AddressMode) {
	if mode == Accumulator {
		value := cpu.A
		cpu.SetFlag(C, value&0x0001 != 0)
		cpu.A = cpu.A >> 1
		cpu.UpdateZeroAndNegativeFlag(cpu.A)
	} else {
		addr := cpu.getOperandAddress(mode)
		value := cpu.read(addr)
		result := value >> 1
		cpu.SetFlag(C, value&0x0001 != 0)
		cpu.write(addr, result)

		cpu.UpdateZeroAndNegativeFlag(result)
	}
}

// ROL - Rotate Left
// Move each of the bits in either A or M one place to the left. Bit 0 is filled with the current value of the carry
// flag whilst the old bit 7 becomes the new carry flag value.
func (cpu *CPU) ROL(mode AddressMode) {
	if mode == Accumulator {
		value := (uint16(cpu.A) << 1) | uint16(cpu.GetFlag(C))
		cpu.SetFlag(C, value&0xFF00 != 0)
		cpu.A = uint8(value)
		cpu.UpdateZeroAndNegativeFlag(cpu.A)
	} else {
		addr := cpu.getOperandAddress(mode)
		value := cpu.read(addr)
		tmp := (uint16(value) << 1) | uint16(cpu.GetFlag(C))
		cpu.SetFlag(C, tmp&0xFF00 != 0)
		cpu.write(addr, uint8(tmp))
		cpu.UpdateZeroAndNegativeFlag(uint8(tmp))
	}
}

// ROR - Rotate Right
// Move each of the bits in either A or M one place to the right.
// Bit 7 is filled with the current value of the carry flag whilst the old bit 0 becomes the new carry flag value.
func (cpu *CPU) ROR(mode AddressMode) {
	if mode == Accumulator {
		value := (uint16(cpu.A) >> 1) | uint16(cpu.GetFlag(C))<<7
		cpu.SetFlag(C, cpu.A&0x0001 != 0)
		cpu.A = uint8(value)
		cpu.UpdateZeroAndNegativeFlag(cpu.A)
	} else {
		addr := cpu.getOperandAddress(mode)
		value := cpu.read(addr)
		tmp := (uint16(value) >> 1) | uint16(cpu.GetFlag(C))<<7
		cpu.SetFlag(C, value&0x0001 != 0)
		cpu.write(addr, uint8(tmp))
		cpu.UpdateZeroAndNegativeFlag(uint8(tmp))
	}
}

/* Logical */
// The following instructions perform logical operations on the contents of the accumulator and another value held
// in memory. The BIT instruction performs a logical AND to test the presence of bits in the memory value to set
// the flags but does not keep the result.

// AND Bitwise Logic AND
// A logical AND is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
// Function:    A = A & M
// Flags Out:   N, Z
func (cpu *CPU) AND(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)

	cpu.A &= value

	cpu.UpdateZeroAndNegativeFlag(cpu.A)
}

// EOR Bitwise Exclusive OR
// An exclusive OR is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
// Function:    A = A xor M
// Flags Out:   N, Z
func (cpu *CPU) EOR(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	cpu.A ^= value

	cpu.SetFlag(N, cpu.A&0x80 != 0)
	cpu.SetFlag(Z, cpu.A == 0)
}

// ORA Bitwise Logic OR
// An inclusive OR is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
// Function:    A = A | M
// Flags Out:   N, Z
func (cpu *CPU) ORA(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	cpu.A |= value

	cpu.SetFlag(N, cpu.A&0x80 != 0)
	cpu.SetFlag(Z, cpu.A == 0)
}

// BIT Test
// These instructions is used to test if one or more bits are set in a target memory location.
// The mask pattern in A is ANDed with the value in memory to set or clear the zero flag, but the result is not kept.
// Bits 7 and 6 of the value from memory are copied into the N and V flags.
func (cpu *CPU) BIT(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	tmp := cpu.A & value

	cpu.SetFlag(N, value&0x80 != 0)
	cpu.SetFlag(V, value&0x40 != 0)
	cpu.SetFlag(Z, tmp == 0)
}

/* Status Flag Changes */
// The following instructions change the values of specific status flags.

// CLC Clear carry flag
// Set the carry flag to zero.
// Function:    C = 0
func (cpu *CPU) CLC(mode AddressMode) {
	cpu.SetFlag(C, false)
}

// CLD Clear Decimal Flag
// Sets the decimal mode flag to zero.
// Function:    D = 0
func (cpu *CPU) CLD(mode AddressMode) {
	cpu.SetFlag(D, false)
}

// CLI Disable Interrupts / Clear Interrupt Flag
// Clears the interrupt disable flag allowing normal interrupt requests to be serviced.
// Function:    I = 0
func (cpu *CPU) CLI(mode AddressMode) {
	cpu.SetFlag(I, false)
}

// CLV Clear Overflow Flag
// Clears the overflow flag.
// Function:    V = 0
func (cpu *CPU) CLV(mode AddressMode) {
	cpu.SetFlag(V, false)
}

// SEC Set Carry Flag
// Set the carry flag to one.
// Function:    C = 1
func (cpu *CPU) SEC(mode AddressMode) {
	cpu.SetFlag(C, true)
}

// SED - Set Decimal Flag
// Function:    D = 1
func (cpu *CPU) SED(mode AddressMode) {
	cpu.SetFlag(D, true)
}

// SEI - Set Interrupt Disable
// Set the interrupt disable flag to one.
// Function:    I = 1
func (cpu *CPU) SEI(mode AddressMode) {
	cpu.SetFlag(I, true)
}

/* System Functions */
// The remaining instructions perform useful but rarely used functions.

// BRK - Force Interrupt
// The BRK instruction forces the generation of an interrupt request. The program counter and processor status
// are pushed on the stack then the IRQ interrupt vector at $FFFE/F is loaded into the PC and the break flag
// in the status set to one.
// Function:    Program Sourced Interrupt
func (cpu *CPU) BRK(mode AddressMode) {
	cpu.SetFlag(I, true)
	cpu.StackPushU16(cpu.PC)
	cpu.SetFlag(B, true)
	cpu.StackPush(uint8(cpu.Status))
	cpu.SetFlag(B, false)

	cpu.PC = cpu.readU16(0xFFFE)

}

/* Stack Operations */
// The 6502 microprocessor supports a 256 byte stack fixed between memory locations $0100 and $01FF.
// A special 8-bit register, S, is used to keep track of the next free byte of stack space.
// Pushing a byte on to the stack causes the value to be stored at the current free location (e.g. $0100,S) and then
// the stack pointer is post decremented. Pull operations reverse this procedure.
// The stack register can only be accessed by transferring its value to or from the X register.
// Its value is automatically modified by push/pull instructions, subroutine calls and returns, interrupts
// and returns from interrupts.

// Instruction: Transfer Stack Pointer to X Register
// Function:    X = stack pointer
// Flags Out:   N, Z
func (cpu *CPU) TSX(mode AddressMode) {
	cpu.X = cpu.SP
	cpu.UpdateZeroAndNegativeFlag(cpu.X)
}

// TXS Transfer X Register to Stack Pointer
// Copies the current contents of the X register into the stack register.
// Function:    stack pointer = X
func (cpu *CPU) TXS(mode AddressMode) {
	cpu.SP = cpu.X
}

// PHA Push Accumulator to Stack
// Pushes a copy of the accumulator on to the stack.
// Function:    A -> stack
func (cpu *CPU) PHA(mode AddressMode) {
	cpu.StackPush(cpu.A)
}

// PHP Push Status Register to Stack
// Pushes a copy of the status flags on to the stack.
// Function:    status -> stack
// Note:        Break flag is set to 1 before push and Unused
func (cpu *CPU) PHP(mode AddressMode) {
	cpu.StackPush(uint8(cpu.Status) | uint8(B) | uint8(U))
}

// PLA Pull Accumulator off Stack
// Pulls an 8 bit value from the stack and into the accumulator. The zero and negative flags are set as appropriate.
// Function:    A <- stack
// Flags Out:   N, Z
func (cpu *CPU) PLA(mode AddressMode) {
	cpu.A = cpu.StackPop()
	cpu.UpdateZeroAndNegativeFlag(cpu.A)
}

// PLP Pull Processor Status
// Pulls an 8 bit value from the stack and into the processor flags.
// The flags will take on new states as determined by the value pulled
// Function:    Status <- stack
func (cpu *CPU) PLP(mode AddressMode) {
	cpu.Status = StatusFlag(cpu.StackPop())
	cpu.SetFlag(U, true)
	cpu.SetFlag(B, false)
}

/* Jumps & Calls */
// The following instructions modify the program counter causing a break to normal sequential execution.
// The JSR instruction pushes the old PC onto the stack before changing it to the new location allowing a subsequent
// RTS to return execution to the instruction after the call.

// JMP Jump To Location
// Sets the program counter to the address specified by the operand.
// An original 6502 has does not correctly fetch the target address if the indirect vector falls on a page boundary
// (e.g. $xxFF where xx is any value from $00 to $FF).
// In this case fetches the LSB from $xxFF as expected but takes the MSB from $xx00.
// This is fixed in some later chips like the 65SC02 so for compatibility always ensure the indirect vector
// is not at the end of the page.
// Function:    pc = address
// let indirect_ref = self.mem_read_u16(mem_address);
// 6502 bug mode with with page boundary:
//
//	if address $3000 contains $40, $30FF contains $80, and $3100 contains $50,
//
// the result of JMP ($30FF) will be a transfer of control to $4080 rather than $5080 as you intended
// i.e. the 6502 took the low byte of the address from $30FF and the high byte from $3000
func (cpu *CPU) JMP(mode AddressMode) {
	if mode == Indirect {
		var ref uint16
		addr := cpu.readU16(cpu.PC)
		if addr&0x00FF == 0x00FF {
			lo := cpu.read(addr)
			hi := cpu.read(addr & 0xFF00)
			ref = uint16(hi)<<8 | uint16(lo)
		} else {
			ref = cpu.readU16(addr)
		}
		cpu.PC = ref
	} else {
		cpu.PC = cpu.getOperandAddress(mode)
	}
}

// JSR Jump To Sub-Routine
// The JSR instruction pushes the address (minus one) of the return point on to the stack and then sets
// the program counter to the target memory address.
// Function:    Push current pc to stack, pc = address
func (cpu *CPU) JSR(mode AddressMode) {
	cpu.StackPushU16(cpu.PC + 2 - 1)
	cpu.PC = cpu.readU16(cpu.PC)
}

// RTS - Return from Subroutine
// The RTS instruction is used at the end of a subroutine to return to the calling routine.
// It pulls the program counter (minus one) from the stack.
func (cpu *CPU) RTS(mode AddressMode) {
	cpu.PC = cpu.StackPopU16() + 1
}

// RTI - Return from Interrupt
// The RTI instruction is used at the end of an interrupt processing routine.
// It pulls the processor flags from the stack followed by the program counter.
func (cpu *CPU) RTI(mode AddressMode) {
	cpu.Status = StatusFlag(cpu.StackPop())
	cpu.SetFlag(B, false)
	cpu.SetFlag(U, true)

	cpu.PC = cpu.StackPopU16()
}

/* Branches */
// Branch instructions break the normal sequential flow of execution by changing the program counter if a specified
// condition is met. All the conditions are based on examining a single bit within the processor status.

// BCC - Branch if Carry Clear.
// If the carry flag is clear then add the relative displacement
// to the program counter to cause a branch to a new location.
// Function:    if(C == 0) pc = address
func (cpu *CPU) BCC(mode AddressMode) {
	jump := int8(cpu.read(cpu.PC))
	if cpu.GetFlag(C) == 0x00 {
		cpu.Cycle += 1

		addr := cpu.PC + uint16(jump) + 1
		cpu.PageCross(addr, cpu.PC+1)
		cpu.PC = addr
	}
}

// BCS - Branch if Carry Set.
// If the carry flag is set then add the relative displacement to the program counter to cause a branch
// to a new location.
// Function:    if(C == 1) pc = address
func (cpu *CPU) BCS(mode AddressMode) {
	jump := int8(cpu.read(cpu.PC))
	if cpu.GetFlag(C) == 0x01 {
		cpu.Cycle += 1

		addr := cpu.PC + uint16(jump) + 1
		cpu.PageCross(addr, cpu.PC+1)
		cpu.PC = addr
	}
}

// BEQ - Branch if Equal.
// If the zero flag is set then add the relative displacement to the program counter to cause a branch
// to a new location.
// Function:    if(Z == 1) pc = address
func (cpu *CPU) BEQ(mode AddressMode) {
	jump := int8(cpu.read(cpu.PC))
	if cpu.GetFlag(Z) == 0x01 {
		cpu.Cycle += 1
		addr := cpu.PC + uint16(jump) + 1
		cpu.PageCross(addr, cpu.PC+1)
		cpu.PC = addr
	}
}

// BMI - Branch if Minus
// If the negative flag is set then add the relative displacement to the program counter to cause a branch
// to a new location.
// Function:    if(N == 1) pc = address
func (cpu *CPU) BMI(mode AddressMode) {
	jump := int8(cpu.read(cpu.PC))
	if cpu.GetFlag(N) == 0x01 {
		cpu.Cycle += 1

		addr := cpu.PC + uint16(jump) + 1
		cpu.PageCross(addr, cpu.PC+1)
		cpu.PC = addr
	}
}

// BNE - Branch if Not Equal.
// If the zero flag is clear then add the relative displacement to the program counter to cause a branch
// to a new location.
// Function:    if(Z == 0) pc = address
func (cpu *CPU) BNE(mode AddressMode) {
	jump := int8(cpu.read(cpu.PC))
	if cpu.GetFlag(Z) == 0x00 {
		cpu.Cycle += 1

		addr := cpu.PC + uint16(jump) + 1
		cpu.PageCross(addr, cpu.PC+1)
		cpu.PC = addr
	}
}

// BPL - Branch if Positive.
// If the negative flag is clear then add the relative displacement to the program counter to cause a branch
// to a new location.
// Function:    if(N == 0) pc = address
func (cpu *CPU) BPL(mode AddressMode) {
	jump := int8(cpu.read(cpu.PC))
	if cpu.GetFlag(N) == 0x00 {
		cpu.Cycle += 1

		addr := cpu.PC + uint16(jump) + 1
		cpu.PageCross(addr, cpu.PC+1)
		cpu.PC = addr
	}
}

// BVS Branch if Overflow Set
// Function:    if(V == 1) pc = address
func (cpu *CPU) BVS(mode AddressMode) {
	jump := int8(cpu.read(cpu.PC))
	if cpu.GetFlag(V) == 0x01 {
		cpu.Cycle += 1

		addr := cpu.PC + uint16(jump) + 1
		cpu.PageCross(addr, cpu.PC+1)
		cpu.PC = addr
	}
}

// BVC Branch if Overflow Clear.
// If the overflow flag is clear then add the relative displacement to the program counter to cause a branch
// to a new location.
// Function:    if(V == 0) pc = address
func (cpu *CPU) BVC(mode AddressMode) {
	jump := int8(cpu.read(cpu.PC))
	if cpu.GetFlag(V) == 0x00 {
		cpu.Cycle += 1

		addr := cpu.PC + uint16(jump) + 1
		cpu.PageCross(addr, cpu.PC+1)
		cpu.PC = addr
	}
}

// NOP - No Operation
// The NOP instruction causes no changes to the processor other than the normal incrementing of the program
// counter to the next instruction.
func (cpu *CPU) NOP(mode AddressMode) {
	return
}

/* Register Transfers */
// The contents of the X and Y registers can be moved to or from the accumulator, setting the negative (N) and zero (Z)
// flags as appropriate.

// TAX Transfer Accumulator to X Register
// Copies the current contents of the accumulator into the X register and sets the zero and negative flags
// as appropriate.
// Function:    X = A
// Flags Out:   N, Z
func (cpu *CPU) TAX(mode AddressMode) {
	cpu.X = cpu.A
	cpu.UpdateZeroAndNegativeFlag(cpu.X)
}

// TAY Transfer Accumulator to Y Register
// Copies the current contents of the accumulator into the Y register and sets the zero and negative flags
// as appropriate.
// Function:    Y = A
// Flags Out:   N, Z
func (cpu *CPU) TAY(mode AddressMode) {
	cpu.Y = cpu.A
	cpu.UpdateZeroAndNegativeFlag(cpu.Y)
}

// TXA Transfer X Register to Accumulator
// Copies the current contents of the X register into the accumulator and sets the zero and negative flags
// as appropriate.
// Function:    A = Y
// Flags Out:   N, Z
func (cpu *CPU) TXA(mode AddressMode) {
	cpu.A = cpu.X
	cpu.UpdateZeroAndNegativeFlag(cpu.A)
}

// TYA Transfer Y Register to Accumulator
// Copies the current contents of the Y register into the accumulator and sets the zero and negative flags
// as appropriate.
// Function:    A = Y
// Flags Out:   N, Z
func (cpu *CPU) TYA(mode AddressMode) {
	cpu.A = cpu.Y
	cpu.UpdateZeroAndNegativeFlag(cpu.A)
}

/* Load and Store Operations */
// These instructions transfer a single byte between memory and one of the registers.
// Load operations set the negative (N) and zero (Z) flags depending on the value of transferred.
// Store operations do not affect the flag settings.

// LDA Load Accumulator Loads a byte of memory into the accumulator setting the zero and negative flags as appropriate.
// Function:  A = M
// Flags out: N, Z
func (cpu *CPU) LDA(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	cpu.A = value
	cpu.UpdateZeroAndNegativeFlag(cpu.A)
}

// LDX Load X Register
// Loads a byte of memory into the X register setting the zero and negative flags as appropriate.
// Function: X = M
// Flags: N, Z
func (cpu *CPU) LDX(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	cpu.X = value
	cpu.UpdateZeroAndNegativeFlag(cpu.X)
}

func (cpu *CPU) LDY(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	cpu.Y = value

	cpu.UpdateZeroAndNegativeFlag(cpu.Y)
}

// STA Store Accumulator at Address
// Function:    M = A
// Stores the contents of the accumulator into memory.
func (cpu *CPU) STA(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	cpu.write(addr, cpu.A)
}

// STX Store X Register at Address
// Function:    M = X
// Stores the contents of the X register into memory.
func (cpu *CPU) STX(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	cpu.write(addr, cpu.X)
}

// STY Store Y Register at Address
// Function:    M = Y
func (cpu *CPU) STY(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	cpu.write(addr, cpu.Y)
}

// DCP Decrement memory then Compare with A - DEC + CMP
func (cpu *CPU) DCP(mode AddressMode) {
	cpu.DEC(mode)
	cpu.CMP(mode)
}

// RLA Rotate Left then 'And' - ROL + AND
func (cpu *CPU) RLA(mode AddressMode) {
	cpu.ROL(mode)
	cpu.AND(mode)
}

// SLO Shift Left then 'Or' - ASL + ORA
func (cpu *CPU) SLO(mode AddressMode) {
	cpu.ASL(mode)
	cpu.ORA(mode)
}

// SRE Shift Right then "Exclusive-Or" - LSR + EOR
func (cpu *CPU) SRE(mode AddressMode) {
	cpu.LSR(mode)
	cpu.EOR(mode)
}

// NOPU - do nothing
func (cpu *CPU) NOPU(mode AddressMode) {
}

// AXS - A 'And' X, then Subtract memory, to X
func (cpu *CPU) AXS(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	cpu.SetFlag(C, value <= cpu.A&cpu.X)
	result := cpu.A&cpu.X - value
	cpu.UpdateZeroAndNegativeFlag(result)
	cpu.X = result
}

// ARR - AND then Rotate Right - AND+ROR
func (cpu *CPU) ARR(mode AddressMode) {
	cpu.AND(mode)
	cpu.ROR(mode)
	result := cpu.A
	bit5 := (result >> 5) & 1
	bit6 := (result >> 6) & 1
	cpu.SetFlag(C, bit6 == 1)
	cpu.SetFlag(V, bit5^bit6 == 1)
	cpu.UpdateZeroAndNegativeFlag(result)
}

// ANC -AND then copy N to C
func (cpu *CPU) ANC(mode AddressMode) {
	cpu.AND(mode)
	cpu.SetFlag(C, cpu.GetFlag(N) == 0x01)
}

// ALR - And then Logical Shift Right - AND+LSR
func (cpu *CPU) ALR(mode AddressMode) {
	cpu.AND(mode)
	cpu.LSR(mode)
}

// RRA - Rotate Right then Add with Carry - ROR + ADC
func (cpu *CPU) RRA(mode AddressMode) {
	cpu.ROR(mode)
	cpu.ADC(mode)
}

// ISB -  Increment memory then Subtract with Carry - INC + SBC
func (cpu *CPU) ISB(mode AddressMode) {
	cpu.INC(mode)
	cpu.SBC(mode)
}

// LXA LDA + TAX
func (cpu *CPU) LXA(mode AddressMode) {
	cpu.LDA(mode)
	cpu.TAX(mode)
}

// XAA TAX + AND
func (cpu *CPU) XAA(mode AddressMode) {
	cpu.TAX(mode)
	cpu.AND(mode)
}

// LAS
func (cpu *CPU) LAS(mode AddressMode) {

	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	result := value & cpu.SP
	cpu.A = result
	cpu.X = result
	cpu.SP = result
	cpu.UpdateZeroAndNegativeFlag(result)
}

// TAS
func (cpu *CPU) TAS(mode AddressMode) {
	value := cpu.A & cpu.X
	cpu.SP = value
	addr := cpu.readU16(cpu.PC) + uint16(cpu.Y)

	result := (uint8(addr>>8) + 1) & cpu.SP
	cpu.write(addr, result)
}

func (cpu *CPU) AHX(mode AddressMode) {
	switch mode {
	case IndirectY:
		pos := uint16(cpu.read(cpu.PC))
		addr := cpu.readU16(pos) + uint16(cpu.Y)
		value := cpu.A & cpu.X & uint8(addr>>8)
		cpu.write(addr, value)
	case AbsoluteY:
		addr := cpu.readU16(cpu.PC) + uint16(cpu.Y)
		value := cpu.A & cpu.X & uint8(addr>>8)
		cpu.write(addr, value)
	}
}

func (cpu *CPU) SHX(mode AddressMode) {
	addr := cpu.readU16(cpu.PC) + uint16(cpu.X)
	result := cpu.Y & (uint8(addr>>8) + 1)
	cpu.write(addr, result)
}

func (cpu *CPU) SHY(mode AddressMode) {
	addr := cpu.readU16(cpu.PC) + uint16(cpu.Y)
	result := cpu.X & (uint8(addr>>8) + 1)
	cpu.write(addr, result)
}

func (cpu *CPU) LAX(mode AddressMode) {
	addr := cpu.getOperandAddress(mode)
	value := cpu.read(addr)
	cpu.A = value
	cpu.UpdateZeroAndNegativeFlag(cpu.A)
	cpu.X = cpu.A
}

func (cpu *CPU) SAX(mode AddressMode) {
	value := cpu.A & cpu.X
	addr := cpu.getOperandAddress(mode)
	cpu.write(addr, value)
}
