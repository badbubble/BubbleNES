package cpu

type AddressMode string

const (
	Implied     AddressMode = "Implied"
	Accumulator             = "Accumulator"
	Immediate               = "Immediate"
	ZeroPage                = "ZeroPage"
	ZeroPageX               = "ZeroPageX"
	ZeroPageY               = "ZeroPageY"
	Relative                = "Relative"
	Absolute                = "Absolute"
	AbsoluteX               = "AbsoluteX"
	AbsoluteY               = "AbsoluteY"
	Indirect                = "Indirect"
	IndirectX               = "IndirectX"
	IndirectY               = "IndirectY"
)

// Implied
// For many 6502 instructions the source and destination of the information to be manipulated is implied directly by
// the function of the instruction itself and no further operand needs to be specified. Operations like '
// Clear Carry Flag' (CLC) and 'Return from Subroutine' (RTS) are Implied.
// There is no additional data required for this instruction. The instruction
// does something very simple like sets a status bit. However, we will
// target the accumulator, for instructions like PHA
func (cpu *CPU) Implied() uint16 {
	return 0x0000
}

// Accumulator Some instructions have an option to operate directly upon the accumulator. The programmer specifies this by using a special operand value, 'A'. For example:
// LSR A           ;Logical shift right one bit
// ROR A           ;Rotate right one bit
func (cpu *CPU) Accumulator() uint16 {
	return 0x0000
}

// Immediate
// this addressing mode allows the programmer to directly specify an 8 bit constant within the instruction.
// It is indicated by a '#' symbol followed by a numeric expression.
// For example:
//
//	LDA #10         ;Load 10 ($0A) into the accumulator
//	LDX #LO LABEL   ;Load the LSB of a 16-bit address into X
//	LDY #HI LABEL   ;Load the MSB of a 16-bit address into Y
//
// The instruction expects the next byte to be used as a value, so we'll prep
// the read address to point to the next byte
func (cpu *CPU) Immediate() uint16 {
	addr := cpu.PC
	return addr
}

// ZeroPage
// To save program bytes, zero page addressing allows you to absolutely address
// a location in first 0xFF bytes of address range. Clearly this only requires
// one byte instead of the usual two.
// An instruction using zero page addressing mode has only an 8-bit address operand. This limits it to addressing only
// the first 256 bytes of memory (e.g. $0000 to $00FF) where the most significant byte of the address is always zero.
// In zero-page mode only the least significant byte of the address is held in the instruction making it shorter by
// one byte (important for space-saving) and one less memory fetch during execution (important for speed).
// An assembler will automatically select zero page addressing mode if the operand evaluates to a zero-page address
// and the instruction supports the mode (not all do).
//
//	LDA $00         ;Load accumulator from $00
//	ASL ANSWER      ;Shift labelled location ANSWER left
func (cpu *CPU) ZeroPage() uint16 {
	data := uint16(cpu.read(cpu.PC))
	return data
}

// ZeroPageX Zero Page with X Offset
// Fundamentally the same as Zero Page addressing, but the contents of the X Register
// is added to the supplied single byte address. This is useful for iterating through
// ranges within the first page.
// The address to be accessed by an instruction using indexed zero page addressing is calculated by taking
// the 8 bit zero-page address from the instruction and adding the current value of the X register to it.
// For example if the X register contains $0F and the instruction LDA $80,X is executed then the accumulator will
// be loaded from $008F (e.g. $80 + $0F => $8F).
// The address calculation wraps around if the sum of the base address and the register exceed $FF.
// If we repeat the last example but with $FF in the X register then the accumulator will be loaded from
// $007F (e.g. $80 + $FF => $7F) and not $017F.
func (cpu *CPU) ZeroPageX() uint16 {
	addr := cpu.read(cpu.PC)
	addr += cpu.X
	// wraps around
	return uint16(addr) & 0x00FF
}

// ZeroPageY Zero Page with Y Offset
// Same as above but uses Y Register for offset
// The address to be accessed by an instruction using indexed zero page addressing is calculated by taking the 8 bit
// zero-page address from the instruction and adding the current value of the Y register to it.
// This mode can only be used with the LDX and STX instructions.
// LDX $10,Y       ;Load the X register from a location on zero page
// STX TEMP,Y      ;Store the X register in a location on zero page
func (cpu *CPU) ZeroPageY() uint16 {
	addr := cpu.read(cpu.PC)
	addr += cpu.Y
	return uint16(addr) & 0x00FF
}

// Relative
// This address mode is exclusive to branch instructions. The address
// must reside within -128 to +127 of the branch instruction, i.e.
// you cant directly branch to any address in the addressable range.
// Relative addressing mode is used by branch instructions (e.g. BEQ, BNE, etc.)
// which contain a signed 8 bit relative offset (e.g. -128 to +127) which is added to program counter if the condition
// is true.
// As the program counter itself is incremented during instruction execution by two the effective address range for the
// target instruction must be with -126 to +129 bytes of the branch.
//
//	BEQ LABEL       ;Branch if zero flag set to LABEL
//	BNE *+4         ;Skip over the following 2 byte instruction
func (cpu *CPU) Relative() uint16 {
	offset := uint16(cpu.read(cpu.PC))
	if offset&0x80 != 0 {
		return offset | 0xFF00
	}
	return offset
}

// Absolute
// A full 16-bit address is loaded and used
// Instructions using absolute addressing contain a full 16-bit address to identify the target location.
//
// JMP $1234       ;Jump to location $1234
// JSR WIBBLE      ;Call subroutine WIBBLE
func (cpu *CPU) Absolute() uint16 {
	data := cpu.readU16(cpu.PC)
	return data
}

// AbsoluteX with X Offset
// Fundamentally the same as absolute addressing, but the contents of the X Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required.
// The address to be accessed by an instruction using X register indexed absolute addressing is computed by
// taking the 16-bit address from the instruction and added the contents of the X register.
// For example if X contains $92 then an STA $2000,X instruction will store the accumulator at $2092 (e.g. $2000 + $92).
// STA $3000,X     ;Store accumulator between $3000 and $30FF
// ROR CRC,X       ;Rotate right one bit
func (cpu *CPU) AbsoluteX() uint16 {
	source := cpu.readU16(cpu.PC)
	result := source + uint16(cpu.X)
	cpu.PageCross(source, result)
	return result & 0xFFFF
}

// AbsoluteY with Y Offset
// Fundamentally the same as absolute addressing, but the contents of the Y Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required.
// The Y register indexed absolute addressing mode is the same as the previous mode only with the contents of the
// Y register added to the 16-bit address from the instruction.
// AND $4000,Y     ;Perform a logical AND with a byte of memory
// STA MEM,Y       ;Store accumulator in memory
func (cpu *CPU) AbsoluteY() uint16 {
	source := cpu.readU16(cpu.PC)
	result := source + uint16(cpu.Y)
	cpu.PageCross(source, result)
	return result & 0xFFFF
}

// Indirect
// The supplied 16-bit address is read to get the actual 16-bit address. This is
// instruction is unusual in that it has a bug in the hardware! To emulate its
// function accurately, we also need to emulate this bug. If the low byte of the
// supplied address is 0xFF, then to read the high byte of the actual address
// we need to cross a page boundary. This does not actually work on the chip as
// designed, instead it wraps back around in the same page, yielding an
// invalid actual address.
// JMP is the only 6502 instruction to support indirection.
// The instruction contains a 16-bit address which identifies the location of the least significant byte of another
// 16-bit memory address which is the real target of the instruction.
// For example if location $0120 contains $FC and location $0121 contains $BA then the instruction JMP ($0120)
// will cause the next instruction execution to occur at $BAFC (e.g. the contents of $0120 and $0121).
// JMP ($FFFC)     ;Force a power on reset
// JMP (TARGET)    ;Jump via a labelled memory area
func (cpu *CPU) Indirect() uint16 {
	inAddr := cpu.readU16(cpu.PC)
	return cpu.readU16(inAddr)
}

// IndirectX
// The supplied 8-bit address is offset by X Register to index
// a location in page 0x00. The actual 16-bit address is read
// from this location
// Indexed indirect addressing is normally used in conjunction with a table of address held on zero page.
// The address of the table is taken from the instruction and the X register added to it (with zero page wrap around)
// to give the location of the least significant byte of the target address.
// LDA ($40,X)     ;Load a byte indirectly from memory
// STA (MEM,X)     ;Store accumulator indirectly into memory
func (cpu *CPU) IndirectX() uint16 {
	baseAddr := uint16(cpu.read(cpu.PC))
	baseAddr += uint16(cpu.X)
	baseAddr &= 0x00FF
	lo := uint16(cpu.read(baseAddr))
	hi := uint16(cpu.read((baseAddr + 1) & 0x00FF))
	return (hi << 8) | lo
}

// IndirectY
// The supplied 8-bit address indexes a location in page 0x00. From
// here the actual 16-bit address is read, and the contents of
// Y Register is added to it to offset it. If the offset causes a
// change in page then an additional clock cycle is required.
// Indirect indexed addressing is the most common indirection mode used on the 6502.
// In instruction contains the zero-page location of the least significant byte of 16-bit address.
// The Y register is dynamically added to this value to generate the actual target address for operation.
// LDA ($40),Y     ;Load a byte indirectly from memory
// STA (DST),Y     ;Store accumulator indirectly into memory
func (cpu *CPU) IndirectY() uint16 {
	baseAddr := uint16(cpu.read(cpu.PC))
	lo := uint16(cpu.read(baseAddr))
	hi := uint16(cpu.read((baseAddr + 1) & 0x00FF))
	source := (hi << 8) | lo
	result := source + uint16(cpu.Y)
	cpu.PageCross(source, result)
	return result
}
