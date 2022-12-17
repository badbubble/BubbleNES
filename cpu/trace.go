package cpu

import "fmt"

func (cpu *CPU) Trace() string {
	var hexdump []interface{}
	var memAddr uint16
	var address uint8
	var addressU16 uint16
	var jumAddr uint16
	var value uint8
	var hexStr string
	var asmStr string
	var tmpStr string

	code := cpu.read(cpu.PC)
	ops := cpu.Lookup[code]
	begin := cpu.PC
	hexdump = append(hexdump, code)

	if ops.Mode == Immediate || ops.Mode == Implied {
		memAddr = 0
		value = 0
	} else {
		memAddr = cpu.getAbsoluteAddress(ops.Mode, begin+1)
		value = cpu.read(memAddr)
	}

	if ops.Length == 1 {
		if code == 0x0a || code == 0x4a || code == 0x2a || code == 0x6a {
			tmpStr = "A "
		} else {
			tmpStr = ""
		}
	} else if ops.Length == 2 {
		address = cpu.read(begin + 1)
		hexdump = append(hexdump, address)

		switch ops.Mode {
		case Implied, Accumulator:
			tmpStr = fmt.Sprintf("$%04X", uint(begin)+2+uint(address))
		case Immediate:
			tmpStr = fmt.Sprintf("#$%02X", address)
		case ZeroPage:
			tmpStr = fmt.Sprintf("$%02X = %02X", memAddr, value)
		case ZeroPageX:
			tmpStr = fmt.Sprintf("$%02X,X @ %02X = %02X", address, memAddr, value)
		case ZeroPageY:
			tmpStr = fmt.Sprintf("$%02X,Y @ %02X = %02X", address, memAddr, value)
		case IndirectX:
			tmpStr = fmt.Sprintf("($%02X,X) @ %02X = %04X = %02X", address, address+cpu.X, memAddr, value)
		case IndirectY:
			tmpStr = fmt.Sprintf("($%02X),Y = %04X @ %04X = %02X", address, memAddr-uint16(cpu.Y), memAddr, value)
		case Relative:
			tmpStr = fmt.Sprintf("$%04X", memAddr)
		default:
			panic(fmt.Sprintf("unexpected mode %s for ops has two bytes", ops.Mode))
		}
	} else if ops.Length == 3 {
		lo := cpu.read(begin + 1)
		hi := cpu.read(begin + 2)
		hexdump = append(hexdump, lo)
		hexdump = append(hexdump, hi)

		addressU16 = cpu.readU16(begin + 1)

		switch ops.Mode {
		case Implied, Accumulator:
			tmpStr = fmt.Sprintf("$%04X", addressU16)
		case Absolute:
			switch code {
			case 0x4c, 0x20:
				tmpStr = fmt.Sprintf("$%04X", memAddr)

			default:
				tmpStr = fmt.Sprintf("$%04X = %02X", memAddr, value)

			}
		case AbsoluteX:
			tmpStr = fmt.Sprintf("$%04X,X @ %04X = %02X", addressU16, memAddr, value)
		case AbsoluteY:
			tmpStr = fmt.Sprintf("$%04X,Y @ %04X = %02X", addressU16, memAddr, value)
		case Indirect:
			if code == 0x6c {
				if addressU16&0x00FF == 0x00FF {
					lo := cpu.read(addressU16)
					hi := cpu.read(addressU16 & 0xFF00)
					jumAddr = uint16(hi)<<8 | uint16(lo)
				} else {
					jumAddr = cpu.readU16(addressU16)
				}
				tmpStr = fmt.Sprintf("($%04X) = %04X", addressU16, jumAddr)
			}
		default:
			panic(fmt.Sprintf("unexpected mode %s for ops has three bytes", ops.Mode))
		}

	} else {
		tmpStr = ""
	}
	for _, hex := range hexdump {
		if len(hexStr) != 0 {
			hexStr += fmt.Sprintf(" ")
		}
		hexStr += fmt.Sprintf("%02X", hex)
	}

	asmStr = fmt.Sprintf("%04X  %-8s %4s %s", begin, hexStr, ops.Name, tmpStr)
	return fmt.Sprintf("%-47s A:%02X X:%02X Y:%02X P:%02X SP:%02X", asmStr, cpu.A, cpu.X, cpu.Y, cpu.Status, cpu.SP)
}
