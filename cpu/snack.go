package cpu

import (
	"fmt"
	"unsafe"

	"github.com/veandco/go-sdl2/sdl"
)

func getColor(m uint8) (r, g, b uint8) {
	switch m {
	// black
	case 0:
		return 0, 0, 0
	// white
	case 1:
		return 255, 255, 255
	// gray
	case 2, 9:
		return 128, 128, 128
	// red
	case 3, 10:
		return 255, 0, 0
	// green
	case 4, 11:
		return 0, 128, 0
	// blue
	case 5, 12:
		return 0, 0, 255
	// magenta
	case 6, 13:
		return 255, 0, 255
	// yellow
	case 7, 14:
		return 255, 255, 0
	// cyan
	default:
		return 0, 255, 255
	}

}

//	func getUserInput(c *cpu.CPU) {
//		var joysticks [16]*sdl.Joystick
//		sdl.JoystickEventState(sdl.ENABLE)
//		running := true
//		for running {
//			for event := sdl.PollEvent(); event != nil; event = sdl.PollEvent() {
//				switch t := event.(type) {
//				case *sdl.QuitEvent:
//					running = false
//				case *sdl.JoyDeviceAddedEvent:
//					joysticks[int(t.Which)] = sdl.JoystickOpen(int(t.Which))
//					if joysticks[int(t.Which)] != nil {
//						fmt.Printf("Joystick %d connected\n", t.Which)
//					}
//				case *sdl.JoyButtonEvent:
//					if t.State == 1 {
//						switch t.Button {
//						case 11:
//							c.Write(0xff, 0x77)
//						case 12:
//							c.Write(0xff, 0x73)
//						case 13:
//							c.Write(0xff, 0x61)
//						case 14:
//							c.Write(0xff, 0x64)
//						}
//					}
//				}
//			}
//
//			sdl.Delay(16)
//		}
//	}
func (cpu *CPU) ReadScreen(frame *[32 * 32 * 3]uint8) (fresh bool) {
	fresh = false
	frameID := 0
	for _, value := range cpu.Memory[0x200:0x600] {
		r, g, b := getColor(value)
		if frame[frameID] != r || frame[frameID+1] != g || frame[frameID+2] != b {
			fresh = true
			frame[frameID] = r
			frame[frameID+1] = g
			frame[frameID+2] = b
		}
		frameID += 3
	}
	return
}

func (cpu *CPU) RunSnack(renderer *sdl.Renderer, texture *sdl.Texture) {
	var opcode uint8
	var pcStatus uint16
	var screenData [32 * 32 * 3]uint8

	// CPU cycle
	for {
		opcode = cpu.read(cpu.PC)
		mode := cpu.Lookup[opcode].Mode
		cpu.PC += 1
		pcStatus = cpu.PC
		fmt.Printf("%X-%X-%X-%X-%X-%X\n", opcode, cpu.PC, cpu.A, cpu.X, cpu.Y, uint8(cpu.Status))

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
			//cpu.BRK(mode)
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
		//time.Sleep(1 * time.Second)

		if cpu.ReadScreen(&screenData) {
			texture.Update(nil, unsafe.Pointer(&screenData), 32*3)
			renderer.Copy(texture, nil, nil)
			renderer.Present()
			//sdl.Delay(16000)
		}

	}
}
