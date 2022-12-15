package cpu

import (
	"fmt"
	"testing"
)

func TestCPU_LDX_Immediate(t *testing.T) {
	cpu := New()
	program := []uint8{0xA2, 0x05, 0x00}
	cpu.LoadAndRun(program)
	if cpu.X != 0x05 {
		t.Fatalf("X register should be %X but got %X instead.", 0x05, cpu.X)
	}
	if cpu.Status&0b0000_0010 != 0 {
		t.Fatalf("Zero flag should be %d but got %d instead.", 0, 1)
	}
	if cpu.Status&0b1000_0000 != 0 {
		t.Fatalf("Negative flag should be %d but got %d instead.", 0, 1)
	}

	// Immediate Zero Flag
	cpu.Reset()
	program = []uint8{0xA2, 0x00, 0x00}
	cpu.LoadAndRun(program)
	if cpu.X != 0x00 {
		t.Fatalf("X register should be %X but got %X instead.", program[1], cpu.A)
	}
	if cpu.Status&0b0000_0010 == 0 {
		t.Fatalf("Zero flag should be %d but got %d instead.", 1, 0)
	}
	if cpu.Status&0b1000_0000 != 0 {
		t.Fatalf("Negative flag should be %d but got %d instead.", 0, 1)
	}

	// Immediate Negative Flag
	cpu.Reset()
	program = []uint8{0xA2, 0xFF, 0x00}
	cpu.LoadAndRun(program)
	if cpu.X != 0xFF {
		t.Fatalf("X register should be %X but got %X instead.", program[1], cpu.X)
	}
	if cpu.Status&0b0000_0010 != 0 {
		t.Fatalf("Zero flag should be %d but got %d instead.", 0, 1)
	}
	if cpu.Status&0b1000_0000 == 0 {
		t.Fatalf("Negative flag should be %d but got %d instead.", 1, 0)
	}
}

func TestCPU_LDX_ZeroPage(t *testing.T) {
	cpu := New()
	cpu.write(0x0022, 0xFF)
	program := []uint8{0xA6, 0x22, 0x00}
	cpu.LoadAndRun(program)

	if cpu.X != 0xFF {
		t.Fatalf("X register should be %X but got %X instead.", program[1], cpu.A)
	}
}

func TestCPU_LDX_ZeroPageY(t *testing.T) {

	cpu := New()
	cpu.write(0x0022, 0xFF)
	program := []uint8{0xa0, 0x01, 0xB6, 0x21, 0x00}
	cpu.LoadAndRun(program)

	if cpu.X != 0xFF {
		t.Fatalf("X register should be %X but got %X instead.", program[1], cpu.X)
	}
}

func TestCPU_LDX_Absolute(t *testing.T) {
	cpu := New()
	cpu.write(0x1234, 0xff)
	program := []uint8{0xae, 0x34, 0x12, 0x00}
	cpu.LoadAndRun(program)

	if cpu.X != 0xFF {
		t.Fatalf("X register should be %X but got %X instead.", program[1], cpu.A)
	}
}

func TestCPU_LDX_AbsoluteY(t *testing.T) {

	cpu := New()
	cpu.write(0x1234, 0xff)
	program := []uint8{0xa0, 0x01, 0xbe, 0x33, 0x12, 0x00}
	cpu.LoadAndRun(program)

	if cpu.X != 0xFF {
		t.Fatalf("X register should be %X but got %X instead.", 0xff, cpu.X)
	}
}

func TestCPU_LDY_Immediate(t *testing.T) {
	cpu := New()
	program := []uint8{0xa0, 0x05, 0x00}
	cpu.LoadAndRun(program)

	if cpu.Y != 0x05 {
		t.Fatalf("Y register should be %X but got %X instead.", 0x05, cpu.Y)
	}
}

func TestCPU_LDY_ZeroPage(t *testing.T) {
	cpu := New()
	cpu.write(0x0012, 0xff)
	program := []uint8{0xa4, 0x12, 0x00}
	cpu.LoadAndRun(program)

	if cpu.Y != 0xff {
		t.Fatalf("Y register should be %X but got %X instead.", 0xff, cpu.Y)
	}
}

func TestCPU_LDY_ZeroPageX(t *testing.T) {
	cpu := New()
	cpu.write(0x0012, 0xff)
	program := []uint8{0xa2, 0x01, 0xb4, 0x11, 0x00}
	cpu.LoadAndRun(program)

	if cpu.Y != 0xff {
		t.Fatalf("Y register should be %X but got %X instead.", 0xff, cpu.Y)
	}
}

func TestCPU_LDY_Absolute(t *testing.T) {
	cpu := New()
	cpu.write(0x1234, 0xff)
	program := []uint8{0xac, 0x34, 0x12, 0x00}
	cpu.LoadAndRun(program)

	if cpu.Y != 0xff {
		t.Fatalf("Y register should be %X but got %X instead.", 0xff, cpu.Y)
	}
}

func TestCPU_LDY_AbsoluteX(t *testing.T) {
	cpu := New()
	cpu.write(0x1234, 0xff)
	program := []uint8{0xa2, 0x01, 0xbc, 0x33, 0x12, 0x00}
	cpu.LoadAndRun(program)

	if cpu.Y != 0xff {
		t.Fatalf("Y register should be %X but got %X instead.", 0xff, cpu.Y)
	}
}

func TestCPU_LDA_Immediate(t *testing.T) {
	cpu := New()
	// Immediate
	program := []uint8{0xA9, 0x05, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0x05 {
		t.Fatalf("A register should be %X but got %X instead.", 0x05, cpu.A)
	}
	if cpu.Status&0b0000_0010 != 0 {
		t.Fatalf("Zero flag should be %d but got %d instead.", 0, 1)
	}
	if cpu.Status&0b1000_0000 != 0 {
		t.Fatalf("Negative flag should be %d but got %d instead.", 0, 1)
	}

	// Immediate Zero Flag
	cpu.Reset()
	program = []uint8{0xA9, 0x00, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != program[1] {
		t.Fatalf("A register should be %X but got %X instead.", program[1], cpu.A)
	}
	if cpu.Status&0b0000_0010 == 0 {
		t.Fatalf("Zero flag should be %d but got %d instead.", 1, 0)
	}
	if cpu.Status&0b1000_0000 != 0 {
		t.Fatalf("Negative flag should be %d but got %d instead.", 0, 1)
	}
	// Immediate Negative Flag
	cpu.Reset()
	program = []uint8{0xA9, 0xFF, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != program[1] {
		t.Fatalf("A register should be %X but got %X instead.", program[1], cpu.A)
	}
	if cpu.Status&0b0000_0010 != 0 {
		t.Fatalf("Zero flag should be %d but got %d instead.", 0, 1)
	}
	if cpu.Status&0b1000_0000 == 0 {
		t.Fatalf("Negative flag should be %d but got %d instead.", 1, 0)
	}
}

func TestCPU_LDA_ZerPage(t *testing.T) {
	cpu := New()
	cpu.Reset()
	cpu.write(0x0011, 0xFF)
	program := []uint8{0xA5, 0x11, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0xFF {
		t.Fatalf("A register should be %X but got %X instead.", 0xFF, cpu.A)
	}
}

func TestCPU_LDA_ZeroPageX(t *testing.T) {
	cpu := New()
	cpu.write(0x0011, 0xFF)
	program := []uint8{0xa2, 0x10, 0xb5, 0x01, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0xFF {
		t.Fatalf("A register should be %X but got %X instead.", 0xFF, cpu.A)
	}
}

func TestCPU_LDA_ZeroPageX_Overflow(t *testing.T) {
	cpu := New()
	cpu.write(0x0010, 0xFF)
	program := []uint8{0xa2, 0xff, 0xb5, 0x11, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0xFF {
		t.Fatalf("A register should be %X but got %X instead.", 0xFF, cpu.A)
	}
}

func TestCPU_LDA_Absolute(t *testing.T) {
	cpu := New()
	cpu.write(0x1234, 0xff)
	program := []uint8{0xad, 0x34, 0x12, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0xff {
		t.Fatalf("A register should be %X but got %X instead.", 0xFF, cpu.A)
	}

}

func TestCPU_LDA_AbsoluteX(t *testing.T) {
	cpu := New()
	cpu.write(0x1234, 0xff)
	program := []uint8{0xa2, 0x01, 0xbd, 0x33, 0x12, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0xff {
		t.Fatalf("A register should be %X but got %X instead.", 0xFF, cpu.A)
	}
}

func TestCPU_LDA_AbsoluteY(t *testing.T) {
	cpu := New()
	cpu.write(0x1234, 0xff)
	program := []uint8{0xa0, 0x01, 0xb9, 0x33, 0x12, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0xff {
		t.Fatalf("A register should be %X but got %X instead.", 0xFF, cpu.A)
	}
}

func TestCPU_LDA_IndirectX(t *testing.T) {
	cpu := New()
	cpu.write(0x1234, 0xff)
	cpu.write(0x0010, 0x34)
	cpu.write(0x0011, 0x12)

	program := []uint8{0xa2, 0x01, 0xa1, 0x0F, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0xff {
		t.Fatalf("A register should be %X but got %X instead.", 0xFF, cpu.A)
	}
}

func TestCPU_LDA_IndirectY(t *testing.T) {
	cpu := New()
	cpu.write(0x1234, 0xff)
	cpu.write(0x0010, 0x34)
	cpu.write(0x0011, 0x12)

	program := []uint8{0xa0, 0x01, 0xb1, 0x0F, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0xff {
		t.Fatalf("A register should be %X but got %X instead.", 0xFF, cpu.A)
	}
}

func TestCPU_ADC_Immediate(t *testing.T) {
	cpu := New()
	program := []uint8{0xa9, 0x01, 0x69, 0x01, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0x02 {
		t.Fatalf("A register should be %X but got %X instead.", 0x02, cpu.A)
	}
}

func TestCPU_ADC_Immediate_Carry(t *testing.T) {
	cpu := New()
	program := []uint8{0xa9, 0xff, 0x69, 0x01, 0x69, 0x01, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0x02 {
		t.Fatalf("A register should be %X but got %X instead.", 0x02, cpu.A)
	}
}

func TestCPU_ADC_Immediate_Overflow(t *testing.T) {
	cpu := New()
	program := []uint8{0xa9, 0x7f, 0x69, 0x01, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0x80 {
		t.Fatalf("A register should be %X but got %X instead.", 0x80, cpu.A)
	}
	fmt.Println(cpu.GetFlag(V))
}

func TestCPU_SBC(t *testing.T) {
	cpu := New()
	program := []uint8{0xa9, 0x04, 0xe9, 0x01, 0x00}
	cpu.LoadAndRun(program)
	if cpu.A != 0x03 {
		t.Fatalf("A register should be %X but got %X instead.", 0x03, cpu.A)
	}
}

func TestCPU_TAX(t *testing.T) {
	cpu := New()
	program := []uint8{0xA9, 0x05, 0xAA, 0x00}
	cpu.LoadAndRun(program)

	if cpu.A != cpu.X || cpu.X != program[1] {
		t.Fatalf("X register got wrong value. should=%X, got=%X", program[1], cpu.X)
	}
}

func TestCPU_INX(t *testing.T) {
	cpu := New()
	program := []uint8{0xA9, 0xC0, 0xAA, 0xE8, 0x00}
	cpu.LoadAndRun(program)
	if cpu.X != 0xC0+1 {
		t.Fatalf("X register got wrong value. should=%X, got=%X", program[1]+1, cpu.X)
	}
}
