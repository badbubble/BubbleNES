package nes

import (
	"Nes/bus"
	"Nes/cartridge"
	"Nes/cpu"
	"Nes/ppu"
	"log"
	"os"
)

type Nes struct {
	CPU          *cpu.CPU
	PPU          *ppu.PPU
	Bus          *bus.Bus
	Cart         *cartridge.Cartridge
	SystemCycles uint
}

func (nes *Nes) Clock() {
	nes.PPU.Clock()
	if nes.SystemCycles%3 == 0 {
		nes.CPU.Clock()
	}
	if nes.PPU.NMI {
		nes.PPU.NMI = false
		nes.CPU.NMI()
	}
	nes.SystemCycles += 1
}

func (nes *Nes) Reset() {
	nes.CPU.Reset()
	nes.Cart.Reset()
	nes.Bus.Reset()
	nes.PPU.Reset()

	nes.SystemCycles = 0
}

func New(gamePath string) *Nes {
	data, err := os.ReadFile(gamePath)
	if err != nil {
		log.Fatal(err)
	}
	cart := cartridge.New(data)
	p := ppu.New(cart)
	b := bus.New(cart, p)
	c := cpu.New(b)
	return &Nes{
		CPU:          c,
		PPU:          p,
		Bus:          b,
		Cart:         cart,
		SystemCycles: 0,
	}
}
