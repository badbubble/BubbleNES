package main

import (
	"Nes/bus"
	"Nes/cartridge"
	"Nes/cpu"
	"log"
	"os"
)

func main() {
	data, err := os.ReadFile("/Users/badbubble/GolandProjects/Nes/test/nestest.nes")
	if err != nil {
		log.Fatal(err)
	}
	r := cartridge.New([]uint8(data))
	b := bus.New(r)
	c := cpu.New(b)
	c.Reset()
	c.PC = 0xC000
	c.Run()
}
