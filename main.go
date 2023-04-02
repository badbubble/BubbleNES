package main

import (
	"Nes/game"
	"fmt"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Println("need .nes file path.")
		fmt.Println("./BubbleNes /path/to/nes/file")
	}

	g := game.New(os.Args[1])
	g.Run()
}
