package main

import (
	"Nes/game"
	"embed"
	"fmt"
	"log"
	"os"
)

//go:embed game/fonts/UbuntuMono-Regular.ttf
var FontFile embed.FS

func main() {
	if len(os.Args) != 2 {
		fmt.Println("need .nes file path.")
		fmt.Println("./BubbleNes /path/to/nes/file")
	}
	data, _ := FontFile.ReadFile("game/fonts/UbuntuMono-Regular.ttf")
	if err := os.WriteFile("font.ttf", data, 0666); err != nil {
		log.Fatal(err)
	}

	g := game.New(os.Args[1], "font.ttf")
	g.Run()
}
