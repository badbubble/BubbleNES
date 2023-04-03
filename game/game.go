package game

import (
	"Nes/nes"
	"bufio"
	"fmt"
	"github.com/veandco/go-sdl2/sdl"
	"github.com/veandco/go-sdl2/ttf"
	"image"
	"image/jpeg"
	"os"
	"unsafe"
)

var (
	TitleName         = "Bubble Nes"
	WIDTH     int32   = 1280
	HEIGHT    int32   = 1000
	SCALE     float32 = 3.0
	FontSize  int     = 30
	KEY_W     uint8   = 0b0000_1000
	KEY_S     uint8   = 0b0000_0100
	KEY_A     uint8   = 0b0000_0010
	KEY_D     uint8   = 0b0000_0001
	KEY_L     uint8   = 0b1000_0000
	KEY_K     uint8   = 0b0100_0000
	KEY_O     uint8   = 0b0010_0000
	KEY_P     uint8   = 0b0001_0000
)

type Game struct {
	Nes      *nes.Nes
	Window   *sdl.Window
	Renderer *sdl.Renderer
	Font     *ttf.Font
}

func (g *Game) Run() {
	var FrameCount int
	var LastTick uint64
	var Frame int

	// Keep the window open until the user closes it
	for {

		tick := sdl.GetTicks64()
		g.Nes.Bus.Controllers[0] = 0x00

		for event := sdl.PollEvent(); event != nil; event = sdl.PollEvent() {
			switch t := event.(type) {
			case *sdl.QuitEvent:
				return
			case *sdl.KeyboardEvent:
				if t.Repeat == 0 && t.Type == sdl.KEYDOWN {
					switch t.Keysym.Scancode {
					case sdl.SCANCODE_W:
						g.Nes.Bus.Controllers[0] |= KEY_W
					case sdl.SCANCODE_S:
						g.Nes.Bus.Controllers[0] |= KEY_S
					case sdl.SCANCODE_A:
						g.Nes.Bus.Controllers[0] |= KEY_A
					case sdl.SCANCODE_D:
						g.Nes.Bus.Controllers[0] |= KEY_D
					case sdl.SCANCODE_O:
						g.Nes.Bus.Controllers[0] |= KEY_O
					case sdl.SCANCODE_P:
						g.Nes.Bus.Controllers[0] |= KEY_P
					case sdl.SCANCODE_K:
						g.Nes.Bus.Controllers[0] |= KEY_K
					case sdl.SCANCODE_L:
						g.Nes.Bus.Controllers[0] |= KEY_L
					}
				}
			}
		}
		err := g.Renderer.Clear()
		if err != nil {
			panic(err)
		}
		for {
			if g.Nes.PPU.IsFrameComplete {
				g.UpdateFrame()
				g.Nes.PPU.IsFrameComplete = false
				break
			} else {
				g.Nes.Clock()
			}
		}
		g.DrawCPU()
		g.DrawPatternTable()
		g.DrawCart()
		g.DrawPPU(Frame)
		g.DrawMem()

		if tick-LastTick >= 1000 {
			LastTick = tick
			Frame = FrameCount
			FrameCount = 0
		} else {
			FrameCount += 1
		}
		g.Renderer.Present()
		elapsed := sdl.GetTicks64() - tick

		if elapsed < 1000/30 {
			sdl.Delay(uint32(1000/30 - elapsed))
		}

	}
}
func saveImage(img image.Image, filename string) error {
	outFile, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer outFile.Close()
	b := bufio.NewWriter(outFile)
	err = jpeg.Encode(b, img, nil)
	if err != nil {
		return err
	}
	err = b.Flush()
	if err != nil {
		return err
	}
	return nil
}

func (g *Game) UpdateFrame() {
	img := g.Nes.PPU.Frame
	//saveImage(g.Nes.PPU.Frame, "123.jpeg")
	surface, err := sdl.CreateRGBSurfaceFrom(
		unsafe.Pointer(&img.Pix[0]),
		int32(img.Bounds().Dx()),
		int32(img.Bounds().Dy()),
		32,
		img.Stride,
		0x000000ff, 0x0000ff00, 0x00ff0000, 0xff000000,
	)
	if err != nil {
		panic(err)
	}
	defer surface.Free()

	texture, err := g.Renderer.CreateTextureFromSurface(surface)
	if err != nil {
		panic(err)
	}
	defer func(texture *sdl.Texture) {
		err := texture.Destroy()
		if err != nil {
			panic(err)
		}
	}(texture)
	// render to windows
	err = g.Renderer.Copy(texture, &sdl.Rect{
		X: 0,
		Y: 0,
		W: 256,
		H: 240,
	}, &sdl.Rect{
		X: 0,
		Y: 0,
		W: 256,
		H: 240,
	})
	if err != nil {
		panic(err)
	}
}

func (g *Game) DrawMem() {
	g.DrawString("Memory Trace", 240, 242, [3]uint8{0x0E, 0xA2, 0x93})
	mem := g.Nes.CPU.PC
	length := g.Nes.CPU.Lookup[g.Nes.CPU.ReadTrace(mem)].Length
	for i := 0; i < 5; i++ {
		if i == 0 {
			g.DrawString(g.Nes.CPU.TraceForUI(mem), 190, int32(255+i*15), [3]uint8{0xF9, 0x4A, 0x29})
		} else {
			g.DrawString(g.Nes.CPU.TraceForUI(mem), 190, int32(255+i*15), [3]uint8{0x00, 0x23, 0x5B})
		}

		mem += length
	}

}

func (g *Game) DrawPPU(fps int) {
	g.DrawString("PPU", 330, 160, [3]uint8{0x0E, 0xA2, 0x93})
	g.DrawString(fmt.Sprintf("FPS: %d", fps), 315, 175, [3]uint8{0, 0, 0})

}

func (g *Game) DrawCart() {
	g.DrawString("Cartridge", 320, 80, [3]uint8{0x0E, 0xA2, 0x93})
	g.DrawString(fmt.Sprintf("Mirroring Type: %s", g.Nes.Cart.ScreenMirroring), 280, 95, [3]uint8{0, 0, 0})
	g.DrawString(fmt.Sprintf("Mapper ID: %d", g.Nes.Cart.MapperID), 280, 110, [3]uint8{0, 0, 0})
	g.DrawString(fmt.Sprintf("PRG Size: %d bytes", len(g.Nes.Cart.PRGRom)/1024), 280, 125, [3]uint8{0, 0, 0})
	g.DrawString(fmt.Sprintf("CHR Size: %d bytes", len(g.Nes.Cart.CHRRom)/1024), 280, 140, [3]uint8{0, 0, 0})
}

func (g *Game) DrawPatternTable() {
	g.Nes.PPU.GetPatternTable(0, 0)
	g.Nes.PPU.GetPatternTable(1, 0)
	// Create a new surface and fill it with random black and white pixels
	PatternTbl1Surf, _ := sdl.CreateRGBSurfaceFrom(unsafe.Pointer(&g.Nes.PPU.PatternTableImage[0].Pix[0]), int32(g.Nes.PPU.PatternTableImage[0].Bounds().Dx()), int32(g.Nes.PPU.PatternTableImage[0].Bounds().Dy()), 32, g.Nes.PPU.PatternTableImage[0].Stride, 0x000000ff, 0x0000ff00, 0x00ff0000, 0xff000000)
	defer PatternTbl1Surf.Free()
	// Create a new texture from the surface
	PatternTbl1Frame, _ := g.Renderer.CreateTextureFromSurface(PatternTbl1Surf)
	defer PatternTbl1Frame.Destroy()
	PatternTbl2Surf, _ := sdl.CreateRGBSurfaceFrom(unsafe.Pointer(&g.Nes.PPU.PatternTableImage[1].Pix[0]), int32(g.Nes.PPU.PatternTableImage[0].Bounds().Dx()), int32(g.Nes.PPU.PatternTableImage[0].Bounds().Dy()), 32, g.Nes.PPU.PatternTableImage[0].Stride, 0x000000ff, 0x0000ff00, 0x00ff0000, 0xff000000)
	defer PatternTbl2Surf.Free()
	// Create a new texture from the surface
	PatternTbl2Frame, _ := g.Renderer.CreateTextureFromSurface(PatternTbl2Surf)
	defer PatternTbl2Frame.Destroy()

	g.DrawString("Pattern Tabel", 55, 242, [3]uint8{0x0E, 0xA2, 0x93})
	g.Renderer.Copy(PatternTbl1Frame, &sdl.Rect{
		X: 0,
		Y: 0,
		W: 128,
		H: 128,
	}, &sdl.Rect{
		X: 5,
		Y: 253,
		W: 80,
		H: 80,
	})

	g.Renderer.Copy(PatternTbl2Frame, &sdl.Rect{
		X: 0,
		Y: 0,
		W: 128,
		H: 128,
	}, &sdl.Rect{
		X: 95,
		Y: 253,
		W: 80,
		H: 80,
	})

}

func (g *Game) DrawCPU() {

	g.DrawString("CPU", 330, 0, [3]uint8{0x0E, 0xA2, 0x93})

	g.DrawCPUStatus("C", 300, 15, g.Nes.CPU.Status&0b1000_0000 != 0)

	g.DrawCPUStatus("Z", 310, 15, g.Nes.CPU.Status&0b0100_0000 != 0)

	g.DrawCPUStatus("I", 320, 15, g.Nes.CPU.Status&0b0010_0000 != 0)

	g.DrawCPUStatus("D", 330, 15, g.Nes.CPU.Status&0b0001_0000 != 0)

	g.DrawCPUStatus("B", 340, 15, g.Nes.CPU.Status&0b0000_1000 != 0)

	g.DrawCPUStatus("U", 350, 15, g.Nes.CPU.Status&0b0000_0100 != 0)

	g.DrawCPUStatus("V", 360, 15, g.Nes.CPU.Status&0b0000_0010 != 0)

	g.DrawCPUStatus("N", 370, 15, g.Nes.CPU.Status&0b0000_0001 != 0)

	g.DrawString(fmt.Sprintf("PC: 0x%04X", g.Nes.CPU.PC), 315, 30, [3]uint8{0, 0, 0})

	g.DrawString(fmt.Sprintf("SP: 0x%02X", g.Nes.CPU.SP), 315, 45, [3]uint8{0, 0, 0})

	g.DrawString(fmt.Sprintf("A: 0x%02X, X: 0x%02X, Y: 0x%02X", g.Nes.CPU.A, g.Nes.CPU.X, g.Nes.CPU.Y),
		285, 60,
		[3]uint8{0, 0, 0})

}

func (g *Game) DrawCPUStatus(text string, x int32, y int32, isActive bool) {
	if isActive {
		g.DrawString(text, x, y, [3]uint8{0, 0, 128})
	} else {
		g.DrawString(text, x, y, [3]uint8{255, 0, 0})
	}
}

// DrawString in Windows
func (g *Game) DrawString(text string, x int32, y int32, color [3]uint8) {
	var err error
	textColor := sdl.Color{R: color[0], G: color[1], B: color[2], A: 255}
	textsurf, err := g.Font.RenderUTF8Blended(text, textColor)
	if err != nil {
		panic(err)
	}
	defer textsurf.Free()
	texture, err := g.Renderer.CreateTextureFromSurface(textsurf)
	if err != nil {
		panic(err)
	}
	defer func(texture *sdl.Texture) {
		err := texture.Destroy()
		if err != nil {
			panic(err)
		}
	}(texture)
	err = g.Renderer.Copy(texture, &sdl.Rect{
		X: 0,
		Y: 0,
		W: int32(len(text) * FontSize),
		H: int32(FontSize),
	}, &sdl.Rect{
		X: x,
		Y: y,
		W: int32(len(text)*FontSize) / 6,
		H: int32(FontSize / 3),
	})
	if err != nil {
		panic(err)
	}
}

func New(gamePath string) *Game {
	// initialize TTF
	if err := ttf.Init(); err != nil {
		panic(err)
	}
	// initialize SDL2
	if err := sdl.Init(sdl.INIT_EVERYTHING); err != nil {
		panic(err)
	}
	// create a window
	window, err := sdl.CreateWindow(
		TitleName,
		sdl.WINDOWPOS_UNDEFINED,
		sdl.WINDOWPOS_UNDEFINED,
		WIDTH,
		HEIGHT,
		sdl.WINDOW_SHOWN,
	)
	if err != nil {
		panic(err)
	}
	// create a renderer
	renderer, err := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED)
	if err != nil {
		panic(err)
	}
	// create a font
	font, err := ttf.OpenFont("game/fonts/UbuntuMono-Regular.ttf", FontSize)
	if err != nil {
		panic(err)
	}
	// set background color
	err = renderer.SetDrawColor(0xF5, 0xF3, 0xC1, 0xFF)
	if err != nil {
		panic(err)
	}
	// set scale
	err = renderer.SetScale(SCALE, SCALE)
	if err != nil {
		panic(err)
	}
	n := nes.New(gamePath)
	n.Reset()
	return &Game{
		Nes:      n,
		Window:   window,
		Renderer: renderer,
		Font:     font,
	}
}
