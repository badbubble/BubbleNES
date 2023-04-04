# BubbleNes
![BubbleNes](.github/nes.gif)

## TODOs
still working on it...
- [ ] More Mappers
- [ ] APU

## Install
### Requirements
```bash
# MAC
brew install sdl2{,_image,_mixer,_ttf,_gfx} pkg-config
```

```bash
# Ubuntu
apt install libsdl2{,-image,-mixer,-ttf,-gfx}-dev
```
for other operating system you can see [this](https://github.com/veandco/go-sdl2#requirements)
### Build

```bash
git clone git@github.com:badbubble/BubbleNes.git
cd BubbleNes/
go mod tidy
go build  -o BubbleNes
```


## Play
```bash
./BubbleNes NES_GAME_PATH
```

## Game buttons
| Keyboard | joypads |
|----------|---------|
| W        | Up      |
| S        | Down    |
| A        | Left    |
| D        | Right   |
| P        | Start   |
| O        | Select  |
| K        | A       |
| L        | B       |

## Acknowledgments
- [obelisk-6502](https://web.archive.org/web/20210727210256/http://obelisk.me.uk/6502/index.html)
- [nesdev.org](https://www.nesdev.org/)
- [Easy 6502](https://skilldrick.github.io/easy6502/#instructions)
- [Writing NES Emulator in Rust](https://bugzmanov.github.io/nes_ebook/chapter_1.html)
- [olcNES](https://github.com/OneLoneCoder/olcNES)