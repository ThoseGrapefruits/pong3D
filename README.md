Pong3D
===
A 3D version of the classic arcade game Pong, written in Racket.

https://github.com/user-attachments/assets/1c53b46a-4dcd-4244-b2e3-8999851b4314

### Notable features:

- 3D text rendering and a basic 3D font: `lib/on-draw/font.rkt`
- Syntax patterns for handling struct updates to a struct's parent's fields,
  without knowing which child struct is being used: `lib/state/syntax.rkt`
- [Fork of the `pict3d` library](https://github.com/ThoseGrapefruits/pict3d)
  which includes a new event for when the window size changes, allowing for
  proper handling of mouse input.

### Install

1. This package requires the Racket runtime. Install it at
   [racket-lang.org](https://racket-lang.org/).

2. Run the `raco pkg install` command inside the repo.

3. Run `raco make -j <thread-count> pong.rkt` to build from source

4. Run `racket pong.rkt` to run the built game.
