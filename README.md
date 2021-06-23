# RustyWorld
Rust implementation of Another World (aka Out of this world) game engine.

![Intro video](https://raw.githubusercontent.com/EdMcBane/rustyworld/main/rustyworld.gif "Intro video")

I wanted a fun project to challenge myself while learning Rust, and what better than Eric Chahi's masterpiece on its 30th anniversary?

I started from Fabien Sanglard's articles and [implementation](https://github.com/fabiensanglard/Another-World-Bytecode-Interpreter), based in turn on Gregory Montoir's original work.

**I hadn't had so much fun programming in years**! Thoroughly recommend trying yourself.

# How to run
Right now it is pretty barebones:
* copy the original data folder in $PWD/data
* `cargo run`

# Highlights
* mostly complete, main missing feature is load / save
* tries to provide a cleaner model for the VM, which is compatible with the source material but often more generic
  * clearly separates instruction decoding from execution
  * opcode arguments and flags are parsed early, simplifying downstream logic
  * groups several opcodes together, with the instruction decoding stage providing compatibility with original resources
    * conditional and unconditional jumps are a single opcode
    * mov/movc, add/addc, etc are grouped together, with immediate and register variants for arguments
  * threads have an unbound stack
  * redundant vm state has been removed (e.g. requested_state, requested_pc)
  * uses one byte per pixel in video pages
* uses minifb for video and input, and rodio for audio; should work cross-platform, but I only tested it on Linux right now
* still a work in progress

# Next steps
* curious to try WASM...

# See also
Other Another World engine implementations in rust:
* https://github.com/malandrin/another-world-suite
* https://github.com/Gnurou/awer
