`inty-midi` is a program which will convert a [MIDI][1] file into a
format playable by [IntyBASIC][2] on an [Intellivision][3] game
console.  Specifically, `inty-midi` generates an IntyBASIC file
containing `MUSIC` lines, which can then be included in an IntyBASIC
program with the `include` directive.  (Or, when given the `-m`
command-line option, `inty-midi` generates a stand-alone IntyBASIC
file which will play the music when run.)

IntyBASIC and the Intellivision support a maximum of three voices, or
six when the optional [ECS][4] component is used.  If the MIDI file
contains more than three voices (simultaneous notes), then `inty-midi`
automatically uses the ECS.  If more than six voices are used, then
`inty-midi` will drop some notes to stay within the voice limit.
`inty-midi` will also drop notes if they are outside the range
supported by IntyBASIC ([C2 to C7][5]).

IntyBASIC has some support for drums, and `inty-midi` will convert any
note on [Channel 10][6] into an `M1` drum note.  (This has only been
minimally tested.)  Notes on any other MIDI channel are played on one
of the four IntyBASIC instruments: piano, clarinet, flute, or bass.
(See "Program Change", below.)

`inty-midi` reads the time signature from the MIDI file, and
automatically places a blank line in the output between each measure,
to make the output somewhat more human-readable.  If the MIDI file
does not begin with a complete measure, you'll need to help it out
with the `-p` option (see below).  `inty-midi` does not currently
support changes in time signature, or changes in tempo, in the middle
of a file.

For some example MIDI files and the resulting IntyBASIC files produced
by `inty-midi`, see the [examples](examples) directory.

## Usage

```
Usage: inty-midi [-i inst] [-m] [-p n] [-q n] input.mid output.bas
       inty-midi -v
    -i inst  use specified instrument (W, X, Y, or Z)
    -m       include a main program in output
    -p n     number of quarter notes in first measure (can be fractional)
    -q n     quantize to 1/n notes (e. g. 16 for 16th notes)
    -v       print version number and exit
```

The `-i` option specifies an instrument.  If specified, this
instrument will override all of the Program Change messages (see
below).  Instruments are:

* W - Piano
* X - Clarinet
* Y - Flute
* Z - Bass

The `-m` option outputs a complete, stand-alone IntyBASIC program,
rather than an IntyBASIC fragment meant to be included from another
program.  This can be useful for easily previewing the results of a
conversion.

If the MIDI file does not start with a complete measure, `-p` can be
used to specify the number of quarter notes in the first measure.
This number can be fractional, such as `0.5` for a single eighth-note
pickup.  Note that this only affects where blank lines are printed in
the output; it does not change how the music sounds.

The `-q` option can be used to quantize the MIDI file before
converting it.  For example, `-q 16` would quantize to sixteenth
notes, while `-q 32` would quantize to thirty-second notes.  See below
for more information about quantization.

## Quantization

Input to `inty-midi` must be [quantized][7].  If your MIDI file is not
already quantized, you can ask `inty-midi` to do it for you with the
`-q` option (see above).

The following could all be signs that you need to quantize the input:

* `inty-midi` fails with the error `Needs quantization`

* `inty-midi` takes a long time to run (normally it should be almost
  instantaneous)

* the generated `.bas` file is huge

* `as1600` fails with the error `Address overflow`

* the resulting ROM crashes when run

Generally you want to quantize as much as possible (i. e. a `-q`
parameter as small as possible) without losing notes or making the
rhythm sound wrong.

Unfortunately, the IntyBASIC music player does not handle triplets
well.  In order to play triplets accurately, while also playing
sixteenth notes accurately, you would need to quantize to forty-eighth
notes, which generally is not quantized enough for good results.

## Program Change

IntyBASIC supports four instruments: Piano (W), Clarinet (X), Flute
(Y), and Bass (Z).  `inty-midi` does its best to map the
[General MIDI Program Change messages][12] to the four instruments:

* Bass (33-40) maps to Bass (Z)
* Reed (65-72) maps to Clarinet (X)
* Pipe (73-80) maps to Flute (Y)
* Everything else maps to Piano (W)

This feature has only been minimally tested.

## Binary releases

Binaries for Mac OS X, Linux, and Windows are available under the
[releases][13] tab on GitHub.

## Building from source

`inty-midi` is written in [Haskell][8].  If you're familiar with
Haskell, you already know how to build `inty-midi`.  `inty-midi` can
be built with either [Cabal][9] or [Stack][10].

If you're not familiar with Haskell, here's the quick-start:

* Clone this repo (or download a source distribution from [releases][13])
* [Install Stack][11]
* In the top directory of this repo, run `stack --install-ghc install`
* Wait a really long time (it only takes this long the first time)

This will copy `inty-midi` to the directory `~/.local/bin`.  (Or on
Windows, `%APPDATA%/local/bin`.)  You can either put that directory on
your `PATH`, or copy the binary to wherever you want to have it.

If you get a weird-sounding error, such as `AesonException`, it
probably means your version of `stack` is not new enough.

## Using the ECS

If your file requires more than three voices, you will need the ECS to
run your program.  If you are running on real Intellivision hardware,
you will need the physical [ECS][4] hardware component.

If you are using [jzIntv][14], you will need to have `ecs.bin` in your
ROM search path in order to enable emulation of the ECS.  To obtain
`ecs.bin`, do the following:

* Go to the [jzIntv home page][14], and download the latest jzIntv
  source archive, which is currently [jzintv-20200712-src.zip][15].
* Extract the zipfile you downloaded, and find the file
  `jzintv-20200712-src/src/emscripten/fake_ecs.bin` within it.
* Rename `fake_ecs.bin` to `ecs.bin` and copy it into a directory
  which is on your ROM search path.

## Alternatives

This is not the first program to convert MIDI files to IntyBASIC
programs.  AtariAge user "decle" previously wrote the program
[MusoCheat][16], which does much the same thing.

The two programs take different approaches, so you may find one or the
other more to your liking.  MusoCheat gives the user a lot more
options, although this may also require the user to do more work.
`inty-midi` has very few options, and tries to "do the right thing"
automatically.

[1]: https://en.wikipedia.org/wiki/MIDI
[2]: http://nanochess.org/intybasic.html
[3]: https://en.wikipedia.org/wiki/Intellivision
[4]: https://en.wikipedia.org/wiki/Entertainment_Computer_System
[5]: https://en.wikipedia.org/wiki/Scientific_pitch_notation#Table_of_note_frequencies
[6]: https://en.wikipedia.org/wiki/General_MIDI#Percussion
[7]: https://en.wikipedia.org/wiki/Quantization_%28music%29
[8]: https://www.haskell.org/
[9]: https://www.haskell.org/cabal/
[10]: https://haskellstack.org/
[11]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[12]: https://en.wikipedia.org/wiki/General_MIDI#Program_change_events
[13]: https://github.com/ppelleti/inty-midi/releases
[14]: http://spatula-city.org/~im14u2c/intv/
[15]: http://spatula-city.org/~im14u2c/intv/dl/jzintv-20200712-src.zip
[16]: https://atariage.com/forums/topic/264422-musocheat-adequate-inty-music-for-non-musicians
