# mdconvert

* `mdconvert` is a primitive Markdown conversion tool
utilizing special syntax.

## Compiling mdconvert

* Source code can be compiled via `ghc`:

> `ghc mdconvert`

## Using mdconvert

Running `mdconvert` with no arguments or `-h` prints usage information.

> `./mdconvert`

> `usage: mdconvert infile [outfile]`

To use `mdconvert`, give
an input file name and an output file name.

> `./mdconvert test.md output.html`

This will convert the contents of `test.md` and store
the result in the newly-created `output.html`.

Optionally, one can run `mdconvert` giving only an input file name.

> `./mdconvert test.md`

This will convert the contents of `test.md` and store the
result in `test.html`.

## mdconvert's Syntax

### Multimedia

#### Images

* Images are simplified in `mdconvert`.
* Only the `!`, source, and parenthesis are required.

> `!(image.png)`

#### Videos

* Videos are similar to images, except they use the `+` character.
* Videos are added with a `320x240` resolution and with controls.
* All video formats supported by HTML are supported by `mdconvert`.
* Browsers that do not support the `<video>` tag will display an error message.

> `+(video.mp4)`

#### Audio

* Audio is similar to both images and video, except using the `@` character.
* Audio is added with controls.
* All audio formats supported by HTML are supported by `mdconvert`.
* Browsers that do not support the `<audio>` tag will display an error message.

> `@(audio.mp3)`

