# mdconvert

_mdconvert_ is a primitive Markdown conversion tool
utilizing special syntax.

## Using mdconvert

Program can be compiled via `ghc`:

> `ghc mdconvert`

Input is piped into the program and parsed:

> `cat ./tests/f5.txt | ./mdconvert`

Output is stored in `output.html`.

## mdconvert's Syntax

### Multimedia

#### Images

* Images are simplified in _mdconvert_.
* Only the `!`, source, and parenthesis are required.

> `!(image.png)`

#### Videos

* Videos are similar to images, except they use the `+` character.
* Videos are added with a `320x240` resolution and with controls.
* All video formats supported by HTML are supported by _mdconvert_.
* Browsers that do not support the `<video>` tag will display an error message.

> `+(video.mp4)`

#### Audio

* Audio is similar to both images and video
* Audio is added with controls.
* All audio formats supported by HTML are supported by _mdconvert_.
* Browsers that do not support the `<audio>` tag will display an error message.

> `@(audio.mp3)`

