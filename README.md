# mdconvert

* `mdconvert` is a Markdown conversion tool
with extra features and special syntax.

### Compiling mdconvert

* Source code can be compiled via `ghc`:

> `ghc mdconvert`

* Alternatively, an executable of the latest version
has been provided.

## Using mdconvert

* Running `mdconvert` with no arguments or `-h` prints usage information.

> `./mdconvert`<br>
> `usage: mdconvert infile [outfile] [-s stylefile]`

* To use `mdconvert`, give
an input file name and an output file name.

> `./mdconvert test.md output.html`

* This will convert the contents of `test.md` and store
the result in the newly-created `output.html`.

* Optionally, one can run `mdconvert` giving only an input file name.

> `./mdconvert test.md`

* This will convert the contents of `test.md` and store the
result in `test.html`.

### Embedding Stylesheets with mdconvert

* Stylesheets can be embedded into output files by using the `-s` flag,
then giving the stylesheet's file name.

> `./mdconvert test.md output.html -s style.css`

* This will convert the contents of `test.md` and store the result in `output.html`
while embedding `style.css` in `output.html`.

> `./mdconvert test.md -s style.css`

* This will convert the contents of `test.md` and store the result in `test.html`
while embedding `style.css` in `test.html`.

## mdconvert's Syntax

### Paragraphs

* Normal text is parsed as paragraph.
* Paragraphs are separated by a blank line.
* Lines breaks can be inserted by putting two spaces
at the end of a line.

### Headings 

* Headings are inserted at the beginning of a paragraph using the `#` character.
* The heading level is determined by the number of `#` symbols.
* The below example produces an `<h1>`:

> `# Level 1 Heading`

* The below example produces an `<h2>`:

> `## Level 2 Heading`

* The below example produces an `<h6>`:

> `###### Level 6 Heading`

### Emphasis

#### Bold

* Surround text with `**`.

> `Bolding the word **bold**.`

#### Italics

* Surround text with `_`.

> `Italicizing the word _italics_.`

#### Monospace/Code

* Surround text with `` ` ``.

> ``Monospacing the word `monospace`.``

#### Strikethrough

* Surround text with `~~`.

> `Striking through the word ~~strikethrough~~`

### Lists

#### Unordered Lists

* Unordered lists are inserted using the `*` character
followed by a space.
* Unordered lists must be in their own paragraph.
* Nested lists are not supported.

> `* An item`<br>
> `* Another item`<br>
> `* One more item`

#### Ordered Lists

* Ordered lists are inserted using a numeric character
followed by a `.` and a space.
* Ordered lists must be in their own paragraph.
* Nested lists are not supported.

> `1. First item`<br>
> `2. Second item`<br>
> `3. Third item`

### Links

* Links have been modified in `mdconvert`.
* Links are surrounded by double-brackets (`[[ ]]`)
and contain the text in quotes followed by the source in quotes.

> `I love [["mdconvert" "https://github.com/swansonmp/mdconvert"]]!`

### Multimedia

#### Images

* Images are simplified in `mdconvert`.
* Only the `!`, source, and parenthesis are required.
* Images must be in their own paragraph.

> `!(image.png)`

#### Videos

* Videos are similar to images, except they use the `^` character.
* Video must be in its own paragraph.
* Videos are added with a `320x240` resolution and with controls.
* All video formats supported by HTML are supported by `mdconvert`.
* Browsers that do not support the `<video>` tag will display an error message.

> `^(video.mp4)`

#### Audio

* Audio is similar to both images and video, except using the `@` character.
* Audio must be in its own paragraph.
* Audio is added with controls.
* All audio formats supported by HTML are supported by `mdconvert`.
* Browsers that do not support the `<audio>` tag will display an error message.

> `@(audio.mp3)`

### Tables

* Tables are an addition to `mdconvert`,
and are easier to use than `reStructuredText`.
* Tables must be in their own paragraph.
* The first line of the table must be a `+`,
with additional lines containing data.
* Data is entered in `CSV` format, meaning that columns
are delineated by `,` and rows by a new line.

> `+`<br>
> `R0C0, R0C1, R0C2`<br>
> `R1C0, R1C1, R1C2`<br>
> `R2C0, R2C1, R2C2`

### Horizontal Rules

* Horizontal rules are inserted using `---`, `***`, or `___`.
* Horizontal rules must be in their own paragraph.

> `---`

> `***`

> `___`


