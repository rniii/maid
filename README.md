# maid

Maid is a task runner like `make(1)`, using markdown for its tasks!

## Usage

In your `README.md` or `CONTRIBUTING.md`, put `<!-- maid-tasks -->` under the desired section for
your commands

Here's this very README.md:

````md
## Development

<!-- maid-tasks -->

### build

```sh
cabal build
```
````

Under this section, any subheading which contains a codeblock will be interpreted as a task. The
first paragraph is shown on the CLI as a brief description. Let's see the output on this repo:

```
$ maid
Tasks in README.md

  build
    Build the executable

  package
    ...
```

## Development

<!-- maid-tasks -->

Of course, you can use maid to run its own tasks!

### build

Build the executable

```sh
cabal build maid
```

### package

Create a tarball `$pkg`

```sh
dstdir="$pkg" maid install
tar --remove-files -cf "$pkg.tar.gz" "$pkg"
```

### install

Install project onto `$dstdir`

```sh
install -Dm755 "$(cabal list-bin maid)" "$dstdir/bin/maid"
install -Dm644 LICENSE -t "$dstdir/share/licenses/maid"
install -Dm644 extras/completion/zsh "$dstdir/share/zsh/site-functions/_maid"
ln -s maid "$dstdir/bin/made"
```

### version

Display the current version

This is used in build scripts

```hs
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as I

main = I.readFile "maid-build.cabal"
  >>= (I.putStrLn . T.strip)
    . (head . catMaybes . map (T.stripPrefix $ T.pack "version:") . T.lines)
```
