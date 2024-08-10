# maid

Maid is a task runner like `make(1)`, using markdown for its tasks!

The idea is simple: contributors should be able to tell right away how a project's workflow works,
so why shouldn't commands be in the first file they read? Without even installing maid, people can
already use it! (but of course, it's a good idea to still link to this project next to your tasks ;)

This is a sucessor to the [original maid], keeping the same spirit of being easy to read and write.

[original maid]: https://github.com/egoist/maid

## Install

An AUR package is available as [`maid-bin`]. For other Linux systems and Windows, you can grab a
fresh static binary from the [releases]. If you'd like to see maid packaged for another distro,
please [open an issue]!

```sh
paru -S maid-bin
```

[`maid-bin`]: https://aur.archlinux.org/packages/maid-bin
[releases]: https://github.com/rniii/maid/releases/latest
[open an issue]: https://github/rniii/maid/issues/new/choose

## Usage

In your `README.md` or `CONTRIBUTING.md`, add `<!-- maid-tasks -->` under the section you'd like to
define your tasks. Let's get meta, here's this very README!

````md
## Development

<!-- maid-tasks -->

Of course, you can use maid to run its own tasks!

### build

Build the executable

```sh
cabal build maid
```
````

Under the section you chose, any sub-section which contains a codeblock will be interpreted as a
task. You can write whatever you want in it, really. The first paragraph will be shown on the CLI as
a brief description. Let's see:

```
$ maid
Tasks in README.md

  build
    Build the executable
```

Easy as!

Tasks support a couple different languages:

- `sh`/`bash`: shell script, the default if a codeblock doesn't have a language
- `js`/`javascript`: runs with `node`
- `hs`/`haskell`: runs with `runhaskell`

## Development

<!-- maid-tasks -->

Of course, you can use maid to run its own tasks!

### build

Build the executable

```sh
cabal build maid
```

### install

Install project onto `$dstdir`

```sh
install -Dm755 "$(cabal list-bin maid)" "$dstdir/bin/maid"
install -Dm644 LICENSE -t "$dstdir/share/licenses/maid"
install -Dm644 extras/completion/zsh "$dstdir/share/zsh/site-functions/_maid"
install -Dm644 extras/completion/fish "$dstdir/share/fish/vendor_completions.d/maid.fish"
install -Dm644 extras/completion/bash "$dstdir/share/bash-completion/completions/maid"
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
