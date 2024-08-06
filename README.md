# maid

Maid is a task runner like make which uses readable markdown files for its commands.

## Usage

In your `README.md`, put `<!-- maid-tasks -->` under the desired section for your commands

Here's this very README.md:

````md
## Development

<!-- maid-tasks -->

### build

```sh
cabal build
```
````

## Development

<!-- maid-tasks -->

Of course, you can use maid to run its own tasks!

### build

``` sh
cabal build
```

### test

``` sh
cabal test
```
