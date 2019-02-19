# zen
Download public domain ebooks from <http://zeno.org> and <http://gutenberg.spiegel.de>.

## Build
- nix: `cabal2nix --shell . > shell.nix && nix-build shell.nix`
- stack: `stack build`

## Usage
```
$ zen -o george.epub http://www.zeno.org/Literatur/M/George,+Stefan/Gesamtausgabe+der+Werke/Das+Jahr+der+Seele
...
generating epub2 to george.epub
```
