# An opinionated directory listing tool

Separates directories, executables, links, and "everything else". The
first three are identified by a trailing '/', '*', and '@' character,
respectively, and are also displayed in colour (dulled cyan, red, and
magenta) unless the NO_COLOR environment variable is set or the output
stream does not support colour.

The names are split to fit the terminal width (and will be truncated
if necessary).

```
% nix run
dist-newstyle/                 nix/                           src/                          

result@                       

README.md                      default.nix                    flake.lock                     flake.nix                     
mydir.cabal                    release.nix                    shell.nix                      stack.lts.yaml                
```
