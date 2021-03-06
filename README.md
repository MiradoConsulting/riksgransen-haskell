# Riksgränsen Haskell

## Part 1 (for developers & non-developers)

The first part of the workshop will be using a web app, so you will only need a web browser to join in.  Even if you're not a developer by day, you will get your hands dirty with some Haskell.

You can stop reading here.

## Part 2 (for developers)

The second part will be more developer-focussed and will require you to install some things.

### Checklist of things to have (installation instructions further below):
#### Compiler
`ghc` should be installed and in your $PATH

#### Package manager
`cabal` should be installed and in your $PATH

#### IDE
* Bring your own favourite text editor :)
* My preference on Ubuntu is Kate.
* vscode and emacs are pretty good too.

#### Native libraries
Some Haskell libraries call out to native libraries.  I believe the only one we'll need is zlib.

#### Other utilities which may be useful
* `git`
* `curl`

### Installation on debian/ubuntu-like OSes
```sudo apt install ghc cabal-install git curl zlib1g-devel```

### Installation on MacOS
* Try https://www.haskell.org/platform/
* I haven't confirmed this on a Mac yet.

### Installation on Windows
* Try https://www.haskell.org/platform/
* Otherwise if you're running Windows 10, you can install Ubuntu as an app and then the Ubuntu instructions above will work.

### Sanity Check

There is a sample `starter` project included in this repo to make sure your toolchain is all working.

Sync your packages:
```
~$ cabal update
```

Wrap the starter project in a sandbox (dependencies will be installed inside the project, rather than polluting `~/.ghc` `~/.cabal`)
```
~$ cd riksgransen-haskell/starter
~/riksgransen-haskell/starter$ cabal sandbox init
```

Download/build the dependencies:
(I've deliberately included way more dependencies than necessary, in case we want to try different things out)
```
~/riksgransen-haskell/starter$ cabal install --dependencies-only 
```

Build/run the `starter` project:
```
~/riksgransen-haskell/starter$ cabal run
```

You should now be able to curl/visit http://127.0.0.1:3000/ and see
```Hello, world!```






