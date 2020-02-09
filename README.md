# eh

WIP ed/ex editor clone written in Haskell

[![Build Status](https://travis-ci.com/urxvtcd/eh.svg?branch=master)](https://travis-ci.com/urxvtcd/eh)

## Features

Commands:

  - [ ] append
  - [ ] copy
  - [x] delete
  - [ ] edit
  - [ ] filter (`!`)
  - [ ] global
  - [x] go to address (for example `:12`, `/foo`)
  - [ ] insert
  - [ ] mark
  - [ ] move
  - [x] print
  - [ ] put
  - [ ] quit
  - [ ] read
  - [ ] redo
  - [ ] substitute
  - [ ] undo
  - [ ] vglobal
  - [ ] write
  - [ ] yank

Line addresses:

  - [x] absolute number
  - [ ] current line (`.`)
  - [ ] relative (`-` and `+`)
  - [ ] relative with count (`-2`)
  - [ ] last line (`$`)
  - [ ] whole file (`%`)
  - [ ] default
  - [ ] marks (`'a`)
  - [ ] search (`/`)
  - [ ] backwards search (`?`)
  - [ ] last search (`//` and `??`)

Where  applicable, these  should also  work on  ranges, so  `.,++d` will
delete  the current  line and  two following  it; and  be chainable,  so
command `'a///foo/?bar?-4d` will
  - take us to mark `a`,
  - then from there search using last used search,
  - then from there search for `foo`,
  - then from there search backwards for `bar`,
  - and from there go four lines up and delete that line.
