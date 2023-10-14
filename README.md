# vim-gym

[![asciicast](https://asciinema.org/a/lZ8iw0UE7D2BL3nOudT7z5MSo.svg)](https://asciinema.org/a/lZ8iw0UE7D2BL3nOudT7z5MSo)

## Building

Requires `stack`, `sqlite3`.

### Setup DB

```
sqlite3 vim-gym.db < init_db.sql
```

### Build Library

```
stack build --copy-bins
```

You might need to add the stack build dir to your path. See output of `stack build` above.

## Usage

### Add a drill

Create a file containing a drill. There's no special format, just create a file that you will want to edit.
Add some instructions at the top.
```javascript
// foo_bar.js
/***
 * Add a semicolon at the end of each line using the dot formula.
***/
const foo = "foo"
const bar = "bar"
const baz = foo + bar
```

Add the drill.
```
vim-gym-exe add foo_bar.js
```

A drill can be spread across multiple files.
```
vim-gym-exe add foo.js bar.js baz.js
```

You can delete or edit your drill file(s).

### Review

Run the review command. It will select the first drill scheduled for practice.

```
vim-gym-exe review
```

This will open the drill in vim. Make your edits and then exit.
You will be prompted to enter a score from 0-5. Enter `?` to learn more.

vim-gym will use your score to determine when to schedule this drill for the next review.
