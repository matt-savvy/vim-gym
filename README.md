# vim-gym


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
