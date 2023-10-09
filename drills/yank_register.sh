#
# Yank SOME_COMPLICATED_CONSTANT, delete the marked paragraph, then type a line
# that puts the yanked text. Do not leave insert mode when typing.
#
# Do not specify any register when yanking or deleting.
#

#!/bin/bash
SOME_COMPLICATED_CONSTANT="death and taxes";

# delete this paragraph.
ls;

# type a line line this that uses $SOME_COMPLICATED_CONSTANT like so
# echo "Nothing in life is constant except $SOME_COMPLICATED_CONSTANT.";

