#!/bin/sh
COMMIT_MSG_FILE=$1

sed -i '0,/^diff/{s/^diff /#+begin_src diff\ndiff /}' "${COMMIT_MSG_FILE}"
echo '#+end_src' >> "${COMMIT_MSG_FILE}"
