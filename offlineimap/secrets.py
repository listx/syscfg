#!/usr/bin/env python3
# pylint: disable=invalid-name,missing-docstring,redefined-outer-name

import os
import subprocess


HOME = os.getenv("HOME")

def decrypt_secret(secret_type):
    """ Shell out to gpg CLI to retrieve the desired secret."""
    cmd = [ \
        "gpg2",
        "-dq",
        HOME + "/secure/" + secret_type + ".gpg"]
    secret = subprocess.check_output(cmd).rstrip()
    return secret

# Decrypt the notmuch tags file.
output_path = HOME + "/tmp/notmuch-tags"
cmd = [ \
    "gpg2",
    "--quiet",
    "--output",
    output_path,
    "--decrypt",
    HOME + "/secure/notmuch-tags.gpg"]
try:
    os.remove(output_path)
except OSError:
    pass
subprocess.call(cmd)
# Notice the 0o600, which is 600 in octal (plain "600" would be decimal, which
# is not what we want to do).
os.chmod(output_path, 0o600)
