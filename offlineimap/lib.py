#!/usr/bin/env python3
# pylint: disable=invalid-name,missing-docstring,redefined-outer-name

import os
import subprocess
import sys


HOME = os.getenv("HOME")
FOLDERS = {
    "drafts": "[Gmail]/Drafts",
    "inbox": "INBOX",
    "sent": "[Gmail]/Sent Mail",
    "spam": "[Gmail]/Spam",
    "trash": "[Gmail]/Trash"}
FOLDERS_REVERSED = {v: k for k, v in FOLDERS.items()}

def nametrans_local(folder_name):
    return FOLDERS.get(folder_name, folder_name)

def nametrans_remote(folder_name):
    return FOLDERS_REVERSED.get(folder_name, folder_name)

def decrypt_secret(secret_type):
    """ Shell out to gpg CLI to retrieve the desired secret."""
    cmd = [ \
        "gpg2",
        "-dq",
        HOME + "/secure/" + secret_type + ".gpg"]
    secret = subprocess.check_output(cmd).rstrip()
    return secret

def decrypt_notmuch_tags(fname):
    """ Decrypt the notmuch tags file. """
    output_path = HOME + "/tmp/" + fname
    cmd = [ \
        "gpg2",
        "--quiet",
        "--output",
        output_path,
        "--decrypt",
        HOME + "/secure/" + fname + ".gpg"]
    try:
        os.remove(output_path)
    except OSError:
        pass
    subprocess.call(cmd)
    # Notice the 0o600, which is 600 in octal (plain "600" would be decimal, which
    # is not what we want to do).
    os.chmod(output_path, 0o600)

if __name__ == "__main__":
    if len(sys.argv) == 2:
        decrypt_notmuch_tags(sys.argv[1])
