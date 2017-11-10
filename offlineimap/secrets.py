#!/usr/bin/env python3
# pylint: disable=missing-docstring

import os
import subprocess


def decrypt_secret(secret_type):
    """ Shell out to gpg CLI to retrieve the desired secret."""
    home = os.getenv("HOME")
    cmd = "gpg2 -dq {}/secure/{}.gpg".format(home, secret_type)
    secret = subprocess.check_output(cmd, shell=True).rstrip()
    return secret
