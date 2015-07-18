#!/bin/python2
from subprocess import check_output
import re

def get_pass():
    out = check_output("gpg -dq $HOME/.mutt/.my-pwds.gpg", shell=True)
    # encrypted file includes Mutt variable, so we only match the password
    r = re.match(r'set\s+my_pw_personal\s+=\s(.+)', out).group(1)

    return r
