#!/bin/sh
ssh-add -L | grep -q "$(cat ~/.ssh/id_ed25519.pub)" || age -d -i ~/.passage/identities ~/.ssh/id_ed25519.age | ssh-add -q -t 300 -
