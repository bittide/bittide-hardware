#!/usr/bin/env bash

# This is a temporary workaround for 'archive.ubuntu.com' being down. Our CI
# runners are located in Germany, hence us using a German mirror.
sed -i 's/archive.ubuntu.com/de.archive.ubuntu.com/g' /etc/apt/sources.list
sed -i 's/security.ubuntu.com/de.archive.ubuntu.com/g' /etc/apt/sources.list
