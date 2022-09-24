#!/bin/bash
# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
#
# Usage:
#
#    register_runners <n_runners> <github_token>
#
# Example:
#
#    register_runners 8 jbg2PSMoQLfqvj0ZeBUW
#
# If you're installing this on a fresh server, make sure to install docker:
#
#    apt install docker.io
#
# And allow the user (e.g., bittide) you want to run the GHA software with to use it:
#
#    adduser bittide docker
#
# Next up, a hack to get docker to write files as the 'bittide' user, instead of
# 'root'. The latter interferes with clean jobs. Make `/etc/subgid` contain:
#
#     bittide:1000:1
#     bittide:100000:65536
#
# Make `/etc/subuid` contain:
#
#     bittide:1000:1
#     bittide:100000:65536
#
# Make `/etc/docker/daemon.json` contain:
#
#     {
#         "userns-remap": "bittide"
#     }
#
# These hacks will probably ruin your normal Docker experience, so only use on
# servers dedicated for Bittide CI!
#
# Last but not least, reboot. This should apply all the changes and start the GHA
# runners. If not, good luck debugging.
#
set -euf -o pipefail

N_RUNNERS="$1"
TOKEN="$2"

ORG=bittide
REPO=bittide-hardware

URL="https://github.com/${ORG}/${REPO}"

RUNNER_VERSION="2.296.2"
RUNNER_HASH="34a8f34956cdacd2156d4c658cce8dd54c5aef316a16bbbc95eb3ca4fd76429a"

# Stop previous runners (if any)
sudo systemctl stop "actions.runner.${ORG}-${REPO}.$(hostname)-*.service"

# Download runner, verify contents
curl -o actions-runner-linux-x64-"${RUNNER_VERSION}".tar.gz -L https://github.com/actions/runner/releases/download/v"${RUNNER_VERSION}"/actions-runner-linux-x64-"${RUNNER_VERSION}".tar.gz
echo "${RUNNER_HASH}  actions-runner-linux-x64-"${RUNNER_VERSION}".tar.gz" | shasum -a 256 -c

# Install and register a bunch of runners
for i in $(seq $N_RUNNERS)
do
    runner_name="$(hostname)-$i"
    echo "$(tput bold)Registering ${runner_name}$(tput sgr0)"
    continue

    rm -rf actions-runner-$i
    mkdir -p actions-runner-$i
    cd actions-runner-$i

    # Install runner
    tar xzf ../actions-runner-linux-x64-"${RUNNER_VERSION}".tar.gz

    # Register with GitHub
    ./config.sh \
      --unattended \
      --replace \
      --url "${URL}" \
      --token "${TOKEN}" \
      --name "${runner_name}"

    # TODO:
    #  --labels vivado_standard_2022.1_0420_0327 \

    # Remove previous systemd jobs
    sudo rm -f "/etc/systemd/system/multi-user.target.wants/actions.runner.${ORG}-${REPO}.${runner_name}.service"
    sudo rm -f "/etc/systemd/system/actions.runner.${ORG}-${REPO}.${runner_name}.service"

    # Install as service
    sudo ./svc.sh install

    cd ..
done
