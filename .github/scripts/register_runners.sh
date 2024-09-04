#!/bin/bash
# SPDX-FileCopyrightText: 2022,2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0
#
# Usage:
#
#    register_runners <n_runners> <labelA,labelB,...> <github_token>
#
# Example:
#
#    register_runners 8 compute jbg2PSMoQLfqvj0ZeBUW
#
# Currently we're labelling runners with either "compute" or "hardware-access".
# Note that a label "self-hosted" gets applied automatically.
#
# With a privileged enough account the token can be found at:
# https://github.com/organizations/[ORG_NAME]/settings/actions/runners/new
#
# If you're installing this on a fresh server, make sure to install docker:
#
#    apt install docker.io
#
# And allow the user (e.g., gha-user) you want to run the GHA software with to use it:
#
#    adduser gha-user docker
#
# Next up, a hack to get docker to write files as the 'gha-user' user, instead of
# 'root'. The latter interferes with clean jobs. Update the UID from 1000 to the UID of
# the gha-user, and make `/etc/subgid` contain:
#
#     gha-user:1000:1
#     gha-user:100000:65536
#
# Make `/etc/subuid` contain (also update the UID from 1000 to the UID of the gha-user):
#
#     gha-user:1000:1
#     gha-user:100000:65536
#
# Make `/etc/docker/daemon.json` contain:
#
#     {
#         "userns-remap": "gha-user"
#     }
#
# These hacks will probably ruin your normal Docker experience, so only use on
# servers dedicated for CI!
#
# Last but not least, reboot. This should apply all the changes and start the GHA
# runners. If not, good luck debugging.
#
set -euf -o pipefail

N_RUNNERS="$1"
LABELS="$2"
TOKEN="$3"

ORG=bittide
REPO=bittide-hardware

URL="https://github.com/${ORG}/${REPO}"

RUNNER_VERSION="2.317.0"
RUNNER_HASH="9e883d210df8c6028aff475475a457d380353f9d01877d51cc01a17b2a91161d"

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
      --labels "${LABELS}" \
      --name "${runner_name}"

    # TODO:
    #  --labels vivado_standard_2022.1_0420_0327 \

    # Remove previous systemd jobs
    service_name="actions.runner.${ORG}-${REPO}.${runner_name}.service"
    service_file="/etc/systemd/system/${service_name}"
    sudo rm -f "/etc/systemd/system/multi-user.target.wants/${service_name}"
    sudo rm -f "${service_file}"

    # Install as service
    sudo ./svc.sh install

    # Remove ill-advised entries
    sudo sed -i '/^KillMode=/d' "${service_file}"
    sudo sed -i '/^KillSignal=/d' "${service_file}"

    # Make systemd aware of changes
    sudo systemctl daemon-reload
    sudo systemctl restart "${service_name}"

    cd ..
done
