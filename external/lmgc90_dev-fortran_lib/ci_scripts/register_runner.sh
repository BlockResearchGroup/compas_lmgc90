#!/bin/bash

set -e

server="dellvisu"
tags="ub24,py312,hdf5"
image="ub24py312hdf5"
token=$1

sudo gitlab-runner register \
  --non-interactive \
  --url "https://git-xen.lmgc.univ-montp2.fr/" \
  --registration-token "${token}" \
  --name "${server}_${image}" \
  --executor "docker" \
  --docker-image $image \
  --docker-pull-policy "if-not-present" \
  --tag-list $tags \
  --locked="false"

