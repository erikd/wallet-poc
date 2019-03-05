#!/bin/bash -eu

if test ! -f wallet-poc/stack.yaml ; then
  echo "Could not find file wallet-poc/stack.yaml"
  exit 0
  fi

for repo in cardano-ledger cardano-prelude cardano-shell iohk-monitoring-framework ouroboros-network  ; do
  oldhash=$(cd ${repo} && git rev-parse HEAD)
  (cd ${repo} && git pull > /dev/null)
  newhash=$(cd ${repo} && git rev-parse HEAD)
  if test "${oldhash}" != "${newhash}" ; then
	echo "Updating ${repo} from ${oldhash} to ${newhash}"
	sed -i "s/${oldhash}/${newhash}/" wallet-poc/stack.yaml wallet-poc/cabal.project
    fi
  done
