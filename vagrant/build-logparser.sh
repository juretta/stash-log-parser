#!/bin/bash

TAG=${1:-"v1.9"}

set -e
set -u
set -x

BUILD_DIR=/tmp/repos

mkdir -p $BUILD_DIR
pushd $BUILD_DIR

rm -rf stash-log-parser
git clone file:///repo/.git stash-log-parser

cd stash-log-parser

git checkout -b tag-${TAG} ${TAG}

cd logparser

rake test
rake package

cp *.tar.gz /vagrant/

popd
rm -rf $BUILD_DIR
