#! /bin/bash

cabal run ernie -- -f example/project.json -o example/project.dot --num-samples 10000 --watch
