#!/bin/bash
set -e

sbt ++$TRAVIS_SCALA_VERSION clean compile publish
