#!/bin/bash
set -e

git config --global user.email "davidgregory084@gmail.com"
git config --global user.name "David Gregory"
git config --global push.default simple

sbt docs/publishMicrosite
