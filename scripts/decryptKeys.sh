#!/bin/sh

openssl aes-256-cbc -K $encrypted_7393a8c3474e_key -iv $encrypted_7393a8c3474e_iv -in travis-deploy-key.enc -out travis-deploy-key -d;
chmod 600 travis-deploy-key;
cp travis-deploy-key ~/.ssh/id_rsa;
