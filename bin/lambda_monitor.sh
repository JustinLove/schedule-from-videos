#!/usr/bin/env bash
cd lambda && watch "elm make src/Lambda.elm --output lambda.js" src/
