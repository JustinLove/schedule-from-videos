#!/usr/bin/env bash
cd lambda && watch "elm make src/Handler.elm --output handler.js" src/
