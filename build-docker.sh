#!/bin/sh

cd Scripts/Packages
docker build -t utm-base .

cd ../..
docker build -t samuelgerber/utm .

