#!/usr/bin/env sh
TAG="danlooo/corona"
docker pull $TAG || :
docker build --tag $TAG
docker push $TAG