#!/usr/bin/env bash

godoc -http=localhost:6060 &
dufs --allow-search ~/prog/doc/go.dev/ &
