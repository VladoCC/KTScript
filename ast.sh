#!/usr/bin/env bash
./gradlew run
dot -Tpng ast.dot > ast.png