#!/bin/bash

FILENAME=$1
BUILD_DIR=${2:-out}

mkdir -p "$BUILD_DIR"

dune exec hugorm "$FILENAME" > "$BUILD_DIR"/a.s
nasm -f macho64 -o "$BUILD_DIR"/a.o "$BUILD_DIR"/a.s
clang -g -arch x86_64 -o "$BUILD_DIR"/a main.c "$BUILD_DIR"/a.o
