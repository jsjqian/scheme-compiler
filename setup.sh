#!/bin/bash
clang++ header.cpp -S -emit-llvm -o header.ll --std=c++11 -pthread
