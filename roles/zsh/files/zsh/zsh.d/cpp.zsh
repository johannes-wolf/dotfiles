#!/usr/bin/env zsh

CMAKE_GENERATOR=Ninja
export CMAKE_GENERATOR

function cmb() {
    if [[ ! -f CMakeLists.txt ]]; then
        echo "No CMakeLists.txt at ${PWD}"
    fi

    cmake -Bbuild -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=YES "$@"
    cmake --build build
}
