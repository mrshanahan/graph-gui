#!/usr/bin/env bash

build_release=0
while [[ $# -gt 0 ]]; do
    case $1 in
        -r|--release)
            build_release=1
            shift
            ;;
        *)
            echo "[$(date)] error: argument '$1' not recognized" >&2
            exit 1
            ;;
    esac
done

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd $SCRIPT_DIR >/dev/null
echo "[$(date)] Building..."
if [[ $build_release -eq 0 ]]; then
    elm make ./src/ChromaticNumber.elm --output build/elm.js >/dev/null
else
    elm make ./src/ChromaticNumber.elm --output build/elm.js --optimize >/dev/null
fi
if [[ $? -ne 0 ]]; then
    echo "[$(date)] Build failed; see output above for details"
    popd >/dev/null
    exit 1
fi

if [[ -d ./dist ]]; then
    rm -rf ./dist
fi
mkdir ./dist
cp index.html ./dist
cp -r build/* ./dist
# cp style.css ./dist
echo "[$(date)] Build complete!"
popd >/dev/null
