wasm32-wasi-cabal build
rm jiexi-unstubbed.wasm jiexi.wasm
cp -T "$(wasm32-wasi-cabal list-bin jiexi)" ./jiexi-unstubbed.wasm
wasi-stub jiexi-unstubbed.wasm -o jiexi.wasm --return-value 0
cp jiexi.wasm packages/jiexi