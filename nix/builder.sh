export PATH="$gnutar/bin:$gzip/bin:$ghc/bin:$coreutils/bin:$elm/bin:$elm_format/bin"
export ELM_HOME=$(mktemp -d)

set -euo pipefail

mkdir -p $out/bin
mkdir -p $out/assets/_build

tar -xzf $src
cd teg-$tag

ghc \
    --make \
    -odir _build \
    -hidir _build \
    -XOverloadedStrings \
    -fwarn-unused-imports -Wno-name-shadowing \
    -threaded -rtsopts -with-rtsopts=-N \
    -isrc \
    -o $out/bin/teg \
    app/Main.hs

$out/bin/teg --codegen

cd ui

cp -R static/* $out/assets

elm make src/Homepage.elm --output $out/assets/_build/home.js
elm make src/Main.elm --output $out/assets/_build/game.js
