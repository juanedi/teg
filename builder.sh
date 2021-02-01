export PATH="$ghc/bin:$coreutils/bin:$elm/bin:$elm_format/bin"

mkdir -p $out/bin
mkdir -p $out/assets

ghc \
    --make \
    -odir _build \
    -hidir _build \
    -XOverloadedStrings \
    -fwarn-unused-imports -Wno-name-shadowing \
    -threaded -rtsopts -with-rtsopts=-N \
    -i$src/src \
    -o $out/bin/teg \
    $src/app/Main.hs

# TODO: parametrize output path to avoid having to cd here
# cd $src
# $out/bin/teg --codegen

cd $src/ui

elm make src/Homepage.elm --output $out/assets/home.js
elm make src/Main.elm --output $out/assets/game.js
