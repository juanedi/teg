export PATH="$ghc/bin:$coreutils/bin"

mkdir -p $out/bin

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
