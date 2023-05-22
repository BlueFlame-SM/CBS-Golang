#!/bin/bash

script_dir="$(dirname $0)"

source_dir="$script_dir/../GO-cbs/GO"
target_dir=$(realpath --relative-to="$source_dir" "$script_dir/app")

# Generate Haskell files
cd "$source_dir"

mkdir -p "$target_dir"

for source_file in $(find . -name '*.cbs'); do
    cbsc $source_file $target_dir Golang
done


cd "$target_dir/Funcons/Golang"

target_files=$(find . -name '*.hs' ! -name 'Library.hs')

# Fix module names
for target_file in $target_files; do
    module=$(basename -s .hs $target_file)
    sed -i "s/^module .* where$/module Funcons.Golang.$module.$module where/" $target_file
done

# Generate Library.hs
echo "module Funcons.Golang.Library(" > Library.hs
echo "    funcons, entities, types," >> Library.hs

for target_file in $target_files; do
    module=$(basename -s .hs $target_file)
    echo "    module Funcons.Golang.$module.$module," >> Library.hs
done

echo ") where" >> Library.hs
echo "" >> Library.hs
echo "import Funcons.EDSL" >> Library.hs

for target_file in $target_files; do
    module=$(basename -s .hs $target_file)
    echo "import Funcons.Golang.$module.$module hiding (funcons, entities, types)" >> Library.hs
    echo "import qualified Funcons.Golang.$module.$module" >> Library.hs
done

echo "" >> Library.hs
echo "funcons = libUnions" >> Library.hs

start="    ["
for target_file in $target_files; do
    module=$(basename -s .hs $target_file)
    echo "$start Funcons.Golang.$module.$module.funcons" >> Library.hs
    start="    ,"
done

echo "    ]" >> Library.hs
echo "" >> Library.hs
echo "entities = concat" >> Library.hs

start="    ["
for target_file in $target_files; do
    module=$(basename -s .hs $target_file)
    echo "$start Funcons.Golang.$module.$module.entities" >> Library.hs
    start="    ,"
done

echo "    ]" >> Library.hs
echo "" >> Library.hs
echo "types = typeEnvUnions" >> Library.hs

start="    ["
for target_file in $target_files; do
    module=$(basename -s .hs $target_file)
    echo "$start Funcons.Golang.$module.$module.types" >> Library.hs
    start="    ,"
done

echo "    ]" >> Library.hs

cd "../../../"

# TODO: Automatically generate cabal file

cabal build && cabal install --overwrite-policy=always
