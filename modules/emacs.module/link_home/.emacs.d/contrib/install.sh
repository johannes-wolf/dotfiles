#!/bin/bash
set -eu

echo "Installing language-tool..."
curl -s -o langtool.zip \
     https://languagetool.org/download/LanguageTool-stable.zip
unzip langtool.zip && rm langtool.zip
rm -rf LanguageTool
mv LanguageTool-* LanguageTool

echo "Installing reveal.js..."
git clone https://github.com/hakimel/reveal.js.git 2>/dev/null
(cd reveal.js && npm install)

echo "Installing tree-sitter grammars..."
git clone https://github.com/casouri/tree-sitter-module.git 2>/dev/null
(cd tree-sitter-module && ./build.sh typst && ./build.sh lua)
