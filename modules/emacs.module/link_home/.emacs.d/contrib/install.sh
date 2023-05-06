#!/bin/bash
set -eu

echo "Installing language-tool..."
curl -s -o langtool.zip \
     https://languagetool.org/download/LanguageTool-stable.zip
unzip langtool.zip && rm langtool.zip
mv LanguageTool-* LanguageTool

echo "Installing reveal.js..."
git clone https://github.com/hakimel/reveal.js.git 2>/dev/null || {
    (cd reveal.js && git pull)
}
cd reveal.js && npm install
