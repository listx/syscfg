#!/usr/bin/env bash
set -euo pipefail

url_sans="https://raw.githubusercontent.com/adobe-fonts/source-sans/3.052R"
url_serif="https://raw.githubusercontent.com/adobe-fonts/source-serif/4.005R"
url_code="https://raw.githubusercontent.com/adobe-fonts/source-code-pro/2.042R-u%2F1.062R-i%2F1.026R-vf"

mkdir -p vendor/source-fonts
pushd vendor/source-fonts

curl -OL "${url_sans}/WOFF2/VF/SourceSans3VF-Italic.otf.woff2"
curl -OL "${url_sans}/WOFF2/VF/SourceSans3VF-Upright.otf.woff2"

curl -OL "${url_serif}/WOFF2/VAR/SourceSerif4Variable-Italic.otf.woff2"
curl -OL "${url_serif}/WOFF2/VAR/SourceSerif4Variable-Roman.otf.woff2"

curl -OL "${url_code}/WOFF2/VF/SourceCodeVF-Italic.otf.woff2"
curl -OL "${url_code}/WOFF2/VF/SourceCodeVF-Upright.otf.woff2"

cat << EOF > source-fonts.css
@font-face{
    font-family: 'Source Code Pro';
    font-weight: 200 900;
    font-style: normal;
    font-stretch: normal;
    src: url('SourceCodeVF-Upright.otf.woff2') format('woff2');
}

@font-face{
    font-family: 'Source Code Pro';
    font-weight: 200 900;
    font-style: italic;
    font-stretch: normal;
    src: url('SourceCodeVF-Italic.otf.woff2') format('woff2');
}

@font-face{
    font-family: 'Source Sans 3';
    font-weight: 200 900;
    font-style: normal;
    font-stretch: normal;
    src: url('SourceSans3VF-Upright.otf.woff2') format('woff2');
}

@font-face{
    font-family: 'Source Sans 3';
    font-weight: 200 900;
    font-style: italic;
    font-stretch: normal;
    src: url('SourceSans3VF-Italic.otf.woff2') format('woff2');
}

@font-face{
    font-family: 'Source Serif 4';
    font-weight: 200 900;
    font-style: normal;
    font-stretch: normal;
    src: url('SourceSerif4Variable-Roman.otf.woff2') format('woff2');
}

@font-face{
    font-family: 'Source Serif 4';
    font-weight: 200 900;
    font-style: italic;
    font-stretch: normal;
    src: url('SourceSerif4Variable-Italic.otf.woff2') format('woff2');
}
EOF
