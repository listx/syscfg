#!/usr/bin/env bash

set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

pushd "${HOME}/Library/LaunchAgents"

filename=com.github.nix-community.lorri.plist

launchctl unload "${filename}" || true

cat <<EOF > "${filename}"
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
  <dict>

    <key>Label</key>
    <string>${filename}</string>

    <key>EnvironmentVariables</key>
    <dict>
      <key>PATH</key>
      <string><![CDATA[${PATH}]]></string>
    </dict>

    <key>Program</key>
    <string>${SCRIPT_ROOT}/run.sh</string>

    <key>KeepAlive</key>
    <true/>

  </dict>
</plist>

EOF

launchctl load "${filename}"
