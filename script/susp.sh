#!/bin/sh

set -eu

systemctl suspend
# For whatever reason, we cannot restart the openvpn-home.service with sudo (it
# dies immediately).
# systemctl restart openvpn-home.service
