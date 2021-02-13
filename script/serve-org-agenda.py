#!/usr/bin/env python3

# Usage: serve-org-agenda.sh
#
# This runs a single instance of a simple Python 3 web server.
#
# Taken from
# https://stackabuse.com/serving-files-with-pythons-simplehttpserver-module/.

import http.server
import socketserver


class MyHttpRequestHandler(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        if self.path == '/':
            # NOTE: This file must be in a path that is relative to where this
            # script is located.
            self.path = 'agenda.html'
        return http.server.SimpleHTTPRequestHandler.do_GET(self)


if __name__ == "__main__":
    PORT = 8011

    handler_object = MyHttpRequestHandler
    my_server = socketserver.TCPServer(("", PORT), handler_object)

    my_server.serve_forever()
