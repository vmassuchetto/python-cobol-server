#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import sys
import datetime
import SocketServer
from cStringIO import StringIO
from lib.daemon import daemon
from api import CobolApi, CobolApiException
from conf import SERVER_HOST, SERVER_PORT

BASEDIR = os.path.dirname(os.path.abspath(__file__))
PIDFILE = BASEDIR + '/cache/SERVICE.PID'
OPTIONS = ['start', 'stop', 'restart']
DEBUG = False

class CobolBase():
    _debug = DEBUG
    _colors = {
        'prefix': '\033[36m',
        'normal': '\33[90m',
        'warning': '\033[93m',
        'error': '\033[91m'
    }

    def log(self, message, level = 'log', color = 'normal'):
        if level == 'debug' and not self._debug:
            return

        color = self._colors[color] if color in self._colors else self._colors['normal']
        date = '[' + datetime.datetime.today().strftime('%x %X') + ']'
        end = '\033[0m '
        print(self._colors['prefix'] + date + end + color + message + end)

class CobolDaemon(CobolBase, daemon.Daemon):

    def run(self):
        server = None
        done = False
        while not done:
            try:
                self.log("Starting TCP server.")
                server = SocketServer.TCPServer((SERVER_HOST, SERVER_PORT), CobolServer)
                server.serve_forever()
                done = True
            except KeyboardInterrupt:
                pass
            except Exception as e:
                self.log("Couldn't start TCP server: %s" % str(e))
                done = False
            finally:
                if server is not None:
                    self.log("Closing TCP server.")
                    server.server_close()
                    self.log("Server closed.")

class CobolServer(CobolBase, SocketServer.BaseRequestHandler):

    def handle(self):
        self._debug = DEBUG
        self.data = self.request.recv(1024).strip()
        self.log("REQUEST: " + self.data, 'debug')

        try:
            response = CobolApi().run(json_args = self.data)
            if response:
                self.log("RESPONSE:" + response, 'debug')
                self.request.sendall(response)
        except CobolApiException as e:
            response = str(e).decode('utf-8')
            self.log(response)
            self.request.sendall(response.encode('ascii', 'replace'))

if __name__ == "__main__":

    if len(sys.argv) < 2 or sys.argv[1] not in OPTIONS:
        print("Usage: ./service.py [ start | stop | restart ] [ --debug ]")
        exit(1)

    if len(sys.argv) == 3 and sys.argv[2] == '--debug':
        DEBUG = True

    daemon = CobolDaemon(PIDFILE)
    daemon._debug = DEBUG

    if sys.argv[1] == 'start':
        daemon.start()
    elif sys.argv[1] == 'stop':
        daemon.stop()
    elif sys.argv[1] == 'restart':
        daemon.restart()