#!/usr/bin/env python
from socket import socket, AF_UNIX
import sys
import os
import os.path

from bottle import route, run, static_file, request


SOCK_ADDR = "/home/henry/.config/mpd/socket"
SERVER_ROOT = "/home/henry/Entwicklung/HyProGlo-purescript/server"


def _mpd(query, sock_addr=SOCK_ADDR):
    result = None

    sock = socket(AF_UNIX)
    sock.connect(sock_addr)
    status = sock.recv(64)
    if status.startswith(b"OK MPD"):
        sock.sendall(query)
        result = sock.recv(4096)
        if not result.startswith(b"ACK"):
            while not result.endswith(b"OK\n"):
                result += sock.recv(4096)
    sock.close()
    return status, result


@route('/')
def home():
    return static_file('index.html', root=SERVER_ROOT)


@route('/mpd', method='POST')
def mpd():
    answer = {"status": None, "result": None, "error": None}
    try:
        query = request.body.read()
        if query[-1:] != b"\n":
            query += b"\n"
        status, result = _mpd(query)
        answer["status"] = status.decode()
        answer["result"] = result.decode()
    except:
        answer["error"] = str(sys.exc_info()[1])
        raise
    return answer


MPD_ROOT = None
IMAGES = [".jpg", ".jpeg", ".png", ".gif"]

@route('/image/<path:path>')
def image(path):
    global MPD_ROOT
    if MPD_ROOT is None:
        _, result = _mpd(b"config\n")
        for line in result.decode().splitlines():
            if line.startswith("music_directory: "):
                MPD_ROOT = line.split(": ", 1)[1]

    path = os.path.dirname(os.path.join(MPD_ROOT, path))
    images = [f for f in os.listdir(path) if os.path.splitext(f)[1].lower() in IMAGES]
    if len(images) == 0:
        return None
    else:
        full = os.path.join(path, images[0])
        relpath = os.path.relpath(full, start=MPD_ROOT)
        print(MPD_ROOT, full, relpath)
        return static_file(relpath, root=MPD_ROOT)


@route('/<path:path>')
def static(path):
    return static_file(path, root=SERVER_ROOT)


run(host='0.0.0.0', port=5000, server='cherrypy')
