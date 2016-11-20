#!/usr/bin/env python
# -*- coding: utf-8 -*-
from socket import socket, AF_UNIX
import json
import os
import os.path
import sys

SOCK_ADDR = "/home/henry/.config/mpd/socket"


def mpd(query, sock_addr=SOCK_ADDR):
    result = None

    sock = socket(AF_UNIX)
    sock.connect(sock_addr)
    status = sock.recv(64)
    if status.startswith(b"OK MPD"):
        sock.sendall(query.encode())
        result = sock.recv(4096 * 4)
        if not result.startswith(b"ACK"):
            while not result.endswith(b"OK\n"):
                result += sock.recv(4096)
    return status, result


def main():
    answer = {"status": None, "result": None, "error": None}
    try:
        query = sys.stdin.read(int(os.environ["CONTENT_LENGTH"]))
        if query[-1:] != "\n":
            query += "\n"
        status, result = mpd(query)
        answer["status"] = status.decode()
        answer["result"] = result.decode()
    except:
        answer["error"] = str(sys.exc_info()[1])
        raise
    finally:
        print("Content-Type: application/json\n")
        json.dump(answer, sys.stdout)


if __name__ == "__main__":
    main()
