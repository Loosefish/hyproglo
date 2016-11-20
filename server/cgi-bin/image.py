#!/usr/bin/env python
# -*- coding: utf-8 -*-
import cgi
import os
import os.path
import sys
from shutil import copyfileobj
from urllib.parse import unquote

from mpd import mpd

TYPES = {
    ".jpg": "jpeg",
    ".jpeg": "jpeg",
    ".png": "png",
    ".gif": "gif"
}

def image(filename):
    _, result = mpd("config\n")
    if result is None:
        return

    images = (".jpg", ".jpeg", ",png", ".gif")
    for line in result.decode().splitlines():
        if line.startswith("music_directory: "):
            basedir = line.split(": ", 1)[1]
            path = os.path.join(basedir, os.path.dirname(filename))
            images = [f for f in os.listdir(path) if os.path.splitext(f)[1].lower() in TYPES]
            if len(images) == 0:
                return None
            else:
                return os.path.join(path, images[0])


def main():
    try:
        filename = unquote(os.environ["QUERY_STRING"].split("=", 1)[1])
    except ValueError:
        fields = cgi.FieldStorage()
        filename = fields["file"].value
    image_path = image(filename)

    if image_path is None:
        sys.exit(-1)

    sys.stdout.write("Content-Type: image/%s\n" % TYPES[os.path.splitext(image_path)[1]])
    sys.stdout.write("Content-Length: %s\n\n" % os.path.getsize(image_path))
    sys.stdout.flush()
    copyfileobj(open(image_path, "rb"), sys.stdout.buffer)


if __name__ == "__main__":
    main()
