"""
webapp to store notes and files.

### todo
* BUG - paste one file,
        then paste another.
  expect: both pastes saved
  actual: page errors *[0]
    _WITHOUT_ page reload

* non-blocking app start
  - pastebin_app() returns an object
    and interpreter prompt reappears
  - object provides access to logs,
    datastorage
  - object's interaction with external
    schedulers (threads, event loops,
    etc.) permits composable application
    design
* sketchpad
* multiple files per item
* rename
"""
[
    """
### request

##### headers
POST / HTTP/1.1
Host: 192.168.1.64:5005
Connection: keep-alive
Content-Length: 1129
Pragma: no-cache
Cache-Control: no-cache
Upgrade-Insecure-Requests: 1
User-Agent: Mozilla/5.0 (X11; FreeBSD amd64; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/84.0.4147.135 Safari/537.36
Origin: http://192.168.1.64:5005
Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryUrVe30DyIvjrpW1y
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9
Referer: http://192.168.1.64:5005/
Accept-Encoding: gzip, deflate
Accept-Language: en-US,en;q=0.9

##### body


### response

##### headers
HTTP/1.1 400 BAD REQUEST
Content-Type: text/html; charset=utf-8
Content-Length: 192
Date: Mon, 05 Jul 2021 00:24:29 GMT
Server: Python/3.8 aiohttp/3.7.4.post0


##### body
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">
<title>400 Bad Request</title>
<h1>Bad Request</h1>
<p>The browser (or proxy) sent a request that this server could not understand.</p>
""",
]

import asyncio
from io import (
    BytesIO,
)
import flask
import uuid0
import werkzeug
from aiohttp import (
    web as aioweb,
)
import aiohttp_wsgi


def pastebin_app(port=5005, run_aio_srv=True):
    app = flask.Flask("pastebin")

    pastes = {}

    def heartbeat():
        while True:
            for pasteid in pastes:
                if paste_time < now:
                    pass

#    thread = threading.Thread(

    def store(text, f):
        pastes[str(uuid0.generate())] = {
            "text": text,
            "file": f,
        }

    def retrieve(pid):
        return pastes[pid]

    def paste_ids():
        return list(pastes.keys())

    @app.route("/download/<pid>/<filename>")
    def download(pid, filename):
        paste = retrieve(pid)
        if filename != paste["file"]["name"]:
            warn("url filename not equal to stored filename")
        return flask.send_file(
            BytesIO(paste["file"]["bytes"]),
            attachment_filename=paste["file"]["name"],
        )

    @app.route("/<pid>")
    def look(pid):
        if pid == "favicon.ico":
            return "", 404
        paste = retrieve(pid)
        return (
            (
                """
        <h3>{time}</h3>
        <p><pre>{text}</pre></p>
        """
            ).format(
                time=uuid0.UUID(pid).datetime.isoformat(),
                text=paste["text"],
            )
            + (
                (
                    """
        <a href="{get}">download</a>
        """
                ).format(
                    get=flask.url_for(
                        "download",
                        pid=pid,
                        filename=paste["file"]["name"],
                    ),
                )
                if paste["file"]
                else ""
            )
        )

    @app.route("/", methods=["GET", "POST"])
    def paste():
        text_name = "pastetext"
        file_name = "pastefile"
        if flask.request.method == "POST":
            text = flask.request.form[text_name]
            file_storage = flask.request.files[file_name]
            file_storage.mimetype
            file_name = file_storage.filename
            file_bytes = file_storage.read() if file_storage.filename else None
            store(
                text,
                {
                    "bytes": file_bytes,
                    "name": file_name,
                },
            )

        return """
        <form method="post" enctype="multipart/form-data">
            <textarea name={text} style="width:95vw; height:80vh;"></textarea>
            <p style="height:15vh;">
            <input type=file name={file_name}>
            <input type=submit value=PASTE>
            </p>
        </form>
        <h2>{list_prefix}pastes</h2>
        <ul>{paste_list}</ul>
    """.format(
            text=text_name,
            file_name=file_name,
            list_prefix=("" if paste_ids() else "no "),
            paste_list="".join(
                [
                    """<li><a href="{url}">{pid} - {time}</a></li>""".format(
                        url=flask.url_for("look", pid=pid),
                        time=uuid0.UUID(pid).datetime.isoformat(),
                        pid=pid,
                    )
                    for pid in paste_ids()
                ]
            ),
        )

    if not run_aio_srv:
        werkzeug.serving.run_simple(
            "0.0.0.0",
            port,
            app,
            use_debugger=True,
        )
        return

    # todo: error on Interrupt and rerun (build event loop).
    # todo: libuv
    flask_app = app
    loop = asyncio.get_event_loop()
    app = aioweb.Application(loop=loop)
    app.router.add_route(
        "*", "/{path_info:.*}",
       aiohttp_wsgi.WSGIHandler(flask_app.wsgi_app))
    aio_app = app
    aioweb.run_app(aio_app,
                   # host='localhost',
                   host="0.0.0.0",
                   port=port,
    )
