import os
import sys
import re
import inspect

from pyutils.pastebin import pastebin_app


PORT = inspect.getcallargs(pastebin_app)['port']

try:
    PORT = int(os.environ['PORT'])
except KeyError:
    pass
except ValueError as err:
    print(("got invalid port number '" +
           re.sub(r'.*\'(.*)\'.*', '\\1', err.args[0]) + "'"),
          file=sys.stderr)
    sys.exit(1)


pastebin_app(port=PORT,)

