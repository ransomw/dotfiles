""""""
import flask
from pyutils.pyjuke import review as jk_rev
from pyutils.pyjuke import store as jk_stor
from wsgiref.simple_server import make_server

"""
jk_stor.save_cards()
jk_stor.add_new_cards(new_cards)
jk_stor.load_new_cards(card_file, delim=':', comment='#')
jk_stor.add_card(front, back)
"""

bp = flask.Blueprint(
        'jukeweb_blueprint',
        __name__,
        template_folder='templates')


@bp.route('/', methods=['GET', 'POST'])
def home():
    if flask.request.method == 'POST':
        front = flask.request.form['front']
        back = flask.request.form['back']
        jk_stor.add_card(front, back)
        flask.flash("added card")
    return render_template('home.html',
            cards=py_stor.CARDS)


def make_app():
    app = flask.Flask(__name__)
    app.register_blueprint(bp)
    return app

def flashcard_app(port=5011,):
    jk_stor.load_cards()
    flask_app = make_app()
    with make_server('', port, flask_app) as httpd:

        httpd.server_forever()

# .handle_request # one req, then exit

    return

