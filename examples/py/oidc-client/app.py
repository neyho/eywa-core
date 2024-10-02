from flask import Flask, redirect, url_for, session, jsonify
from authlib.integrations.flask_client import OAuth
import requests
import random
import string
import os

app = Flask(__name__)
app.secret_key = os.urandom(24)

# OAuth configuration
oauth = OAuth(app)

# OpenID Connect configuration
oidc = oauth.register(
    name='oidc',
    client_id='GMTXXCLYXPIJHCLYHPVZDWWARYPMVLGIKFXKFAZNIGKLIDNJ',
    client_secret='-55tzQoNsHYcKqxPi9SCNvLDi9eYJgMl7n3vVAsy5uXhZrFx',
    server_metadata_url='http://localhost:8080/.well-known/openid-configuration',
    client_kwargs={
        'scope': 'openid profile email preferred_username permissions roles',
    }
)


def generate_nonce(length=16):
    return ''.join(random.choices(
        string.ascii_letters
        + string.digits, k=length))


# Route to serve the home page with buttons
@app.route('/')
def home():
    return '''
        <h1>OIDC Demo</h1>
        <a href="/login"><button>Login</button></a>
        <a href="/profile"><button>View Profile</button></a>
        <a href="/list-users"><button>List Users</button></a>
        <a href="/logout"><button>Logout</button></a>
    '''


# Route to start the OIDC login process
@app.route('/login')
def login():
    nonce = generate_nonce()
    session['nonce'] = nonce
    app.logger.debug(f'Generated Nonce: {nonce}')
    redirect_uri = url_for('auth', _external=True)
    return oidc.authorize_redirect(redirect_uri, nonce=nonce)


# OIDC callback route to handle the authorization code
@app.route('/auth')
def auth():
    token = oidc.authorize_access_token()
    app.logger.debug(f'Received Token Set: {token}')

    session['user'] = token
    session['access_token'] = token.get('access_token')
    session['refresh_token'] = token.get('refresh_token')

    return redirect(url_for('profile'))


# Route to display user profile after login
@app.route('/profile')
def profile():
    if 'user' not in session:
        return redirect(url_for('login'))

    nonce = session.get('nonce')

    app.logger.debug(f'Retrieved Nonce from Session: {nonce}')

    user_info = oidc.parse_id_token(session['user'], nonce=nonce)
    app.logger.debug(f'User Info: {user_info}')
    return f'<h1>User Profile</h1><pre>{user_info}</pre>'


# Route to interact with Resource Provider (RP) using GraphQL
@app.route('/list-users')
def list_users():
    # Check if the user is logged in and access token is available
    if 'access_token' not in session:
        return redirect(url_for('login'))
    access_token = session.get('access_token')
    # GraphQL API URL of the Resource Provider
    graphql_url = "http://localhost:8080/graphql"

    # The GraphQL query to list users
    query = """
    {
      searchUser {
        euuid
        name
        settings
        avatar
      }
    }
    """

    # Set up headers with the access token
    headers = {
        "Authorization": f"Bearer {access_token}",
        "Content-Type": "application/json"
    }

    # Send the GraphQL request
    response = requests.post(
        graphql_url,
        json={'query': query},
        headers=headers)

    # Check if the request was successful
    if response.status_code == 200:
        data = response.json()
        return jsonify(data['data']['searchUser'])
    else:
        return f"Failed to fetch users. Status code: {response.status_code}. Response: {response.text}"



# Route to logout and clear session
@app.route('/logout')
def logout():
    session.pop('user', None)
    session.pop('nonce', None)
    return redirect(url_for('home'))


if __name__ == '__main__':
    app.run(debug=True)
