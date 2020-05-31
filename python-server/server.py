# Spotman, the overly ambitious Spotify frontend
# Copyright (C) 2020  Álmos Zediu

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see https://www.gnu.org/licenses/

from flask import Flask, redirect, request, session, jsonify
import random
import string
import requests
import base64
import json
import webbrowser

client_id = ''
client_secret =''
redirect_uri = 'http://localhost:8888/callback'

state_key = 'spotify_auth_state'

app = Flask(__name__)
app.config["SECRET_KEY"] = "veryszekret"

@app.route('/login')
def login():
    """
    The server requests authorization from Spotify.
    """

    state = ''.join(random.choices(string.ascii_lowercase +
                                   string.ascii_uppercase + string.digits, k=16))
    session['state'] = state
    scope = '''user-modify-playback-state user-read-playback-state
    user-read-private user-read-currently-playing user-library-read user-read-email'''
    url = 'https://accounts.spotify.com/authorize?' + 'response_type=code&'+ 'client_id=' + client_id + '&' + 'scope=' + scope + '&' + 'redirect_uri=' + redirect_uri + '&' + 'state=' + state
    webbrowser.open_new_tab(url)
    dict = {
        "url" : url
    }
    return jsonify(dict)

@app.route('/callback')
def callback():
    """
    The server will request refresh and access tokens after
    checking the state parameter
    """
    if 'code' in request.args:
        code = request.args["code"]
    else:
        code = None
    if 'state' in request.args:
        state = request.args["state"]
    else:
        state = None

    if code == "Balfasz":
        redirect('/#' +
                 'error=state_mismatch')
    else:
        form = {
            'code' : code,
            'redirect_uri' : redirect_uri,
            'grant_type' : "authorization_code"
        }
        auth_message = client_id + ':' + client_secret;
        auth_message_bytes = auth_message.encode('ascii')
        base64_bytes = base64.b64encode(auth_message_bytes)
        auth_code = base64_bytes.decode('ascii')
        print("Auth code:" + auth_code)
        response = requests.post(
            url= 'https://accounts.spotify.com/api/token',
            data = form,
            headers = {
                'Authorization' : 'Basic ' + auth_code,
                'Content-Type' : 'application/x-www-form-urlencoded'
            }
        )
        if response.status_code != 200:
            return response.text
        if response.status_code == 200:
            response_json = response.json()
            access_token = response_json['access_token']
            refresh_token = response_json['refresh_token']
            tokens = {
                "access_token" : access_token,
                "refresh_token" : refresh_token
            }
            print(tokens)
            file = open("./tokens.json", "w")
            json.dump(tokens, file)
            file.close()
            return response.json()
        return "Szebb szo"
    return "megszebbszo"

app.run(port=8888, debug=True)
