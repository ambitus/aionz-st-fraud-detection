#----------------------------------------------------------------------------#
# Imports
#----------------------------------------------------------------------------#

from flask import Flask, render_template, request, flash
import logging
from logging import Formatter, FileHandler
import json
import requests
from time import localtime, strftime
from flask_cors import CORS, cross_origin
import os

#----------------------------------------------------------------------------#
# App Config
#----------------------------------------------------------------------------#

app = Flask(__name__)
app.config.from_object('config')


class InvalidWMLzPasswordException(Exception):
    "Raised when the WMLz Password has been expired"
    pass

def get_auth_token():
    url = 'https://' + os.environ["WML_IP_W_PORT"] + '/auth/generateToken'
    username = os.environ["WML_USER"]
    password = os.environ["WML_PASS"]

    payload = json.dumps({
        "username": username,
        "password": password
    })

    headers = {
        "Content-Type": "application/json",
        "Control": "no-cache"
    }

    response = requests.request(
        "POST", url, headers=headers, data=payload, verify=False)
    auth_response_json = response.json()
    try:
        auth_token = auth_response_json["token"]
    except:
        raise InvalidWMLzPasswordException

    return auth_token


# Get WMLz details from env vars
WMLZ_TOKEN = "CHANGE_ME"
try:
        WMLZ_TOKEN = get_auth_token()
except InvalidWMLzPasswordException:
    raise InvalidWMLzPasswordException

WML_IP_W_PORT = os.environ["WML_IP_W_PORT"]
WML_USER = os.environ["WML_USER"]
WML_PASS = os.environ["WML_PASS"]
WMLZ_ENDPOINT = os.environ["WMLZ_ENDPOINT"]



prediction_detail_names = [
    'Prediction',
    'Deployment ID',
    'Time'
]

predictions = [
]


#----------------------------------------------------------------------------#
# Controllers
#----------------------------------------------------------------------------#

@app.route('/')
def latest_predictions():
    return render_template('pages/placeholder.latest_predictions.html', predictions=predictions, colnames=prediction_detail_names)

@app.route('/prediction_details/<id>')
def prediction_details(id=None):
    PREDICTION_ID = id

    return render_template('pages/placeholder.prediction_details.html', predction_data=predictions[int(PREDICTION_ID)], feature_data=predictions[int(PREDICTION_ID)]['features'], colnames=prediction_detail_names)

@app.route('/make_prediction')
def make_prediction():
    return render_template('pages/placeholder.make_prediction.html')

@app.route('/run_AI', methods =["GET", "POST"])
def run_AI():
    transaction_data = ''
    prediction = ''
    prediction_result = None

    try:  
        if request.method == "POST":
            transaction_data = request.form["transaction_data"] 
            payload = json.loads(transaction_data)
          
        with open('temp.json', 'w') as f:
            json.dump([payload], f)
        with open('temp.json', 'r') as f:
            headers = {'content-type': 'application/json', 'Authorization': 'Bearer ' + WMLZ_TOKEN}
            r = requests.post(WMLZ_ENDPOINT, data=f, headers=headers, verify=False)
        
            # Currently only handling one prediction at a time
            prediction = r.json()[0]

            # View inferencing results
            print('prediction')
            print(prediction)

            SCORING_THRESHOLD = 0.3
            if prediction['probability(Yes)'] > SCORING_THRESHOLD:
                # Fraud/default
                prediction_result = "Yes (" + str(round(prediction['probability(Yes)'], 2)) + " confidence)"
            else:
                # Not fraud/do not default
                prediction_result = "No (" + str(round(prediction['probability(No)'], 2)) + " confidence)"
            
            deployment_id = WMLZ_ENDPOINT.split('/')[-1]
            current_time = strftime("%Y-%m-%d %H:%M:%S", localtime())

            print(payload)

            prediction = {
                'id': len(predictions),
                'Prediction': prediction_result,
                'Deployment ID': deployment_id,
                'Time': current_time,
                'features': {}
            }

            for feature in payload:
                prediction['features'][feature] = payload[feature]

            predictions.append(prediction)

    except Exception as e:
        print(e)
        flash('Something went wrong. Please check your input data.')

    return render_template('pages/placeholder.make_prediction.html', prediction=prediction_result)

@app.route('/fraud_detector', methods =["POST"])
@cross_origin()
def fraud_detector():
    transaction_data = ''
    prediction = ''
    prediction_result = ''

    try:
        if request.method == "POST":
            transaction_data = request.get_json()

            print('TRANSACTION DATA:')
            print(transaction_data)

            payload = transaction_data

            with open('temp.json', 'w') as f:
                json.dump([payload], f)
            with open('temp.json', 'r') as f:
                headers = {'content-type': 'application/json', 'Authorization': 'Bearer ' + WMLZ_TOKEN}
                r = requests.post(WMLZ_ENDPOINT, data=f, headers=headers, verify=False)

                # Currently only handling one prediction at a time
                prediction = r.json()[0]

                # View inferencing results
                print('prediction')
                print(prediction)

                SCORING_THRESHOLD = 0.3
                if prediction['probability(Yes)'] > SCORING_THRESHOLD:
                    # Fraud/default
                    prediction_result = 'Fraud'
                else:
                    # Not fraud/do not default
                    prediction_result = 'Not fraud'

                deployment_id = WMLZ_ENDPOINT.split('/')[-1]
                current_time = strftime("%Y-%m-%d %H:%M:%S", localtime())

                print(payload)

                prediction = {
                    'id': len(predictions),
                    'Prediction': prediction_result,
                    'Deployment ID': deployment_id,
                    'Time': current_time,
                    'features': {}
                }

                for feature in payload:
                    prediction['features'][feature] = payload[feature]

                predictions.append(prediction)

    except Exception as e:
        print(e)
        print('Something went wrong. Please check your input data.')

    return {'prediction': prediction_result}


@app.errorhandler(500)
def internal_error(error):
    return render_template('errors/500.html'), 500


@app.errorhandler(404)
def not_found_error(error):
    return render_template('errors/404.html'), 404

if not app.debug:
    file_handler = FileHandler('error.log')
    file_handler.setFormatter(
        Formatter('%(asctime)s %(levelname)s: %(message)s [in %(pathname)s:%(lineno)d]')
    )
    app.logger.setLevel(logging.INFO)
    file_handler.setLevel(logging.INFO)
    app.logger.addHandler(file_handler)
    app.logger.info('errors')

#----------------------------------------------------------------------------#
# Launch
#----------------------------------------------------------------------------#

# Default port:
if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5002)
