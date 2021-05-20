import requests, json



donnees = json.loads('{"direction" : "N"}')



r = requests.post('http://localhost:8080/1/deplacement',donnees)
