var express = require('express');
var router = express();
var jeu = require('./jeu');
var bodyParser = require('body-parser')

router.use(bodyParser.json());

router.use(bodyParser.urlencoded({ extended: true }));

router.get('/', function(req, res){
  res.send('API Donjon');
})

var etatActuel = jeu.creerPlateau(1);

// Connexion
router.get('/connect', function(req, res, next){
  res.status(201).json(JSON.parse(jeu.connecterJSON(etatActuel)));
  etatActuel = jeu.connecterJoueur(etatActuel);
})

// Regarder
router.get('/:id/regarder', function (req,res){
    res.json(JSON.parse(jeu.regarderJSON(etatActuel)(Number(req.params['id']))));
})

// Déplacer
router.post('/:id/deplacement', function (req,res){
  if ((req.body['direction']) == undefined){
    res.status(400);
    console.log(400);
  }
  else{
    console.log(JSON.parse(jeu.deplacementJSON(etatActuel)(Number(req.params['id']))(String(req.body['direction']))));
    res.json(JSON.parse(jeu.deplacementJSON(etatActuel)(Number(req.params['id']))(String(req.body['direction']))));
    etatActuel = jeu.deplacerJoueurId(etatActuel)(Number(req.params['id']))(String(req.body['direction']));
  }})

// Taper
router.get('/:idjoueur/taper/:idcible', function (req,res){
    res.json(JSON.parse(jeu.attaquerJSON(etatActuel)(Number(req.params['idjoueur']))(String(req.params['idcible']))));
    etatActuel = jeu.attaquerId(etatActuel)(Number(req.params['idjoueur']))(String(req.params['idcible']));
})

// Examiner
router.get('/:idjoueur/examiner/:idcible' , function (req,res){
    res.json(JSON.parse(jeu.examinerJSON(etatActuel)(Number(req.params['idjoueur']))(String(req.params['idcible']))));
})

// Statut Joueur
router.get('/:id/statut' , function (req,res){
    res.json(JSON.parse(jeu.statutJSON(etatActuel)(Number(req.params['id']))));
})

// Utiliser objet
router.get('/:idjoueur/consommer/:idcible', function (req,res){
    res.json(JSON.parse(jeu.utiliserObjetJSON(etatActuel)(Number(req.params['idjoueur']))(String(req.params['idcible']))));
    etatActuel = jeu.utiliserObjetId(etatActuel)(Number(req.params['idjoueur']))(String(req.params['idcible']));
})

PORT = 8080
router.listen(PORT, () => console.log('Express server currently running on port 8080'));
