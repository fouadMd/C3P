module Jeu exposing (..)

import Html
import Json.Encode as Encode


-----------------------------------------------------
-----------------DECLARATION TYPE--------------------


type Entite
    = Monstre { pv : Int, id : String }
    | Soin { p : Int, id : String }
    | Puissance { p : Int, id : String }
    | PointeurJoueur {id : String}


type alias Salle =
    { id : Int
    , description : String
    , entites : List Entite
    , nord : Int
    , sud : Int
    , est : Int
    , ouest : Int
    }


type alias Joueur =
    { id : Int
    , salle : Salle
    , vie : Int
    , puissance : Int
    }


type alias Jeu =
    { joueurs : List Joueur
    , salles : List Salle
    }

-------------------------------------------------------
-------------------GESTION SERVEUR---------------------


type alias ServeurAPI =
    { name: String
    , connecterJoueur : Jeu -> Jeu
    , connecterJSON : Jeu -> String
    , creerPlateau : Int -> Jeu
    , regarderJSON : Jeu -> Int -> String
    , attaquerJSON : Jeu -> Int -> String -> String
    , deplacerJoueurId : Jeu -> Int -> String -> Jeu
    , examinerJSON : Jeu -> Int -> String -> String
    , attaquerId : Jeu -> Int -> String -> Jeu
    , statutJSON : Jeu -> Int -> String
    , deplacementJSON : Jeu -> Int -> String -> String
    , utiliserObjetId : Jeu -> Int -> String -> Jeu
    , utiliserObjetJSON : Jeu -> Int -> String -> String
    }

serveurApi : ServeurAPI
serveurApi =
    { name = "serveurAPI"
    , connecterJoueur = connecterJoueur
    , connecterJSON = connecterJSON
    , creerPlateau = creerPlateau
    , regarderJSON = regarderJSON
    , attaquerJSON = attaquerJSON
    , deplacerJoueurId = deplacerJoueurId
    , examinerJSON = examinerJSON
    , attaquerId = attaquerId
    , statutJSON = statutJSON
    , deplacementJSON = deplacementJSON
    , utiliserObjetId = utiliserObjetId
    , utiliserObjetJSON = utiliserObjetJSON
    }

main =
    Html.text serveurApi.name

-----------------------------------------------------
--------------------INITIALISATION-------------------

creerPlateau n = if n == 1 then
                  let
                    monstre1 = Monstre { pv = 39, id = "monstre1" }
                    monstre2 = Monstre { pv = 25, id = "monstre2" }
                    objet1 =  Puissance { p = 25, id = "objetpuissance1" }
                    salle1 =  Salle 1 "la salle d'entrée" [ objet1, monstre1 ] 2 -1 -1 3
                    salle2 =  Salle 2 "la salle au nord de la 1ere" [monstre2] -1 1 -1 -1
                    salle3 =  Salle 3 "la salle à l'ouest de la 1ere" [] -1 -1 1 -1
                  in Jeu [] [ salle1, salle2,salle3]
                 else
                   Jeu [ ] [ ]

-----------------------------------------------------
--------------------ACCES PAR ID---------------------
estMort jeu idJoueur =
        case parId jeu.joueurs idJoueur of
          Nothing -> False
          Just x -> if x.vie > 0 then True else False


parId liste index =
    case liste of
        [] -> Nothing
        x :: xs -> if x.id == index then Just x
                   else parId xs index

entiteParId liste idE =
              case liste of
                [] -> Nothing
                x::xs -> case x of
                          Monstre { pv, id } -> if id == idE then Just x
                                                else entiteParId xs idE
                          Soin { p, id } -> if id == idE then Just x
                                                else entiteParId xs idE
                          Puissance { p, id } -> if id == idE then Just x
                                                else entiteParId xs idE
                          PointeurJoueur { id } -> if id == idE then Just x
                                                else entiteParId xs idE

accesJoueurPointeur jeu idCible =
                               let fonctionAide l id =
                                      case l of
                                        [] -> Nothing
                                        x::xs -> if id == ("joueur"++ Debug.toString(x.id)) then Just x
                                                 else fonctionAide xs id
                               in fonctionAide jeu.joueurs idCible


-----------------------------------------------------
-----------------GESTION JOUEUR----------------------

changerSalleJoueur salle joueur =
    Joueur joueur.id salle joueur.vie joueur.puissance


changerVieJoueur vie joueur =
    Joueur joueur.id joueur.salle vie joueur.puissance


changerPuissanceJoueur puissance joueur =
    Joueur joueur.id joueur.salle joueur.vie puissance


creerIdJoueur jeu x =
    let j = parId jeu.joueurs x
    in if j == Nothing then x
      else creerIdJoueur jeu (x + 1)

-----------------------------------------------------
------------------GESTION SALLE----------------------

listeDirection salle = let
                        l = []
                        lo = if salle.ouest /= -1 then "O" :: l
                             else l
                        le = if salle.est /= -1 then "E" :: lo
                             else lo
                        ls = if salle.sud /= -1 then "S" :: le
                             else le
                        ln = if salle.nord /= -1 then "N" :: ls
                             else ls
                      in ln

listeEntites salle = let aideListe k =
                              case k of
                                [] -> []
                                x :: xs -> case x of
                                  Soin { p, id } -> id :: aideListe xs
                                  Puissance { p, id } -> id :: aideListe xs
                                  Monstre { pv, id } ->  id :: aideListe xs
                                  PointeurJoueur {id} -> id :: aideListe xs
                      in aideListe salle.entites

retirerEntiteSalle salle entite =
    let
      nvEntites = List.filter (\s -> s /= entite) salle.entites
    in Salle salle.id salle.description nvEntites salle.nord salle.sud salle.est salle.ouest

modifierEntiteSalle salle entite nvPV =
    case entite of
        Soin _ -> salle
        Puissance _ -> salle
        Monstre { pv, id } -> let
                                nvEntites = List.filter (\g -> entite /= g) salle.entites
                                nvId = id
                              in Salle salle.id salle.description (Monstre { pv = nvPV, id = nvId } :: nvEntites) salle.nord salle.sud salle.est salle.ouest
        PointeurJoueur _ -> salle
-----------------------------------------------------
--------------------GESTION JEU----------------------
construirePointeur joueur = PointeurJoueur {id = ("joueur"++ Debug.toString(joueur.id))}

extraireId liste = case liste of
                        [] -> []
                        (PointeurJoueur{ id })::xs -> id :: extraireId xs
                        _ -> []

tousJoueursSalle jeu joueur salle = let joueursSalle = List.filter (\x -> x.salle.id == joueur.salle.id) jeu.joueurs
                                        listeFinale = List.filter (\x -> x.id /= joueur.id) joueursSalle
                                    in extraireId (List.map construirePointeur listeFinale)

tousJoueursSalle2 jeu salle = let joueursSalle = List.filter (\x -> x.salle.id == salle.id) jeu.joueurs
                              in extraireId (List.map construirePointeur joueursSalle)


retirerEntite jeu joueur entite =
    let nvSalle = retirerEntiteSalle joueur.salle entite
        joueurConcernes = List.map (changerSalleJoueur nvSalle) (List.filter (\x -> x.salle.id == joueur.salle.id) jeu.joueurs)
        joueurNonConcernes = List.filter (\x -> x.salle.id /= joueur.salle.id) jeu.joueurs
        listeSalles = List.filter (\x -> x.id /= joueur.salle.id) jeu.salles
    in
      Jeu (joueurConcernes ++ joueurNonConcernes) (nvSalle :: listeSalles)


utiliserObjetJeu jeu joueur entite =
    case entite of
        Monstre _ -> jeu
        Soin { p ,id} -> let joueurConcerne = changerVieJoueur (p + joueur.vie) joueur
                             autresJoueurs = List.filter (\y -> y.id /= joueur.id) jeu.joueurs
                             jeubis = Jeu (joueurConcerne :: autresJoueurs) jeu.salles
                         in
                             retirerEntite jeubis joueur entite
        Puissance { p,id } -> let joueurConcerne = changerPuissanceJoueur (p + joueur.puissance) joueur
                                  autresJoueurs = List.filter (\y -> y.id /= joueur.id) jeu.joueurs
                                  jeubis = Jeu (joueurConcerne :: autresJoueurs) jeu.salles
                              in
                                 retirerEntite jeubis joueur entite
        PointeurJoueur _ -> jeu


utiliserObjetId jeu idJoueur idEntite =
                let joueur = parId jeu.joueurs idJoueur
                in case joueur of
                      Nothing -> jeu
                      Just j -> case (entiteParId j.salle.entites idEntite) of
                                    Nothing -> jeu
                                    Just ent -> utiliserObjetJeu jeu j ent


attaquerMonstreJeu jeu joueur entite =
    case entite of
        Soin _ -> jeu
        Puissance _ -> jeu
        PointeurJoueur _ -> jeu
        Monstre { pv } -> if pv - joueur.puissance <= 0 then retirerEntite jeu joueur entite
                          else
                            let
                              nvSalle = modifierEntiteSalle joueur.salle entite (pv - joueur.puissance)
                              listeSalles = List.filter (\y -> y.id /= nvSalle.id) jeu.salles
                              joueurConcernes = List.map (changerSalleJoueur nvSalle) (List.filter (\x -> x.salle.id == nvSalle.id) jeu.joueurs)
                              joueurNonConcernes = List.filter (\x -> x.salle.id /= nvSalle.id) jeu.joueurs
                          in
                            Jeu (joueurConcernes ++ joueurNonConcernes) (nvSalle :: listeSalles)

attaquerJoueurJeu jeu joueur joueurcible =
                    if (joueur.salle == joueurcible.salle) && (joueur.id /= joueurcible.id) then
                            let degats = joueurcible.vie - joueur.puissance
                            in if degats > 0 then
                                    let nvJoueur = Joueur joueurcible.id joueurcible.salle degats joueurcible.puissance
                                        listeJoueur = List.filter (\y -> y.id /= joueurcible.id) jeu.joueurs
                                    in Jeu (nvJoueur::listeJoueur)  jeu.salles
                               else let listeJoueur = List.filter (\y -> y.id /= joueurcible.id) jeu.joueurs
                                    in Jeu listeJoueur  jeu.salles
                    else jeu



attaquerId jeu idJoueur idEntite =
              let joueur = parId jeu.joueurs idJoueur
              in case joueur of
                    Nothing -> jeu
                    Just j -> let entite = entiteParId j.salle.entites idEntite
                              in case entite of
                                  Nothing -> case (accesJoueurPointeur jeu idEntite) of
                                              Nothing -> jeu
                                              Just c -> attaquerJoueurJeu jeu j c
                                  Just m -> case m of
                                              Soin _ -> jeu
                                              Puissance _ -> jeu
                                              PointeurJoueur _ -> jeu
                                              Monstre _ -> attaquerMonstreJeu jeu j m



deplacerJoueurJeu jeu joueur direction =
    case direction of
        "N" -> if joueur.salle.nord == -1 then jeu
               else let
                      autreJoueurs = List.filter (\x -> x.id /= joueur.id) jeu.joueurs
                      nvSalle = parId jeu.salles joueur.salle.nord
                    in case nvSalle of
                      Nothing -> jeu
                      Just s -> let
                                  joueurBis = changerSalleJoueur s joueur
                                in
                                  Jeu (joueurBis :: autreJoueurs) jeu.salles
        "S" -> if joueur.salle.sud == -1 then jeu
               else let
                      autreJoueurs = List.filter (\x -> x.id /= joueur.id) jeu.joueurs
                      nvSalle = parId jeu.salles joueur.salle.sud
                     in case nvSalle of
                       Nothing -> jeu
                       Just s -> let
                                   joueurBis = changerSalleJoueur s joueur
                                 in
                                   Jeu (joueurBis :: autreJoueurs) jeu.salles
        "E" -> if joueur.salle.est == -1 then jeu
               else let
                      autreJoueurs = List.filter (\x -> x.id /= joueur.id) jeu.joueurs
                      nvSalle = parId jeu.salles joueur.salle.est
                    in case nvSalle of
                           Nothing -> jeu
                           Just s -> let
                                       joueurBis = changerSalleJoueur s joueur
                                     in
                                       Jeu (joueurBis :: autreJoueurs) jeu.salles
        "O" -> if joueur.salle.ouest == -1 then jeu
               else let
                      autreJoueurs = List.filter (\x -> joueur.id /= x.id) jeu.joueurs
                      nvSalle = parId jeu.salles joueur.salle.ouest
                    in case nvSalle of
                          Nothing -> jeu
                          Just s -> let
                                      joueurBis = changerSalleJoueur s joueur
                                    in
                                      Jeu (joueurBis :: autreJoueurs) jeu.salles
        _ -> jeu

deplacerJoueurId jeu id direction =
                    let joueur = parId jeu.joueurs id
                    in case joueur of
                        Nothing -> jeu
                        Just j -> deplacerJoueurJeu jeu j direction

connecterJoueur jeu =
    let
      salleDebut = parId jeu.salles 1
      idJoueur = creerIdJoueur jeu 1
    in case salleDebut of
          Nothing -> jeu
          Just x -> let nvJoueur = Joueur idJoueur x 50 15
                    in Jeu (nvJoueur :: jeu.joueurs) jeu.salles


-----------------------------------------------------------------------------
--------------------------Fonction JSON--------------------------------------


connecterJSON jeu =
    let partie = connecterJoueur jeu
        idJoueur = creerIdJoueur jeu 1
        joueur = parId partie.joueurs idJoueur
    in case joueur of
        Nothing -> Encode.encode 1 (Encode.object [("erreur", Encode.string "400")])
        Just x -> let object =
                        Encode.object
                            [ ( "guid", Encode.int idJoueur )
                            , ( "totalvie", Encode.int x.vie )
                            , ( "salle"
                              , Encode.object
                                    [ ( "description", Encode.string x.salle.description )
                                    , ( "passages", Encode.list Encode.string (listeDirection x.salle) )
                                    , ( "entites", Encode.list Encode.string (listeEntites x.salle) )
                                    ]
                              )
                            ]
                  in Encode.encode 0 object


regarderJSON jeu id = let
                       joueur = parId jeu.joueurs id
                      in case joueur of
                          Nothing -> mortJSON
                          Just x -> let object = Encode.object
                                                   [("description",Encode.string x.salle.description)
                                                   ,("passages",Encode.list Encode.string (listeDirection x.salle))
                                                   ,("entites", Encode.list Encode.string (List.append (listeEntites x.salle)(tousJoueursSalle jeu x x.salle)))]
                                    in Encode.encode 0 object


attaquerJSON jeu idJoueur idEntite =
              let joueur = parId jeu.joueurs idJoueur
              in case joueur of
                  Nothing -> mortJSON
                  Just j -> let entite = entiteParId j.salle.entites idEntite
                            in case entite of
                                Nothing -> case (accesJoueurPointeur jeu idEntite) of
                                            Nothing -> Encode.encode 0 (Encode.object [("erreur", Encode.string "la cible selectionnée ne peut être attaquée")])
                                            Just adversaire ->if (j.id /= adversaire.id ) && (j.salle.id == adversaire.salle.id )
                                                              then let degat = if (adversaire.vie-j.puissance) > 0 then (adversaire.vie-j.puissance)
                                                                                else 0
                                                                   in Encode.encode 0 (Encode.object
                                                                       [("attaquant",Encode.object
                                                                       [("guid",Encode.int j.id)
                                                                       ,("degats",Encode.int j.puissance)
                                                                       ,("vie",Encode.int j.vie)])
                                                                       ,("attaqué",Encode.object
                                                                       [("guid",Encode.string idEntite)
                                                                       ,("vie",Encode.int degat)])])
                                                              else Encode.encode 0 (Encode.object [("erreur", Encode.string "la cible selectionnée ne peut être attaquée")])
                                Just m ->  case m of
                                            Puissance _ -> Encode.encode 0 (Encode.object [("erreur", Encode.int 400)])
                                            Soin _ -> Encode.encode 0 (Encode.object [("erreur", Encode.int 400)])
                                            PointeurJoueur _ -> Encode.encode 0 (Encode.object [("erreur", Encode.int 400)])
                                            Monstre {pv,id} -> let degat = if (pv-j.puissance) > 0 then (pv-j.puissance)
                                                                           else 0
                                                               in Encode.encode 0 (Encode.object
                                                                     [("attaquant",Encode.object
                                                                     [("guid",Encode.int j.id)
                                                                     ,("degats",Encode.int j.puissance)
                                                                     ,("vie",Encode.int j.vie)])
                                                                     ,("attaqué",Encode.object
                                                                     [("guid",Encode.string idEntite)
                                                                     ,("vie",Encode.int degat)])])



examinerJSON jeu idJoueur idCible =
                let joueur = parId jeu.joueurs idJoueur
                in case joueur of
                    Nothing -> mortJSON
                    Just j -> let cibleJ = accesJoueurPointeur jeu idCible
                              in case cibleJ of
                                  Nothing -> let entite = entiteParId j.salle.entites idCible
                                             in case entite of
                                                  Nothing -> Encode.encode 0 (Encode.object [("erreur", Encode.string "vous ne pouvez examiner cette cible")])
                                                  Just ent -> case ent of
                                                            Monstre {pv , id} -> Encode.encode 0 (Encode.object
                                                                                [("description",Encode.string "Il s'agit d'un Monstre")
                                                                                ,("type",Encode.string "MONSTRE")
                                                                                ,("puissance",Encode.int (pv//5))
                                                                                ,("totalvie",Encode.int pv)])
                                                            Puissance {p , id} -> Encode.encode 0 (Encode.object
                                                                                [("description",Encode.string "Il s'agit d'un boost de puissance")
                                                                                ,("type",Encode.string "OBJET DE PUISSANCE")
                                                                                ,("boost",Encode.int p)])
                                                            Soin {p , id}      -> Encode.encode 0 (Encode.object
                                                                                [("description",Encode.string "Il s'agit d'un boost de puissance")
                                                                                ,("type",Encode.string "OBJET DE PUISSANCE")
                                                                                ,("boost",Encode.int p)])
                                                            _                  -> Encode.encode 0 (Encode.object
                                                                                [("cas impossible",Encode.int 101)])
                                  Just c -> Encode.encode 0 (Encode.object
                                                            [("attaquant",Encode.string "Il s'agit d'un joueur adverse")
                                                            ,("type",Encode.string "JOUEUR")
                                                            ,("puissance",Encode.int c.puissance)
                                                            ,("totalvie",Encode.int c.vie)])



statutJSON jeu idJoueur = let joueur = parId jeu.joueurs idJoueur
                          in case joueur of
                              Nothing -> mortJSON
                              Just j -> Encode.encode 0 (Encode.object
                                                        [("guid",Encode.int j.id)
                                                        ,("idSalle",Encode.int j.salle.id)
                                                        ,("puissance",Encode.int j.puissance)
                                                        ,("vie",Encode.int j.vie)])

mortJSON = Encode.encode 0 (Encode.object [("type",Encode.string "MORT")
                                          ,("message",Encode.string "Votre personnage est mort ! Veuillez relancer une partie")])


murJSON = Encode.encode 0 (Encode.object [("type",Encode.string "MUR")
                                          ,("message",Encode.string "Vous ave indiquez une mauvaise direction")])


deplacementJSON jeu id direction =
                  let joueur = parId jeu.joueurs id
                  in case joueur of
                      Nothing -> mortJSON
                      Just j -> case direction of
                                 "N" -> if j.salle.nord == -1 then murJSON
                                        else let nvSalle = parId jeu.salles j.salle.nord
                                             in case nvSalle of
                                                     Nothing -> murJSON
                                                     Just s -> let object = Encode.object
                                                                              [("description",Encode.string s.description)
                                                                              ,("passages",Encode.list Encode.string (listeDirection s))
                                                                              ,("entites", Encode.list Encode.string (List.append (listeEntites s)(tousJoueursSalle2 jeu s)))]
                                                               in Encode.encode 0 object
                                 "S" -> if j.salle.nord == -1 then murJSON
                                        else let nvSalle = parId jeu.salles j.salle.sud
                                             in case nvSalle of
                                                     Nothing -> murJSON
                                                     Just s -> let object = Encode.object
                                                                              [("description",Encode.string s.description)
                                                                              ,("passages",Encode.list Encode.string (listeDirection s))
                                                                              ,("entites", Encode.list Encode.string (List.append (listeEntites s)(tousJoueursSalle2 jeu s)))]
                                                               in Encode.encode 0 object
                                 "E" -> if j.salle.nord == -1 then murJSON
                                        else let nvSalle = parId jeu.salles j.salle.est
                                             in case nvSalle of
                                                     Nothing -> murJSON
                                                     Just s -> let object = Encode.object
                                                                              [("description",Encode.string s.description)
                                                                              ,("passages",Encode.list Encode.string (listeDirection s))
                                                                              ,("entites", Encode.list Encode.string (List.append (listeEntites s)(tousJoueursSalle2 jeu s)))]
                                                               in Encode.encode 0 object
                                 "O" -> if j.salle.nord == -1 then murJSON
                                        else let nvSalle = parId jeu.salles j.salle.ouest
                                             in case nvSalle of
                                                     Nothing -> murJSON
                                                     Just s -> let object = Encode.object
                                                                              [("description",Encode.string s.description)
                                                                              ,("passages",Encode.list Encode.string (listeDirection s))
                                                                              ,("entites", Encode.list Encode.string (List.append (listeEntites s)(tousJoueursSalle2 jeu s)))]
                                                               in Encode.encode 0 object
                                 _ -> murJSON


utiliserObjetJSON jeu idJoueur idEntite =
                let joueur = parId jeu.joueurs idJoueur
                in case joueur of
                    Nothing -> mortJSON
                    Just j -> let entite = entiteParId j.salle.entites idEntite
                              in case entite of
                                  Nothing -> Encode.encode 0 (Encode.object [("erreur", Encode.string "vous ne pouvez consommer cette cible")])
                                  Just ent -> case ent of
                                                Puissance {p,id} -> Encode.encode 0 (Encode.object
                                                                         [("guid",Encode.int j.id)
                                                                         ,("type",Encode.string "PUISSANCE")
                                                                         ,("boost", Encode.int p)])
                                                Soin {p,id} -> Encode.encode 0 (Encode.object
                                                                         [("guid",Encode.int j.id)
                                                                         ,("type",Encode.string "SOIN")
                                                                         ,("boost", Encode.int p)])
                                                _           -> Encode.encode 0 (Encode.object [("erreur", Encode.string "vous ne pouvez consommer cette cible")])
