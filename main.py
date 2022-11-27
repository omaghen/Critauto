from tools import import_bdd_csv, XX, splitting_train_test, rég_log, ADL, Random_forest, catégorie_émission_voiture_rég_log, catégorie_émission_voiture_random_forest
import statistics

if __name__ == '__main__':

    path = 'C:/Users/b/Documents/Projet CO2'  # chemin où se trouve le jeu de données

    # Importation du jeu de données (nettoyé)
    data = import_bdd_csv(path, 'Complete')
    data.head()

    # Remplacement des variables catégorielles en variables quantitatives
    data = XX(data)

    # Demander la saisie des variables optimales sélectionnées par les algorithmes de sélection de modèles
    print('Veuillez saisir la liste des variables optimales sélectionnées par les algorithmes de sélection de modèles ')
    liste_var_opt = input()

    #liste_var_opt = ['conso_urb', 'conso_exurb', 'FE', 'puiss_admin_98', 'TS TERRAINS/CHEMINS', 'GO']

    #Création des données d'apprentissage et de test
    X_train = splitting_train_test(data, liste_var_opt)[0]
    y_train = splitting_train_test(data, liste_var_opt)[1]
    X_test = splitting_train_test(data, liste_var_opt)[2]
    y_test = splitting_train_test(data, liste_var_opt)[3]

    '''1st model: logistic regression'''
    #Les coefficients de la régression logistique
    coeff_reg_log = rég_log(data, X_train, y_train, X_test, y_test)[0]

    #La matrice de confusion de la régression logisitique
    matrice_confusion_reg_log = rég_log(data, X_train, y_train, X_test, y_test)[1]
    print("La matrice de confusion de la régression logistique : \n", matrice_confusion_reg_log)

    '''2nd model : ADL'''
    # Les coefficients de la régression logistique
    coeff_adl = rég_log(data, X_train, y_train, X_test, y_test)[0]

    # La matrice de confusion de l'ADL ( = 0.05917576611482922)
    matrice_conf_ADL = ADL(X_train, y_train, X_test, y_test)[1]
    print("La matrice de confusion de l'ADL : \n", matrice_conf_ADL)


    '''3rd model : Random forest'''
    # La matrice de confusion de Random Forest
    matrice_conf_random_forest = Random_forest(X_train, y_train, X_test, y_test)[1]
    print("La matrice de confusion de Random Forest: \n", matrice_conf_random_forest)



    '''Comparaison des modèles: Erreurs de classification'''
    M = 100
    Erreur_reg_log = [0] * M
    Erreur_ADL = [0] * M
    Erreur_random_forest = [0] * M
    for i in range(0, M):
        # Création des données d'apprentissage et de test
        X_train = splitting_train_test(data, liste_var_opt)[0]
        y_train = splitting_train_test(data, liste_var_opt)[1]
        X_test = splitting_train_test(data, liste_var_opt)[2]
        y_test = splitting_train_test(data, liste_var_opt)[3]

        '''1st model: logistic regression'''
        # Erreur de classification de la régression logisitique ( = 0.008805917576611533)
        Erreur_reg_log[i] = rég_log(data, X_train, y_train, X_test, y_test)[2]

        '''2nd model : ADL'''
        # Erreur de classification de l'ADL ( = 0.002465656921451176)
        Erreur_ADL[i] = ADL(X_train, y_train, X_test, y_test)[2]

        '''3rd model : Random forest'''
        # Erreur de classification de Random Forest
        Erreur_random_forest[i] = Random_forest(X_train, y_train, X_test, y_test)[2]

    Erreur_classif_reg_log = statistics.mean([Erreur_reg_log])
    print("L'erreur de classification de la régression logistique est ", Erreur_classif_reg_log)

    Erreur_classif_ADL = statistics.mean([Erreur_ADL])
    print("L'erreur de classification de l'ADL est ", Erreur_classif_ADL)

    Erreur_classif_random_forest = statistics.mean([Erreur_random_forest])
    print("L'erreur de classification de Random Forest est ", Erreur_classif_random_forest)

    Erreur_minim = min(Erreur_classif_reg_log, Erreur_classif_ADL, Erreur_classif_random_forest)

    if Erreur_minim == Erreur_classif_reg_log:
        modele_optimal = "la régression logistique"
    elif Erreur_minim == Erreur_classif_ADL:
        modele_optimal = "l'ADL"
    else:
        modele_optimal = "le Random Forest"

    print('Le modèle qui classifie le mieux les véhicules est ', modele_optimal )

    '''Exemple classification d'un vehicule selon le Random Forest'''

    statut_pollution_voiture_random_forest = catégorie_émission_voiture_random_forest(X_train, y_train, X_test, y_test,
                                                                                      puiss_admin_98, conso_urb,
                                                                                      conso_exurb,
                                                                                      cod_cbrFE, cod_cbrGO,
                                                                                      carrosserieTS_TERRAINS_CHEMINS)
    print("Selon le Random Forest, ce véhicule a une", statut_pollution_voiture_random_forest, "de CO2")


    '''Exemple classification d'un vehicule selon la régression logistique'''
    puiss_admin_98 = 100
    conso_urb = 15
    conso_exurb = 7
    cod_cbrFE = 1
    cod_cbrGO = 0  # Attention si 1 GO alors 0 FE
    carrosserieTS_TERRAINS_CHEMINS = 1

    statut_pollution_voiture_reg_log = catégorie_émission_voiture_rég_log(coeff_reg_log, puiss_admin_98, conso_urb, conso_exurb,
                                                                  cod_cbrFE, cod_cbrGO, carrosserieTS_TERRAINS_CHEMINS)
    print("Selon la régression logistique, ce véhicule a une", statut_pollution_voiture_reg_log, "de CO2")


