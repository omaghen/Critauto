from tools import import_bdd_excel, supression_NA, XX, rég_log, catégorie_émission_voiture

if __name__ == '__main__':
    path = 'C:/Users/b/Documents/Projet CO2'    # chemin où se trouve le jeu de données

    # Importation du jeu de données (nettoyé)
    data = import_bdd_excel(path, 'new_CO2_2')
    data.head()

    # Remplacement des variables catégorielles en variables quantitatives
    liste = ['conso_urb', 'conso_exurb', 'cod_cbr', 'lib_mrq', 'typ_boite_nb_rapp', 'gamme']
    data = XX(data, liste)

    # Demande des variables optimales sélectionnées par les algorithmes de sélection de modèles
    print('Veuillez saisir la liste des variables optimales sélectionnées par les algorithmes de sélection de modèles ')
    liste_var_opt = input()

    # Les coefficients de la régression logistique
    coeff_reg_log = rég_log(data, liste_var_opt)[0]
    print(coeff_reg_log)

    # La matrice de confusion de la régression logisitique
    matrice_de_confusion = rég_log(data, liste_var_opt)[1]
    print(matrice_de_confusion)

    # Catégorie d'émission d'un véhicule (catégorie : Forte, faible ou moyenne)
    conso_urb = 15
    conso_exurb = 7
    cod_cbrGO = 0  # Attention si 1 GO alors 0 FE
    lib_mrqMERCEDES = 0
    cod_cbrFE = 1
    gammeMOY_INFER = 1
    lib_mrqLEXUS = 1  # Attention si 1 alors aux autres !
    typ_boite_nb_rappA_6 = 0

    statut_pollution_voiture = catégorie_émission_voiture(coeff_reg_log, conso_urb, conso_exurb, cod_cbrGO,
                                                          lib_mrqMERCEDES, cod_cbrFE, gammeMOY_INFER, lib_mrqLEXUS,
                                                          typ_boite_nb_rappA_6)
    print(statut_pollution_voiture)
