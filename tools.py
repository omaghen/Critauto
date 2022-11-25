from pylab import*
import pandas as pd
import random
from sklearn.model_selection import train_test_split
from sklearn import linear_model
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix

'''Importation du jeu de donnees'''
def import_bdd_csv(path, nom_bdd):
    datapath = path + '/' + nom_bdd + '.csv'
    data = pd.read_csv(datapath, sep=";", encoding='utf-8')
    return data

def import_bdd_excel(path, nom_bdd):
    datapath = path + '/' + nom_bdd + '.xlsx'
    data = pd.read_excel(datapath)
    return data

'''quelques statistiques'''
def summary(nom_bdd):
    return nom_bdd.describe()


'''Nombre total de cellules'''
def total_cellules(nom_bdd):
    return size(nom_bdd)


'''nombre total de valeurs manquantes'''
def num_NA(nom_bdd):
    return nom_bdd.isnull().sum().sum()


'''Pourcentage des valeurs manquantes '''
def perc_NA(nom_bdd):
    return (num_NA(nom_bdd) * 100) / total_cellules(nom_bdd)


'''Supression des valeurs manquantes'''
def supression_NA(nom_bdd):
    return nom_bdd.dropna()


''' Convertir une variable qualitative en catégorielle '''
def columns_string_to_factor(nom_colonne):
    return nom_colonne.astype('category')


''' Nom des colonnes d'une base '''
def nom_colonnes(nom_bdd):
    return nom_bdd.columns


'''Nombre de colonnes d'une bases '''
def nombre_colonnes(nom_bdd):
    return nom_bdd.shape[1]


'''Remplacement chacune des variables qualitatives par les indicatrices de ses modalités'''
# liste_var = contient les noms des variables quali à remplacer par des indicatrices
def XX(data, liste):
    n = len(liste)      #nombre des varibles à convertir
    for i in range(0,n):
        if (data[liste[i]].dtype == dtype('O')) & ((data[liste[i]].name) != 'var_co2'):     #type = object (qualitative)
            dummies = pd.get_dummies(data[liste[i]])
            #dummies = dummies.drop([dummies.columns[0]], axis=1)
            #data = data.drop([liste[i]], axis=1)
            data = pd.concat([data, dummies], axis=1)
    return data

'''Fractionnement des données d'entraînement et de test'''
def splitting_train_test(data, liste_var):
    k = len(data.columns)
    d = data.copy()
    for i in range(0, k):
        if (data.columns[i] not in liste_var) & (data.columns[i] != 'var_co2'):
            p = data.columns[i]
            d = d.drop([p], axis=1)  # pour garder que les var du modèle optimal
            k = k - 1
    data = d
    # Creating the features and target
    data['var_co2'] = data['var_co2'].astype('category')
    features = data.select_dtypes(np.number)  # pour sélectionner que les variables de type numérique
    target = data['var_co2'].cat.codes  # Return Series of codes as well as the index: [0 = faible], [1 = forte], [2 = moyenne]
    # Creating the training and testing data
    X_train, X_test, y_train, y_test = train_test_split(features, target, test_size = 0.33, random_state = 42, stratify = target)

    return X_train, y_train, X_test, y_test


''' régression logistique sur le modèle optimal '''
def rég_log(data, X_train, y_train, X_test, y_test):
    data['var_co2'] = data['var_co2'].astype('category')
    features = data.select_dtypes(np.number)  # pour sélectionner que les variables de type numérique
    # Initializing an logistic regression object
    logistic_reg = linear_model.LogisticRegression()
    # Fitting the model to the training and test sets
    logistic_reg.fit(X_train, y_train)
    # Accuracy score of the logistic regression model
    logistic_reg.score(X_test, y_test)  # précision du modèle #on obtient un score entre 0 et 1 (0.95 = 95%)

    logistic_reg.intercept_
    logistic_reg.coef_

    # Tableau variables et leurs coefficents de la régression logistique
    coeff_reg_log = pd.DataFrame(np.concatenate([logistic_reg.intercept_.reshape(-1, 1), logistic_reg.coef_], axis=1),
                                 index=["Faible", "Forte", "Moyenne"],
                                 columns=["intercept"] + list(features.columns)).T

    #calcul de la matrice de confusion
    y_pred = []
    for i in range(len(X_test)):
        pred = int(logistic_reg.predict([np.array(X_test)[i]]))
        y_pred.append(pred)
    matrice = confusion_matrix(y_true=y_test, y_pred=y_pred)

    return coeff_reg_log, matrice


'''Analyse discriminante linéaire'''
def ADL(X_train, y_train, X_test, y_test):
    # Initializing an discriminant analysis object
    adl = LinearDiscriminantAnalysis()
    # Fitting the model to the training and test sets
    adl.fit(X_train, y_train)
    # Accuracy score of the logistic regression model
    adl.score(X_test, y_test)  # précision du modèle #on obtient un score entre 0 et 1 (0.95 = 95%)

    adl.intercept_
    adl.coef_

    # Tableau variables et leurs coefficents de la régression logistique
    coeff_adl = pd.DataFrame(np.concatenate([adl.intercept_.reshape(-1, 1), adl.coef_], axis=1),
                             index=["Faible", "Forte", "Moyenne"],
                             columns=["intercept"] + list(features.columns)).T

    # matrice de confusion
    y_pred = []
    for i in range(len(X_test)):
        pred = int(adl.predict([np.array(X_test)[i]]))
        y_pred.append(pred)

    matrice = confusion_matrix(y_true=y_test, y_pred=y_pred)

    return coeff_adl, matrice

'''Random Forest '''
def Random_forest(X_train, y_train, X_test, y_test):
    modele_rf = RandomForestClassifier(
        n_estimators=100,
        criterion='gini',
        max_depth=None,
        min_samples_split=2,
        min_samples_leaf=1,
        min_weight_fraction_leaf=0.0,
        max_features='auto',
        max_leaf_nodes=None,
        min_impurity_decrease=0.0,
        bootstrap=True,
        oob_score=False,
        n_jobs=None,
        random_state=None,
        verbose=0,
        warm_start=False,
        class_weight=None,
        ccp_alpha=0.0,
        max_samples=None, )
    # Apprentissage
    model_rf.fit(X_train, y_train)

    # l'importance des variables de notre modèle de foret aléatoire
    classement_var = pd.DataFrame(modele_rf.feature_importances_,
                                  index=x_train.columns,
                                  columns=["importance"]).sort_values("importance", ascending=False)
    # Validation du modèle et prédiction
    # accuracy
    print(f"Le pourcentage de bien classés est de : {accuracy_score(y_test, modele_rf.predict(x_test)) * 100} %")

    # Matrice de confusion (pour avoir une analyse plus fine des résultats)
    matrice = pd.DataFrame(confusion_matrix(y_test, modele_rf.predict(x_test)),
                           index=["blanc_données", "rouge_données"],
                           columns=["blanc_predit", "rouge_predit"])
    return classement_var, matrice

'''Catégorie d'émission de la voiture: Forte, faible ou moyenne'''
def catégorie_émission_voiture(coeff_reg_log, conso_urb, conso_exurb, cod_cbrGO, lib_mrqMERCEDES, cod_cbrFE,
                               gammeMOY_INFER, lib_mrqLEXUS, typ_boite_nb_rappA_6):
    f = coeff_reg_log['Forte']
    forte = f[0] + f[1] * conso_urb + f[2] * conso_exurb + f[33] * cod_cbrGO + [46] * gammeMOY_INFER + f[
        15] * lib_mrqLEXUS + f[35] * typ_boite_nb_rappA_6
    # f[3]*lib_mrqMERCEDES + f[]*cod_cbrFE +

    m = coeff_reg_log['Moyenne']
    moyenne = m[0] + m[1] * conso_urb + m[2] * conso_exurb + m[31] * cod_cbrGO + m[46] * gammeMOY_INFER + m[
        15] * lib_mrqLEXUS + m[35] * typ_boite_nb_rappA_6
    # 'm[3]*lib_mrqMERCEDES + m[]*cod_cbrFE +'

    Pfaible = 1 / (1 + exp(forte) + exp(moyenne))  # proba Y=faible conditionnellement au var explicaive
    Pforte = exp(forte) / (1 + exp(forte) + exp(moyenne))  # proba Y=forte conditionnellement au var explicaive
    Pmoyenne = exp(moyenne) / (1 + exp(forte) + exp(moyenne))  # proba Y=moyenne conditionnellement au var explicaive

    # prendre la proba max
    maxi = max(Pfaible, Pforte, Pmoyenne)

    if maxi == Pfaible:
        catégorie = "Faible émission"
    elif maxi == Pforte:
        catégorie = "Forte émission"
    else:
        catégorie = "moyenne émission"

    return catégorie
