from pylab import*
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn import linear_model
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix, accuracy_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import Lasso
from sklearn.linear_model import MultiTaskLassoCV

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
def XX(data):
    liste = data.columns
    n = len(liste)      #nombre des varibles à convertir
    for i in range(0,n):        
        if (data[liste[i]].dtype == dtype('O')) & ((data[liste[i]].name) != 'var_co2'):     #type = object (qualitative)
            dummies = pd.get_dummies(data[liste[i]])
            data = data.drop([liste[i]], axis=1)
            data = pd.concat([data, dummies], axis=1)
    return data


''' Régression Lasso pour la sélection des variables'''
def reg_LASSO(data):
    data['var_co2'] = data['var_co2'].astype('category')
    df3 = data.select_dtypes(include=np.number)  # sélectionne toutes les variables numériques (features)
    df3.replace([np.inf, -np.inf], np.nan, inplace=True)
    df3 = df3.fillna(0)
    scaler = StandardScaler()
    yindex = data['var_co2'].cat.codes  # target
    df3_scale = scaler.fit(df3).transform(df3)
    X_train, X_test , y_train, y_test = train_test_split(np.delete(data, yindex, axis = 1),data[:,yindex], test_size=0.2, random_state=0)
    my_alphas = [0.001, 0.01, 0.02, 0.025, 0.05, 0.1, 0.25, 0.5, 0.8, 1.0]
    lcv = MultiTaskLassoCV(alphas=my_alphas, normalize=False, fit_intercept=False, random_state=0, cv=5)
    lcv.fit(np.delete(df3_scale, yindex, axis=1), df3_scale[:, yindex])
    # Alpha optimal
    print("alpha optimale : ", lcv.alpha_)
    lasso2 = Lasso(fit_intercept=True, alpha=lcv.alpha_).fit(X_train, y_train)
    features_selec2 = data.select_dtypes(include=np.number).drop("var_co2", axis=1).columns[
        np.abs(lasso2.coef_) > 0].tolist()
    print(features_selec2)
    # nombre de variables sélectionnées
    nlasso = sum(np.abs(lasso2.coef_) > 0)

    return features_selec2

'''Fractionnement des données d'entraînement et de test'''
def splitting_train_test(data, liste_var):
    k = len(data.columns)
    d = data.copy()
    for i in range(0, k):
        if (data.columns[i] not in liste_var) & (data.columns[i] != 'var_co2'):
            p = data.columns[i]
            d = d.drop([p], axis=1)  # pour ne garder que les variables du modèle optimal
            k = k - 1
    data = d
    # Creating the features and target
    data['var_co2'] = data['var_co2'].astype('category')
    features = data.select_dtypes(np.number)  # pour sélectionner que les variables de type numérique
    target = data['var_co2'].cat.codes  # Return Series of codes as well as the index: [0 = faible], [1 = forte], [2 = moyenne]
    # Creating the training and testing data
    X_train, X_test, y_train, y_test = train_test_split(features, target, test_size=0.2619, random_state=42, stratify=target)

    return X_train, y_train, X_test, y_test


''' régression logistique sur le modèle optimal '''
def rég_log(data, X_train, y_train, X_test, y_test):
    features = X_train.select_dtypes(np.number)
    # Initializing an logistic regression object
    logistic_reg = linear_model.LogisticRegression(solver='lbfgs', max_iter=1000)
    # Fitting the model to the training and test sets
    logistic_reg.fit(X_train, y_train)

    logistic_reg.intercept_
    logistic_reg.coef_

    # Tableau variables et leurs coefficents de la régression logistique
    coeff_reg_log = pd.DataFrame(np.concatenate([logistic_reg.intercept_.reshape(-1, 1), logistic_reg.coef_], axis=1),
                                 index=["Faible", "Forte", "Moyenne"],
                                 columns=["intercept"] + list(features.columns)).T

    # Accuracy score of the logistic regression model
    accuracy = logistic_reg.score(X_test, y_test)  # précision du modèle
    Erreur_classif = 1 - accuracy

    #matrice de confusion
    y_pred = []
    for i in range(len(X_test)):
        pred = int(logistic_reg.predict([np.array(X_test)[i]]))
        y_pred.append(pred)
    matrice = confusion_matrix(y_true=y_test, y_pred=y_pred)

    return coeff_reg_log, matrice, Erreur_classif

'''Analyse discriminante linéaire'''
def ADL(X_train, y_train, X_test, y_test):
    features = X_train.select_dtypes(np.number)
    # Initializing an discriminant analysis object
    adl = LinearDiscriminantAnalysis()
    # Fitting the model to the training and test sets
    adl.fit(X_train, y_train)
    # Accuracy score of the logistic regression model
    adl.score(X_test, y_test)  # précision du modèle #on obtient un score entre 0 et 1 (0.95 = 95%)

    adl.intercept_
    adl.coef_

    # Tableau variables et leurs coefficents de l'ADL
    coeff_adl = pd.DataFrame(np.concatenate([adl.intercept_.reshape(-1, 1), adl.coef_], axis=1),
                             index=["Faible", "Forte", "Moyenne"],
                             columns=["intercept"] + list(features.columns)).T

    # Accuracy score of the logistic regression model
    accuracy = adl.score(X_test, y_test)  # précision du modèle
    Erreur_classif = 1 - accuracy

    # matrice de confusion
    y_pred = []
    for i in range(len(X_test)):
        pred = int(adl.predict([np.array(X_test)[i]]))
        y_pred.append(pred)

    matrice = confusion_matrix(y_true=y_test, y_pred=y_pred)

    return coeff_adl, matrice, Erreur_classif

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
    modele_rf.fit(X_train, y_train)

    # l'importance des variables de notre modèle de foret aléatoire
    classement_var = pd.DataFrame(modele_rf.feature_importances_,
                                  index=X_train.columns,
                                  columns=["importance"]).sort_values("importance", ascending=False)
    # Accuracy
    Accuracy_random_forest = accuracy_score(y_test, modele_rf.predict(X_test))
    Erreur_classif = 1 - Accuracy_random_forest
    # print(f"Le pourcentage de bien classés est de : {accuracy_score(y_test, modele_rf.predict(X_test))*100} %")

    # Matrice de confusion (pour avoir une analyse plus fine des résultats)
    matrice = pd.DataFrame(confusion_matrix(y_test, modele_rf.predict(X_test)),
                           index=["Faible_donnees", "Forte_donnees", "Moyenne_donnees"],
                           columns=["Faible_predit", "Forte_predit", "Moyenne_predit"])

    return classement_var, matrice, Erreur_classif


'''Classification des voitures selon le modèle de régression logistique'''
def catégorie_émission_voiture_rég_log(coeff_reg_log, puiss_admin_98, conso_urb, conso_exurb, cod_cbrFE, cod_cbrGO,carrosserieTS_TERRAINS_CHEMINS):
    f = coeff_reg_log['Forte']
    forte = f[0] + f[1] * puiss_admin_98 + f[2] * conso_urb + f[3] * conso_exurb + f[4] * cod_cbrFE + f[5] * cod_cbrGO + \
            f[6] * carrosserieTS_TERRAINS_CHEMINS

    m = coeff_reg_log['Moyenne']
    moyenne = m[0] + m[1] * puiss_admin_98 + m[2] * conso_urb + m[3] * conso_exurb + m[4] * cod_cbrFE + m[
        5] * cod_cbrGO + m[6] * carrosserieTS_TERRAINS_CHEMINS

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

'''Classification des voitures selon le modèle de Random Forest'''
def catégorie_émission_voiture_random_forest(X_train, y_train, X_test, y_test, puiss_admin_98, conso_urb, conso_exurb,
                                             cod_cbrFE, cod_cbrGO, carrosserieTS_TERRAINS_CHEMINS):
    X_new_car = [puiss_admin_98, conso_urb, conso_exurb, cod_cbrFE, cod_cbrGO, carrosserieTS_TERRAINS_CHEMINS]
    modele_rf = Random_forest(X_train, y_train, X_test, y_test)[0]
    m = int(modele_rf.predict([X_new_car]))

    if m == 0:
        catégorie = "Faible émission"
    elif m == 1:
        catégorie = "Forte émission"
    else:
        catégorie = "Moyenne émission"

    return catégorie