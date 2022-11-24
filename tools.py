''' Importation du jeu de données '''
def import_bdd_csv(path, nom_bdd):
    datapath = path + '/' + nom_bdd + '.csv'
    data = pd.read_csv( datapath, sep = ";", encoding='utf-8')
    return data

def import_bdd_excel(path, nom_bdd):
    datapath = path + '/' + nom_bdd + '.xlsx'
    data = pd.read_excel( datapath)
    return data

''' quelques statistiques '''
def summary(nom_bdd):
    return nom_bdd.describe()


'''Nombre total de cellules '''
def total_cellules(nom_bdd):
    return size(nom_bdd)


'''nombre total de valeurs manquantes '''
def num_NA(nom_bdd):
    return nom_bdd.isnull().sum().sum()


'''Pourcentage des valeurs manquantes '''
def perc_NA(nom_bdd):
    return (num_NA(nom_bdd) * 100) / total_cellules(nom_bdd)


'''Supression des valeurs manquantes '''
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
def XX(data, liste_var):
    n = len(liste_var)  # nombre des varibles à convertir
    for i in range(0, n):
        if data[liste_var[i]].dtype == dtype('O'):  # type = object (qualitative)
            # Converting the colomns to categorical
            data[liste_var[i]] = data[liste_var[i]].astype('category')
            # Integer encoding the colomns
            type_encode = LabelEncoder()
            data[liste_var[i]] = type_encode.fit_transform(data[liste_var[i]])
            # One hot encoding the columns (donner un orde)
            type_one_hot = OneHotEncoder()

            type_one_hot_encode1 = type_one_hot.fit_transform(data[liste_var[i]].values.reshape(-1, 1)).toarray()

            ohe_variable1 = pd.DataFrame(type_one_hot_encode1, columns=[liste_var[i] + "_" + str(int(j)) for j in
                                                                        range(type_one_hot_encode1.shape[1])])
            d0 = ohe_variable1.columns[0]
            ohe_variable1 = ohe_variable1.drop([d0], axis=1)

            data = pd.concat([data, ohe_variable1], axis=1)

            data = data.drop([liste_var[i]], axis=1)

            nombre_indicatrices = ohe_variable1.shape[
                1]  # nombre d'indicatrices créées pour cette variable (=le même nombre de ses modalités)
            nom_indicatrices = ohe_variable1.columns
            for k in range(0, nombre_indicatrices):
                data[nom_indicatrices[k]] = data[nom_indicatrices[k]].astype('category')
    return data
