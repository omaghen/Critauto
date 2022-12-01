''' Tests unitaires '''

import pytest
from tools import import_bdd_csv, total_cellules, num_NA, nombre_colonnes,XX,  splitting_train_test, rég_log, ADL, Random_forest, catégorie_émission_voiture_rég_log, catégorie_émission_voiture_random_forest

path = 'C:/Users/b/Documents/Projet CO2'
data1 = import_bdd_csv(path, 'Complete')
data2 = import_bdd_csv(path, 'Validation')

def test_total_cellules_for_data1():
    assert total_cellules(data1) == 910476

def test_total_cellules_for_data2():
    assert total_cellules(data2) == 42585


def test_num_NA_for_data1():
    assert num_NA(data1) == 0
def test_num_NA_for_data2():
    assert num_NA(data2) == 0

def test_nombre_colonnes_for_data1():
    assert nombre_colonnes(data1) == 84
def test_nombre_colonnes_for_data2():
    assert nombre_colonnes(data2) == 15

data1 = XX(data1)
liste_var = ['conso_urb', 'conso_exurb', 'FE', 'puiss_admin_98', 'TS TERRAINS/CHEMINS', 'GO']

X_train= splitting_train_test(data1, liste_var)[0]
y_train= splitting_train_test(data1, liste_var)[1]
X_test= splitting_train_test(data1, liste_var)[2]
y_test= splitting_train_test(data1, liste_var)[3]
coeff_reg_log = rég_log(data1, X_train, y_train, X_test, y_test)[0]
puiss_admin_98= 11
conso_urb= 9.5
conso_exurb= 7.8
cod_cbrFE= 0
cod_cbrGO= 1
carrosserieTS_TERRAINS_CHEMINS= 0

def test_rég_log():
    assert rég_log(data1, X_train, y_train, X_test, y_test)[2] == 0.008805917576611533      #0.008805917576611533

def test_ADL():
    assert ADL(X_train, y_train, X_test, y_test)[2] == 0.05917576611482922         #0.05917576611482922

def test_Random_forest():
    assert Random_forest(X_train, y_train, X_test, y_test)[2] == 0.002465656921451176

def test_catégorie_émission_voiture_random_forest():
    assert catégorie_émission_voiture_random_forest(X_train, y_train, X_test, y_test, puiss_admin_98, conso_urb, conso_exurb,
                                             cod_cbrFE, cod_cbrGO, carrosserieTS_TERRAINS_CHEMINS) == 'Forte émission'

def test_catégorie_émission_voiture_rég_log():
    assert catégorie_émission_voiture_rég_log(coeff_reg_log, puiss_admin_98, conso_urb, conso_exurb, cod_cbrFE, cod_cbrGO,carrosserieTS_TERRAINS_CHEMINS) == 'Forte émission'