from pylab import *
import pandas as pd
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OneHotEncoder
from collections import Counter


if __name__ == '__main__':
    path = 'C:/Users/b/Documents/Projet CO2'
    data = import_bdd_excel(path, 'new_CO2')
    data.head()

    data = supression_NA(data)

    liste_var = data.columns
    dt = XX(data, liste_var)
    dt.dtypes