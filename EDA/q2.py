import pandas as pd
import numpy as np

data = pd.read_csv('./survey.data.csv')
data = data.rename(columns = {'manager': 'managerID', 'country': 'countryCode'})

cat = []
for age in data['age']:
    if age >= 99:
        cat.append(np.nan)
    elif age < 55:
        cat.append("Young")
    elif age >= 55 and age < 75:
        cat.append("Middle Aged")
    else:
        cat.append("Elder")
age_cat = pd.Series(cat)
data = pd.concat([data, age_cat.rename('age.Cat')], axis = 1)

complete_data = data.dropna()
