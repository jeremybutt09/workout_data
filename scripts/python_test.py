# -*- coding: utf-8 -*-
"""
Created on Sun Nov 28 20:45:11 2021

Script is just for interest and for learning python

@author: Jeremy
"""

import numpy as np
import pandas as pd

wo_file = r"C:\Users\Jeremy\Documents\workout_data\data\workout_output.csv"

wo_data = pd.read_csv(wo_file)

print(wo_data)
