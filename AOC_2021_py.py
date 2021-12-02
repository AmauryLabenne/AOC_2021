# -*- coding: utf-8 -*-
"""
Created on Wed Dec  1 15:15:49 2021

@author: amaur
"""

import pandas as pd
from pandas import Series
import numpy as np
from numpy import loadtxt, diff, ones, convolve
import lxml
import os
import sys

##############################################################################
####################     DAY 1     ###########################################
##############################################################################
##################  Day 1.1  #################################################
dt = pd.read_csv("day1.txt", header = None)

l = list(dt[0])
n = len(l)

res = sum([l[i] - l[i-1] > 0 for i in range(n)])
res
# equivalent
sum(diff(l) > 0)

##################  Day 1.2  #################################################
# convolve allows to do moving sum on window
# we only select valid window
# https://stackoverflow.com/questions/12709853/python-running-cumulative-sum-with-a-given-window
sum(diff(np.convolve(l, np.ones(3), mode='valid')) > 0)


##############################################################################
####################     DAY 2     ###########################################
##############################################################################

##################  Day 2.1  #################################################
dt = pd.read_csv("data/day2.txt", sep = " ", header = None, prefix="V")

res = dt.groupby("V0").sum()
res.loc["forward"] * (res.loc["down"] - res.loc["up"])


##################  Day 2.2  #################################################
res = {"h" : 0, "v" : 0, "aim" : 0}

def increase_res(row) :
 
  val = row["V1"]
  move = row["V0"]
  
  if (move == "down") :
    res["aim"] += val
  elif (move == "up") :
    res["aim"] -= val
  else :
    res["h"] += val
    res["v"] += (res["aim"] * val)

  return(res)

#test sur une ligne
increase_res(dt.iloc[5])

#res final
res = {"h" : 0, "v" : 0, "aim" : 0}
dt.apply(increase_res, axis = 1)
res
res["h"] * res["v"]

