from __future__ import division

import pandas as pd
import numpy as np


def weighted_gini(act, pred, weight):
    df = pd.DataFrame({"act": act, "pred": pred, "weight": weight})
    df = df.sort('pred', ascending=False)
    df["random"] = (df.weight / df.weight.sum()).cumsum()
    total_pos = (df.act * df.weight).sum()
    df["cum_pos_found"] = (df.act * df.weight).cumsum()
    df["lorentz"] = df.cum_pos_found / total_pos
    # n = df.shape[0]
    #df["gini"] = (df.lorentz - df.random) * df.weight
    #return df.gini.sum()
    return sum(df.lorentz[1:].values * (df.random[:-1])) - sum(df.lorentz[:-1].values * (df.random[1:]))


def normalized_weighted_gini(act, pred, weight):
    return weighted_gini(act, pred, weight) / weighted_gini(act, act, weight)


def normalized_gini(act, pred):
    weight = np.ones(len(act))
    return normalized_weighted_gini(act, pred, weight)
