# The sortnregress function is taken from https://github.com/Scriddie/Varsortability/blob/main/src/sortnregress.py

import numpy as np
import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression, LassoLarsIC


def sortnregress(X):
    """ Take n x d data, order nodes by marginal variance and
    regresses each node onto those with lower variance, using
    edge coefficients as structure estimates. """
    LR = LinearRegression()
    LL = LassoLarsIC(criterion='bic')

    d = X.shape[1]
    W = np.zeros((d, d))
    increasing = np.argsort(np.var(X, axis=0))

    for k in range(1, d):
        covariates = increasing[:k]
        target = increasing[k]

        LR.fit(X[:, covariates], X[:, target].ravel())
        weight = np.abs(LR.coef_)
        LL.fit(X[:, covariates] * weight, X[:, target].ravel())
        W[covariates, target] = LL.coef_ * weight

    return W


if __name__ == "__main__":
    data = pd.read_csv('a1_data.csv')
    W_hat = sortnregress(np.array(data))
    # Set all non-zero values to 1
    W_hat[W_hat != 0] = 1

    g = nx.from_numpy_array(W_hat, create_using=nx.DiGraph)
    labeldict = {0: "A", 1: "B", 2: "C", 3: "D", 4: "E", 5: "F", 6: "G", 7: "H", 8: "I", 9: "J", 10: "K"}
    nx.draw(g, labels=labeldict, with_labels=True, node_size=1500, node_color="skyblue", pos=nx.circular_layout(g))
    plt.savefig("sortnregress.png")

    # This is the adjacency matrix that resulted from applying Park's algorithm.
    # It is copied here so the plots can have the same style.
    W_hat_park = np.array([[0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                           [1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1],
                           [0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1],
                           [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
                           [0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0],
                           [0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0],
                           [0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0],
                           [0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1],
                           [0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1],
                           [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                           [0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0]])

    g_park = nx.from_numpy_array(W_hat_park, create_using=nx.DiGraph)
    nx.draw(g_park, labels=labeldict, with_labels=True, node_size=1500, node_color="skyblue", pos=nx.circular_layout(g))
    plt.savefig("parksalg.png")
