import math
import csv
import pandas as pd
import json
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import scipy.sparse as sp
from os import path

with open("tokyo-metro.json") as f:
    data = json.load(f)

g = nx.Graph()
for line in data.values():
    g.add_weighted_edges_from(line["travel_times"])
    g.add_edges_from(line["transfers"])

for n1, n2 in g.edges():
    g[n1][n2]["transfer"] = "weight" not in g[n1][n2]

on_foot = [e for e in g.edges() if g.get_edge_data(*e)["transfer"]]
on_train = [e for e in g.edges() if not g.get_edge_data(*e)["transfer"]]
colors = [data[n[0].upper()]["color"] for n in g.nodes()]

fig, ax = plt.subplots(1, 1, figsize=(14, 10))
pos = nx.drawing.nx_agraph.graphviz_layout(g, prog="neato")

nx.draw(g, pos, ax=ax, node_size=200, node_color=colors)
nx.draw_networkx_labels(g, pos=pos, ax=ax, font_size=6)
nx.draw_networkx_edges(g, pos=pos, ax=ax, edgelist=on_train, width=2)
nx.draw_networkx_edges(g, pos=pos, ax=ax, edgelist=on_foot, edge_color="blue")

def sp_permute(A, perm_r, perm_c):
    M, N = A.shape
    Pr = sp.coo_matrix((np.ones(M), (perm_r, np.arange(N)))).tocsr()
    Pc = sp.coo_matrix((np.ones(M), (np.arange(M), perm_c))).tocsr()
    return Pr.T * A * Pc.T

A = nx.to_scipy_sparse_matrix(g)

perm = sp.csgraph.reverse_cuthill_mckee(A)
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(8, 4))

ax1.spy(A, markersize=2)
ax2.spy(sp_permute(A, perm, perm), markersize=2)

universe = ['IBM',
            'MSFT',
            'GOOG',
            'AAPL',
            'AMZN',
            'FB',
            'NFLX',
            'TSLA',
            'ORCL',
            'SAP']

# Getting stock data
data = [] 
for company in universe:
    file = open(path.join(path.dirname(__file__), f"data/{company}.csv"), newline="")
    reader = csv.reader(file)
    header = next(reader)
    curr_stock = []
    for row in reader:
        close = float(row[4])
        adj_close = float(row[5])
        curr_stock.append([close, adj_close])
    data.append(curr_stock)

# Getting formated data
dataframe = []
for day in range(len(data[0])):
    temp_day = []
    for company in data:
        temp_day.append(company[day][0])
        temp_day.append(company[day][1])
    dataframe.append(temp_day)


def simulate_stocks(method, datapoints, interval, df):
    # Starting conditions
    companies = [0, 1, 2, 3, 4]
    day = 0
    cash = 5000000

    # 0: # stocks, 1: $ spent
    invested = [[0, 0], [0, 0], [0, 0], [0, 0], [0, 0]]

    while day < 249:
        for num in range(5):
            stocks = (cash/5)/df[companies[num]*2][day]
            invested[num] = [math.floor(stocks),
                             (cash/5) - (math.floor(stocks) *
                                         df[companies[num]*2][day])]
        cash = sum([invested[a][1] for a in range(len(invested))])
        if day + interval <= 249:
            day += interval
        else:
            day = 249

        # Getting percentage changes of all companies using Adj Close value
        changes = {}
        for num in range(10):
            first = df[num * 2 + 1][day-interval]
            last = df[num * 2 + 1][day]
            changes[num] = ((last-first)/first)*100

        # Selling off stocks
        for company in companies:
            num_stocks = invested[companies.index(company)][0]
            cash += df[company * 2][day] * num_stocks

        datapoints[method].append(cash)
        # Getting 5 greatest decreases
        import operator
        sort = sorted(changes.items(), key=operator.itemgetter(1))
        companies = [comp[0] for comp in sort[method*5:(method+1)*5]]


def plot_data(method, interval, df):
    low_high = ["_buy_low", "_buy_high"]
    # 0: buy_low, 1: buy_high
    datapoints = [[], []]
    # If you are trying to find the optimal interval
    if interval == -1:
        greatest = [0, 0]
        for a in range(1, 249):
            optimal_interval = a
            simulate_stocks(method, datapoints, optimal_interval, df)
            if datapoints[method][len(datapoints[method]) - 1] > greatest[1]:
                greatest[0] = a
                greatest[1] = datapoints[method][len(datapoints[method]) - 1]

        # Plotting optimal_MTM change through the year
        print("optimal_interval" + low_high[method] + ":", greatest[0])
        print("optimal_MTM" + low_high[method] + ":", greatest[1])

        datapoints = [[], []]
        simulate_stocks(method, datapoints, greatest[0], df)
    else:
        simulate_stocks(method, datapoints, interval, df)
        print("MTM" + low_high[method] + ":",
              datapoints[method][len(datapoints[method]) - 1])

    # Plotting change through the year
    fig, ax = plt.subplots()
    plt.plot([i for i in range(len(datapoints[method]))], datapoints[method])
    ax.set_xlabel("MTM" + low_high[method] if interval != -
                  1 else "optimal_MTM" + low_high[method])
    ax.set_ylabel("Money (Millions USD)")
    plt.show()


df = pd.DataFrame(dataframe)

plot_data(0, 5, df)
plot_data(1, 5, df)
plot_data(0, -1, df)
plot_data(1, -1, df)
