from sklearn import datasets
from sklearn import model_selection
from sklearn import metrics

# Classifiers
from sklearn import neighbors
from sklearn import svm
from sklearn import ensemble

# Clustering
from sklearn import cluster

import matplotlib.pyplot as plt
import numpy as np

# Using handwritten digits dataset
data = datasets.load_digits()

# 10% - 90% training size by 1% increments
train_size_vec = np.floor(np.linspace(0.1, 0.9, 81)*100)/100

# Classifiers
classifiers = [neighbors.KNeighborsClassifier,
               svm.SVC,
               ensemble.RandomForestClassifier]
classes = 10
cm_diags = np.zeros((classes, len(train_size_vec),
                     len(classifiers)), dtype=float)

for n, train_size in enumerate(train_size_vec):
    X_train, X_test, y_train, y_test = \
        model_selection.train_test_split(data.data,
                                         data.target,
                                         train_size=train_size)
    for m, Classifier in enumerate(classifiers):
        classifier = Classifier()
        classifier.fit(X_train, y_train)
        y_test_p = classifier.predict(X_test)
        cm_diags[:, n, m] = \
            metrics.confusion_matrix(y_test, y_test_p).diagonal()
        cm_diags[:, n, m] /= np.bincount(y_test)

for m in range(len(classifiers)):
    fig, axes = plt.subplots(1, 1, figsize=(12, 3))
    for n in range(classes):
        axes.plot(train_size_vec, cm_diags[n, :, m],
                  label=data.target_names[n])
    axes.set_title(classifiers[m].__name__)
    axes.set_ylim(0.5, 1.1)
    axes.set_xlim(0.05, 1)
    axes.set_ylabel("Classification Accuracy")
    axes.set_xlabel("Training-Size Ratio")
    axes.legend(loc=4)

# Clustering
X, y_true = data.data, data.target

n_clusters = classes
clustering = cluster.KMeans(n_clusters=n_clusters)
clustering.fit(X)
y_pred = clustering.predict(X)
idx_0, idx_1, idx_2, idx_3, idx_4, idx_5, idx_6, idx_7, idx_8, idx_9 = \
    (np.where(y_pred == n) for n in range(n_clusters))

# Finding optimal indicies
nums = []
for n in range(n_clusters):
    index = 0
    while globals()["idx_" + str(n)][0][index] < n_clusters:
        index += 1
    nums.append(index)

# Converting clusters to right values
# (e.g. clustering algorithm labeled 0s as 9s -> converting 0s to 9s)
for n in range(n_clusters):
    y_pred[globals()["idx_" + str(n)]] = \
        y_true[globals()["idx_" + str(n)][0][nums[n]]]

# Printing confusion matrix
confusion = metrics.confusion_matrix(y_true, y_pred)
print(confusion)

# Accuracy
acc = []
for n, pred in enumerate(confusion):
    tot = 0
    for i in pred:
        tot += i
    acc.append(pred[n]/tot)
print(acc)
