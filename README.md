[![Build Status](https://travis-ci.org/choener/HoxCluster.svg?branch=master)](https://travis-ci.org/choener/HoxCluster)

[*generalized Algebraic Dynamic Programming Homepage*](http://www.bioinf.uni-leipzig.de/Software/gADP/)

# HoxCluster: determine the most likely Hox cluster ordering.

This program accept a matrix with distances between nodes (see below for an
example). It then proceeds to calculate the Hamiltonian path with the shortest
distance between each pair of nodes, where the path has to travel from the
start, then to all other nodes, finally stopping at the last node.

We further calculate all neighbour probabilities via Inside/Outside. This means
that for any two nodes we calculate the weight of the edge between these two
nodes. The weight is between ``[0, ... ,1]`` where ``0`` denotes the the nodes
are almost surely not direct neighbours on a weighted-randomly drawn path,
while ``1`` denotes that they almost surely are.

Finally, we calculate the probability that a node is one of the terminal nodes
in the Hamiltonian path, i.e. either the first or the last node.

## The Biological Problem We Solve

Hox clusters are a set of genes that are linearly ordered. It is assumed that
local duplications ...

## Example matrix:

In this artificial distance matrix, we have prime numbers as distances between
nodes.

```
#   A   B   C   D   E
A   0   2   3   5   7
B   2   0  11  13  17
C   3  11   0  19  23
D   5  13  19   0  27
E   7  17  23  27   0
```


#### Contact

Christian Hoener zu Siederdissen  
Leipzig University, Leipzig, Germany  
choener@bioinf.uni-leipzig.de  
http://www.bioinf.uni-leipzig.de/~choener/  

