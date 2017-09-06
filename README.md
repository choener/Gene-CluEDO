[![Build Status](https://travis-ci.org/choener/Gene-CluEDO.svg?branch=master)](https://travis-ci.org/choener/Gene-CluEDO)

[*generalized Algebraic Dynamic Programming Homepage*](http://www.bioinf.uni-leipzig.de/Software/gADP/)

# Gene-CluEDO: Gene Cluster Evolution Determined Order

The first paper describes the biological problem. The 2nd and 3rd paper provide
algorithmic background.

1.  Prohaska, Sonja J. and Berkemer, Sarah and Externbrink, Fabian and Gatter, Thomas  
    and Retzlaff, Nancy and The Students of the Graphs and Biological Networks Lab 2017  
    and Hoener zu Siederdissen, Christian and Stadler, Peter F.  
    *Expansion of Gene Clusters and the Shortest Hamiltonian Path Problem*  
    2017  
    preprint: http://www.bioinf.uni-leipzig.de/~choener/pdfs/pro-ber-2017.pdf  

1.  Hoener zu Siederdissen, Christian and Prohaska, Sonja J. and Stadler, Peter F.  
    *Algebraic Dynamic Programming over General Data Structures*  
    2015, BMC Bioinformatics  
    oa: https://doi.org/10.1186/1471-2105-16-S19-S2  

1.  Hoener zu Siederdissen, Christian and Prohaska, Sonja J. and Stadler, Peter F.  
    *Dynamic Programming for Set Data Types*  
    2014, Lecture Notes in Bioinformatics, 8826,  
    preprint: http://www.bioinf.uni-leipzig.de/~choener/pdfs/hoe-pro-2014.pdf  

This program accepts a matrix with distances between nodes (see below for an
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


## Installation / Pre-compiled Binaries

- Binaries are available from github for Linux x86-64. They can be downloaded
  here: <https://github.com/choener/Gene-CluEDO/releases>
- Installation from sources is possible using the Haskell stack tool, as
  described at the bottom of this page:
  <http://www.bioinf.uni-leipzig.de/~choener/software/Gene-CluEDO.html>
- Another installation option is via ``cabal new-install`` (preferred for
  development, but more involved to setup)


## Input data used for the *Expansion of Gene Clusters* paper

The data sets are available together with the sources or the binary release.
Check the ``data`` folder. The ``run-all.sh`` script runs the four examples.


## The Biological Problem We Solve

[Wikipedia on Hox clusters.](https://en.wikipedia.org/wiki/Hox_cluster)

Hox clusters are a set of genes that are linearly ordered. The genes are
(assumed) to have a single originating gene, and repeated duplication has led
to the cluster with unknown duplication tree.

The long time scales involved make it hard to produce a tree that can be
trusted. This program therefore produces summary information in the form of
edge path probabilities.


## Example matrix:

In this artificial distance matrix, we have prime numbers as distances between
nodes. Store the matrix in a file, say ``mat.dat``.

```
#   A   B   C   D   E
A   0   2   3   5   7
B   2   0  11  13  17
C   3  11   0  19  23
D   5  13  19   0  27
E   7  17  23  27   0
```

Now, run the algorithm ``./GeneCluEDO -o output.run ./mat.dat``. After the
program has run, ``output.run`` contains the a wealth of information about the
input. The maximum likelihood path, the edge weights, end probabilities, and
maximum expected accuracy path are calculated. Two additional files, here
``output.boundary.svg``, and ``output.edge.svg`` are produced. The boundary
plot provides graphical output of the probability that a node (or gene) is the
start or end node. The edge probability plot provides probabilities for each
edge (i,j) between nodes. This shows the most likely neighbors, and therefore
genetic relationship, over all possible gene orders.



#### Contact

Christian Hoener zu Siederdissen  
Leipzig University, Leipzig, Germany  
choener@bioinf.uni-leipzig.de  
http://www.bioinf.uni-leipzig.de/~choener/  

