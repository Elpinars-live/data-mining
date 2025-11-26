#les données:
# 
# 20      5       12      14      15      3       18      10      10      10      14      14      5       20      17   15      19      17      10      16      12      16      15      12
# 10      20      17      16      14      20      17      19      13      17      11      13      8       10      6     1      17      17      16      17      13      17      17      19
# 15      9       20      11      19      17      13      7       16      8       9       2       3       15      14   10      13      14      14      10      10      16      16      17
# 15      12      11      20      10      17      12      16      13      15      12      10      8       8       11   11      13      13      17      16      14      14      15      18
# 15      4       19      11      20      17      15      8       16      9       3       1       2       15      10   10      10      3       13      9       13      15      15      13
# 9       19      18      13      18      20      10      5       12      2       2       1       5       2       12    3      17      15      15      11      11      12      16      14
# 16      15      14      10      15      15      20      10      16      10      8       2       1       16      6    13      20      2       8       10      8       18      16      14
# 17      18      15      13      16      18      12      20      15      16      17      13      11      16      10   14      16      15      12      19      18      18      17      18
# 14      14      12      10      8       10      15      10      20      9       14      8       5       14      13   11      14      9       10      11      13      18      19      15
# 19      10      15      12      10      15      13      20      15      20      12      8       12      17      12    3      12      12      10      20      14      15      15      15
# 19      12      15      12      15      8       14      14      17      9       20      15      5       19      16   19      17      16      10      10      19      18      18      15
# 20      15      14      17      16      13      13      10      17      13      14      20      13      19      10   12      11      19      8       14      14      18      18       9
# 13      2       15      11      3       19      10      12      12      18      8       19      20      12      16   12      12      20      4       18      10      15      16      17
# 20      5       13      14      15      5       18      16      10      10      14      13      5       20      17   16      17      13      10      16      11      15      15      13
# 14      10      8       6       14      10      5       9       15      7       11      6       3       13      20   13      19      14      1       10      7       18      17       2
# 10      10      5       3       6       6       8       7       9       6       15      12      1       11      10   20       8      10      4       5       11      13       8       3
# 18      17      10      10      14      17      20      17      16      10      16      1       1       18      17   15      20      1       10      9       10      19      17      13
# 15      12      10      10      3       16      10      13      10      10      11      20      17      15      18   10      10      20      10      13      19      14      14      10
# 12      14      14      13      17      18      14      12      13      11      10      5       12      12      12   11      11      10      20      11      11      11      12      14
# 18      9       11      16      6       15      13      20      17      19      12      8       13      18      19   10      11      11      14      20      14      19      17      15
# 20      8       14      12      16      8       7       10      10      6       19      11      4       19      7    16      11      16      2       16      20      14      13      12
# 14      13      13      12      13      10      16      12      18      12      10      12      12      14      18    9      15      10      9       13      10      20      19      18
# 14      13      13      9       10      11      15      9       19      10      12      12      6       14      13   12      13      13      13      12      13      18      20      16
# 12      15      16      19      15      15      11      12      13      13      4       12      13      12      12    7      12      11      14      13      11      17      17      20


library(readr)
socio_matrice <- read_table("C:/Users/speye/Downloads/socio-matrice.txt", 
                            col_names = FALSE)
View(socio_matrice)

#on visu
boxplot(socio_matrice)
#centrage reduction
socio_matrice_CR = scale(socio_matrice,center=TRUE,scale=TRUE)
View(socio_matrice_CR)
#revisu
boxplot(socio_matrice_CR)

#matrice corr
matcor = cor(socio_matrice_CR)
View(matcor)

#visualisé la matrice
image(matcor)

help(princomp)

# ACP avec princomp
acp <- princomp(socio_matrice_CR, cor=FALSE, scores=TRUE)
summary(acp)      # Résumé : variance expliquée, valeurs propres
acp$loadings      # Vecteurs propres (axes principaux)
acp$sdev^2        # Valeurs propres (variances expliquées)
acp$scores        # Coordonnées des individus sur les axes
biplot(acp)       # Visualisation biplot
