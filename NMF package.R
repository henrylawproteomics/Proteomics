setwd("D:/DIA primary tumor_cachexia/top5_NMF")
df <- read.csv("top5-Hb.csv", row.names = 1)
library(NMF)
#--optimizing rank for clustering--#
#--cophenic coeff start decrease; inflection point in RSS--#
res <- nmf(df, 2:7)
plot(res)
consensusmap(res)
#--n=3, the real run--#
res <- nmf(df, 6, nrun=50)
summary(res)
#--extract the W and H--#
basismap(res)
coefmap(res, Colv = 'euclidean')
write.csv(coef(res),"top5-Hb_coef_6.csv")
write.csv(basis(res),"top5-Hb_basis_6.csv")