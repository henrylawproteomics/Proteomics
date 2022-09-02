library(DiscriMiner)
library(NMF)
qda_input <- read.csv("qda_input.csv", row.names = 1)
plsDA <- plsDA(qda_input[,-1], qda_input[,1])
write.csv(plsDA$Q2,"plsDA_Q2.csv")
write.csv(plsDA$R2,"plsDA_R2.csv")
#optimal at t2
plsDA <- plsDA(qda_input[,-1], qda_input[,1], comps = 2)
#predict B and L
head(df[,c(1:6)])
df_aa <- df[df$Race == "AA",]
df_aa <- df_aa[,-c(1,2)]
df_aa <- df_aa[,colnames(df_w_top10)]
aa.classify <- classify(plsDA, df_aa)
