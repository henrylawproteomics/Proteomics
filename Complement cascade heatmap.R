setwd("D:/DIA primary tumor_cachexia")
rm(df)
df <- read.csv("df.csv", row.names = 1)
df[c(1:10),c(1:8)]

comp <- read.csv("R-HSA-166658_2.csv")
comp <- comp[-9,]

#--subset complement genes--#
norm <- df[,as.vector(comp$Gene.Symbol)]
norm <- data.frame(scale(norm))
norm$Row.names <- df$Row.names
norm$Race <- df$Race
norm$Weight.loss <- df$Weight.loss...5..at.diagnsois
colnames(norm)
norm <- norm[,c(18:20, 1:17)]

#--average by race--#
rm(matrix)
matrix <- data.frame(comp[,2])
matrix$W.mean <- apply(data.frame(comp$Gene.Symbol),1,function(x){
  mean(norm[which(norm$Race == "W"),][,grep(paste("\\b",x,"\\b", sep = ""),colnames(norm))])
})
matrix$AA.mean <- apply(data.frame(comp$Gene.Symbol),1,function(x){
  mean(norm[which(norm$Race == "AA"),][,grep(paste("\\b",x,"\\b", sep = ""),colnames(norm))])
})
matrix$HL.mean <- apply(data.frame(comp$Gene.Symbol),1,function(x){
  mean(norm[which(norm$Race == "H/L"),][,grep(paste("\\b",x,"\\b", sep = ""),colnames(norm))])
})
matrix <- matrix[-c(14,16),]
#--heatmap--#
library(gplots)
heatmap.2(as.matrix(matrix[,2:4]),
          Rowv = FALSE,
          scale = "none",
          trace = "none",
          dendrogram = "none",
          labRow = matrix[,1],
          labCol = c("Caucasian","African-American","Hispanic/Latinx"),
          cexRow = 0.8,
          cexCol = 0.8,
          key = FALSE,
          col =  colorRampPalette(c("darkblue","white","darkred"))
)
          
          
          
          
          
          
          
          
          
          
          
          
