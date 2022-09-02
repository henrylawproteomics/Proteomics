df <- read.csv("df.csv", row.names = 1)
sum <- data.frame(colnames(df[,-c(1:3)]))
#--W cachexia t-test--#
sum$W.mean.no.cach <- apply(data.frame(c(4:5903)),1,function(x){
  mean(df[which(df$Weight.loss...5..at.diagnsois == 0 & df$Race == "W"),][,x], na.rm = TRUE)
})
sum$W.mean.cach <- apply(data.frame(c(4:5903)),1,function(x){
  mean(df[which(df$Weight.loss...5..at.diagnsois == 1 & df$Race == "W"),][,x], na.rm = TRUE)
})
sum$W.ratio <- sum$W.mean.cach/sum$W.mean.no.cach
sum$W.log2.ratio <- log2(sum$W.ratio)
sum$W.p.value <- apply(data.frame(c(4:5903)),1,function(x){
  if(sum(!is.na(df[which(df$Weight.loss...5..at.diagnsois == 0 & df$Race == "W"),][,x]))>1 & 
     sum(!is.na(df[which(df$Weight.loss...5..at.diagnsois == 1 & df$Race == "W"),][,x]))>1){
    t.test(df[which(df$Weight.loss...5..at.diagnsois == 0 & df$Race == "W"),][,x],df[which(df$Weight.loss...5..at.diagnsois == 1 & df$Race == "W"),][,x], na.action = na.omit)$p.value
  } else {
    "NaN"
  }
})
sum$W.p.adj <- p.adjust(sum$W.p.value)
#--AA cachexia t-test--#
sum$AA.mean.no.cach <- apply(data.frame(c(4:5903)),1,function(x){
  mean(df[which(df$Weight.loss...5..at.diagnsois == 0 & df$Race == "AA"),][,x], na.rm = TRUE)
})
sum$AA.mean.cach <- apply(data.frame(c(4:5903)),1,function(x){
  mean(df[which(df$Weight.loss...5..at.diagnsois == 1 & df$Race == "AA"),][,x], na.rm = TRUE)
})
sum$AA.ratio <- sum$AA.mean.cach/sum$AA.mean.no.cach
sum$AA.log2.ratio <- log2(sum$AA.ratio)
sum$AA.p.value <- apply(data.frame(c(4:5903)),1,function(x){
  if(sum(!is.na(df[which(df$Weight.loss...5..at.diagnsois == 0 & df$Race == "AA"),][,x]))>1 & 
     sum(!is.na(df[which(df$Weight.loss...5..at.diagnsois == 1 & df$Race == "AA"),][,x]))>1){
    t.test(df[which(df$Weight.loss...5..at.diagnsois == 0 & df$Race == "AA"),][,x],df[which(df$Weight.loss...5..at.diagnsois == 1 & df$Race == "AA"),][,x], na.action = na.omit)$p.value
  } else {
    "NaN"
  }
})
sum$AA.p.adj <- p.adjust(sum$AA.p.value)
#--H/L cachexia t-test--#
sum$HL.mean.no.cach <- apply(data.frame(c(4:5903)),1,function(x){
  mean(df[which(df$Weight.loss...5..at.diagnsois == 0 & df$Race == "H/L"),][,x], na.rm = TRUE)
})
sum$HL.mean.cach <- apply(data.frame(c(4:5903)),1,function(x){
  mean(df[which(df$Weight.loss...5..at.diagnsois == 1 & df$Race == "H/L"),][,x], na.rm = TRUE)
})
sum$HL.ratio <- sum$HL.mean.cach/sum$HL.mean.no.cach
sum$HL.log2.ratio <- log2(sum$HL.ratio)
sum$HL.p.value <- apply(data.frame(c(4:5903)),1,function(x){
  if(sum(!is.na(df[which(df$Weight.loss...5..at.diagnsois == 0 & df$Race == "H/L"),][,x]))>1 & 
     sum(!is.na(df[which(df$Weight.loss...5..at.diagnsois == 1 & df$Race == "H/L"),][,x]))>1){
    t.test(df[which(df$Weight.loss...5..at.diagnsois == 0 & df$Race == "H/L"),][,x],df[which(df$Weight.loss...5..at.diagnsois == 1 & df$Race == "H/L"),][,x], na.action = na.omit)$p.value
  } else {
    "NaN"
  }
})
