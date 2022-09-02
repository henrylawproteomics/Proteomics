#' TMT quantification
#'
#' @param JobTitle Name of the Job, will be used as the prefix in the file name.
#' @param ProteinFile The .txt file with the suffix "_Proteins.txt" in the Proteome Discoverer exports.
#' @param PSMFile The .txt file with the suffix "_PSMs.txt" in the Proteome Discoverer exports.
#' @param Conf The FDR confidence threshold for identification ("High", "Medium" or "Low"); "High" mean 95-percent confidence.
#' @param Pept The minimal number of peptides required for a protein to be deemed as identified.
#' @param SpC The minimal number of peptide spectral matches required for a protein to be deemed as quantified.
#' @param Den Channels that used as denominators in the calculations, e.g. X126, X127N, X127C for 10-plex, X127, X128 for 6-plex.
#' @param Num Channels that used as numerators in the calculations, e.g. X126, X127N, X127C for 10-plex, X127, X128 for 6-plex.
#' @export The protein ratios for the quantified proteins will be tabulated in a .csv file.

TMTquant <- function(JobTitle, ProteinFile, PSMFile, Conf, Pept, SpC, Den, Num){
  #---read input files---#
  input_protein <- read.table(ProteinFile, header=TRUE)
  input_PSM <- read.table(PSMFile, header=TRUE)
  #---filtering protein by Conf and Pept---#
  filtered_protein <- subset(input_protein, input_protein[, "Protein.FDR.Confidence"]==Conf)
  filtered_protein <- subset(filtered_protein, filtered_protein[, "Master"]=="IsMasterProtein")
  filtered_protein <- subset(filtered_protein, filtered_protein[, "X..Peptides"]>=Pept)
  ProteinList <- data.frame(filtered_protein[,"Accession"], filtered_protein[,"Description"])
  colnames(ProteinList)<- c("Accession","Description")
  write.csv(ProteinList, paste(JobTitle,"_Protein.csv"))
  #---filtering PSM with Quan Usage and Quan Info---#
  filtered_PSM <- subset(input_PSM, input_PSM[, "Peptide.Quan.Usage"]=="Use")
  filtered_PSM <- subset(filtered_PSM, filtered_PSM[, "Quan.Info"]=="Unique")
  PSMList <- data.frame(filtered_PSM[,"Reporter.Quan.Result.ID"],filtered_PSM[,"Master.Protein.Accessions"])
  colnames(PSMList)<- c("Reporter.Quan.Result.ID","Master.Protein.Accessions")
  for (tag in Den){
    PSMList[,tag] <- filtered_PSM[,tag]
  }
  for (tag in Num){
    PSMList[,tag] <- filtered_PSM[,tag]
  }
  PSMList <- na.omit(PSMList)
  write.csv(PSMList, paste(JobTitle,"_PSM.csv"))
  #---filtering protein based on SpC---#
  colnames(PSMList)[colnames(PSMList)=="filtered_PSM....Reporter.Quan.Result.ID.."] <- "Reporter.Quan.Result.ID"
  colnames(PSMList)[colnames(PSMList)=="filtered_PSM....Master.Protein.Accessions.."] <- "Master.Protein.Accessions"
  ProteinList[,"PSMsum"] <- apply(data.frame(ProteinList[,"Accession"]),1, function (x) {sum(PSMList[,"Master.Protein.Accessions"] == x)})
  ProteinList_Quant <- ProteinList[ProteinList$PSMsum >= SpC,]
  write.csv(ProteinList_Quant,"ProteinList_Quant.csv")
  write.csv(PSMList,"PSMList.csv")
  #---Calculate the normalized ratios---#
  for (D in Den){
    for (R in Num){
      PSMList[,paste(R,":",D)]<-PSMList[,R]/PSMList[,D]
      ProteinList_Quant[,paste(R,":",D)]<- apply(data.frame(ProteinList_Quant$Accession), 1, function(x){median(PSMList[PSMList$"Master.Protein.Accessions"==x,paste(R,":",D)])})
      ProteinList_Quant[paste("Normalized",R,":",D)] <- data.frame(ProteinList_Quant[paste(R,":",D)])/median(data.frame(ProteinList_Quant[,paste(R,":",D)])[,1])
      #---Calculate p-value by wilcoxon signed rank test---#
      PSMList[paste("Normalized",R,":",D)] <- data.frame(PSMList[paste(R,":",D)])[,1]/median(data.frame(ProteinList_Quant[paste(R,":",D)])[,1])
      PSMList[paste("ln",R,":",D)] <- apply(data.frame(PSMList[paste("Normalized",R,":",D)]),1,function(x){log(x)})
      ProteinList_Quant[paste("p-value",R,":",D)] <- apply(data.frame(ProteinList_Quant$Accession), 1, function(x){wilcox.test(as.numeric(subset(PSMList[PSMList$Master.Protein.Accessions==x,],select=c(paste("Normalized",R,":",D)))[,1]),mu=1, alternative="two.sided")$p.value})
    }
  }
  write.table(ProteinList_Quant, paste(JobTitle,"_Protein_Quant.csv"))
  write.table(PSMList, paste(JobTitle,"_PSM_Quant.csv"))
}

