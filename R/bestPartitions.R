##############################################################################
# BEST PARTITION MACRO-F1 CLUS                                               #
# Copyright (C) 2021                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri           #
# Ferrandin | Federal University of Sao Carlos                               #
# (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos | Computer Department  #
# (DC: https://site.dc.ufscar.br/) | Program of Post Graduation in Computer  #
# Science (PPG-CC: http://ppgcc.dc.ufscar.br/) | Bioinformatics and Machine  #
# Learning Group (BIOMAL: http://www.biomal.ufscar.br/)                      #
#                                                                            #
##############################################################################


###########################################################################
#
###########################################################################
FolderRoot = "~/Best-Partition-MaF1-Clus"
FolderScripts = "~/Best-Partition-MaF1-Clus/R"




########################################################################
# 
########################################################################
bestPart <- function(ds,
                     namesLabels,
                     dataset_name,
                     number_dataset, 
                     number_cores, 
                     number_folds, 
                     folderResults, 
                     similarity){
  
  retorno = list()
  
  diretorios = directories(dataset_name, folderResults, similarity)
  
  partition = c(0)
  measures = c("")
  Fold.1 = c(0)
  Fold.2 = c(0)
  Fold.3 = c(0)
  Fold.4 = c(0)
  Fold.5 = c(0)
  Fold.6 = c(0)
  Fold.7 = c(0)
  Fold.8 = c(0)
  Fold.9 = c(0)
  Fold.10 = c(0)
  allMacroF1Part = data.frame(partition, measures, Fold.1, Fold.2, Fold.3,
                              Fold.4, Fold.5, Fold.6, Fold.7, Fold.8, Fold.9, 
                              Fold.10)
  
  i = 2
  while(i<ds$Labels){
    cat("\nPartition ", i)
    setwd(diretorios$folderResultsDataset)
    nome = paste("Partition-", i, "-Evaluated-Validation.csv", sep="")
    #print(nome)
    arquivo = data.frame(read.csv(nome))
    result = arquivo[8,]
    partition = i
    macroF1 = cbind(partition, result)
    allMacroF1Part = rbind(allMacroF1Part, macroF1)
    nome2 = paste("Partition-", i, "-Mean-10-folds-Validation.csv", sep="")
    unlink(nome2, recursive = TRUE)
    if(interactive()==TRUE){ flush.console() }
    i = i + 1
    gc()
  }
  
  setwd(diretorios$folderResultsDataset)
  write.csv(allMacroF1Part[-2], paste(dataset_name, 
                                      "-All-MacroF1-Partitions.csv", sep=""), 
            row.names = FALSE)
  
  setwd(diretorios$folderOutputDataset)
  write.csv(allMacroF1Part[-2], paste(dataset_name, 
                                      "-All-MacroF1-Partitions.csv", sep=""), 
            row.names = FALSE)
  
  # depois escolher a partição com a melhor Macro-F1
  # de P2 até PN, qual obteve a maior Macro-f1 no Fold-x?
  
  fold = c(0)
  partition = c(0)
  groups = c(0)
  value = c(0)
  result2 = data.frame(fold, partition, groups, value)
  
  j = 1
  while(j<=number_folds){
    
    cat("\nFold", j)
    
    a = j + 1
    allMacroF1Part2 = allMacroF1Part[-1,-2]
    value_max = as.numeric(max(allMacroF1Part2[,a]))
    index_max = which.max(allMacroF1Part2[,a])
    
    fold = j
    partition = index_max+1
    groups = index_max+1
    value = value_max
    result = data.frame(fold, partition, groups, value)
    result2 = rbind(result2, result)
    
    j = j + 1
    if(interactive()==TRUE){ flush.console() }
    gc()
  }
  
  result3 = result2[-1,]
  
  setwd(diretorios$folderResultsDataset)
  write.csv(result3, paste(dataset_name, 
                           "-best-macroF1-partitions.csv", sep=""), 
            row.names = FALSE)
  
  setwd(diretorios$folderOutputDataset)
  write.csv(result3, paste(dataset_name, 
                           "-best-macroF1-partitions.csv", sep=""), 
            row.names = FALSE)
  
  u = 1
  while(u<=number_folds){
    cat("\nfold: ", u)
    
    result4 = result3[u,]
    num.part = as.numeric(result4$partition)
    
    Folder = paste(diretorios$folderPartitions, "/", dataset_name, sep="")
    FolderS = paste(Folder, "/Split-", u, sep="")
    
    destino = paste(diretorios$folderOutputDataset, "/Split-", u, sep="")
    if(dir.exists(destino)==FALSE){
      dir.create(destino)
    }
    
    origem = paste(FolderS, "/Partition-", num.part, sep="")
    comando = paste("cp -r ", origem, " ", destino, sep="")
    print(system(comando))
    
    origem2 = paste(FolderS, "/fold-", u, "-groups-per-partition.csv", sep="")
    comando2 = paste("cp ", origem2, " ", destino, sep="")
    print(system(comando2))
    
    u = u + 1
    gc()
  }
  
  retorno$AllMacroF1 = allMacroF1Part[-1,-2]
  retorno$BestPartitions = result2[-1,]
  return(retorno)
  
  if(interactive()==TRUE){ flush.console() }
  
  gc()
  cat("\n###############################################################")
  cat("\n# Best Partitions: END                                        #") 
  cat("\n###############################################################")
  cat("\n\n\n\n")
  
}



########################################################################
# FUNCTION ASD                                                          
#   Objective                                                           
#       Compute statistics about the partitions                         
#   Parameters                                                          
#       ds: specific dataset information                                
#       dataset_name: dataset name. It is used to save files.           
#   Return                                                              
#       Sum, mean, median, standart deviation, max and min partitions   
########################################################################
asd <- function(ds,
                namesLabels,
                dataset_name,
                number_dataset, 
                number_cores, 
                number_folds, 
                folderResults,
                similasrity){
  
  diretorios = directories(dataset_name, folderResults, similarity)
  
  # function return 
  retorno = list()
  library("dplyr")
  
  # get the best partitions of the dataset
  setwd(diretorios$folderOutputDataset)
  nome = paste(dataset_name, "-best-macroF1-partitions.csv", sep="")
  bP = data.frame(read.csv(nome))
  
  frequencia = data.frame(count(bP, vars = partition))
  names(frequencia) = c("partition","frequency")
  
  setwd(diretorios$folderOutputDataset)
  write.csv(frequencia, paste(dataset_name, "-frequency-chosed-groups.csv", 
                              sep=""), row.names = FALSE)

  # computes statistics
  soma = apply(bP, 2, sum)
  media = apply(bP, 2, mean)
  mediana = apply(bP, 2, median)
  desvioPadrao = apply(bP, 2, sd)
  minimo = apply(bP, 2, min)
  maximo = apply(bP, 2, max)
  sumario = rbind(soma, media, mediana, desvioPadrao, minimo, maximo)
  
  # saves results in the RESULTS folder
  setwd(diretorios$folderOutputDataset)
  write.csv(sumario, paste(dataset_name, "-statistic-sumary-best-part.csv", 
                           sep=""))
  
  # function return
  retorno$sumario = sumario
  return(retorno)
  
  gc()
  cat("\n#############################################################")
  cat("\n# Statistics: END                                           #")
  cat("\n#############################################################")
  cat("\n\n\n\n")
}

#######################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com        #
# Thank you very much!                                                #
#######################################################################