##################################################################################################
# Select the best macro f1 partition                                                             #
# Copyright (C) 2021                                                                             #
#                                                                                                #
# This code is free software: you can redistribute it and/or modify it under the terms of the    #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #
# the License, or (at your option) any later version. This code is distributed in the hope       #
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for    #
# more details.                                                                                  #
#                                                                                                #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
#                                                                                                #
##################################################################################################


##################################################################################################
# Script 
##################################################################################################


##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/Best-Partition-MacroF1", sep="")
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/Best-Partition-MacroF1", sep="")
}
FolderScripts = paste(FolderRoot, "/scripts", sep="")


##################################################################################################
# LOAD INTERNAL LIBRARIES                                                                        #
##################################################################################################
setwd(FolderScripts)
source("libraries.R")

setwd(FolderScripts)
source("utils.R")

setwd(FolderScripts)
source("validation.R")

setwd(FolderScripts)
source("bestPartitions.R")


##################################################################################################
# Runs for all datasets listed in the "datasets.csv" file                                        #
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             # 
# delete: if you want, or not, to delete all folders and files generated                         #
##################################################################################################
executeBPC <- function(number_dataset, number_cores, number_folds, folderResults){
  
  diretorios = directories(dataset_name, folderResults)
  
  if(number_cores == 0){
    cat("\nZero is a disallowed value for number_cores. Please choose a value greater than or equal to 1.")
  } else {
    cl <- parallel::makeCluster(number_cores, outfile="")
    
    #registerDoSNOW(cl)
    
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(number_cores==1){
      cat("\n\n################################################################################################")
      cat("\n# Running Sequentially!                                                                          #")
      cat("\n##################################################################################################\n\n") 
    } else {
      cat("\n\n################################################################################################")
      cat("\n# Running in parallel with ", number_cores, " cores!                                             #")
      cat("\n##################################################################################################\n\n") 
    }
  }
  cl = cl
  
  retorno = list()
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Get dataset information: ", number_dataset, "                                                  #")
  ds = datasets[number_dataset,]
  names(ds)[1] = "Id"
  info = infoDataSet(ds)
  dataset_name = toString(ds$Name)
  cat("\nDataset: ", dataset_name)   
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n# Run: Get the names labels                                                                          #")
  setwd(diretorios$folderNamesLabels)
  namesLabels = data.frame(read.csv(paste(dataset_name, "-NamesLabels.csv", sep="")))
  namesLabels = c(namesLabels$x)
  cat("\n##################################################################################################\n\n") 
  
  cat("\n\n################################################################################################")
  cat("\n#Best Partitions:                                                                                #")
  timeVAl = system.time(resVal <- validate(number_dataset, number_cores, number_folds, dataset_name, ds, folderResults)) 
  cat("\n################################################################################################\n\n")
  
  cat("\n\n################################################################################################")
  cat("\n#Best Partitions:                                                                                #")
  timeBP = system.time(resBP <- bestPart(ds, dataset_name, number_folds, folderResults)) 
  cat("\n################################################################################################\n\n")
  
  cat("\n\n################################################################################################")
  cat("\n#Statistics:                                                                                     #")
  timeASD = system.time(resASD <- asd(ds, dataset_name, diretorios, namesLabels, folderResults)) 
  cat("\n################################################################################################\n\n")
  
  cat("\n\n################################################################################################")
  cat("\n# Run ID PART: Save Runtime                                                                      #")
  Runtime = rbind(timeVAl, timeBP, timeASD)
  setwd(diretorios$folderOutputDataset)
  name2 = paste(dataset_name, "Runtime-Best-Partitions-Clus.csv", sep="")
  write.csv(Runtime, name2)
  cat("\n################################################################################################\n\n")
  
  cat("\n\n################################################################################################")
  cat("\n#Run: Stop Parallel                                                                              #")
  parallel::stopCluster(cl) 	
  cat("\n##################################################################################################\n\n") 
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# RUN: END                                                                                       #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
  
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
