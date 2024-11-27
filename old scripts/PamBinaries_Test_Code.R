## -------------------------------------------------------------------------------------------------------------------------- ## 
 ## LOADING PAMGUARD BINARY FILES DIRECTLY INTO R ## 

## INSTALLING PAMBINARIES PACKAGE 
## install.packages("devtools");
## devtools::install_github('TaikiSan21/PamBinaries')

library(devtools)
library(PamBinaries)
library(dplyr)
library(dbplyr)
library(ggplot2)

## ATTEMPTING TO LOAD BINARY FILES 
BinaryDirectory <- "/Volumes/heyday/MSc_Project/Binary" # THIS DOES NOT READ INTO THE FOLDER 
TESTBinaryDirectory <- "/Users/ShannonMerkle/Desktop/Click_Detector_HF_Click_Detector_Clicks_20210101_000000.pgdf" # THIS WORKS

BinaryDataTEST <- loadPamguardBinaryFile(BinaryDirectory) # NOPE
TESTBinaryData <- loadPamguardBinaryFile(TESTBinaryDirectory) #THIS WORKS FROM HERE

DataFrameTest <- pbToDf(TESTBinaryData)
View(DataFrameTest)


## FULL PROCESS OF WHAT WORKS - SINGLE BINARY FILE DIRECTORY, THEN NAME IT, THEN MUST CREATE DATA FRAME, THEN VIEW 
TestCLICKBinaryDirectory <- "/Users/ShannonMerkle/Desktop/Noise_Monitor_Noise_Monitor_Noise_Monitor_20210101_120000.pgdf"
TestCLICKBinaryData <- loadPamguardBinaryFile(TestCLICKBinaryDirectory)
CLICKdataFrame <- pbToDf(TestCLICKBinaryData)
View(CLICKdataFrame) ## THE VIEW ON THIS IS REALLY WEIRD, NOT HOW IT SHOULD BE 

readNoiseMonData()
readNoiseMonHeader()

## NOISE MONITOR BINARY READ TEST 
Jan2021Feb2021_FILES <- "/Volumes/heyday/MSc_Project/Binary/Binary_01Jan2021-17Feb2021" # directory of larger binary folder
Jan2021Feb2021_DATA <- loadPamguardBinaryFile(Jan2021Feb2021_FILES) # attempting to load files

Jan2021Feb2021_NoiseMonDATA <- readNoiseMonData(Jan2021Feb2021_FILES) # function not found
Jan2021Feb2021_NoiseMonHEADER <- readNoiseMonHeader(Jan2021Feb2021_FILES) # function not found

testDF <- pbToDf(Jan2021Feb2021_FILES) # error says object 'justData' not found 
View(testDF) # object not found 

