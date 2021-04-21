####################################################################################
args=commandArgs(trailing=T)
if(len(args)<1) {
    cat("\n")
    cat("   usage: loadHaloTables.R manifest.csv\n")
    cat("\n")
    cat("      mainfest.csv  - table of Halo files with sampleName mappings")
    cat("\n")
    quit()
}
names(args)=args
####################################################################################
VERSION="v1.0.1"
SDIR=dirname(ndsTools::thisFile())
source(file.path(SDIR,"tools.R"))
####################################################################################
stop("\n\nINCLUDE\n\n")
####################################################################################
suppressPackageStartupMessages({
    require(tidyverse)
})
####################################################################################

manifest=read_csv(args[1])

objs=list()
cols.UUID=c("Image_File_Name","XMin","XMax","YMin","YMax")
cols.extra=c("Classifier_Label")

for(ii in seq(nrow(manifest))) {

    hfile=manifest$HaloFile[ii]
    sampleName=manifest$SampleName[ii]

    objs[[sampleName]]=load_halo(hfile,cols.UUID,sampleName,cols.extra)

}

