####################################################################################
args=commandArgs(trailing=T)
if(len(args)<1) {
    cat("\n")
    cat("   usage: getMetaFiles.R halo01.csv [halo02.csv]\n")
    cat("\n")
    cat("      - halo01.csv, halo02.csv, ... filenames of halo files to scan\n")
    cat("                                    can be compressed (.gz)\n")
    cat("\n")
    quit()
}
names(args)=args
####################################################################################
SDIR=dirname(ndsTools::thisFile())
# source("HaloX/readHalo.R")
# source("HaloX/tools.R")
VERSION="v1.0.1"
print(SDIR)
####################################################################################
####################################################################################
