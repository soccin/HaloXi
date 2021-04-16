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
VERSION="v1.0.1"
SDIR=dirname(ndsTools::thisFile())
source(file.path(SDIR,"tools.R"))
####################################################################################
DATE <- function(){gsub("-", "", Sys.Date())}
####################################################################################
require(tidyverse)
####################################################################################

dx=map(args,read_csv) %>% map(mutate,Sample=gsub(".tif","",`Image File Name`)) %>% bind_rows

markerKey=tibble(ColNames=grep(" Positive Classification",colnames(dx),value=T),Marker="",Type="") %>%
    mutate(Marker=gsub(" .*","",ColNames))

write_csv(markerKey,paste0("markerKey_",DATE(),".csv"))
