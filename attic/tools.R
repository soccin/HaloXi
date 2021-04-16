library(uuid)
library(yaml)
library(dplyr)
library(purrr)
library(digest)
library(readxl)


genTimeUUID <- function() {
    t.uuid=strsplit(UUIDgenerate(T),"-")[[1]]
    cc(t.uuid[3],t.uuid[2],t.uuid[1])
}

#
# For this to work the YAML file needs to be
# written with
#   column.major=F
# ie;
#   write_yaml(xx,ofile,column.major=F)
#
read_yaml_as_tibble <- function(yamlFile) {
    read_yaml(yamlFile) %>% map(as_tibble) %>% bind_rows
}

generateCellUUID <- function(Image_Location,XMin,XMax,YMin,YMax){
    as.character(sapply(paste(Image_Location,XMin,XMax,YMin,YMax,sep=";"),digest,algo="sha1",serialize=F))
}

#####################################################################################
#
# Halo Object Methods
#
#####################################################################################

filterExcludedCells<-function(oo) {

    oo$geom.data=oo$geom.data %>% filter(!Exclude)
    includedUUIDs=oo$geom.data %>% pull(UUID)
    oo$marker.data=oo$marker.data %>% filter(UUID %in% includedUUIDs)

    oo

}

getCellTypeByCell<-function(oo,cellTypeTable) {

    cellTypeMarkers=getCellTypeMarkers(cellTypeTable)
    tt=computePosMarkerTable(oo,cellTypeMarkers)
    mm=getPosMarkerByCell(oo$marker.data,cellTypeMarkers)
    m1=mm %>% distinct(PosMarkers) %>% mutate(CellType=getCellType(PosMarkers,cellTypeTable))
    m2=mm %>% distinct(PosMarkers) %>% mutate(CellSubType=getCellSubType(PosMarkers,cellTypeTable))

    oo$cellTypes=left_join(mm,m1,by="PosMarkers") %>% left_join(m2,by="PosMarkers")
    oo$cellTypeTable=cellTypeTable
    oo$posMarkerToTypeMap=full_join(m1,m2,by="PosMarkers")

    oo

}

#####################################################################################
#
# CellType functions
#
#####################################################################################

loadCellTypes <- function(cellTypeTable) {

    typeTable=read_yaml(cellTypeTable) %>% map(as_tibble) %>% bind_rows
    #
    # All entries in type table are "types"
    #
    #typeTable %>% filter(Classification_type=="type")

    typeTable

}

getCellTypeMarkers <- function(cellTypes) {

    #
    # Bug: 2020-06-06
    #
    # Need to get _BOTH_ Pos and Neg markers
    #
    # Also sort list
    #

    cellTypes %>%
        select(Pos_markers,Neg_markers) %>%
        unlist %>%
        as.character %>%
        map(~strsplit(.,",")) %>%
        unlist %>%
        unique %>%
        sort

}

getPosMarkerByCell <- function(marker.data,cellTypeMarkers) {
    marker.data %>%
        filter(Marker %in% cellTypeMarkers) %>%
        group_by(UUID) %>%
        summarize(PosMarkers=paste0(sort(Marker[Positive==1]),collapse=","))
}

computePosMarkerTable <- function(oo,cellTypeMarkers)
{

    uuidInc=oo$geom.data %>% filter(!Exclude) %>% pull(UUID)
    mm=oo$marker.data %>%
        filter(UUID %in% uuidInc & Marker %in% cellTypeMarkers) %>%
        left_join(oo$geom.data %>% select(UUID,Sample,SPOT))
    tbl=mm %>%
        group_by(Sample,SPOT,UUID) %>%
        summarize(PosMarkers=paste0(sort(Marker[Positive==1]),collapse=",")) %>%
        group_by(Sample,SPOT) %>%
        count(PosMarkers) %>%
        ungroup

    tbl

}

getPosMarkerTable <- function(rFile,cellTypeMarkers) {

    oo=readRDS(rFile)
    computePosMarkerTable(oo,cellTypeMarkers)

}

computePosMarkerByCell <- function(oo,cellTypeMarkers)
{

    uuidInc=oo$geom.data %>% filter(!Exclude) %>% pull(UUID)
    oo$marker.data %>%
        filter(UUID %in% uuidInc & Marker %in% cellTypeMarkers) %>%
        left_join(oo$geom.data %>% select(UUID,Sample,SPOT)) %>%
        group_by(Sample,SPOT,UUID) %>%
        summarize(PosMarkers=paste0(sort(Marker[Positive==1]),collapse=",")) %>%
        ungroup

}


isMarkerStringThisCellType <- function(cellType,mStr) {
    markers=strsplit(mStr,",")[[1]]
    posTypeMarkers=strsplit(cellType$Pos_markers,",")[[1]]
    negTypeMarkers=strsplit(cellType$Neg_markers,",")[[1]]

    if(cellType$Pos_required=="all") {
        if(len(intersect(markers,posTypeMarkers))!=len(posTypeMarkers)) {
            return(FALSE)
        }
    } else if(cellType$Pos_required=="+") {
        if(len(intersect(markers,posTypeMarkers))==0) {
            return(FALSE)
        }
    } else {
        stop(paste("FATAL:ERROR Invalid Pos_require",cellType$Pos_required=="+"))
    }

    len(intersect(markers,negTypeMarkers))==0

}

getCellType<-function(mStr,cellTypes) {

    mt=tibble(mStr=unique(mStr))
    mt$CellType=map(mt$mStr,getCellTypeAtomic,cellTypes) %>% unlist()
    tibble(mStr=mStr) %>% left_join(mt,by="mStr") %>% pull(CellType)

}

getCellSubType<-function(mStr,cellTypes) {

    mt=tibble(mStr=unique(mStr))
    mt$CellType=map(mt$mStr,getCellSubTypeAtomic,cellTypes) %>% unlist()
    tibble(mStr=mStr) %>% left_join(mt,by="mStr") %>% pull(CellType)

}

getCellTypeAtomic<-function(mStr,cellTypes) {

    if(mStr=="") {
        return("superNeg")
    }

    cellTypesList=transpose(cellTypes)
    ci=which(map(cellTypesList,isMarkerStringThisCellType,mStr) %>% unlist)
    if(len(ci)==0) {
        "UNKNOWN"
    } else {
        paste0(unique(cellTypes$Cell_type[ci]),collapse=";")
    }

}

getCellSubTypeAtomic<-function(mStr,cellTypes) {

    if(mStr=="") {
        return("superNeg")
    }

    cellTypesList=transpose(cellTypes)
    ci=which(map(cellTypesList,isMarkerStringThisCellType,mStr) %>% unlist)
    if(len(ci)==0) {
        "UNKNOWN"
    } else {
        paste0(unique(cellTypes$Subtype[ci]),collapse=";")
    }
}

