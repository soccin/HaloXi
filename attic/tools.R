git.describe<-function(){

    system2(
        "git",
        c("describe", "--tags", "--always", "--long", "--dirty='-UNCOMMITED'"),
        stdout=T
        )

}

VERSIONTOOLSFILE="HaloXi.v1"
VERSIONTOOLS=paste0(VERSIONTOOLSFILE,".",git.describe())

DATE <- function(){gsub("-", "", Sys.Date())}

fixColNames <- function(ss) {
    gsub(" ","_",ss) %>% gsub("_\\(.*\\)$","",.)
}

read_halo <- function(ff,...) {

    read_csv(ff,...) %>% rename_all(~fixColNames(.))

}

generateCellUUID <- function(dat,cols.UUID){
    lapply(
        transpose(dat[,cols.UUID]),
        function(x){digest::digest(paste(x,collapse=";"),algo="sha1")}
        ) %>%
    unlist
}

load_halo <- function(hfile,cols.UUID,sampleName,cols.extra) {

    if(missing(cols.UUID)) {
        stop("\n\nFATAL ERROR::tools.R::load_halo\ncols.UUID Missing\n")
    }

    if(missing(sampleName)) {
        sampleName=basename(hfile) %>% gsub("\\.csv.*","",.)
    }

    dd=read_halo(hfile) %>% mutate(Sample=sampleName)
    dd$UUID=generateCellUUID(dd,cols.UUID)

    cell.data=dd %>% select(UUID,Sample,XMin,XMax,YMin,YMax)

    marker.data=dd %>%
        select(UUID,matches("_Positive_Classification")) %>%
        gather(Marker,Positive,-UUID) %>%
        mutate(Marker=gsub("_\\(.*","",Marker)) %>%
        mutate(MarkerNorm=toupper(Marker))

    markerPos=marker.data %>%
        group_by(UUID) %>%
        summarize(MarkerPos=paste0(sort(MarkerNorm[Positive==1]),collapse=";")) %>%
        ungroup

    cell.data=left_join(cell.data,markerPos)

    if(!missing(cols.extra)) {
        extra.data=dd %>% select(UUID,all_of(cols.extra))
        cell.data=left_join(cell.data,extra.data)
    }

    obj=list(cell.data=cell.data,marker.data=marker.data,VERSION=VERSIONTOOLS,DATE=DATE())

    obj

}
