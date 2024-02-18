#' @import dplyr
#' @import tidyr
#' @import purrr

suppressPackageStartupMessages({
    require(dplyr)
    require(tidyr)
    require(purrr)
})

#' Load a Halo Object CSV file
#'
#' @param hfile Halo CSV file
#' @param uuidCols Columns to use to compute cell UUID
#' @param sampleName If string set SID, if function SID=sampleName(hfile)
#' @param colsExtra Any extra columns to include
#' @param markerMap Names vector to rename markers
#'
#' @return A list with elements: cell.data, marker.data, and VERSION
#'
#' @export
load_halo <- function(hfile,uuidCols,sampleName,colsExtra,markerMap) {


    if(missing(uuidCols)) {
        stop("\n\nFATAL ERROR::tools.R::load_halo\nuuidCols Missing\n")
    }

    if(missing(sampleName)) {
        SID=basename(hfile) |> gsub("\\.csv.*","",x=_)
    } else if(is.function(sampleName)) {
        SID=sampleName(hfile)
    } else {
        SID=sampleName
    }

    dd=read_halo(hfile) |> mutate(Sample=SID)
    dd$UUID=generate_Cell_UUID(dd,uuidCols)

    cell.data=dd |> select(UUID,Sample,XMin,XMax,YMin,YMax)

    marker.data=dd |>
        select(UUID,matches("_Positive_Classification$")) |>
        gather(Marker,Positive,-UUID) |>
        mutate(Marker=gsub("_Positive_Classification$","",Marker))

    if(!missing(markerMap)) {
        marker.data$Marker=markerMap[marker.data$Marker]
        marker.data=marker.data %>% filter(!is.na(Marker))
    }


    marker.data = marker.data |>
        mutate(MarkerNorm=toupper(Marker))

    markerPos=marker.data |>
        filter(Marker!="DAPI") |>
        group_by(UUID) |>
        summarize(MarkerPos=paste0(sort(MarkerNorm[Positive==1]),collapse=";")) |>
        ungroup()

    cell.data=left_join(cell.data,markerPos)

    if(!missing(colsExtra)) {
        extra.data=dd %>% select(UUID,all_of(colsExtra))
        cell.data=left_join(cell.data,extra.data)
    }

    obj=list(cell.data=cell.data,marker.data=marker.data,VERSION=VERSION)

    obj

}

generate_Cell_UUID <- function(dat,cols.UUID){
    lapply(
        transpose(dat[,cols.UUID]),
        function(x){digest::digest(paste(x,collapse=";"),algo="sha1")}
        ) |>
    unlist()
}