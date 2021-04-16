source("toolsHaloV20.R")

if(exists(".INCLUDE")) {
    stop("\n\nINCLUDE\n\n")
}

.INCLUDE=T

require(tidyverse)
require(readxl)
require(openxlsx)

dfiles=scan("mainFiles","")

pheno=read_xlsx("phenotype_descriptions.xlsx")

dx=map(dfiles,read_csv) %>% map(mutate,Sample=gsub(".tif","",`Image File Name`)) %>% bind_rows %>%
    rename(Classifier_Label=`Classifier Label`)


totals=dx %>% count(Sample,Classifier_Label,name="TotalCells")

#
# Super fragile need to improve
#

cnames=colnames(dx)
cnames=grep("\\(",cnames,value=T,invert=T)
ORIG=cnames[7:26]
NORM=toupper(ORIG) %>% gsub("FOXP","Foxp",.) %>% gsub("OR","of",.) %>% gsub("AND","and",.) %>% gsub(" ","_",.)

states=tibble(ORIG,NORM) %>% left_join(pheno,by=c(NORM="Phenotype"))

phenoCols=states %>% filter(!is.na(Tag)) %>% pull(ORIG)

xx=dx %>% select(all_of(c("Sample","Classifier_Label",phenoCols))) %>%
    gather(ORIG,Positive,all_of(phenoCols))

ds=xx %>%
    group_by(Sample,Classifier_Label,ORIG) %>%
    summarize(NumPos=sum(Positive==1),Total=n()) %>%
    mutate(PCT.Pos=NumPos/Total) %>%
    ungroup %>%
    left_join(states) %>%
    distinct(Tag,NumPos,.keep_all=T) %>%
    mutate(Classifier_Label=factor(Classifier_Label,levels=sort(unique(Classifier_Label))))

pg0=complete(ds,Sample,nesting(Classifier_Label,Tag)) %>%
    ggplot(aes(Sample,PCT.Pos,fill=Classifier_Label)) +
    geom_bar(stat="identity",position=position_dodge(preserve="single",width=1),width=.75,color="black") +
    facet_wrap(~Tag,nrow=1) +
    theme_classic(base_size=14) +
    theme(axis.text.x=element_text(angle=-45,hjust=0), strip.background = element_blank())


tbl1=ds %>%
    gather(Metric,Value,NumPos,PCT.Pos) %>%
    unite(Tag.Metric,Tag,Metric,sep=":")  %>%
    select(-ORIG,-NORM,-Description) %>%
    spread(Tag.Metric,Value,fill=0)

cellCounts=totals %>% group_by(Sample) %>% mutate(Total=sum(TotalCells)) %>% spread(Classifier_Label, TotalCells)


outFileBase=cc("dewolfs_Res02",DATE())

write.xlsx(list(CellCounts=cellCounts,CellPhenoTypes=tbl1),paste0(outFileBase,".xlsx"))
pdf(file=paste0(outFileBase,".pdf"),width=14,height=8.5)
print(pg0)
dev.off()

diagnose1=dx %>%
    select(Sample,Classifier_Label,"CD8+","FoxP3+","FoxP3+_CD8+") %>%
    mutate(Union=ifelse(`CD8+`==1 | `FoxP3+`==1,1,0)) %>%
    mutate(Intersect=ifelse(`CD8+`==1 & `FoxP3+`==1,1,0)) %>%
    count(`CD8+`, `FoxP3+`, `FoxP3+_CD8+`, Union, Intersect)

write.xlsx(diagnose1,paste0(outFileBase,"___DEBUG__CD8_FoxP3.xlsx"))


stop("\n\n\tPart-I\n\n")

#
#
#


xy=dx %>%
    select(all_of(c("Image File Name",phenoCols)),matches("(KI|CD69).*Positive Classification")) %>%
    rename_at(vars(matches("(KI|CD69).*Positive Classification")),~gsub(" .*","",.)) %>%
    gather(ORIG,Positive,-1,-Ki67,-CD69) %>%
    rename(Sample=`Image File Name`) %>%
    mutate(Sample=gsub(".tif","",Sample)) %>% filter(Positive==1)

tbl2=xy %>%
    filter(Positive==1) %>%
    group_by(Sample,ORIG) %>%
    mutate(Total=n()) %>%
    group_by(Sample,ORIG,Total) %>%
    count(Ki67,CD69) %>%
    mutate(PCT=n/Total) %>%
    gather(Marker,Pos,Ki67,CD69) %>%
    mutate(Pos=ifelse(Pos==1,"+","-")) %>%
    unite(MarkerPos,Marker,Pos,sep="") %>%
    rename(Num=n) %>%
    gather(Metric,Value,Num,PCT) %>%
    unite(MarkerPosMetric,MarkerPos,Metric,sep=":") %>%
    distinct(ORIG,Total,MarkerPosMetric,.keep_all=T) %>%
    spread(MarkerPosMetric,Value) %>%
    ungroup %>%
    left_join(states) %>%
    distinct(Sample,NORM,Total,.keep_all=T) %>%
    select(Sample,Tag,ORIG,Total,matches(":"))

write.xlsx(list(CellPhenoTypes=tbl1,FuntionalMarkers=tbl2),"dewolfs_Res01_2021-04-05.xlsx")
