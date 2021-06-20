#TeamTree, vers. 11, June-20-2021
#Created by Frank W. Pfrieger, CNRS University of Strasbourg, Strasbourg, France (fw-pfrieger@gmx.de OR frank.pfrieger@unistra.fr).
#Written using R (Vers. 3.5.3 or higher; https://www.r-project.org/) and additional libraries that must be installed beforehand. The script can be executed by cut/paste in RGui.
#TeamTree reveals the authors working in a field (topic) of research (see Pfrieger, 2021 https://doi.org/10.1101/2020.06.01.128355) based on a list of publications. Required are author names, a publication-specific ID and the year of publication.
#The present version reads files with articles from the bibliometric databases PubMed (*.csv) or Web of Science (*.txt). The source must be indicated (see below).

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1. SETUP
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Clear memory
rm(list=ls(all=TRUE))

#Measure execution times (only for part 2. DATA ANALYSIS)
start_time_tot <- Sys.time()
start_time_prep <- Sys.time()

#Load required libraries
library(data.table)
library(dunn.test)
library(eulerr)
library(ggfortify)
library(ggplot2)
library(ggrepel)
library(igraph)
library(plot3D)

#User-defined directories where files are read from/saved to. 
User_dir_read <- "d:\\data\\papers\\teamtree\\DBqueries\\"
User_dir_save <- "d:\\data\\papers\\teamtree\\plots"

#Indicate source of publications. Current options are PubMed (T) or Web of Science (F)
Pubmed <- T

#More options
show_plots <- T
save_plots <- F
new_colors <- T

#Remove ambiguous names
RemAmbig <- T
Ambig_names <- "CHEN |GAO |HU |HUANG |JIANG |LEE |LI |LIU |LU |KIM |SHI |SUN |SUNG |WANG |WU |XU |YANG |ZHANG |ZHAO |ZHOU |ZHUO"

#User-defined topic, defines the filename with bibliometric data (Pubmed: Topic.csv | WOS: Topic.txt). Variable also used to define filenames to save data and graphs.
#Topic <- "pub_aplysia"
#Topic <- "wos_aplysia"
#Topic <- "pub_organo"
#Topic <- "wos_organo"
#Topic <- "pub_organo_tiab"
#Topic <- "pub_crispr"
#Topic <- "wos_crispr"
Topic <- "pub_clock"
#Topic <- "wos_clock"
#Topic <- "wos_supra_2021"
#Topic <- "wos_cosmic_2021"
#Topic <- "wos_ice_2021"
#Topic <- "wos_laser_2021c"
#Topic <- "wos_quant_2021"

#Filenames to save TeamTree data
File_TT_data <- paste(User_dir_save, sprintf("%s_TT_data.csv", Topic),sep="\\")
File_TT_rates <- paste(User_dir_save, sprintf("%s_TT_rates.csv", Topic),sep="\\")
File_TT_summary <- paste(User_dir_save, sprintf("%s_TT_summary.csv", Topic),sep="\\")

#Parameters
#Number of ranks to determine "top authors"
Top_n <- 10

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2. DATA ANALYSIS
#----------------------------------------------------------------------------------------------------------------------------------------------------------
#2.1. Read files, prepare data
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Read file containing publications from a user-defined folder. Different formats depending on source: Pubmed: csv, Web of Science: txt
#Note: for PubMed only columns with publication ID, authors and year of publication are required. For WOS, citation counts are included as well.

if(Pubmed){
#Read articles from PubMed in csv file. 
Filename_publications <- paste(User_dir_read,sprintf("%s.csv", Topic),sep="")
Publications <- fread(file = Filename_publications, header=T,encoding = "UTF-8", select=c(1,3,7))

#Rename columns
colnames(Publications) <- c("Pub_ID","Authors","PY")
Publications <- Publications[!duplicated(Publications$Pub_ID)]

#Remove articles with unwanted author entries (can be extended)
Publications_sel <- Publications[!grepl("No authors listed|et al", Publications$Authors),]

#Remove point at the end of author list (PubMed format)
Publications_sel$Authors <- substring(Publications_sel$Authors,1,nchar(Publications_sel$Authors)-1)

#Extract authors per publication, determine first author, last author and number of authors (Author_count)
Author_all <- strsplit(Publications_sel$Authors, ", ")
Author_first <- toupper(sapply(Author_all, '[', 1))
Author_rev <- sapply(Author_all, 'rev')
Author_last <- toupper(sapply(Author_rev, '[', 1))
} else{

#Read articles from WOS in txt file.
Filename_publications <- paste(User_dir_read,sprintf("%s.txt", Topic),sep="")
Publications <- fread(file = Filename_publications, header=F, select=c(2,33,44,52))

#Rename columns
colnames(Publications) <- c("Authors","PY","TC","Pub_ID")

#Remove articles with unwanted author entries (can be extended)
Publications_sel <- Publications[!grepl("No authors listed|et al", Publications$Authors),]

#Extract authors per publication, determine first author, last author and number of authors (Author_count)
Author_all <- strsplit(Publications_sel$Authors, "; ")

#Required for WOS data
#Assemble family names and initials of First Author 
Author_first_temp <- toupper(sapply(Author_all, '[', 1))
Author_first_famname <- sapply(strsplit(Author_first_temp, ", "), '[', 1)
Author_first_init <-  sapply(strsplit(Author_first_temp, ", "), '[', 2)
Author_first_init <- ifelse(nchar(Author_first_init)<=2,gsub("\\.","",Author_first_init),Author_first_init)
Author_first_init <- ifelse(nchar(Author_first_init)==2& grepl("[a-z]", substring(Author_first_init,2,2)),substring(Author_first_init,1,1),Author_first_init)
Author_first_init <- ifelse(nchar(Author_first_init)>2&!grepl("\\.|\\s|\\-", Author_first_init),substring(Author_first_init,1,1),Author_first_init)
Author_first_init <- ifelse(grepl(".\\.\\s.\\.", Author_first_init),gsub("\\.|\\s", "",Author_first_init),Author_first_init)
Author_first_init <- ifelse(grepl(".\\..\\.", Author_first_init),gsub("\\.", "",Author_first_init),Author_first_init)
Author_first_init <- ifelse(nchar(Author_first_init)>2&grepl(" ", Author_first_init), paste(substring(Author_first_init,1,1),substring(Author_first_init,regexpr(" ",Author_first_init)+1,regexpr(" ",Author_first_init)+1),sep=""),Author_first_init)
Author_first_init <- ifelse(nchar(Author_first_init)>2&grepl("-", Author_first_init), paste(substring(Author_first_init,1,1),substring(Author_first_init,regexpr("-",Author_first_init)+1,regexpr("-",Author_first_init)+1),sep=""),Author_first_init)
Author_first <- paste(Author_first_famname, ifelse(is.na(Author_first_init),"", Author_first_init), sep=" ")

#Assemble family names and initials of Last Authors.
Author_rev <- sapply(Author_all, 'rev')
Author_last_temp <- toupper(sapply(Author_rev, '[', 1))
Author_last_famname <- sapply(strsplit(Author_last_temp, ", "), '[', 1)
Author_last_init <-  sapply(strsplit(Author_last_temp, ", "), '[', 2)
Author_last_init <- ifelse(nchar(Author_last_init)<=2,gsub("\\.","",Author_last_init),Author_last_init)
Author_last_init <- ifelse(nchar(Author_last_init)==2& grepl("[a-z]", substring(Author_last_init,2,2)),substring(Author_last_init,1,1),Author_last_init)
Author_last_init <- ifelse(nchar(Author_last_init)>2&!grepl("\\.|\\s|\\-", Author_last_init),substring(Author_last_init,1,1),Author_last_init)
Author_last_init <- ifelse(grepl(".\\.\\s.\\.", Author_last_init),gsub("\\.|\\s", "",Author_last_init),Author_last_init)
Author_last_init <- ifelse(grepl(".\\..\\.", Author_last_init),gsub("\\.", "",Author_last_init),Author_last_init)
Author_last_init <- ifelse(nchar(Author_last_init)>2&grepl(" ", Author_last_init), paste(substring(Author_last_init,1,1),substring(Author_last_init,regexpr(" ",Author_last_init)+1,regexpr(" ",Author_last_init)+1),sep=""),Author_last_init)
Author_last_init <- ifelse(nchar(Author_last_init)>2&grepl("-", Author_last_init), paste(substring(Author_last_init,1,1),substring(Author_last_init,regexpr("-",Author_last_init)+1,regexpr("-",Author_last_init)+1),sep=""),Author_last_init)
Author_last <- paste(Author_last_famname, ifelse(is.na(Author_last_init),"", Author_last_init), sep=" ")
}

Author_count <- sapply(Author_all, 'length')

#Generate data table listing for each publication: year, first and last author, author count and article identifier (Pub_ID), uses data.table library
DT_pub_temp <- data.table("PY"=Publications_sel$PY, "Authors"=Author_all, "Last"=Author_last, "First"=Author_first, "Author_count"=Author_count, "Pub_ID"=Publications_sel$Pub_ID)

#Remove erroneous entries and remove ambiguous names
if(RemAmbig){
DT_pub <- DT_pub_temp[!is.na(Last) & !grepl(Ambig_names, Last)]
} else{
DT_pub <- DT_pub_temp[!is.na(Last)]
}

#Calculate time required for this part
end_time_prep <- Sys.time()
Duration_prep <- end_time_prep - start_time_prep


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#2.2. Analysis of the publication record per author
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Start timer for this part
start_time_pub <- Sys.time()

#Get first (publication year, PY_start) and last year (PY_end) of publications per author, calculate publication period, publication count and average author count as last author
PY_end <- DT_pub[,max(as.numeric(PY)), by=Last]
PY_start <- DT_pub[,min(as.numeric(PY)), by=Last]
Pub_period <- DT_pub[,max(as.numeric(PY))-min(as.numeric(PY))+1, by=Last]
Pub_count <- DT_pub[ , .N, by = Last]
Author_count_avg <- DT_pub[,mean(Author_count), by=Last]

#Generate TeamTree database with author names derived from last authors, publication counts and publication periods of last authors
#Other author-specific information will be saved to the database subsequently
D_all <- data.frame("Author_name"=Pub_count$Last,"PC"=Pub_count$N, "PY_start"=PY_start$V1, "PY_end"=PY_end$V1, "Pub_period"=Pub_period$V1, "Author_count_avg"=Author_count_avg$V1)

D_all$PC_annu <- D_all$PC/D_all$Pub_period

#Analyse first author papers of authors
#Select only publications with first authors that published also as last authors
DT_pub_first <- DT_pub[First %in% Last & Author_count >1]

#Determine start, end and period of publication as First author and save to TeamTree database
PY_first_start <- DT_pub_first[,min(as.numeric(PY)), by=First]
D_all$PY_first_start <- PY_first_start$V1[match(D_all$Author_name, PY_first_start$First)]

PY_first_end <- DT_pub_first[,max(as.numeric(PY)), by=First]
D_all$PY_first_end <- PY_first_end$V1[match(D_all$Author_name, PY_first_end$First)]

First_period <- DT_pub_first[,max(as.numeric(PY))-min(as.numeric(PY))+1, by=First]
D_all$First_period <- First_period$V1[match(D_all$Author_name, First_period$First)]
D_all$First_period[is.na(D_all$First_period)] <- 0

#Calculate number of publications (publication count, PC) as first author
Pub_first_count <- DT_pub_first[,.N, by=First]
D_all$PC_first <- Pub_first_count$N[match(D_all$Author_name, Pub_first_count$First)]
D_all$PC_first[is.na(D_all$PC_first)] <- 0
D_all$PC_first_annu <- D_all$PC_first/D_all$First_period
D_all$PC_first_annu[is.na(D_all$PC_first_annu)] <- 0

#Generate Author_index: unique chronologic identifier for each last author with alternating sign for visual display using TeamTrees
D_all <- D_all[order(D_all$PY_start),]
D_all$Author_index <- seq_along(D_all$Author_name)*((-1)^(1+seq_along(D_all$Author_name)))

#Generate Author_color: unique random color for each last author or read from file if stored previously
if(new_colors){
D_all$Author_color <- rgb(r=sample(x=seq(0,255,by=1),size=nrow(D_all), replace=T),g=sample(x=seq(0,255,by=1),size=nrow(D_all), replace=T), b=sample(x=seq(0,255,by=1),size=nrow(D_all), replace=T), maxColorValue=255)
} else {
D_all_temp <- fread(file = File_TT_data, header=T,encoding = "UTF-8")
D_all$Author_color <- D_all_temp$Author_color}

#Calculate annual rates of publication and of last authors entering the field
PY_rates_min <- min(DT_pub$PY)
PY_rates_max <- max(DT_pub$PY)+1
Rates_bins <- seq(PY_rates_min, PY_rates_max, by=1)
Pub_rates <- hist(DT_pub$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates <- hist(D_all$PY_start, breaks=Rates_bins, plot=F, right=F)

#Create database for annual publication/author rates in the field
D_rates <- data.frame(PY_bins=floor(Pub_rates$mids), pubs_count=Pub_rates$counts)
D_rates$PY_bins_norm <- seq_along(D_rates$PY_bins)/(max(D_rates$PY_bins)-min(D_rates$PY_bins))
D_rates$pubs_freq <-Pub_rates$counts/sum(Pub_rates$counts)
D_rates$authors_count <- Author_rates$counts
D_rates$authors_freq <- Author_rates$counts/sum(Author_rates$counts)

#Calculate time required for this part
end_time_pub <- Sys.time()
Duration_pub <- end_time_pub - start_time_pub


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#2.3. Analysis of genealogic connections between authors based on First-Last author pairs considered as ancestor-offspring.
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Note: Offspring are those last authors whose earliest publication as first author is before the earliest publication as last author and
#where the earliest last author publication of the ancestor is before the earliest last author publication of the offspring

#Start timer for this part
start_time_off <- Sys.time()

#Assemble data
DT_pub_first$PY_ancestor <-  D_all$PY_start[match(DT_pub_first$Last, D_all$Author_name)]
DT_pub_first$PY_offspring <- D_all$PY_start[match(DT_pub_first$First, D_all$Author_name)]
DT_pub_first$AI_ancestor <- D_all$Author_index[match(DT_pub_first$Last, D_all$Author_name)]
DT_pub_first$AI_offspring <- D_all$Author_index[match(DT_pub_first$First, D_all$Author_name)]
DT_pub_first$TC_ancestor <- D_all$Author_color[match(DT_pub_first$Last, D_all$Author_name)]
DT_pub_first$TC_offspring <- D_all$Author_color[match(DT_pub_first$First, D_all$Author_name)]

#Select only offspring whose PY_start is after the PY_start of the ancestor and where the earliest publication as first author is before the earliest publication as last author
#Note, the current version ignores cases where PY_ancestor==PY_offspring. It may "break up" some families.
DT_pub_anc_off <- DT_pub_first[ ,.SD[which(PY_ancestor < PY_offspring & PY <= PY_offspring)],]

#Calculate number of pubs per ancestor-offspring pair
DT_pub_anc_off[, PC_anc_off := .N, by=.(Last,First)]

DT_anc_off <- DT_pub_anc_off[ , .SD[which.min(abs(AI_ancestor))], by = First]

#Determine number of offspring per last author (OC, offspring count)
Off_count <- DT_anc_off[ , .N, by = Last]
D_all$OC <- Off_count$N[match(D_all$Author_name, Off_count$Last)]
D_all$OC[is.na(D_all$OC)] <- 0

#Determine start, end and period of author publications with offspring
PY_off_start <- DT_pub_first[,min(as.numeric(PY)), by=Last]
D_all$PY_off_start <- PY_off_start$V1[match(D_all$Author_name, PY_off_start$Last)]

PY_off_end <- DT_pub_first[,max(as.numeric(PY)), by=Last]
D_all$PY_off_end <- PY_off_end$V1[match(D_all$Author_name, PY_off_end$Last)]

Off_period <- DT_pub_first[,max(as.numeric(PY))-min(as.numeric(PY))+1, by=Last]
D_all$Off_period <- Off_period$V1[match(D_all$Author_name, Off_period$Last)]
D_all$Off_period[is.na(D_all$Off_period)] <- 0

#Calculate PC with offspring
Pub_off_count <- DT_pub_first[,.N, by=Last]
D_all$PC_off <- Pub_off_count$N[match(D_all$Author_name, Pub_off_count$Last)]
D_all$PC_off[is.na(D_all$PC_off)] <- 0
D_all$PC_off_annu <- D_all$PC_off/D_all$Off_period
D_all$PC_off_annu[is.na(D_all$PC_off_annu)] <- 0

#Generate family networks using igraph library
setorder(DT_anc_off, PY_ancestor)
Net_fam <- graph_from_data_frame(d=DT_anc_off[,.(Last,First,PC_anc_off)], directed=T)
Net_fam <- simplify(Net_fam, remove.multiple=T, remove.loops=T)

#Get number of connections per node
V(Net_fam)$degree_all <- degree(Net_fam, mode = "all", loops = FALSE, normalized = FALSE)
V(Net_fam)$degree_out <- degree(Net_fam, mode = "out", loops = FALSE, normalized = FALSE)
V(Net_fam)$degree_in <- degree(Net_fam, mode = "in", loops = FALSE, normalized = FALSE)
V(Net_fam)$color <- as.character(D_all$Author_color[match(V(Net_fam)$'name', D_all$Author_name)])

#Determine chronologic index of families based on first generation ancestors
V_ancestors <- V(Net_fam)[V(Net_fam)$degree_in==0]
V(Net_fam)[V(Net_fam)$degree_in==0]$Family_index <- seq_along(V_ancestors)
V(Net_fam)[is.na(V(Net_fam)$Family_index)]$Family_index <- 0

for (i in seq_along(V_ancestors)){
Net_anc <- ego(Net_fam, order=50, nodes = V_ancestors[i], mode = "out", mindist = 0)
V(Net_fam)[unlist(Net_anc)]$Family_index <- V(Net_fam)[V_ancestors[i]]$Family_index
}

#Determine family size
V(Net_fam)$Family_size <- ego_size(Net_fam, order=50,mode="out")

#Determine generation (AG) of each last author starting from first generation ancestors
Generation_name <- unlist(V_ancestors$name)
Generation_index <- rep(1, length(Generation_name))
V_offspring <- adjacent_vertices(Net_fam, V_ancestors, mode = c("out"))
i=1
while (length(V_offspring)>0){
i <- i+1
Generation_name <- append(Generation_name, V(Net_fam)[unlist(V_offspring)]$name)
Generation_index <- append(Generation_index, rep(i, length(V(Net_fam)[unlist(V_offspring)]$name)))
V_ancestors <- V(Net_fam)[unlist(V_offspring)]
V_offspring <- adjacent_vertices(Net_fam, V_ancestors, mode = c("out"))
}

#Save genealogy-related measures
#Internal check: degree out should be the same as OC!
D_all$Family_degree_out <- V(Net_fam)$degree_out[match(D_all$Author_name, V(Net_fam)$name)]
D_all$Family_degree_out[is.na(D_all$Family_degree_out)] <- 0

D_all$Family_degree_in <- V(Net_fam)$degree_in[match(D_all$Author_name, V(Net_fam)$name)]
D_all$Family_degree_in[is.na(D_all$Family_degree_in)] <- 0

D_all$Family_index <- V(Net_fam)$Family_index[match(D_all$Author_name, V(Net_fam)$name)]
D_all$Family_index[is.na(D_all$Family_index)] <- 0

D_all$Family_size <- V(Net_fam)$Family_size[match(D_all$Author_name, V(Net_fam)$name)]
D_all$Family_size[is.na(D_all$Family_size)] <- 0

D_all$AG <- Generation_index[match(D_all$Author_name, Generation_name)]
D_all$AG[is.na(D_all$AG)] <- 0

#Determine annual rates of authors entering as offspring and their publications and add to rates database
D_off <- subset(D_all, D_all$AG>1)
Off_Pubs <- DT_pub[which(DT_pub$Last %in% D_off$Author_name),]
Off_Pubs_histo <- hist(Off_Pubs$PY, breaks=Rates_bins, plot=F, right=F)
Off_Authors_histo <- hist(D_off$PY_start, breaks=Rates_bins, plot=F, right=F)
D_rates$off_pubs_count <- Off_Pubs_histo$counts
D_rates$off_authors_count <- Off_Authors_histo$counts
D_rates$off_pubs_fract <- D_rates$off_pubs_count/D_rates$pubs_count
D_rates$off_authors_fract <- D_rates$off_authors_count/D_rates$authors_count

#Calculate time required for this part
end_time_off <- Sys.time()
Duration_off <- end_time_off - start_time_off


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#2.4. Analysis of collaborative connections between authors based on co-authorship (2nd to nth-1 of article authors)
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Start timer for this part
start_time_col <- Sys.time()

#Select only publications with more than 2 authors
DT_pub_col <- DT_pub[Author_count>2]

#Generate list of collaborating authors (excluding first and last)
DT_pub_col[,Author_col:=lapply(Authors, function(x) x[2:(length(x)-1)])]

if(Pubmed){
#Author_col <- unlist(DT_pub_col$Author_col)
Author_col <- toupper(unlist(DT_pub_col$Author_col))
}else{
#Assemble Col Author family names and initials for WOS data
Author_col_temp <- toupper(unlist(DT_pub_col$Author_col))
Author_col_famname <- sapply(strsplit(Author_col_temp, ", "), '[', 1)
#Author_col_famname <- paste(substring(Author_col_famname1,1,1),tolower(substring(Author_col_famname1,2)), sep="")
Author_col_init <-  sapply(strsplit(Author_col_temp, ", "), '[', 2)
Author_col_init <- ifelse(nchar(Author_col_init)<=2,gsub("\\.","",Author_col_init),Author_col_init)
Author_col_init <- ifelse(nchar(Author_col_init)==2& grepl("[a-z]", substring(Author_col_init,2,2)),substring(Author_col_init,1,1),Author_col_init)
Author_col_init <- ifelse(nchar(Author_col_init)>2&!grepl("\\.|\\s|\\-", Author_col_init),substring(Author_col_init,1,1),Author_col_init)
Author_col_init <- ifelse(grepl(".\\.\\s.\\.", Author_col_init),gsub("\\.|\\s", "",Author_col_init),Author_col_init)
Author_col_init <- ifelse(grepl(".\\..\\.", Author_col_init),gsub("\\.", "",Author_col_init),Author_col_init)
Author_col_init <- ifelse(nchar(Author_col_init)>2&grepl(" ", Author_col_init), paste(substring(Author_col_init,1,1),substring(Author_col_init,regexpr(" ",Author_col_init)+1,regexpr(" ",Author_col_init)+1),sep=""),Author_col_init)
Author_col_init <- ifelse(nchar(Author_col_init)>2&grepl("-", Author_col_init), paste(substring(Author_col_init,1,1),substring(Author_col_init,regexpr("-",Author_col_init)+1,regexpr("-",Author_col_init)+1),sep=""),Author_col_init)
Author_col <- paste(Author_col_famname, ifelse(is.na(Author_col_init),"", Author_col_init), sep=" ")
}

#Generate list repeating last author, year and PubID for each co-author in article (minus first and last author)
Author_col_last <- c(rep(as.character(DT_pub_col$Last), DT_pub_col$Author_count-2)) 
Author_col_year <- c(rep(DT_pub_col$PY, DT_pub_col$Author_count-2))
Author_col_Pub_ID <- c(rep(DT_pub_col$Pub_ID, DT_pub_col$Author_count-2)) 

#Create data table with last and collaborating co-author pairs per article
DT_col_temp <- data.table("Last"=Author_col_last, "Col"=Author_col, "PY"=Author_col_year, "Pub_ID"=Author_col_Pub_ID)

#Select only those collaborators that also publish as last authors
DT_col <- DT_col_temp[Col %in% Last]

#Analyse collaborative publications per author

#Collaborations "inward", i.e. last authors are listed as co-authors
#Calculate publication start, end and period
PY_col_in_start <- DT_col[,min(as.numeric(PY)), by=Col]
D_all$PY_col_in_start <- PY_col_in_start$V1[match(D_all$Author_name, PY_col_in_start$Col)]

PY_col_in_end <- DT_col[,max(as.numeric(PY)), by=Col]
D_all$PY_col_in_end <- PY_col_in_end$V1[match(D_all$Author_name, PY_col_in_end$Col)]

Col_in_period <- DT_col[,max(as.numeric(PY))-min(as.numeric(PY))+1, by=Col]

D_all$Col_in_period <- Col_in_period$V1[match(D_all$Author_name, Col_in_period$Col)]
D_all$Col_in_period <- ifelse(is.na(D_all$Col_in_period),0,D_all$Col_in_period)

D_PC_col_in <- DT_col[, .N, by=Col]
D_all$PC_col_in <- D_PC_col_in$N[match(D_all$Author_name, D_PC_col_in$Col)]
D_all$PC_col_in[is.na(D_all$PC_col_in)] <- 0

D_all$PC_col_in_annu <- D_all$PC_col_in/D_all$Col_in_period
D_all$PC_col_in_annu[is.na(D_all$PC_col_in_annu)] <- 0

D_all$R_PC_col_in_PC <- D_all$PC_col_in/D_all$PC
D_all$R_PC_col_in_PC_annu <- D_all$PC_col_in_annu/D_all$PC_annu

#Collaborations "outward", i.e. last authors are listed as last authors and collaborating authors are listed as co-author
#Calculate publication start, end and period
PY_col_out_start <- DT_col[,min(as.numeric(PY)), by=Last]
D_all$PY_col_out_start <- PY_col_out_start$V1[match(D_all$Author_name, PY_col_out_start$Last)]

PY_col_out_end <- DT_col[,max(as.numeric(PY)), by=Last]
D_all$PY_col_out_end <- PY_col_out_end$V1[match(D_all$Author_name, PY_col_out_end$Last)]

Col_out_period <- DT_col[,max(as.numeric(PY))-min(as.numeric(PY))+1, by=Last]

D_all$Col_out_period <- Col_out_period$V1[match(D_all$Author_name, Col_out_period$Last)]
D_all$Col_out_period[is.na(D_all$Col_out_period)] <- 0

#Group DT col by last author and Pub_ID
PC_col_temp <- DT_col[,.N, by=.(Last, Pub_ID)]

#Determine number of out degree collaborative papers per last author
PC_col_out <- PC_col_temp[,.N, by=Last]
D_all$PC_col_out <- PC_col_out$N[match(D_all$Author_name, PC_col_out$Last)]
D_all$PC_col_out[is.na(D_all$PC_col_out)] <- 0
D_all$PC_col_out_annu <- D_all$PC_col_out/D_all$Col_out_period
D_all$PC_col_out_annu[is.na(D_all$PC_col_out_annu)] <- 0

D_all$R_PC_col_out_PC_annu <- D_all$PC_col_out_annu/D_all$PC_annu
D_all$R_PC_col_out_PC <- D_all$PC_col_out/D_all$PC

D_all$PC_col_tot <- D_all$PC_col_out+D_all$PC_col_in
D_all$PC_col_tot[is.na(D_all$PC_col_tot)] <- 0

#Generate data table to establish collaborator network
DT_col_net <- DT_col[ , .N, by = .(Last, Col)]
DT_col_net$L_AI <- D_all$Author_index[match(DT_col_net$Last, D_all$Author_name)]
DT_col_net$L_TC <- D_all$Author_color[match(DT_col_net$Last, D_all$Author_name)]
DT_col_net$L_PY_start <- D_all$PY_start[match(DT_col_net$Last, D_all$Author_name)]
DT_col_net$C_AI <- D_all$Author_index[match(DT_col_net$Col, D_all$Author_name)]
DT_col_net$C_TC <- D_all$Author_color[match(DT_col_net$Col, D_all$Author_name)]
DT_col_net$C_PY_start <- D_all$PY_start[match(DT_col_net$Col, D_all$Author_name)]

#Generate collaborator networks using igraph library and assign vertex/node properties
Net_col <- graph.data.frame(d=DT_col_net[,c(1,2,3)], directed=TRUE, v=D_all$Author_name)
V(Net_col)$degree_all <- degree(Net_col, mode = "all", loops = FALSE, normalized = FALSE)
V(Net_col)$degree_out <- degree(Net_col, mode = "out", loops = FALSE, normalized = FALSE)
V(Net_col)$color <- as.character(D_all$Author_color)
V(Net_col)$Family_size <- D_all$Family_size
V(Net_col)$PC <- D_all$PC

#Assign colors to edges from outgoing vertex/node
E(Net_col)$color <- as.character(D_all$Author_color[match(tail_of(Net_col,E(Net_col))[]$name, D_all$Author_name)])

#Save network data
D_all$degree_all <- degree(Net_col, mode = "all", loops = FALSE, normalized = FALSE)
D_all$degree_out <- degree(Net_col, mode = "out", loops = FALSE, normalized = FALSE)
D_all$degree_in <- degree(Net_col, mode = "in", loops = FALSE, normalized = FALSE)
D_all$R_deg_in_out <- D_all$degree_in/D_all$degree_out

#Select data with authors having collaborative connections
D_CC <- D_all[which(D_all$degree_all > 0),]

#Determine annual rates of collaborative papers and authors and store in rates database
Col_Pubs <- DT_pub[Last %in% D_CC$Author_name]
Col_Pubs_histo <- hist(Col_Pubs$PY, breaks=Rates_bins, plot=F, right=F)
Col_Authors_histo <- hist(D_CC$PY_start, breaks=Rates_bins, plot=F, right=F)

D_rates$col_pubs_count <- Col_Pubs_histo$counts
D_rates$col_authors_count <- Col_Authors_histo$counts
D_rates$col_pubs_fract <- D_rates$col_pubs_count/D_rates$pubs_count
D_rates$col_authors_fract <- D_rates$col_authors_count/D_rates$authors_count

#Calculate time required for this part
end_time_col <- Sys.time()
Duration_col <- end_time_col-start_time_col


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#2.5. Calculation of TeamTree product (TTP), a citation-independent measure of author performance, based on the product of PC, OC and CC values.
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Start timer for this part
start_time_ttp <- Sys.time()

#TTP with zero values counted
D_all$PCxOCxCC <- D_all$PC * D_all$OC * D_all$degree_all
D_all$PCxOCxCC_rel_max <- D_all$PCxOCxCC/max(D_all$PCxOCxCC)

#TTP with zero values adjusted (inclusive) and check whether sum of PC+OC+CC is larger than product PCxOCxCC
#nozero_PCxOCxCC <- D_all$PC * ifelse(D_all$OC > 0, D_all$OC, 1) * ifelse(D_all$degree_all > 0, D_all$degree_all, 1)
#PCplusOCplusCC <- D_all$PC + D_all$OC + D_all$degree_all
#D_all$iPCxOCxCC <- ifelse(PCplusOCplusCC >= nozero_PCxOCxCC, PCplusOCplusCC, nozero_PCxOCxCC)

#TTP with zero values adjusted (inclusive)
D_all$iPCxOCxCC <- D_all$PC * ifelse(D_all$OC > 0, D_all$OC, 1) * ifelse(D_all$degree_all > 0, D_all$degree_all, 1)
D_all$iPCxOCxCC_rel_max <- D_all$iPCxOCxCC/max(D_all$iPCxOCxCC)

#Get subset of TeamTree database with authors having TTP greater  than 0.
D_TTP <- D_all[which(D_all$PCxOCxCC > 0),]
D_TTP$TTP_log <- log10(D_TTP$PCxOCxCC)

#Get subset of TeamTree database with authors having iTTP greater  than 1.
D_iTTP <- D_all[which(D_all$iPCxOCxCC > 1),]
D_iTTP$iTTP_log <- log10(D_iTTP$iPCxOCxCC)

#Determine annual rates of pubs/authors with TTP values >0 and save in Rates database
TTP_Pubs <- DT_pub[Last %in% D_TTP$Author_name]
TTP_Pubs_histo <- hist(TTP_Pubs$PY, breaks=Rates_bins, plot=F, right=F)
TTP_Authors_histo <- hist(D_TTP$PY_start, breaks=Rates_bins, plot=F, right=F)

D_rates$TTP_pubs_count <- TTP_Pubs_histo$counts
D_rates$TTP_authors_count <- TTP_Authors_histo$counts
D_rates$TTP_pubs_fract <- D_rates$TTP_pubs_count/D_rates$pubs_count
D_rates$TTP_authors_fract <- D_rates$TTP_authors_count/D_rates$authors_count

#Determine annual rates of pubs/authors with iTTP values >1 and save in Rates database
iTTP_Pubs <- DT_pub[Last %in% D_iTTP$Author_name]
iTTP_Pubs_histo <- hist(iTTP_Pubs$PY, breaks=Rates_bins, plot=F, right=F)
iTTP_Authors_histo <- hist(D_iTTP$PY_start, breaks=Rates_bins, plot=F, right=F)

D_rates$iTTP_pubs_count <- iTTP_Pubs_histo$counts
D_rates$iTTP_authors_count <- iTTP_Authors_histo$counts
D_rates$iTTP_pubs_fract <- D_rates$iTTP_pubs_count/D_rates$pubs_count
D_rates$iTTP_authors_fract <- D_rates$iTTP_authors_count/D_rates$authors_count

#Calculate time required for this part
end_time_ttp <- Sys.time()
Duration_ttp <- end_time_ttp - start_time_ttp

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Calculation of total count of citations and H index for authors (only WOS data)
if(Pubmed){} else{
DT_pub_cit <- data.table("PY"=Publications_sel$PY, "Authors"=Author_all, "Last"=Author_last, "Author_count"=Author_count, "TC"=Publications_sel$TC, "Pub_ID"=Publications_sel$Pub_ID)

#Generate list of authors
DT_pub_cit[,Author_cit:=lapply(Authors, function(x) x[1:length(x)])]

Author_cit_temp <- toupper(unlist(DT_pub_cit$Author_cit))

#Adjust author names for WOS data
Author_cit_famname <- sapply(strsplit(Author_cit_temp, ", "), '[', 1)
#Author_cit_famname <- paste(substring(Author_cit_famname1,1,1),tolower(substring(Author_cit_famname1,2)), sep="")
Author_cit_init <-  sapply(strsplit(Author_cit_temp, ", "), '[', 2)
Author_cit_init <- ifelse(nchar(Author_cit_init)<=2,gsub("\\.","",Author_cit_init),Author_cit_init)
Author_cit_init <- ifelse(nchar(Author_cit_init)==2& grepl("[a-z]", substring(Author_cit_init,2,2)),substring(Author_cit_init,1,1),Author_cit_init)
Author_cit_init <- ifelse(nchar(Author_cit_init)>2&!grepl("\\.|\\s|\\-", Author_cit_init),substring(Author_cit_init,1,1),Author_cit_init)
Author_cit_init <- ifelse(grepl(".\\.\\s.\\.", Author_cit_init),gsub("\\.|\\s", "",Author_cit_init),Author_cit_init)
Author_cit_init <- ifelse(grepl(".\\..\\.", Author_cit_init),gsub("\\.", "",Author_cit_init),Author_cit_init)
Author_cit_init <- ifelse(nchar(Author_cit_init)>2&grepl(" ", Author_cit_init), paste(substring(Author_cit_init,1,1),substring(Author_cit_init,regexpr(" ",Author_cit_init)+1,regexpr(" ",Author_cit_init)+1),sep=""),Author_cit_init)
Author_cit_init <- ifelse(nchar(Author_cit_init)>2&grepl("-", Author_cit_init), paste(substring(Author_cit_init,1,1),substring(Author_cit_init,regexpr("-",Author_cit_init)+1,regexpr("-",Author_cit_init)+1),sep=""),Author_cit_init)
Author_cit <- paste(Author_cit_famname, ifelse(is.na(Author_cit_init),"", Author_cit_init), sep=" ")

#Generate vectors repeating last author, year and PubID for each author in article
Author_cit_last <- c(rep(as.character(DT_pub_cit$Last), DT_pub_cit$Author_count)) 
Author_cit_year <- c(rep(DT_pub_cit$PY, DT_pub_cit$Author_count))
Author_cit_Pub_ID <- c(rep(DT_pub_cit$Pub_ID, DT_pub_cit$Author_count)) 
Author_cit_TC <- c(rep(DT_pub_cit$TC, DT_pub_cit$Author_count)) 

#Create data table with last author and all authors per article
DT_cit_temp <- data.table("Last"=Author_cit_last, "AU"=Author_cit, "PY"=Author_cit_year, "TC"=Author_cit_TC, "Pub_ID"=Author_cit_Pub_ID)

#Restrict to entries with last authors
DT_cit <- DT_cit_temp[AU %in% Last]

PC_tot <- DT_cit[ ,length(Pub_ID), by =AU]
Cit_sum <- DT_cit[ ,sum(TC), by =AU]
H_ind <- DT_cit[TC>0,tail(which(1:length(TC) <=sort(TC,decreasing=T)),1), by=AU]

D_all$PC_tot <- PC_tot$V1[match(D_all$Author_name, PC_tot$AU)]
D_all$Cit_sum <- Cit_sum$V1[match(D_all$Author_name, Cit_sum$AU)]
D_all$Cit_sum_rel_max <- log10(D_all$Cit_sum)/max(log10(D_all$Cit_sum), na.rm=T)
D_all$H_ind <- H_ind$V1[match(D_all$Author_name, H_ind$AU)]
D_all$H_ind_rel_max <- D_all$H_ind/max(D_all$H_ind, na.rm=T)
}

#save TeamTree database, no new entries from here on
write.csv(D_all, file=File_TT_data, row.names=FALSE)


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#2.6. Analysis of field-specific publication and author entry rates in different categories
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Categories include
#a) Newcomers and established authors
#b) Authors entering as offspring/collaborative, offspring only, collaborative only and the rest
#c) Authors publishing one article

start_time_dyn <- Sys.time()

#Determine publication rates of newcomers and save to database
Pub_new <- DT_pub[DT_pub[,.I[PY==min(PY)], keyby=Last]$V1]
Pub_new_off_col <- Pub_new[Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Pub_new_col <- Pub_new[Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]
Pub_new_off <- Pub_new[!Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Pub_new_rest <- Pub_new[!Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]

Pub_rates_new <- hist(Pub_new$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_new_off_col <- hist(Pub_new_off_col$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_new_col <- hist(Pub_new_col$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_new_off <- hist(Pub_new_off$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_new_rest <- hist(Pub_new_rest$PY, breaks=Rates_bins, plot=F, right=F)

D_rates$Pub_rates_new_count <- Pub_rates_new$counts
D_rates$Pub_rates_new_off_col_count <- Pub_rates_new_off_col$counts
D_rates$Pub_rates_new_col_count <- Pub_rates_new_col$counts
D_rates$Pub_rates_new_off_count <- Pub_rates_new_off$counts
D_rates$Pub_rates_new_rest_count <- Pub_rates_new_rest$counts

#Determine number of authors entering the field per year and save to database
Author_new <- Pub_new[,.N, keyby=.(Last,PY)]
Author_new_off_col <- Author_new[Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Author_new_col <- Author_new[Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]
Author_new_off <- Author_new[!Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Author_new_rest <- Author_new[!Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]

Author_rates_new <- hist(Author_new$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_new_off_col <- hist(Author_new_off_col$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_new_col <- hist(Author_new_col$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_new_off <- hist(Author_new_off$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_new_rest <- hist(Author_new_rest$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_single <- hist(D_all[which(D_all$PC==1),]$PY_start, breaks=Rates_bins, plot=F, right=F)

D_rates$Author_rates_new_count <- Author_rates_new$counts
D_rates$Author_rates_new_off_col_count <- Author_rates_new_off_col$counts
D_rates$Author_rates_new_col_count <- Author_rates_new_col$counts
D_rates$Author_rates_new_off_count <- Author_rates_new_off$counts
D_rates$Author_rates_new_rest_count <- Author_rates_new_rest$counts
D_rates$Author_rates_single <- Author_rates_single$counts

#Determine rates of entering authors with more than one publication and save to database
Author_new_mult <-  Author_new[which(!Last %in% D_all[which(D_all$PC==1),]$Author_name),]
Author_new_mult_off_col <- Author_new_mult[Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Author_new_mult_col <- Author_new_mult[Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]
Author_new_mult_off <- Author_new_mult[!Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Author_new_mult_rest <- Author_new_mult[!Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]

Author_rates_mult_new <- hist(Author_new_mult$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_mult_new_off_col <- hist(Author_new_mult_off_col$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_mult_new_col <- hist(Author_new_mult_col$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_mult_new_off <- hist(Author_new_mult_off$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_mult_new_rest <- hist(Author_new_mult_rest$PY, breaks=Rates_bins, plot=F, right=F)

D_rates$Author_rates_mult_new_count <- Author_rates_mult_new$counts
D_rates$Author_rates_mult_new_off_col_count <- Author_rates_mult_new_off_col$counts
D_rates$Author_rates_mult_new_col_count <- Author_rates_mult_new_col$counts
D_rates$Author_rates_mult_new_off_count <- Author_rates_mult_new_off$counts
D_rates$Author_rates_mult_new_rest_count <- Author_rates_mult_new_rest$counts

#Analyse publications of authors leaving the field
Pub_end <- DT_pub[DT_pub[,.I[PY==max(PY)], keyby=Last]$V1]
Pub_end_off_col <- Pub_end[Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Pub_end_col <- Pub_end[Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]
Pub_end_off <- Pub_end[!Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Pub_end_rest <- Pub_end[!Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]

Pub_rates_end <- hist(Pub_end$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_end_off_col <- hist(Pub_end_off_col$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_end_col <- hist(Pub_end_col$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_end_off <- hist(Pub_end_off$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_end_rest <- hist(Pub_end_rest$PY, breaks=Rates_bins, plot=F, right=F)

D_rates$Pub_rates_end_count <- Pub_rates_end$counts
D_rates$Pub_rates_end_off_col_count <- Pub_rates_end_off_col$counts
D_rates$Pub_rates_end_col_count <- Pub_rates_end_col$counts
D_rates$Pub_rates_end_off_count <- Pub_rates_end_off$counts
D_rates$Pub_rates_end_rest_count <- Pub_rates_end_rest$counts

#Analyse authors leaving the field
Author_end <- Pub_end[,.N, keyby=.(Last,PY)]
Author_end_off_col <- Author_end[Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Author_end_col <- Author_end[Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]
Author_end_off <- Author_end[!Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Author_end_rest <- Author_end[!Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]

Author_rates_end <- hist(Author_end$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_end_off_col <- hist(Author_end_off_col$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_end_col <- hist(Author_end_col$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_end_off <- hist(Author_end_off$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_end_rest <- hist(Author_end_rest$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_single <- hist(D_all[which(D_all$PC==1),]$PY_start, breaks=Rates_bins, plot=F, right=F)

#Test validity
Author_rates_end_chk <- hist(D_all$PY_end, breaks=Rates_bins, plot=F, right=F)
D_rates$Author_rates_end_count_chk <- Author_rates_end_chk$counts

#Save to database
D_rates$Author_rates_end_count <- Author_rates_end$counts
D_rates$Author_rates_end_off_col_count <- Author_rates_end_off_col$counts
D_rates$Author_rates_end_col_count <- Author_rates_end_col$counts
D_rates$Author_rates_end_off_count <- Author_rates_end_off$counts
D_rates$Author_rates_end_rest_count <- Author_rates_end_rest$counts

#Determine publication rates from established (old) authors already present in the field
Pub_old <- DT_pub[DT_pub[,.I[PY>min(PY)], keyby=Last]$V1]
Pub_old_off_col <- Pub_old[Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Pub_old_col <- Pub_old[Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]
Pub_old_off <- Pub_old[!Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Pub_old_rest <- Pub_old[!Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]

#Determine rates per year
Pub_rates_old <- hist(Pub_old$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_old_off_col <- hist(Pub_old_off_col$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_old_col <- hist(Pub_old_col$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_old_off <- hist(Pub_old_off$PY, breaks=Rates_bins, plot=F, right=F)
Pub_rates_old_rest <- hist(Pub_old_rest$PY, breaks=Rates_bins, plot=F, right=F)

#Save to database
D_rates$Pub_rates_old_count <- Pub_rates_old$counts
D_rates$Pub_rates_old_off_col_count <- Pub_rates_old_off_col$counts
D_rates$Pub_rates_old_col_count <- Pub_rates_old_col$counts
D_rates$Pub_rates_old_off_count <- Pub_rates_old_off$counts
D_rates$Pub_rates_old_rest_count <- Pub_rates_old_rest$counts

#Identify established/old authors
Author_old <- Pub_old[,.N, keyby=.(Last,PY)]
Author_old_off_col <- Author_old[Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Author_old_col <- Author_old[Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]
Author_old_off <- Author_old[!Last %in% D_CC$Author_name & Last %in% D_off$Author_name]
Author_old_rest <- Author_old[!Last %in% D_CC$Author_name & !Last %in% D_off$Author_name]

#Determine rates per year
Author_rates_old <- hist(Author_old$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_old_off_col <- hist(Author_old_off_col$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_old_col <- hist(Author_old_col$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_old_off <- hist(Author_old_off$PY, breaks=Rates_bins, plot=F, right=F)
Author_rates_old_rest <- hist(Author_old_rest$PY, breaks=Rates_bins, plot=F, right=F)

#Save to database
D_rates$Author_rates_old_count <- Author_rates_old$counts
D_rates$Author_rates_old_off_col_count <- Author_rates_old_off_col$counts
D_rates$Author_rates_old_col_count <- Author_rates_old_col$counts
D_rates$Author_rates_old_off_count <- Author_rates_old_off$counts
D_rates$Author_rates_old_rest_count <- Author_rates_old_rest$counts

#Save rates to file, no new entries from here on, currently disabled
write.csv(D_rates, file=File_TT_rates, row.names=F)

end_time_dyn <- Sys.time()
Duration_dyn <- end_time_dyn - start_time_dyn

end_time_tot <- Sys.time()
Duration_tot <- end_time_tot - start_time_tot

#Show processing time for different subroutines
Duration <- data.frame(Prep=Duration_prep,
Pub=Duration_pub,
Off=Duration_off,
Col=Duration_col,
TTP=Duration_ttp,
Dyn=Duration_dyn,
Tot=Duration_tot)
Duration


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3. VISUALIZATION
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#The following code produces graphs shown in the preprint (Pfrieger, 2021 https://doi.org/10.1101/2020.06.01.128355) and additional visuals not included in the manuscript.

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3.1. Parameters for visual display
#----------------------------------------------------------------------------------------------------------------------------------------------------------

Alpha_all = 0.5
Label_size = 16
Names_size = 12
Line_width = 2.5
Legend_position_1 = c(0.8,0.3)
Legend_position_2 = c(0.5,0.9)
Legend_position_3 = c(0.9,0.8)
Legend_position_4 = c(0.4,0.3)
Legend_position_5 = c(0.5,0.2)

Legend_text_size = 40
Tick_length = 0.6
Tick_years <- 10
Tick_AI <- 1000
Tick_rate <- 500
Tick_PC <- 20
Tick_OC <- 10
Tick_PC_off <- 20
Tick_AG <- 2
Tick_FS <- 10
Tick_AC <- 2
Tick_CC <- 20
Tick_PC_col <- 20
Tick_pub_period <- 20

AI_max <- max(D_all$Author_index)
AI_min <- min(D_all$Author_index)
PY_min <- min(D_all$PY_start)-1
PY_max <- max(DT_pub$PY)+1

#Aplysia
#AI_min=-1850
#AI_max=1850
#PY_min <- min(D_all$PY_start)-1
#PY_max <- max(DT_pub$PY)+1

#CRISPR
#AI_min <- -11500
#AI_max <- 11500
#PY_min <- 1999
#PY_max <- 2022

#Organoids
#AI_min <- -10750
#AI_max <- 10750
#PY_min <- 1939
#PY_max <- 2022

Plot_height_PY <- (PY_max-PY_min)*4
Plot_width_AI <- AI_max/23

Plot_div_height <- 60
Plot_histo_width <- 80
Plot_names_width <- 300
Plot_names_height <- 150


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3.2. Generation of TeamTree graphs (TTGs), a new type of visual, that provides an ad-hoc view on different properties of the workforce.
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Generate data table with publication years and publication count per year for each last author/author
D_chrono <- DT_pub[ , .N, by = .(Last, PY)]
D_chrono$Author_index <- D_all$Author_index[match(D_chrono$Last, D_all$Author_name)]
D_chrono$Author_color <- D_all$Author_color[match(D_chrono$Last, D_all$Author_name)]
setnames(D_chrono, "N", "PC_annual")

#Plot parameters
P_pub_size_min <- min(D_chrono$PC_annual)
P_pub_size_max <- max(D_chrono$PC_annual)

#Get authors with Top_n PC values
D_PC_sorted <- D_all[order(-D_all$PC),]
D_PC_sorted$PC_rank <- seq_along(D_PC_sorted$PC)
D_PC_sorted$legend <- paste(D_PC_sorted$PC_rank,D_PC_sorted$Author_name,sep=" ")
D_PC_top <- D_PC_sorted[1:Top_n,]
D_PC_top_minus <- subset(D_PC_top, D_PC_top$Author_index <0)
D_PC_top_plus <- subset(D_PC_top, D_PC_top$Author_index >0)

#Bubbleplot PC size
D_PC_top$PC_norm <- round(D_PC_top$PC/max(D_PC_top$PC)*100,1)
P_PC_top_size_norm_min <- min(D_PC_top$PC_norm)
P_PC_top_size_norm_max <- max(D_PC_top$PC_norm)
P_PC_top_size_min <- min(D_PC_top$PC)
P_PC_top_size_max <- max(D_PC_top$PC)

#Plotfiles
File_P_pub <- sprintf("%s_P_pub.tiff", Topic)
File_P_pub_nopr <- sprintf("%s_P_pub_nopr.tiff", Topic)
File_P_pub_top <- sprintf("%s_P_pub_top.tiff", Topic)
File_P_PC_names <- sprintf("%s_P_PC_names.tiff", Topic)
File_P_pub_rates <- sprintf("%s_P_pub_rates.tiff", Topic)
File_P_PC <- sprintf("%s_P_PC.tiff", Topic)
File_P_PC_histo <- sprintf("%s_P_PC_histo.tiff", Topic)

#Visualize publication records of authors: The TeamTree visual, version for CLOCK field
P_pub <- ggplot()
P_pub <- P_pub + geom_line(data = D_chrono, aes(x = Author_index, y = PY, group = Author_index), color = "gray75", size = 0.5)
P_pub <- P_pub + geom_point(data = D_chrono, aes(x = Author_index, y = PY, group = Author_index, size = PC_annual, color=D_chrono$Author_color), shape = 16, alpha = Alpha_all)
P_pub <- P_pub + scale_size_continuous(range = c(P_pub_size_min/1.5, P_pub_size_max/1.5), breaks = c(P_pub_size_min, P_pub_size_max))
#P_pub <- P_pub + geom_text_repel(data = D_PC_top_minus, aes(x = Author_index, y = PY_start, label = PC_rank, color = Author_color), nudge_x = Tick_AI*-0.5, nudge_y = -3, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
#P_pub <- P_pub + geom_text_repel(data = D_PC_top_plus, aes(x = Author_index, y = PY_start, label = PC_rank, color = Author_color), nudge_x = Tick_AI*0.5, nudge_y = -3, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
P_pub <- P_pub + scale_fill_identity()
P_pub <- P_pub + scale_colour_identity()
P_pub <- P_pub + theme_bw()
P_pub <- P_pub + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_pub <- P_pub + theme(legend.text = element_text(size = Legend_text_size), legend.position = Legend_position_1, legend.title = element_blank(), legend.background=element_rect(fill=NA), legend.key=element_blank()) 
P_pub <- P_pub + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_pub <- P_pub + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_pub <- P_pub + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_pub <- P_pub + scale_y_continuous(limits = c(PY_min,PY_max), breaks = seq(1700, 2040, by = Tick_years))
if(show_plots){P_pub}
if(save_plots){ggsave(File_P_pub, path = User_dir_save, height = Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}

#Visualize top PC authors with TTG for CLOCK field
P_pub_top <- ggplot()
P_pub_top <- P_pub_top + geom_line(data = D_all, aes(x = Author_index, y = PY_start), size=0.5, color="gray75")
P_pub_top <- P_pub_top + geom_point(data = D_all, aes(x = Author_index, y = PY_start, color = Author_color), size=2.0, shape = 16)
P_pub_top <- P_pub_top + geom_point(data = D_PC_top, aes(x = Author_index, y = PY_start, color = Author_color, size = PC), shape = 16, alpha=0.6)
P_pub_top <- P_pub_top + scale_size(range = c(P_PC_top_size_min/3, P_PC_top_size_max/3), breaks = c(P_PC_top_size_min, P_PC_top_size_max))
P_pub_top <- P_pub_top + geom_text_repel(data = D_PC_top_minus, aes(x = Author_index, y = PY_start, label = PC_rank, color = Author_color), nudge_x = Tick_AI*-1.5, nudge_y = 0, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
P_pub_top <- P_pub_top + geom_text_repel(data = D_PC_top_plus, aes(x = Author_index, y = PY_start, label = PC_rank, color = Author_color), nudge_x = Tick_AI*2, nudge_y = -1, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
P_pub_top <- P_pub_top + scale_fill_identity()
P_pub_top <- P_pub_top + scale_colour_identity()
P_pub_top <- P_pub_top + theme_bw()
P_pub_top <- P_pub_top + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_pub_top <- P_pub_top + theme(legend.text = element_text(size = Legend_text_size), legend.position = c(0.5,0.85), legend.title = element_blank(), legend.background=element_rect(fill=NA), legend.key=element_blank())
P_pub_top <- P_pub_top + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_pub_top <- P_pub_top + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_pub_top <- P_pub_top + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_pub_top <- P_pub_top + scale_y_continuous(limits = c(PY_min,PY_max), breaks = seq(1700, 2040, by = Tick_years))
if(show_plots){P_pub_top}
if(save_plots){ggsave(File_P_pub_top, path = User_dir_save, height = Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}


#Visualize TeamTree graph without publication record
P_pub <- ggplot()
P_pub <- P_pub + geom_line(data = D_all, aes(x = Author_index, y = PY_start), color= "gray75", size=0.5)
P_pub <- P_pub + geom_point(data = D_all, aes(x = Author_index, y = PY_start, group = Author_index, color=Author_color), shape = 16, size=4)
P_pub <- P_pub + scale_fill_identity()
P_pub <- P_pub + scale_colour_identity()
P_pub <- P_pub + theme_bw()
P_pub <- P_pub + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_pub <- P_pub + theme(legend.text = element_text(size = Legend_text_size), legend.position = Legend_position_1, legend.title = element_blank(), legend.background=element_rect(fill=NA), legend.key=element_blank()) 
P_pub <- P_pub + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_pub <- P_pub + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_pub <- P_pub + coord_cartesian(expand=FALSE)
P_pub <- P_pub + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_pub <- P_pub + scale_y_continuous(limits = c(PY_min,PY_max), breaks = seq(1700, 2040, by = Tick_years))
if(show_plots){P_pub}
if(save_plots){ggsave(File_P_pub_nopr, path = User_dir_save, height = Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}


#Visualize publication records of authors with Top_n PC values

P_pub_top <- ggplot()
P_pub_top <- P_pub_top + geom_line(data = D_chrono, aes(x = Author_index, y = PY, group = Author_index), color = "gray75", size = 0.5)
P_pub_top <- P_pub_top + geom_point(data = D_chrono, aes(x = Author_index, y = PY, group = Author_index), size=2, color = "gray75", shape = 16, alpha = Alpha_all)
P_pub_top <- P_pub_top + geom_line(data = D_all, aes(x = Author_index, y = PY_start), size=0.5, color="gray75")
P_pub_top <- P_pub_top + geom_point(data = D_all, aes(x = Author_index, y = PY_start, color = Author_color), size=2.0, shape = 16)
P_pub_top <- P_pub_top + geom_point(data = D_PC_top, aes(x = Author_index, y = PY_start, color = Author_color, size = PC), shape = 16, alpha=Alpha_all)

#Organoids
#P_pub_top <- P_pub_top + scale_size(range = c(P_PC_top_size_min/2, P_PC_top_size_max/2), breaks = c(P_PC_top_size_min, P_PC_top_size_max))

#CRISPR
P_pub_top <- P_pub_top + scale_size(range = c(P_PC_top_size_min/4, P_PC_top_size_max/4), breaks = c(P_PC_top_size_min, P_PC_top_size_max))

#Aplysia
#P_pub_top <- P_pub_top + scale_size(range = c(P_PC_top_size_min/5, P_PC_top_size_max/5), breaks = c(P_PC_top_size_min, P_PC_top_size_max))

#P_pub_top <- P_pub_top + scale_size_continuous(range = c(P_pub_size_min/1.5, P_pub_size_max/1.5), breaks = c(P_pub_size_min, P_pub_size_max))
#P_pub_top <- P_pub_top + scale_size_continuous(range = c(P_pub_size_min, P_pub_size_max), breaks = c(P_pub_size_min, P_pub_size_max))

#P_pub_top <- P_pub_top + geom_text_repel(data = D_PC_top_minus, aes(x = Author_index, y = PY_start, label = PC_rank, color = Author_color), nudge_x = Tick_AI*-1, nudge_y = -3, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)

#CRISPR
P_pub_top <- P_pub_top + geom_text_repel(data = D_PC_top_minus, aes(x = Author_index, y = PY_start, label = PC_rank, color = Author_color), nudge_x = Tick_AI*-2, nudge_y = -3, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
#CRISPR
P_pub_top <- P_pub_top + geom_text_repel(data = D_PC_top_plus, aes(x = Author_index, y = PY_start, label = PC_rank, color = Author_color), nudge_x = Tick_AI, nudge_y = -3, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)

#Organoids
#P_pub_top <- P_pub_top + geom_text_repel(data = D_PC_top_plus, aes(x = Author_index, y = PY_start, label = PC_rank, color = Author_color), nudge_x = 2*Tick_AI, nudge_y = -0, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)

#Aplysia
#P_pub_top <- P_pub_top + geom_text_repel(data = D_PC_top_plus, aes(x = Author_index, y = PY_start, label = PC_rank, color = Author_color), nudge_x = 2*Tick_AI, nudge_y = 0, hjust = 1, vjust = 0, size = Label_size, inherit.aes=FALSE)

P_pub_top <- P_pub_top + scale_fill_identity()
P_pub_top <- P_pub_top + scale_colour_identity()
P_pub_top <- P_pub_top + theme_bw()
P_pub_top <- P_pub_top + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))

#Organoids, CRISPR
P_pub_top <- P_pub_top + theme(legend.text = element_text(size = Legend_text_size), legend.position = Legend_position_1, legend.title = element_blank(), legend.background=element_rect(fill=NA), legend.key=element_blank())

#Aplysia
#P_pub_top <- P_pub_top + theme(legend.text = element_text(size = Legend_text_size), legend.position = Legend_position_4, legend.title = element_blank(), legend.background=element_rect(fill=NA), legend.key=element_blank())

P_pub_top <- P_pub_top + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_pub_top <- P_pub_top + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
#P_pub_top <- P_pub_top + coord_cartesian(expand=FALSE)
P_pub_top <- P_pub_top + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_pub_top <- P_pub_top + scale_y_continuous(limits = c(PY_min,PY_max), breaks = seq(1700, 2040, by = Tick_years))
if(show_plots){P_pub_top}
if(save_plots){ggsave(File_P_pub_top, path = User_dir_save, height = Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}

#Plot names of authors with Top_n publication counts
P_PC_names <- ggplot(D_PC_top)
P_PC_names  <- P_PC_names  + geom_text(aes(x=rep(0, times=10), y=rev(PC_rank), label=legend), , color="black", family="Arial", size=Names_size, parse=F, hjust=0, nudge_x=-15)
P_PC_names  <- P_PC_names  + theme_bw()
P_PC_names  <- P_PC_names  + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(), panel.background = element_blank(), plot.background = element_blank())
P_PC_names  <- P_PC_names  + theme(legend.text = element_blank(), legend.title = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank())
if(show_plots){P_PC_names}
if(save_plots){ggsave(File_P_PC_names, path = User_dir_save, height = Plot_names_height, width = Plot_names_width, units = "mm", dpi=300)}

#Visualize annual rates of publications/new authors
P_pub_rates <- ggplot()
P_pub_rates <- P_pub_rates + geom_col(data=D_rates[-nrow(D_rates),], aes(x=PY_bins, y = pubs_count), size=1, color=NA, fill="black")
P_pub_rates <- P_pub_rates + geom_col(data=D_rates[-nrow(D_rates),], aes(x=PY_bins, y = authors_count), size=1.5, color=NA, fill="orange")
P_pub_rates <- P_pub_rates + theme_bw()
P_pub_rates <- P_pub_rates + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_pub_rates <- P_pub_rates + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_pub_rates <- P_pub_rates + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_pub_rates <- P_pub_rates + coord_flip()
P_pub_rates <- P_pub_rates + scale_x_continuous(limits = c(PY_min,PY_max), breaks = seq(1700, 2040, by = Tick_years))
P_pub_rates <- P_pub_rates + scale_y_continuous(breaks = seq(0, max(D_rates$pubs_count), by=Tick_rate))
if(show_plots){P_pub_rates}
if(save_plots){ggsave(File_P_pub_rates, path = User_dir_save, height = Plot_height_PY, width = Plot_histo_width, units = "mm", dpi=300)}

#Plot publication counts (PC) as last and as first author for each author for authors with more than 1 article in each category.
D_PC <- D_all[which(D_all$PC > 1),]
D_PC$PC_annu_rd <- round(D_PC$PC_annu,2)

D_PC_first <- D_PC[which(D_PC$PC_first > 1),]
D_PC_first$PC_first_annu_rd <- round(D_PC_first$PC_first_annu,2)

#Plot PC per last author/author
#Bubble size
P_PC_annu_size_min <- min(c(D_PC$PC_annu_rd,D_PC_first$PC_first_annu_rd))
P_PC_annu_size_max <- max(c(D_PC$PC_annu_rd,D_PC_first$PC_first_annu_rd))

#Axis limits
P_PC_max <- max(D_all$PC)
P_PC_min <- max(D_all$PC_first)

P_PC <- ggplot()
P_PC <- P_PC + geom_segment(data=D_PC, aes(x = Author_index, xend = Author_index, y = 0, yend = PC, color = Author_color), size = 1)
P_PC <- P_PC + geom_segment(data = D_PC_first, aes(x = Author_index, xend = Author_index, y = PC, yend = PC_first*-1, color = Author_color), size = 1)
P_PC <- P_PC + geom_point(data=D_PC, aes(x = Author_index, y = PC, size = PC_annu_rd, color = Author_color), shape = 16)
P_PC <- P_PC + geom_point(data=D_PC_first, aes(x = Author_index, y = PC_first*-1, size = PC_first_annu_rd, color = Author_color), shape = 16)
P_PC <- P_PC + scale_size_area(limits = c(P_PC_annu_size_min, P_PC_annu_size_max), breaks = c(P_PC_annu_size_min, P_PC_annu_size_max))
P_PC <- P_PC + geom_hline(yintercept=0, color="black", size=1)
P_PC <- P_PC + scale_fill_identity()
P_PC <- P_PC + scale_color_identity()
P_PC <- P_PC + theme_bw()
P_PC <- P_PC + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_PC <- P_PC + theme(legend.text = element_text(size = Legend_text_size), legend.background = element_blank(), legend.position =  Legend_position_3, legend.title = element_blank())
P_PC <- P_PC + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_PC <- P_PC + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_PC <- P_PC + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_PC <- P_PC + scale_y_continuous(limits = c((P_PC_min+1)*-1,P_PC_max+1), breaks = seq(-500,500, by = Tick_PC))
if(show_plots){P_PC}
if(save_plots){ggsave(File_P_PC, path = User_dir_save, height = Plot_div_height*2, width = Plot_width_AI, units = "mm", dpi=300)}

#Calculate histograms of publication counts as last and as first author
Hist_PC <- hist(D_all$PC, breaks=seq(0, max(D_all$PC), by=1), include.lowest=T, plot=F)
D_hist_PC <- data.frame(x=ceiling(Hist_PC$mid), count=Hist_PC$count, density=Hist_PC$density)
P_hist_PC <- D_hist_PC[which(D_hist_PC$density >0),]

#Calculate histograms of publication counts as last and as first author minus last two years!
Hist_PC_2yrs <- hist(D_all[which(D_all$PY_start <= 2019),]$PC, breaks=seq(0, max(D_all$PC), by=1), include.lowest=T, plot=F)
D_hist_PC_2yrs <- data.frame(x=ceiling(Hist_PC_2yrs$mid), count=Hist_PC_2yrs$count, density=Hist_PC_2yrs$density)

Hist_PC_first <- hist(D_all$PC_first, breaks=seq(-1, max(D_all$PC_first), by=1), include.lowest=T, plot=F)
D_hist_PC_first <- data.frame(x=ceiling(Hist_PC_first$mid), count=Hist_PC_first$count, density=Hist_PC_first$density)
P_hist_PC_first <- D_hist_PC_first[which(D_hist_PC_first$density > 0),]

#Plot histograms of PC values (last author: positive, first author: negative)
P_PC_hist <- ggplot()
P_PC_hist <- P_PC_hist + geom_col(data=P_hist_PC_first, aes(x*-1, density), fill="orange", color="orange", size=1)
P_PC_hist <- P_PC_hist + geom_col(data=P_hist_PC, aes(x, density), fill="black", color="black", size=1)
P_PC_hist <- P_PC_hist + coord_flip()
P_PC_hist <- P_PC_hist + theme_bw()
P_PC_hist <- P_PC_hist + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_PC_hist <- P_PC_hist + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_PC_hist <- P_PC_hist + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_PC_hist <- P_PC_hist + scale_x_continuous(limits = c((P_PC_min+1)*-1,P_PC_max+1), breaks = seq(-500,500, by = Tick_PC))
P_PC_hist <- P_PC_hist + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.25))
if(show_plots){P_PC_hist}
if(save_plots){ggsave(File_P_PC_histo, path = User_dir_save, height = Plot_div_height*2, width = Plot_histo_width, units = "mm", dpi=300)}


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3.3. Generate TTGs showing genealogic connections
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Prepare data for offspring plot
#Generate data table with ancestor - offspring connections, "half-time" just to draw connecting lines
D_geneal <- DT_anc_off[,list(AI_ancestor,PY_ancestor,"AI_offspring"=abs(AI_offspring)*sign(AI_ancestor),PY_offspring,TC_ancestor,TC_offspring)]
D_geneal$Half_time <- ((D_geneal$PY_offspring-D_geneal$PY_ancestor)/2)+D_geneal$PY_ancestor

#Select only authors with offspring
D_OC <- D_all[which(D_all$OC>0),]

#Get authors with Top_n OC values
D_OC_sorted <- D_OC[order(-D_OC$OC),]
D_OC_sorted$OC_rank <- seq_along(D_OC_sorted$OC)
D_OC_sorted$legend <- paste(D_OC_sorted$OC_rank,D_OC_sorted$Author_name,sep=" ")
D_OC_top <- D_OC_sorted[1:Top_n,]
D_OC_top_minus <- subset(D_OC_top, D_OC_top$Author_index <0)
D_OC_top_plus <- subset(D_OC_top, D_OC_top$Author_index >0)

#Separate genealogic connections of top n OC authors from the rest
D_geneal_top <- D_geneal[which(D_geneal$AI_ancestor %in% D_OC_top$Author_index),]
D_geneal_bottom <-  D_geneal[which(!D_geneal$AI_ancestor %in% D_OC_top$Author_index),]

#Bubbleplot OC size
D_OC_top$OC_norm <- round(D_OC_top$OC/max(D_OC_top$OC)*100,1)
P_OC_top_size_norm_min <- min(D_OC_top$OC_norm)
P_OC_top_size_norm_max <- max(D_OC_top$OC_norm)
P_OC_top_min <- min(D_OC_top$OC)
P_OC_top_max <- max(D_OC_top$OC)

#Plotfiles
File_P_off <- sprintf("%s_P_off.tiff", Topic)
File_P_off_fract <- sprintf("%s_P_off_rates.tiff", Topic)
File_P_OC <- sprintf("%s_P_OC.tiff", Topic)
File_P_OC_histo <- sprintf("%s_P_OC_histo.tiff", Topic)
File_P_PC_off <-  sprintf("%s_P_PC_off.tiff", Topic)
File_P_PC_off_histo <-  sprintf("%s_P_PC_off_histo.tiff", Topic)
File_P_OC_names <- sprintf("%s_P_OC_names.tiff", Topic)

#Offspring connections for CLOCK field
P_off <- ggplot()
P_off <- P_off + geom_segment(data = D_geneal_bottom, aes(x = AI_ancestor, xend = AI_offspring, y = PY_ancestor, yend = Half_time), color="grey80", size = 0.3)
P_off <- P_off + geom_segment(data = D_geneal_bottom, aes(x = AI_offspring, xend = AI_offspring, y = Half_time, yend = PY_offspring), color="grey80", size = 0.3)
P_off <- P_off + geom_segment(data = D_geneal_top, aes(x = AI_ancestor, xend = AI_offspring, y = PY_ancestor, yend = Half_time, color=TC_ancestor), size = 0.4)
P_off <- P_off + geom_segment(data = D_geneal_top, aes(x = AI_offspring, xend = AI_offspring, y = Half_time, yend = PY_offspring, color=TC_ancestor), size = 0.4)
P_off <- P_off + geom_point(data = D_geneal_bottom, aes(x = AI_offspring, y = PY_offspring, color = TC_offspring), size=2, shape = 16, alpha= 0.6)
P_off <- P_off + geom_point(data = D_geneal_bottom, aes(x = AI_ancestor, y = PY_ancestor, color = TC_ancestor), size=2, shape = 16, alpha= 0.6)
P_off <- P_off + geom_point(data = D_OC_top, aes(x = Author_index, y = PY_start, color = Author_color, size = OC), shape = 16, alpha=0.6)
P_off <- P_off + geom_text_repel(data = D_OC_top_minus, aes(x = Author_index, y = PY_start, label = OC_rank, color = Author_color), nudge_x = Tick_AI*-2, nudge_y = -5, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
P_off <- P_off + geom_text_repel(data = D_OC_top_plus, aes(x = Author_index, y = PY_start, label = OC_rank, color = Author_color), nudge_x = Tick_AI*2, nudge_y = -5, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
P_off <- P_off + scale_size_continuous(range = c(P_OC_top_size_norm_min/3, P_OC_top_size_norm_max/3), breaks=c(P_OC_top_min, P_OC_top_max))
P_off <- P_off + scale_fill_identity()
P_off <- P_off + scale_colour_identity()
P_off <- P_off + theme_bw()
P_off <- P_off + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_off <- P_off + theme(legend.text = element_text(size = Legend_text_size), legend.background = element_blank(), legend.position =  c(0.5,0.85), legend.title = element_blank(), legend.key=element_blank())
P_off <- P_off + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_off <- P_off + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_off <- P_off + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_off <- P_off + scale_y_continuous(limits = c(PY_min,PY_max), breaks = seq(1700, 2040, by = Tick_years))
if(show_plots){P_off}
if(save_plots){ggsave(File_P_off, path = User_dir_save, height = Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}

#Offspring connections for other fields
P_off <- ggplot()
P_off <- P_off + geom_segment(data = D_geneal_bottom, aes(x = AI_ancestor, xend = AI_offspring, y = PY_ancestor, yend = Half_time), color="grey80", size = 0.3)
P_off <- P_off + geom_segment(data = D_geneal_bottom, aes(x = AI_offspring, xend = AI_offspring, y = Half_time, yend = PY_offspring), color="grey80", size = 0.3)
P_off <- P_off + geom_segment(data = D_geneal_top, aes(x = AI_ancestor, xend = AI_offspring, y = PY_ancestor, yend = Half_time, color=TC_ancestor), size = 0.4)
P_off <- P_off + geom_segment(data = D_geneal_top, aes(x = AI_offspring, xend = AI_offspring, y = Half_time, yend = PY_offspring, color=TC_ancestor), size = 0.4)

P_off <- P_off + geom_point(data = D_geneal_bottom, aes(x = AI_offspring, y = PY_offspring, color = TC_offspring), size=2, shape = 16, alpha= Alpha_all)
P_off <- P_off + geom_point(data = D_geneal_bottom, aes(x = AI_ancestor, y = PY_ancestor, color = TC_ancestor), size=2, shape = 16, alpha= Alpha_all)
#P_off <- P_off + geom_point(data = D_geneal_top, aes(x = AI_offspring, y = Half_time, color = TC_ancestor), shape = 17, size = 1)

#Add OC top
P_off <- P_off + geom_point(data = D_OC_top, aes(x = Author_index, y = PY_start, color = Author_color, size = OC), shape = 16, alpha=Alpha_all)
#P_off <- P_off + geom_text_repel(data = D_OC_top_minus, aes(x = Author_index, y = PY_start, label = OC_rank, color = Author_color), nudge_x = Tick_AI*-0.8, nudge_y = -7, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
#P_off <- P_off + geom_text_repel(data = D_OC_top_plus, aes(x = Author_index, y = PY_start, label = OC_rank, color = Author_color), nudge_x = Tick_AI*0.8, nudge_y = -7, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)

#CRISPR
P_off <- P_off + geom_text_repel(data = D_OC_top_minus, aes(x = Author_index, y = PY_start, label = OC_rank, color = Author_color), nudge_x = Tick_AI*-2, nudge_y = -5, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
P_off <- P_off + geom_text_repel(data = D_OC_top_plus, aes(x = Author_index, y = PY_start, label = OC_rank, color = Author_color), nudge_x = Tick_AI*2, nudge_y = -5, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)

P_off <- P_off + scale_size_continuous(range = c(P_OC_top_size_norm_min/3, P_OC_top_size_norm_max/3), breaks=c(P_OC_top_min, P_OC_top_max))
P_off <- P_off + scale_fill_identity()
P_off <- P_off + scale_colour_identity()
P_off <- P_off + theme_bw()
P_off <- P_off + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))

#Aplysia
#P_off <- P_off + theme(legend.text = element_text(size = Legend_text_size), legend.background = element_blank(), legend.position = Legend_position_5, legend.title = element_blank(), legend.key=element_blank())

#Organoid, Crispr
P_off <- P_off + theme(legend.text = element_text(size = Legend_text_size), legend.background = element_blank(), legend.position = Legend_position_1, legend.title = element_blank(), legend.key=element_blank())

P_off <- P_off + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_off <- P_off + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_off <- P_off + coord_cartesian(expand=FALSE)
P_off <- P_off + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_off <- P_off + scale_y_continuous(limits = c(PY_min,PY_max), breaks = seq(1700, 2040, by = Tick_years))
if(show_plots){P_off}
if(save_plots){ggsave(File_P_off, path = User_dir_save, height = Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}

#Plot offspring rates
P_off_fract <- ggplot()
P_off_fract <- P_off_fract + geom_col(data=D_rates[-nrow(D_rates),], aes(x=PY_bins, y = off_pubs_fract), size=1, color=NA, fill="black")
P_off_fract <- P_off_fract + geom_col(data=D_rates[-nrow(D_rates),], aes(x=PY_bins, y = off_authors_fract), size=1.5, color=NA, fill="orange")
P_off_fract <- P_off_fract + coord_flip()
P_off_fract <- P_off_fract + theme_bw()
P_off_fract <- P_off_fract + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_off_fract <- P_off_fract + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_off_fract <- P_off_fract + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_off_fract <- P_off_fract + scale_x_continuous(limits = c(PY_min, PY_max), breaks = seq(1700, 2040, by = Tick_years))
P_off_fract <- P_off_fract + scale_y_continuous(limits = c(0,1), breaks = seq(0, 1.0, by=0.25))
if(show_plots){P_off_fract}
if(save_plots){ggsave(File_P_off_fract, path = User_dir_save, height = Plot_height_PY, width = Plot_histo_width, units = "mm", dpi=300)}

#Plot OC values per last author/author

OC_max <- max(D_OC$OC)

P_OC <- ggplot()
P_OC <- P_OC + geom_segment(data = D_OC, aes(x = Author_index, xend = Author_index, y = 0, yend = OC, color = Author_color), size = 1)
P_OC <- P_OC + geom_point(data = D_OC, aes(x = Author_index, y = OC, color = Author_color), shape = 16, size=2)
P_OC <- P_OC + scale_fill_identity()
P_OC <- P_OC + scale_color_identity()
P_OC <- P_OC + theme_bw()
P_OC <- P_OC + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_OC <- P_OC + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_OC <- P_OC + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_OC <- P_OC + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_OC <- P_OC + scale_y_continuous(limits = c(-1,OC_max+1), breaks = seq(-200,200, by = Tick_OC))
if(show_plots){P_OC}
if(save_plots){ggsave(File_P_OC, path = User_dir_save, height = Plot_div_height, width = Plot_width_AI, units = "mm", dpi=300)}

#Generate histogram of OC values
Hist_OC <- hist(D_all$OC, breaks=seq(-1, max(D_all$OC), by=1), include.lowest=T, plot=F)
D_hist_OC <- data.frame(x=ceiling(Hist_OC$mid), count=Hist_OC$count, density=Hist_OC$density)

P_hist_OC <- D_hist_OC[which(D_hist_OC$density >0),]

P_OC_hist <- ggplot()
P_OC_hist <- P_OC_hist + geom_col(data=P_hist_OC, aes(x, density), color="black", fill="black", size=1)
P_OC_hist <- P_OC_hist + coord_flip()
P_OC_hist <- P_OC_hist + theme_bw()
P_OC_hist <- P_OC_hist + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_OC_hist <- P_OC_hist + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_OC_hist <- P_OC_hist + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_OC_hist <- P_OC_hist + scale_x_continuous(limits = c(-1,OC_max+1), breaks = seq(-200,200, by = Tick_OC))
P_OC_hist <- P_OC_hist + scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by=0.25))
if(show_plots){P_OC_hist}
if(save_plots){ggsave(File_P_OC_histo, path = User_dir_save, height = Plot_div_height, width = Plot_histo_width, units = "mm", dpi=300)}

#Plot names of authors with Top_n offspring counts
P_OC_names <- ggplot(D_PC_top)
P_OC_names  <- P_OC_names  + geom_text(aes(x=rep(0, times=Top_n), y=rev(D_OC_top$OC_rank), label=D_OC_top$legend), color="black", family="Arial", size=Names_size, parse=F, hjust=0, nudge_x=-15)
P_OC_names  <- P_OC_names  + theme_bw()
P_OC_names  <- P_OC_names  + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(), panel.background = element_blank(), plot.background = element_blank())
P_OC_names  <- P_OC_names  + theme(legend.text = element_blank(), legend.title = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank())
if(show_plots){P_OC_names}
if(save_plots){ggsave(File_P_OC_names, path = User_dir_save, height = Plot_names_height, width = Plot_names_width, units = "mm", dpi=300)}

#Plot PC_off
D_PC_off <- D_OC[which(D_OC$PC_off>0),]
PC_off_max <- max(D_PC_off$PC_off)
P_PC_off_annu_min <- round(min(D_PC_off$PC_off_annu),2)
P_PC_off_annu_max <- round(max(D_PC_off$PC_off_annu),2)

P_PC_off <- ggplot()
P_PC_off <- P_PC_off + geom_segment(data = D_PC_off, aes(x = Author_index, xend = Author_index, y = 0, yend = PC_off, color = Author_color), size = 1)
P_PC_off <- P_PC_off + geom_point(data = D_PC_off, aes(x = Author_index, y = PC_off, size = PC_off_annu, color = Author_color), shape = 16)
P_PC_off <- P_PC_off + scale_size_area(limits=c(P_PC_off_annu_min, P_PC_off_annu_max), breaks = c(P_PC_off_annu_min, P_PC_off_annu_max))
P_PC_off <- P_PC_off + scale_fill_identity()
P_PC_off <- P_PC_off + scale_color_identity()
P_PC_off <- P_PC_off + theme_bw()
P_PC_off <- P_PC_off + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_PC_off <- P_PC_off + theme(legend.text = element_text(size = Legend_text_size), legend.background = element_blank(), legend.position = Legend_position_3, legend.title = element_blank())
P_PC_off <- P_PC_off + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_PC_off <- P_PC_off + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_PC_off <- P_PC_off + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_PC_off <- P_PC_off + scale_y_continuous(limits = c(-1,PC_off_max+1), breaks = seq(-200,200, by = Tick_PC_off))
if(show_plots){P_PC_off}
if(save_plots){ggsave(File_P_PC_off, path = User_dir_save, height = Plot_div_height, width = Plot_width_AI, units = "mm", dpi=300)}

#Histogram PC_off
Hist_PC_off <- hist(D_all$PC_off, breaks=seq(-1, max(D_all$PC_off), by=1), include.lowest=T, plot=F)
D_hist_PC_off <- data.frame(x=ceiling(Hist_PC_off$mid), count=Hist_PC_off$count, density=Hist_PC_off$density)
P_hist_PC_off <- D_hist_PC_off[which(D_hist_PC_off$density >0),]

P_PC_off_hist <- ggplot()
P_PC_off_hist <- P_PC_off_hist + geom_col(data=P_hist_PC_off, aes(x, density), color="black", fill="black", size=1)
P_PC_off_hist <- P_PC_off_hist + coord_flip()
P_PC_off_hist <- P_PC_off_hist + theme_bw()
P_PC_off_hist <- P_PC_off_hist + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_PC_off_hist <- P_PC_off_hist + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_PC_off_hist <- P_PC_off_hist + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_PC_off_hist <- P_PC_off_hist + scale_x_continuous(limits = c(-1,PC_off_max+1), breaks = seq(-200,200, by = Tick_PC_off))
P_PC_off_hist <- P_PC_off_hist + scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by=0.25))
if(show_plots){P_PC_off_hist}
if(save_plots){ggsave(File_P_PC_off_histo, path = User_dir_save, height = Plot_div_height, width = Plot_histo_width, units = "mm", dpi=300)}

#Plot author generation
D_AG <- D_all[which(D_all$AG >0),]
AG_max <- max(D_AG$AG)

File_P_AG <- sprintf("%s_P_AG.tiff", Topic)
File_P_AG_histo <- sprintf("%s_P_AG_histo.tiff", Topic)
File_P_FS <- sprintf("%s_P_FS.tiff", Topic) 
File_P_FS_histo <- sprintf("%s_P_FS_histo.tiff", Topic) 

P_AG <- ggplot()
P_AG <- P_AG + geom_point(data = D_AG, aes(x = Author_index, y = AG, color = Author_color), shape = 16, size=2)
P_AG <- P_AG + scale_fill_identity()
P_AG <- P_AG + scale_color_identity()
P_AG <- P_AG + theme_bw()
P_AG <- P_AG + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_AG <- P_AG + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_AG <- P_AG + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_AG <- P_AG + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_AG <- P_AG + scale_y_continuous(limits = c(0.5,max(D_AG$AG)+0.5), breaks = seq(1,20, by = 1))
if(show_plots){P_AG}
if(save_plots){ggsave(File_P_AG, path = User_dir_save, height = Plot_div_height, width = Plot_width_AI, units = "mm", dpi=300)}

#Histogram AG
Hist_AG <- hist(D_AG$AG, breaks=seq(0, max(D_AG$AG), by=1), include.lowest=T, plot=F)
D_hist_AG <- data.frame(x=ceiling(Hist_AG$mid), count=Hist_AG$count, density=Hist_AG$density)
P_hist_AG <- D_hist_AG[which(D_hist_AG$density >0),]

P_AG_hist <- ggplot()
P_AG_hist <- P_AG_hist + geom_col(data=P_hist_AG, aes(x, density), color="black", fill="black", size=1)
P_AG_hist <- P_AG_hist + coord_flip()
P_AG_hist <- P_AG_hist + theme_bw()
P_AG_hist <- P_AG_hist + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_AG_hist <- P_AG_hist + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_AG_hist <- P_AG_hist + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_AG_hist <- P_AG_hist + scale_x_continuous(limits = c(0.5,max(D_AG$AG)+0.5), breaks = seq(1,20, by = 1))
P_AG_hist <- P_AG_hist + scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by=0.25))
if(show_plots){P_AG_hist}
if(save_plots){ggsave(File_P_AG_histo, path = User_dir_save, height = Plot_div_height, width = Plot_histo_width, units = "mm", dpi=300)}

#Plot Family size (FS)
D_FS <- D_all[which(D_all$Family_size > 0 & D_all$AG==1),]
FS_max <- max(D_FS$Family_size)

P_FS <- ggplot()
P_FS <- P_FS + geom_segment(data = D_FS, aes(x = Author_index, xend = Author_index, y = 0, yend = Family_size, color = Author_color), size = 1)
P_FS <- P_FS + geom_point(data = D_FS, aes(x = Author_index, y = Family_size, color = Author_color), shape = 16, size=2)
P_FS <- P_FS + scale_color_identity()
P_FS <- P_FS + theme_bw()
P_FS <- P_FS + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_FS <- P_FS + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_FS <- P_FS + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_FS <- P_FS + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_FS <- P_FS + scale_y_continuous(limits = c(0,max(D_FS$Family_size)+0.5), breaks = seq(-200,200, by = Tick_FS))
if(show_plots){P_FS}
if(save_plots){ggsave(File_P_FS, path = User_dir_save, height = Plot_div_height, width = Plot_width_AI, units = "mm", dpi=300)}

#Histogram FS
Hist_FS <- hist(D_FS$Family_size, breaks=seq(0, max(D_FS$Family_size), by=1), include.lowest=T, plot=F)
D_Hist_FS <- data.frame(x=ceiling(Hist_FS$mid), count=Hist_FS$count, density=Hist_FS$density)
P_Hist_FS <- D_Hist_FS[which(D_Hist_FS$density >0),]

P_FS_hist <- ggplot()
P_FS_hist <- P_FS_hist + geom_col(data=P_Hist_FS, aes(x, density), color="black", fill="black", size=1)
P_FS_hist <- P_FS_hist + coord_flip()
P_FS_hist <- P_FS_hist + theme_bw()
P_FS_hist <- P_FS_hist + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_FS_hist <- P_FS_hist + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_FS_hist <- P_FS_hist + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_FS_hist <- P_FS_hist + scale_x_continuous(limits = c(0,max(D_FS$Family_size)+0.5), breaks = seq(-200,200, by = Tick_FS))
P_FS_hist <- P_FS_hist + scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by=0.25))
if(show_plots){P_FS_hist}
if(save_plots){ggsave(File_P_FS_histo, path = User_dir_save, height = Plot_div_height, width = Plot_histo_width, units = "mm", dpi=300)}

#Plot family trees for authors with top n OC values
par(mar=c(0,0,0,0))
Net_fam_top <- induced_subgraph(Net_fam, subset(V(Net_fam), V(Net_fam)$Family_index %in% D_OC_top$Family_index), impl="create_from_scratch")

V(Net_fam_top)$OC_rank <- D_OC_sorted$OC_rank[match(V(Net_fam_top)$'name', D_OC_sorted$Author_name)]
V(Net_fam_top)$legend <- D_OC_sorted$legend[match(V(Net_fam_top)$'name', D_OC_sorted$Author_name)] 

File_P_fam1 <- sprintf("d:\\data\\papers\\teamtree\\plots\\%s_P_fam1.tiff", Topic)
File_P_fam2 <- sprintf("d:\\data\\papers\\teamtree\\plots\\%s_P_fam2.tiff", Topic)

#Plot with labels
P_fam_tree_ratio <- 0.7
P_fam_tree_height <- max(D_OC_top$AG)*100
P_fam_tree_width <- P_fam_tree_height/P_fam_tree_ratio
P_fam_layout_1 <- layout_as_tree(Net_fam_top, flip.y=FALSE, circular=FALSE)
if(save_plots){tiff(File_P_fam1, width=P_fam_tree_width, height=P_fam_tree_height, units="mm", res=200, bg="transparent")}
if(show_plots){
plot.igraph(Net_fam_top,
vertex.color=V(Net_fam_top)$color,
vertex.label=ifelse(V(Net_fam_top)$name %in% D_OC_top$Author_name, V(Net_fam_top)$OC_rank, NA),
vertex.label.color="white",
vertex.frame.color=V(Net_fam_top)$color,
vertex.size=V(Net_fam_top)$degree_out,
edge.color="gray50",
edge.arrow.mode=0,
edge.width=2.0,
layout=P_fam_layout_1,
asp=P_fam_tree_ratio)
}
if(save_plots){dev.off()}

#Plot without labels
if(save_plots){tiff(File_P_fam2, width=P_fam_tree_width, height=P_fam_tree_height, units="mm", res=200, bg="transparent")}
if(show_plots){
plot.igraph(Net_fam_top,
vertex.color=V(Net_fam_top)$color,
vertex.label=NA,
vertex.frame.color=V(Net_fam_top)$color,
vertex.size=V(Net_fam_top)$degree_out,
edge.color="gray50",
edge.arrow.mode=0,
edge.width=2.5,
layout=P_fam_layout_1,
asp=P_fam_tree_ratio)
}
if(save_plots){dev.off()}

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3.4. Generate TTGs for collaborative connections
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Plot collaborative connections of top n author-defining last authors

#Get Top_n CC
D_CC_sorted <- D_CC[order(-D_CC$degree_all),]
D_CC_sorted$CC_rank <- seq_along(D_CC_sorted$degree_all)
V(Net_col)$CC_rank <- D_CC_sorted$CC_rank[match(V(Net_col)$'name', D_CC_sorted$Author_name)]

D_CC_sorted$legend <- paste(D_CC_sorted$CC_rank,D_CC_sorted$Author_name,sep=" ")
CC_Largest <- D_CC_sorted$degree_all[Top_n]
D_CC_top <- D_CC_sorted[1:Top_n,]
D_CC_bottom <-  D_CC_sorted[-(1:Top_n),]

#D_CC_top$degree_in_norm <- round(D_CC_top$degree_in/max(D_CC_top$degree_in)*100,1)
#D_CC_top$degree_out_norm <- round(D_CC_top$degree_out/max(D_CC_top$degree_out)*100,1)

D_CC_top$degree_all_norm <- round(D_CC_top$degree_all/max(D_CC_top$degree_all)*100,1)

#P_CC_top_size_norm_min <- min(c(D_CC_top$degree_out_norm,D_CC_top$degree_in_norm))
#P_CC_top_size_norm_max <- max(c(D_CC_top$degree_out_norm,D_CC_top$degree_in_norm))
P_CC_top_min <- min(c(D_CC_top$degree_in,D_CC_top$degree_out))
P_CC_top_max <- max(c(D_CC_top$degree_in,D_CC_top$degree_out))

P_CC_top_min <- min(c(D_CC_top$degree_in,D_CC_top$degree_out))
P_CC_top_max <- max(c(D_CC_top$degree_in,D_CC_top$degree_out))

D_colnet_right <- DT_col_net[which(DT_col_net$Last %in% D_CC$Author_name & !DT_col_net$L_PY_start==DT_col_net$C_PY_start),]
D_colnet_left <- DT_col_net[which(DT_col_net$Col %in% D_CC$Author_name & !DT_col_net$L_PY_start==DT_col_net$C_PY_start),]

DT_col_net_top <- DT_col_net[Last %in% D_CC_top$Author_name | Col %in% D_CC_top$Author_name]
DT_col_net_top_out <- DT_col_net[Last %in% D_CC_top$Author_name]
DT_col_net_top_in <- DT_col_net[Col %in% D_CC_top$Author_name]

#Plotfiles
File_P_col <- sprintf("%s_P_col.tiff", Topic)
File_P_col_fract <- sprintf("%s_P_col_rates.tiff", Topic)
File_P_CC <- sprintf("%s_P_CC.tiff", Topic)
File_P_CC_histo <- sprintf("%s_P_CC_histo.tiff", Topic)
File_P_CC_top <- sprintf("%s_P_CC_top.tiff", Topic)
File_P_PC_col <- sprintf("%s_P_PC_col.tiff", Topic) 
File_P_PC_col_histo <- sprintf("%s_P_PC_col_histo.tiff", Topic) 
File_P_annu_AC <- sprintf("%s_P_annu_AC.tiff", Topic) 

#Generate plot
P_col <- ggplot()
P_col <- P_col + geom_curve(data = DT_col_net[N >= 1], aes(x = abs(L_AI)*-1, xend = abs(C_AI), y = L_PY_start, yend = C_PY_start, group = L_AI), color="grey80", size = 0.3, curvature = 0.2)

#Plot col top
P_col <- P_col + geom_curve(data = DT_col_net_top_out, aes(x = abs(L_AI)*-1, xend = abs(C_AI), y = L_PY_start, yend = C_PY_start, group = L_AI, color = L_TC), size = 0.4, curvature=-0.5)
P_col <- P_col + geom_curve(data = DT_col_net_top_in, aes(x = abs(L_AI)*-1, xend = abs(C_AI), y = L_PY_start, yend = C_PY_start, group = C_AI, color = C_TC), size = 0.4, curvature=-0.5)

P_col <- P_col + geom_point(data = DT_col_net[N >= 1], aes(x = abs(L_AI)*-1, y = L_PY_start, group = L_AI, color=L_TC), size = 2)
P_col <- P_col + geom_point(data = DT_col_net[N >= 1], aes(x = abs(C_AI), y = C_PY_start, group = C_AI, color=C_TC), size = 2)

#Plot CC top
#P_col <- P_col + geom_point(data = D_CC_top, aes(x = abs(Author_index), y = PY_start, size = degree_in, color = Author_color), shape = 21, stroke = 3)
#P_col <- P_col + geom_point(data = D_CC_top, aes(x = abs(Author_index)*-1, y = PY_start, size = degree_out, color = Author_color), shape = 21, stroke = 3)
P_col <- P_col + geom_point(data = D_CC_top, aes(x = abs(Author_index), y = PY_start, size = degree_in, color = Author_color), shape = 16, alpha = 0.6)
P_col <- P_col + geom_point(data = D_CC_top, aes(x = abs(Author_index)*-1, y = PY_start, size = degree_out, color = Author_color), shape = 16, alpha = 0.6)
P_col <- P_col + scale_size(range = c(P_CC_top_min/2, P_CC_top_max/2), breaks = c(P_CC_top_min, P_CC_top_max))
P_col <- P_col + geom_text_repel(data = D_CC_top, aes(x = abs(Author_index)*-1, y = PY_start, label = CC_rank, color = Author_color), nudge_x = Tick_AI*-2, nudge_y = -5, vjust=0, hjust=0, size = Label_size, inherit.aes=FALSE)
P_col <- P_col + scale_colour_identity()
P_col <- P_col + theme_bw()
P_col <- P_col + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_col <- P_col + theme(legend.text = element_text(size = Legend_text_size), legend.position = Legend_position_1, legend.title = element_blank(), legend.background = element_blank(), legend.key=element_blank())
P_col <- P_col + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_col <- P_col + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_col <- P_col + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_col <- P_col + scale_y_continuous(limits = c(PY_min,PY_max), breaks = seq(1700, 2040, by = Tick_years))
if(show_plots){P_col}
if(save_plots){ggsave(File_P_col, path = User_dir_save, height = Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}

#Plot rates of collaborative papers/authors
P_col_fract <- ggplot()
P_col_fract <- P_col_fract  + geom_col(data=D_rates[-nrow(D_rates),], aes(x=PY_bins, y = col_pubs_fract), size=1, color=NA, fill="black")
P_col_fract <- P_col_fract  + geom_col(data=D_rates[-nrow(D_rates),], aes(x=PY_bins, y = col_authors_fract), size=1, color=NA, fill="orange")
P_col_fract <- P_col_fract  + coord_flip()
P_col_fract <- P_col_fract  + theme_bw()
P_col_fract <- P_col_fract  + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_col_fract <- P_col_fract  + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_col_fract <- P_col_fract  + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_col_fract <- P_col_fract  + scale_x_continuous(limits = c(PY_min, PY_max), breaks = seq(1700, 2040, by = Tick_years))
P_col_fract <- P_col_fract  + scale_y_continuous(limits = c(0,1), breaks = seq(0, 1.0, by=0.25))
if(show_plots){P_col_fract}
if(save_plots){ggsave(File_P_col_fract, path = User_dir_save, height = Plot_height_PY, width = Plot_histo_width, units = "mm", dpi=300)}

#Calculate annual mean count of authors per article
D_annu_AC <- DT_pub[,.(AC_mean=mean(Author_count)), by=PY]
P_annu_AC <- ggplot()
P_annu_AC <- P_annu_AC + geom_line(data=D_annu_AC, aes(x=PY, y=AC_mean), size=2)
P_annu_AC <- P_annu_AC + theme_bw()
P_annu_AC <- P_annu_AC + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_annu_AC <- P_annu_AC + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_annu_AC <- P_annu_AC + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_annu_AC <- P_annu_AC + scale_x_continuous(limits= c(PY_min,PY_max), breaks = seq(1700, 2040, by = Tick_years))
P_annu_AC <- P_annu_AC + scale_y_continuous(limits = c(0,max(D_annu_AC$AC_mean)), breaks = seq(0, 10000, by = Tick_AC))
if(show_plots){P_annu_AC}
if(save_plots){ggsave(File_P_annu_AC, path = User_dir_save, width = 100, height = 70, units = "mm", dpi=300)}

#Plot Collaborator counts (CC)
D_CC_out <- D_all[which(D_all$degree_out > 0),]
D_CC_in <- D_all[which(D_all$degree_in > 0),]

CC_in_max <- max(D_all$degree_in)
CC_out_max <- max(D_all$degree_out)

P_CC <- ggplot()
P_CC <- P_CC + geom_segment(data = D_CC_out, aes(x = Author_index, xend = Author_index, y = 0, yend = degree_out, color = Author_color), size = 0.5)
P_CC <- P_CC + geom_point(data = D_CC_out, aes(x = Author_index, y = degree_out, color = Author_color), size = 1)
P_CC <- P_CC + geom_segment(data = D_CC_in, aes(x = Author_index, xend = Author_index, y = 0, yend = degree_in*-1, color = Author_color), size = 0.5)
P_CC <- P_CC + geom_point(data = D_CC_in, aes(x = Author_index, y = degree_in*-1, color = Author_color), size = 1)
P_CC <- P_CC + geom_hline(yintercept=0, color="black", size=Line_width/2)
P_CC <- P_CC + scale_color_identity()
P_CC <- P_CC + theme_bw()
P_CC <- P_CC + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_CC <- P_CC + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_CC <- P_CC + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_CC <- P_CC + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_CC <- P_CC + scale_y_continuous(limits = c(CC_in_max*-1-1,CC_out_max+1), breaks = seq(-400,400, by = Tick_CC))
if(show_plots){P_CC}
if(save_plots){ggsave(File_P_CC, path = User_dir_save, height = Plot_div_height*2, width = Plot_width_AI, units = "mm", dpi=300)}

#Histograms degree/CC
Hist_CC_all <- hist(D_all$degree_all, breaks=seq(-1, max(D_all$degree_all), by=1), include.lowest=T, plot=F)
D_hist_CC_all <- data.frame(x=ceiling(Hist_CC_all$mids), count=Hist_CC_all$counts, density=Hist_CC_all$density)

Hist_CC_out <- hist(D_all$degree_out, breaks=seq(-1, max(D_all$degree_out), by=1), include.lowest=T, plot=F)
D_hist_CC_out <- data.frame(x=ceiling(Hist_CC_out$mids), count=Hist_CC_out$counts, density=Hist_CC_out$density)
P_hist_CC_out <- D_hist_CC_out[which(D_hist_CC_out$density >0),]

Hist_CC_in <- hist(D_all$degree_in, breaks=seq(-1, max(D_all$degree_in), by=1), include.lowest=T, plot=F)
D_hist_CC_in <- data.frame(x=ceiling(Hist_CC_in$mids), count=Hist_CC_in$counts, density=Hist_CC_in$density)
P_hist_CC_in <- D_hist_CC_in[which(D_hist_CC_in$density >0),]

P_CC_hist <- ggplot()
P_CC_hist <- P_CC_hist + geom_col(data=P_hist_CC_in, aes(x*-1, density), color="orange", fill="orange", size=0.5)
P_CC_hist <- P_CC_hist + geom_col(data=P_hist_CC_out, aes(x, density), color="black", fill="black", size=0.5)
P_CC_hist <- P_CC_hist + coord_flip()
P_CC_hist <- P_CC_hist + theme_bw()
P_CC_hist <- P_CC_hist + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_CC_hist <- P_CC_hist + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_CC_hist <- P_CC_hist + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_CC_hist <- P_CC_hist + scale_x_continuous(limits = c(CC_in_max*-1-1,CC_out_max+1), breaks = seq(-400,400, by = Tick_CC))
P_CC_hist <- P_CC_hist + scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by=0.25))
if(show_plots){P_CC_hist}
if(save_plots){ggsave(File_P_CC_histo, path = User_dir_save, height = Plot_div_height*2, width = Plot_histo_width, units = "mm", dpi=300)}

#Plot counts of collaborative publications per last author/author
D_PC_col_out <- D_all[which(D_all$PC_col_out > 0),]
D_PC_col_in <- D_all[which(D_all$PC_col_in > 0),]
P_PC_col_annu_min <- round(min(c(D_PC_col_out$PC_col_out_annu,D_PC_col_in$PC_col_in_annu)),digits=2)
P_PC_col_annu_max <- round(max(c(D_PC_col_out$PC_col_out_annu,D_PC_col_in$PC_col_in_annu)),digits=2)

#Plot Collaborator counts - degree vs index

PC_col_out_max <- max(D_all$PC_col_out)
PC_col_in_max <- max(D_all$PC_col_in)

P_PC_col <- ggplot()
P_PC_col <- P_PC_col + geom_segment(data = D_PC_col_out, aes(x = Author_index, xend = Author_index, y = 0, yend = PC_col_out, color = Author_color), size = 0.3)
P_PC_col <- P_PC_col + geom_point(data = D_PC_col_out, aes(x = Author_index, y = PC_col_out, size=PC_col_out_annu, color = Author_color), shape=16)
P_PC_col <- P_PC_col + geom_segment(data = D_PC_col_in, aes(x = Author_index, xend = Author_index, y = 0, yend = PC_col_in*-1, color = Author_color), size = 0.3)
P_PC_col <- P_PC_col + geom_point(data = D_PC_col_in, aes(x = Author_index, y = PC_col_in*-1, size=PC_col_in_annu, color = Author_color), shape=16)
P_PC_col <- P_PC_col + geom_hline(yintercept=0, color="black", size=Line_width/2)
P_PC_col <- P_PC_col + scale_size_continuous(range = c(P_PC_col_annu_min, P_PC_col_annu_max), breaks = c(1, P_PC_col_annu_max))
P_PC_col <- P_PC_col + scale_color_identity()
P_PC_col <- P_PC_col + theme_bw()
P_PC_col <- P_PC_col + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_PC_col <- P_PC_col + theme(legend.text = element_text(size = Legend_text_size), legend.position = Legend_position_3, legend.background = element_blank(), legend.title = element_blank())
P_PC_col <- P_PC_col + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_PC_col <- P_PC_col + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_PC_col <- P_PC_col + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_PC_col <- P_PC_col + scale_y_continuous(limits = c(max(D_PC_col_in$PC_col_in)*-1-1,max(D_PC_col_out$PC_col_out)+1), breaks = seq(-400,400, by = Tick_PC_col))
if(show_plots){P_PC_col}
if(save_plots){ggsave(File_P_PC_col, path = User_dir_save, height = Plot_div_height*2, width = Plot_width_AI, units = "mm", dpi=300)}

#Generate histograms of PC in and out
Hist_PC_col_out <- hist(D_all$PC_col_out, breaks=seq(-1, max(D_all$PC_col_out), by=1), include.lowest=T, plot=F)
D_hist_PC_col_out <- data.frame(x=ceiling(Hist_PC_col_out$mid), count=Hist_PC_col_out$count, density=Hist_PC_col_out$density)
P_hist_PC_col_out <- D_hist_PC_col_out[which(D_hist_PC_col_out$density >0),]

Hist_PC_col_in <- hist(D_all$PC_col_in, breaks=seq(-1, max(D_all$PC_col_in), by=1), include.lowest=T, plot=F)
D_hist_PC_col_in <- data.frame(x=ceiling(Hist_PC_col_in$mid), count=Hist_PC_col_in$count, density=Hist_PC_col_in$density)
P_hist_PC_col_in <- D_hist_PC_col_in[which(D_hist_PC_col_in$density >0),]

P_PC_col_hist <- ggplot()
P_PC_col_hist <- P_PC_col_hist + geom_col(data=P_hist_PC_col_in, aes(x*-1, density), color="orange", fill="orange", size=0.5)
P_PC_col_hist <- P_PC_col_hist + geom_col(data=P_hist_PC_col_out, aes(x, density), color="black", fill="black", size=0.5)
P_PC_col_hist <- P_PC_col_hist + coord_flip()
P_PC_col_hist <- P_PC_col_hist + theme_bw()
P_PC_col_hist <- P_PC_col_hist + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_PC_col_hist <- P_PC_col_hist + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_PC_col_hist <- P_PC_col_hist + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_PC_col_hist <- P_PC_col_hist + scale_x_continuous(limits = c(max(D_PC_col_in$PC_col_in)*-1-1,max(D_PC_col_out$PC_col_out)+1), breaks = seq(-400,400, by = Tick_PC_col))
P_PC_col_hist <- P_PC_col_hist + scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by=0.25))
if(show_plots){P_PC_col_hist}
if(save_plots){ggsave(File_P_PC_col_histo, path = User_dir_save, height = Plot_div_height*2, width = Plot_histo_width, units = "mm", dpi=300)}

#Visualize collaborator network of authors with top n collaboration counts
Author_sel <- subset(V(Net_col), V(Net_col)$degree_all >= CC_Largest)
Neighb_names <- Author_sel$name
for (i in seq_along(Author_sel)){
#E(Net_col)[from(match(Author_sel[i]$name, V(Net_col)$name))]$color2 <- Author_sel[i]$color
V_neighb <- neighbors(Net_col, Author_sel[i], mode="out")
Neighb_names <- append(Neighb_names, V_neighb$name)}
Neighb_names_unique <- Neighb_names[!duplicated(Neighb_names)]

#Create subgraph based on conditions
Net_col_temp <- induced_subgraph(Net_col, subset(V(Net_col), V(Net_col)$name %in% Neighb_names_unique), impl="create_from_scratch")

Net_col_simpl <- simplify(Net_col_temp, remove.multiple=F, remove.loops=T)

P_col_ratio <- 0.8
P_col_width=600
P_col_height=P_col_width*P_col_ratio

File_P_col_net <- sprintf("d:\\data\\papers\\teamtree\\plots\\%s_P_col_net.tiff", Topic)
File_P_CC_names <- sprintf("%s_P_CC_names.tiff", Topic)

#Plot and save graph
if(save_plots){tiff(File_P_col_net, width=P_col_width, height=P_col_height, units="mm", res=300, bg="transparent")}

if(show_plots){plot.igraph(Net_col_simpl,
edge.color=E(Net_col_simpl)$color,
edge.arrow.mode=0,
vertex.shape=ifelse(V(Net_col_simpl)$Family_size > 0, "circle", "square"),
#vertex.size=V(Net_col_simpl)$degree_all/max(V(Net_col_temp)$degree_all)*10,
vertex.size=ifelse(V(Net_col_simpl)$degree_all >= CC_Largest, V(Net_col_simpl)$degree_all/max(V(Net_col_temp)$degree_all)*10, 1),
vertex.color=vertex_attr(Net_col_simpl, "color"),
#vertex.label=NA,
vertex.label=ifelse(V(Net_col_simpl)$degree_all >= CC_Largest, V(Net_col_simpl)$CC_rank, NA),
vertex.label.family="Arial",
vertex.label.color="white",
vertex.label.cex=3,
vertex.frame.color="black",
asp=P_col_ratio)}
if(save_plots){dev.off()}

P_CC_names <- ggplot()
P_CC_names <- P_CC_names + geom_text(data=D_CC_top, aes(x=rep(0, times=Top_n), y=rev(CC_rank), label=legend), color="black", family="Arial", size=Names_size, parse=F, hjust=0, nudge_x=0)
P_CC_names <- P_CC_names + theme_bw()
P_CC_names <- P_CC_names + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank())
P_CC_names <- P_CC_names + theme(legend.text = element_blank(), legend.title = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank())
if(show_plots){P_CC_names}
if(save_plots){ggsave(File_P_CC_names, path = User_dir_save, height = Plot_names_height, width = Plot_names_width, units = "mm", dpi=300)}


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3.5. TTGs for TTP-based author ranking
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Prepare TTP data for visualization
#Get authors with Top_n TTP values
D_TTP_sorted <- D_TTP[order(-D_TTP$PCxOCxCC),]
D_TTP_sorted$TTP_rank <- seq_along(D_TTP_sorted$PCxOCxCC)
D_TTP_sorted$legend <- paste(D_TTP_sorted$TTP_rank,D_TTP_sorted$Author_name,sep=" ")

D_TTP_top <- D_TTP_sorted[1:Top_n,]
D_TTP_top_minus <- subset(D_TTP_top, D_TTP_top$Author_index <0)
D_TTP_top_plus <- subset(D_TTP_top, D_TTP_top$Author_index >0)
D_TTP_first <- subset(D_all, abs(D_all$Author_index)==1)

P_TTP_top_norm_min <- round(min(D_TTP_top$PCxOCxCC_rel_max),2)
P_TTP_top_norm_max <- round(max(D_TTP_top$PCxOCxCC_rel_max),2)
P_TTP_norm_min <- min(D_TTP$PCxOCxCC_rel_max)
P_TTP_norm_max <- max(D_TTP$PCxOCxCC_rel_max)

P_TTP_top_min <- min(D_TTP_top$TTP_log)
P_TTP_top_max <- max(D_TTP_top$TTP_log)

File_P_TTproduct <- sprintf("%s_P_TTproduct.tiff", Topic)

P_TTproduct <- ggplot()
P_TTproduct <- P_TTproduct + geom_line(data = D_TTP, aes(x = Author_index, y = PY_start), size=0.3, color="gray25")
P_TTproduct <- P_TTproduct + geom_point(data = D_TTP, aes(x = Author_index, y = PY_start, color = Author_color, size = PCxOCxCC_rel_max), shape = 21, fill = "grey80", alpha=Alpha_all)
P_TTproduct <- P_TTproduct + geom_point(data = D_TTP_top, aes(x = Author_index, y = PY_start, color = Author_color, size = PCxOCxCC_rel_max), shape = 16, alpha=Alpha_all)
P_TTproduct <- P_TTproduct + geom_text_repel(data = D_TTP_top_minus, aes(x = Author_index, y = PY_start, label = TTP_rank, color = Author_color), nudge_x = Tick_AI*-2, nudge_y = 0, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
P_TTproduct <- P_TTproduct + geom_text_repel(data = D_TTP_top_plus, aes(x = Author_index, y = PY_start, label = TTP_rank, color = Author_color), nudge_x = Tick_AI*2, nudge_y = 0, hjust = 0, vjust = 0, size = Label_size, inherit.aes=FALSE)
P_TTproduct <- P_TTproduct + scale_size_continuous(range = c(P_TTP_norm_min*50, P_TTP_norm_max*50), breaks = c(P_TTP_top_norm_min,P_TTP_top_norm_max))
P_TTproduct <- P_TTproduct + scale_fill_identity()
P_TTproduct <- P_TTproduct + scale_colour_identity()
P_TTproduct <- P_TTproduct + theme_bw()
P_TTproduct <- P_TTproduct + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_TTproduct <- P_TTproduct + theme(legend.text = element_text(size = Legend_text_size), legend.position = c(0.5,0.85), legend.title = element_blank(), legend.background = element_blank(), legend.key=element_blank())
P_TTproduct <- P_TTproduct + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_TTproduct <- P_TTproduct + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_TTproduct <- P_TTproduct + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_TTproduct <- P_TTproduct + scale_y_continuous(limits = c(PY_min,PY_max), breaks = seq(1700, 2040, by = Tick_years))
if(show_plots){P_TTproduct}
if(save_plots){ggsave(File_P_TTproduct, path = User_dir_save, height = Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}

File_P_TTP_names <- sprintf("%s_P_TTP_names.tiff", Topic)

P_TTP_names <- ggplot()
P_TTP_names <- P_TTP_names + geom_text(data=D_TTP_top, aes(x=rep(0, times=Top_n), y=rev(TTP_rank), label=legend), family="Arial", size=Names_size, parse=F, hjust=0, color = "black")
P_TTP_names <- P_TTP_names + theme_bw()
P_TTP_names <- P_TTP_names + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank())
P_TTP_names <- P_TTP_names + theme(legend.text = element_blank(), legend.title = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank())
if(show_plots){P_TTP_names}
if(save_plots){ggsave(File_P_TTP_names, path = User_dir_save, height = Plot_names_height, width = Plot_names_width, units = "mm", dpi=300)}

File_P_TTP_fract <- sprintf("%s_P_TTP_fract.tiff", Topic)

#Plot annual rates of pubs/authors with TTP values >0
P_TTP_fract <- ggplot()
P_TTP_fract <- P_TTP_fract  + geom_col(data=D_rates, aes(x=PY_bins, y = TTP_pubs_fract), size=1, color=NA, fill="black")
P_TTP_fract <- P_TTP_fract  + geom_col(data=D_rates, aes(x=PY_bins, y = TTP_authors_fract), size=1, color=NA, fill="orange")
P_TTP_fract <- P_TTP_fract  + coord_flip()
P_TTP_fract <- P_TTP_fract  + theme_bw()
P_TTP_fract <- P_TTP_fract  + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_TTP_fract <- P_TTP_fract  + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_TTP_fract <- P_TTP_fract  + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_TTP_fract <- P_TTP_fract  + scale_x_continuous(limits = c(PY_min, PY_max), breaks = seq(1700, 2040, by = Tick_years))
P_TTP_fract <- P_TTP_fract  + scale_y_continuous(limits = c(0,1), breaks = seq(0, 1.0, by=0.25))
if(show_plots){P_TTP_fract}
if(save_plots){ggsave(File_P_TTP_fract, path = User_dir_save, height = Plot_height_PY, width = Plot_histo_width, units = "mm", dpi=300)}

#Plot TTP values
File_P_TTP <- sprintf("%s_P_TTP.tiff", Topic)

P_TTP <- ggplot()
P_TTP <- P_TTP + geom_segment(data = D_TTP, aes(x = Author_index, xend = Author_index, y = 0, yend = TTP_log, color = Author_color), size = 0.7)
P_TTP <- P_TTP + scale_color_identity()
P_TTP <- P_TTP + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_TTP <- P_TTP + theme(legend.text = element_text(size = Legend_text_size), legend.position =  c(0.9,0.3), legend.background = element_blank(), legend.title = element_blank(), axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_TTP <- P_TTP + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_TTP <- P_TTP + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_TTP <- P_TTP + scale_y_continuous(limits=c(-0.25,ceiling(max(D_TTP$TTP_log))), breaks = seq(0,8, by = 2))
if(show_plots){P_TTP}
if(save_plots){ggsave(File_P_TTP, path = User_dir_save, height = Plot_div_height*1.25, width = Plot_width_AI, units = "mm", dpi=300)}

Hist_TTP <- hist(D_all$PCxOCxCC, breaks=seq(-1, max(D_all$PCxOCxCC), by=1), include.lowest=T, plot=F)
D_hist_TTP <- data.frame(x=(Hist_TTP$mid+0.5), count=Hist_TTP$count, density=Hist_TTP$density)
1-D_hist_TTP[1,]$density
Hist_TTP_lg <- hist(D_TTP$TTP_log, breaks=seq(-0.25, ceiling(max(D_TTP$TTP_log)), by=0.25), include.lowest=T, plot=F)
D_hist_TTP_lg <- data.frame(x=(Hist_TTP_lg$mid+0.125), count=Hist_TTP_lg$count, density=Hist_TTP_lg$density, freq=Hist_TTP_lg$count/sum(Hist_TTP_lg$count))

File_P_TTP_histo <- sprintf("%s_P_TTP_hist.tiff", Topic)

P_TTP_hist <- ggplot()
P_TTP_hist <- P_TTP_hist + geom_col(data=D_hist_TTP_lg, aes(x, freq), color="black", fill="black", size=0.5)
P_TTP_hist <- P_TTP_hist + coord_flip()
P_TTP_hist <- P_TTP_hist + theme_bw()
P_TTP_hist <- P_TTP_hist + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_TTP_hist <- P_TTP_hist + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_TTP_hist <- P_TTP_hist + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_TTP_hist <- P_TTP_hist + scale_x_continuous(limits = c(-0.25,ceiling(max(log10(D_all$PCxOCxCC)))), breaks = seq(0,8, by = 2))
P_TTP_hist <- P_TTP_hist + scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by=0.25))
if(show_plots){P_TTP_hist}
if(save_plots){ggsave(File_P_TTP_histo, path = User_dir_save, height = Plot_div_height*1.25, width = Plot_histo_width, units = "mm", dpi=300)}


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Comparison TTP vs. citation counts (only WOS data)

#Plot TTP versus H index
D_TTP_cit <- D_all[which(D_all$Cit_sum > 0 & D_all$PCxOCxCC > 0),]

P_TTP_H_ind <- ggplot()
P_TTP_H_ind <- P_TTP_H_ind + geom_point(data=D_TTP_cit, aes(x = log10(PCxOCxCC), y = H_ind, color=Author_color), shape = 21, size = 3, stroke = 1.5)
P_TTP_H_ind <- P_TTP_H_ind + scale_colour_identity()
P_TTP_H_ind <- P_TTP_H_ind + theme_bw()
P_TTP_H_ind <- P_TTP_H_ind + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_TTP_H_ind <- P_TTP_H_ind + theme(legend.text = element_text(size = Legend_text_size), legend.position = c(0.5,0.8), legend.title = element_blank(), legend.background = element_blank(), legend.key=element_blank())
P_TTP_H_ind <- P_TTP_H_ind + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_TTP_H_ind <- P_TTP_H_ind + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_TTP_H_ind <- P_TTP_H_ind + scale_x_continuous(limits = c(0,ceiling(max(log10(D_TTP_cit$PCxOCxCC)))), breaks = seq(0,8, by = 1))
P_TTP_H_ind <- P_TTP_H_ind + scale_y_continuous(limits = c(0,ceiling(max(D_TTP_cit$H_ind))), breaks = seq(0,1000, by = 20))
if(show_plots){P_TTP_H_ind}
File_P_TTP_H_ind <- sprintf("%s_P_TTP_H_ind.tiff", Topic)
if(save_plots){ggsave(File_P_TTP_H_ind, path = User_dir_save, height = 150, width = 150, units = "mm", dpi=300)}

#Plot TTP versus sum of citations
P_TTP_cit_sum <- ggplot()
P_TTP_cit_sum <- P_TTP_cit_sum + geom_point(data=D_TTP_cit, aes(x = log10(PCxOCxCC), y = log10(Cit_sum), color=Author_color), shape = 21, size = 3, stroke = 1.5)
P_TTP_cit_sum <- P_TTP_cit_sum + scale_colour_identity()
P_TTP_cit_sum <- P_TTP_cit_sum + theme_bw()
P_TTP_cit_sum <- P_TTP_cit_sum + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_TTP_cit_sum <- P_TTP_cit_sum + theme(legend.text = element_text(size = Legend_text_size), legend.position = c(0.5,0.8), legend.title = element_blank(), legend.key=element_blank())
P_TTP_cit_sum <- P_TTP_cit_sum + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_TTP_cit_sum <- P_TTP_cit_sum + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_TTP_cit_sum <- P_TTP_cit_sum + scale_x_continuous(limits = c(0,ceiling(max(log10(D_TTP_cit$PCxOCxCC)))), breaks = seq(0,8, by = 2))
P_TTP_cit_sum <- P_TTP_cit_sum + scale_y_continuous(limits = c(0,ceiling(max(log10(D_TTP_cit$Cit_sum)))), breaks = seq(0,8, by = 2))
P_TTP_cit_sum
if(show_plots){P_TTP_cit_sum}
File_P_TTP_cit_sum <- sprintf("%s_P_TTP_cit_sum.tiff", Topic)
if(save_plots){ggsave(File_P_TTP_cit_sum, path = User_dir_save, height = 150, width = 150, units = "mm", dpi=300)}

#Plot values normalized to max
D_TTP_cit$Cit_sum_rel_max <- log10(D_TTP_cit$Cit_sum)/max(log10(D_TTP_cit$Cit_sum), na.rm=T)
D_TTP_cit$H_ind_rel_max <- D_TTP_cit$H_ind/max(D_TTP_cit$H_ind, na.rm=T)
D_TTP_cit$TTP_rel_max <- log10(D_TTP_cit$PCxOCxCC)/max(log10(D_TTP_cit$PCxOCxCC),na.rm=T)

P_cit_rel <- ggplot()
P_cit_rel <- P_cit_rel + geom_point(data=D_TTP_cit, aes(y = H_ind_rel_max, x = TTP_rel_max), fill="black", shape = 24, color="#87ceeb", stroke=1, size = 3)
P_cit_rel <- P_cit_rel + geom_point(data=D_TTP_cit, aes(y = Cit_sum_rel_max, x = TTP_rel_max), fill="orange", shape = 21, color="#009e73", stroke=1, size = 3)
P_cit_rel <- P_cit_rel + scale_colour_identity()
P_cit_rel <- P_cit_rel + theme_bw()
P_cit_rel <- P_cit_rel + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_cit_rel <- P_cit_rel + theme(legend.text = element_text(size = Legend_text_size), legend.position = c(0.5,0.8), legend.title = element_blank(), legend.background = element_blank(), legend.key=element_blank())
P_cit_rel <- P_cit_rel + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_cit_rel <- P_cit_rel + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_cit_rel <- P_cit_rel + scale_x_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.25))
P_cit_rel <- P_cit_rel + scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.25))
if(show_plots){P_cit_rel}
File_P_cit_rel <- sprintf("%s_P_cit_rel.tiff", Topic)
save_plots <- T
if(save_plots){ggsave(File_P_cit_rel, path = User_dir_save, height = 150, width = 150, units = "mm", dpi=300)}
save_plots <- F

#Correlation of TTP with sum of citations and H index
#Shapiro_cit_sum <- shapiro.test(log10(D_TTP_cit$Cit_sum))
#Shapiro_TTP <- shapiro.test(log10(D_TTP_cit$PCxOCxCC))
#Shapiro_data <- rbind(transpose(as.data.table(unlist(Shapiro_cit_sum))), transpose(as.data.table(unlist(Shapiro_TTP))))
Topic_stat <- c(sprintf("%s_cit_sum", Topic),sprintf("%s_h_ind", Topic))
Sample_size <- c(nrow(D_all), nrow(D_TTP_cit))
Correl_cit_sum <- cor.test(D_TTP_cit$Cit_sum, D_TTP_cit$PCxOCxCC, method=c("spearman"))
Correl_h_ind <- cor.test(D_TTP_cit$H_ind, D_TTP_cit$PCxOCxCC, method=c("spearman"))
Spearman_data <- rbind(transpose(as.data.table(unlist(Correl_cit_sum))), transpose(as.data.table(unlist(Correl_h_ind))))

File_TT_cit_stat <- paste(User_dir_save, sprintf("%s_TT_cit_stat.csv", Topic),sep="\\")
#write.csv(cbind(Topic_stat, Sample_size, Spearman_data), file=File_TT_cit_stat, row.names=FALSE)


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3.6. Euler plot showing overlap of PC, OC and CC ranks
#----------------------------------------------------------------------------------------------------------------------------------------------------------

Top_n2 <- 100
Euler_PC <- D_PC_sorted$Author_name[1:Top_n2]
Euler_OC <- D_OC_sorted$Author_name[1:Top_n2]
Euler_CC <- D_CC_sorted$Author_name[1:Top_n2]

Euler_PCvsOC <- unique(intersect(Euler_PC, Euler_OC), intersect(Euler_OC, Euler_PC))
Euler_PCvsCC <- unique(intersect(Euler_PC, Euler_CC), intersect(Euler_CC, Euler_PC))
Euler_OCvsCC <- unique(intersect(Euler_OC, Euler_CC), intersect(Euler_CC, Euler_OC))
Euler_all <- Reduce(intersect, list(Euler_PC, Euler_OC, Euler_CC))

Euler_array<- c(A=length(Euler_PC), B=length(Euler_OC), C=length(Euler_CC), "A&B"=length(Euler_PCvsOC), "A&C"=length(Euler_PCvsCC), "B&C"=length(Euler_OCvsCC), "A&B&C"=length(Euler_all))
Euler_authors <- euler(Euler_array, shape="ellipse")

File_P_Euler <- paste(User_dir_save, sprintf("%s_P_Euler.tiff", Topic),sep="\\")

if(save_plots){tiff(File_P_Euler, width=150, height=150, units="mm", res=300, bg="transparent")}
eulerr_options(labels=list(fontsize=30), quantities=list(fontsize=30))
if(show_plots){plot(Euler_authors, fill=c("white", "gray80", "gray70"), labels=c("PC", "OC", "CC"), quantities=TRUE)}
if(save_plots){dev.off()}

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3.7. 3D scatter plot of PC, OC and CC values for authors with Top n TTP values
#----------------------------------------------------------------------------------------------------------------------------------------------------------

x <- D_TTP_top$PC
y <- D_TTP_top$OC
z <- D_TTP_top$degree_all

Box_x_lim <- c(0,max(D_TTP_top$PC)*1.1)
Box_y_lim <- c(0,max(D_TTP_top$OC)*1.1)
Box_z_lim <- c(0,max(D_TTP_top$degree_all)*1.1)

File_P_3D <- paste(User_dir_save, sprintf("%s_P_3D.tiff", Topic),sep="\\")

if(save_plots){tiff(File_P_3D, width=150, height=150, units="mm", res=300, bg="transparent")}

#Plot 3D box with top last author/author
if(show_plots){box3D(x0=0, y0=0, z0=0, x1=x[1], y1=y[1], z1=z[1], lwd=2, bty="b2", ticktype="detailed", col=NA, border=as.character(D_TTP_top$Author_color[1]), phi=15, theta=40, colkey=F, xlim=Box_x_lim, ylim=Box_y_lim, zlim=Box_z_lim, xlab=NA, ylab=NA, zlab=NA, cex.lab=1.5, cex.axis=1.5)
scatter3D(x[1], y[1], z[1], bty="n", col=as.character(D_TTP_top$Author_color[1]), cex=2, pch=16, colkey=F, add=T)
}
#Add values for all other authors
Box_seq <- seq(2, length(x),1)
if(show_plots){for (i in Box_seq)
{box3D(x0=0, y0=0, z0=0, x1=x[i], y1=y[i], z1=z[i], lwd=2, bty="n", col=NA, border=as.character(D_TTP_top$Author_color[i]), add=T)
scatter3D(x[i], y[i], z[i], bty="n", col=as.character(D_TTP_top$Author_color[i]), cex=2, pch=16, colkey=F, add=T)}
}
if(save_plots){dev.off()}

#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3.9. Graph showing field growth based on author entry/exit
#----------------------------------------------------------------------------------------------------------------------------------------------------------

D_rates$Author_rates_sum <- D_rates$Author_rates_new_count-D_rates$Author_rates_end_count
D_rates$Author_rates_cum_sum <- cumsum(D_rates$Author_rates_sum)

File_P_growth <- sprintf("%s_P_growth.tiff", Topic)

P_growth <- ggplot()
P_growth <- P_growth + geom_hline(yintercept=0, color="gray50", size=1)
P_growth <- P_growth + geom_step(data=D_rates, aes(x=PY_bins, y = Author_rates_new_count), size=1.5, color="green3")
P_growth <- P_growth + geom_step(data=D_rates, aes(x=PY_bins, y = Author_rates_end_count), size=1, color="red3")
P_growth <- P_growth + geom_col(data=D_rates[which(D_rates$Author_rates_sum >=0),], aes(x=PY_bins, y = Author_rates_sum), color=NA, fill="green3")
P_growth <- P_growth + geom_col(data=D_rates[which(D_rates$Author_rates_sum <0),], aes(x=PY_bins, y = Author_rates_sum), color=NA, fill="red3")
P_growth <- P_growth + geom_line(data=D_rates, aes(x=PY_bins, y = Author_rates_cum_sum), size=2, color="black")
#P_growth <- P_growth + geom_step(data=D_rates, aes(x=PY_bins, y = Author_rates_single), size=1.5, color="sky blue")
P_growth <- P_growth + theme_bw()
P_growth <- P_growth + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_growth <- P_growth + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_growth <- P_growth + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_growth <- P_growth + scale_x_continuous(limits = c(PY_min, PY_max), breaks = seq(1700, 2040, by = Tick_years))
P_growth <- P_growth + scale_y_continuous(breaks=seq(-50000,50000, by = 200))
if(show_plots){P_growth}
if(save_plots){ggsave(File_P_growth, path = User_dir_save, height = 160, width = Plot_width_AI, units = "mm", dpi=300)}


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3.10. Graphs showing publication periods
#----------------------------------------------------------------------------------------------------------------------------------------------------------

#Restrict analysis to authors with last publication in the PY_end - n 
D_period <- D_all[which(D_all$PY_start <= (max(D_all$PY_end)-1)),]
D_period_rest <- D_period[which(!D_period$Author_name %in% D_off$Author_name & !D_period$Author_name %in% D_CC$Author_name),]
D_period_off <- D_period[which(D_period$Author_name %in% D_off$Author_name & !D_period$Author_name %in% D_CC$Author_name),]
D_period_col <- D_period[which(!D_period$Author_name %in% D_off$Author_name & D_period$Author_name %in% D_CC$Author_name),]
D_period_off_col <- D_period[which(D_period$Author_name %in% D_off$Author_name & D_period$Author_name %in% D_CC$Author_name),]

#Plot Pub period per author
File_P_period <- sprintf("%s_P_period.tiff", Topic)
File_P_period_hist <- sprintf("%s_P_period_hist.tiff", Topic)
File_P_period_hist2 <- sprintf("%s_P_period_hist2.tiff", Topic)
File_P_period_cum_rel <- sprintf("%s_P_period_cum_rel.tiff", Topic)

Pub_period_max <- max(D_period$Pub_period)

P_period <- ggplot()
P_period <- P_period + geom_segment(data=D_period, aes(x = Author_index, xend = Author_index, y = 0, yend = Pub_period, color = Author_color), size = 0.5)
P_period <- P_period + scale_fill_identity()
P_period <- P_period + scale_color_identity()
P_period <- P_period + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_period <- P_period + theme(legend.text = element_text(size = Legend_text_size), legend.background = element_blank(), legend.position =  c(0.8,0.8), legend.title = element_blank(), axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_period <- P_period + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_period <- P_period + scale_x_continuous(limits = c(AI_min, AI_max), breaks = seq(-50000,50000, by=Tick_AI))
P_period <- P_period + scale_y_continuous(limits = c(-1,Pub_period_max+1), breaks = seq(-500,500, by = Tick_pub_period))
if(show_plots){P_period}
if(save_plots){ggsave(File_P_period, path = User_dir_save, height = Plot_div_height, width = Plot_width_AI, units = "mm", dpi=300)}

#Calculate histograms
Period_bins <- seq(1,max(D_period$Pub_period),by=1)
H_period <- hist(D_period$Pub_period, breaks=Period_bins,plot=F, right=F)
H_period_col <- hist(D_period_col$Pub_period, breaks=Period_bins,plot=F, right=F)
H_period_off_col <- hist(D_period_off_col$Pub_period, breaks=Period_bins,plot=F, right=F)
H_period_off <- hist(D_period_off$Pub_period, breaks=Period_bins,plot=F, right=F)
H_period_rest <- hist(D_period_rest$Pub_period, breaks=Period_bins,plot=F, right=F)

D_hist_per <- data.frame(x=floor(H_period_col$mid))
D_hist_per$count <- H_period$count
D_hist_per$density <- H_period$density
D_hist_per$off_count <- H_period_off$count
D_hist_per$off_density <- H_period_off$density
D_hist_per$off_col_count <- H_period_off_col$count
D_hist_per$off_col_density <- H_period_off_col$density
D_hist_per$col_count <- H_period_col$count
D_hist_per$col_density <- H_period_col$density
D_hist_per$rest_count <- H_period_rest$count
D_hist_per$rest_density <- H_period_rest$density

#Histogram of period
P_period_hist <- ggplot()
P_period_hist <- P_period_hist + geom_col(data=D_hist_per, aes(x, density), color="black", fill="black", size=1)
P_period_hist <- P_period_hist + coord_flip()
P_period_hist <- P_period_hist + theme_bw()
P_period_hist <- P_period_hist + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_period_hist <- P_period_hist + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_period_hist <- P_period_hist + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_period_hist <- P_period_hist + scale_x_continuous(limits = c(-1,Pub_period_max+1), breaks = seq(-200,200, by = Tick_pub_period))
P_period_hist <- P_period_hist + scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by=0.25))
if(show_plots){P_period_hist}
if(save_plots){ggsave(File_P_period_hist, path = User_dir_save, height = Plot_div_height, width = Plot_histo_width, units = "mm", dpi=300)}

#Statistical analysis of publication periods
Period_comp <- data.frame(group=c(rep(1, times=nrow(D_period_rest)),
rep(2, times=nrow(D_period_off)),
rep(3, times=nrow(D_period_col)),
rep(4, times=nrow(D_period_off_col))),
period=c(D_period_rest$Pub_period,D_period_off$Pub_period,D_period_col$Pub_period,D_period_off_col$Pub_period))

Per_Kruskal <- kruskal.test(period ~ group, data = Period_comp)
Per_Dunn <- data.frame(dunn.test(Period_comp$period, Period_comp$group, method = "bh", table=T))

#Adjust n per group
Period_adj_n <- min(c(nrow(D_period_off_col),nrow(D_period_col),nrow(D_period_off),nrow(D_period_rest)))

D_period_resta <- D_period_rest[sample(nrow(D_period_rest), Period_adj_n), ]
D_period_offa <- D_period_off[sample(nrow(D_period_off), Period_adj_n), ]
D_period_cola <- D_period_col[sample(nrow(D_period_col), Period_adj_n), ]
D_period_off_cola <- D_period_off_col[sample(nrow(D_period_off_col), Period_adj_n), ]

Period_comp_a <- data.frame(group=c(rep(1, times=nrow(D_period_resta)),
rep(2, times=nrow(D_period_offa)),
rep(3, times=nrow(D_period_cola)),
rep(4, times=nrow(D_period_off_cola))),
period=c(D_period_resta$Pub_period, D_period_offa$Pub_period, D_period_cola$Pub_period, D_period_off_cola$Pub_period))

Per_Kruskal_a <- kruskal.test(period ~ group, data = Period_comp_a)
Per_Dunn_a <- data.frame(dunn.test(Period_comp_a$period, Period_comp_a$group, method = "bh", table=T))

#Save stats
Per_KW <- data.frame(stat=Per_Kruskal$statistic)
Per_KW$df <- Per_Kruskal$parameter
Per_KW$p <- Per_Kruskal$p.value
Per_KW$stat_adj <-  Per_Kruskal_a$statistic
Per_KW$df_adj <- Per_Kruskal_a$parameter
Per_KW$p_adj <- Per_Kruskal_a$p.value

File_per_kw <- paste(User_dir_save, sprintf("%s_TT_stat_per_kw.csv", Topic),sep="\\")
File_per_dunn <- paste(User_dir_save, sprintf("%s_TT_stat_per_dunn.csv", Topic),sep="\\")
File_per_dunn_a <- paste(User_dir_save, sprintf("%s_TT_stat_per_dunn_a.csv", Topic),sep="\\")

write.csv(Per_KW, file=File_per_kw, row.names=F)
write.csv(Per_Dunn, file=File_per_dunn, row.names=F)
write.csv(Per_Dunn_a, file=File_per_dunn_a, row.names=F)

#Define colors for categories
c_rest <- "#cc79a7"
c_off <- "sky blue"
c_col <- "orange"
c_off_col <- "#009e73"

#Cum rel freq of categories
P_period_width <- Pub_period_max*2.5
P_per_cumrel <- ggplot()
P_per_cumrel <- P_per_cumrel + geom_step(data=D_hist_per[which(D_hist_per$rest_density >0),], aes(x, cumsum(rest_density)), size=2.5, color=c_rest)
P_per_cumrel <- P_per_cumrel + geom_step(data=D_hist_per[which(D_hist_per$off_density >0),], aes(x, cumsum(off_density)), size=1.5, color=c_off)
P_per_cumrel <- P_per_cumrel + geom_step(data=D_hist_per[which(D_hist_per$col_density >0),], aes(x, cumsum(col_density)), size=2, color=c_col)
P_per_cumrel <- P_per_cumrel + geom_step(data=D_hist_per[which(D_hist_per$off_col_density >0),], aes(x, cumsum(off_col_density)), size=1.5, color=c_off_col)
P_per_cumrel <- P_per_cumrel + theme_bw()
P_per_cumrel <- P_per_cumrel + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_per_cumrel <- P_per_cumrel + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_per_cumrel <- P_per_cumrel + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_per_cumrel <- P_per_cumrel + scale_x_continuous(limits = c(-2,max(D_period$Pub_period)+1), breaks=seq(0,100, by = Tick_pub_period))
P_per_cumrel <- P_per_cumrel + scale_y_continuous(limits = c(-0.05,1.05), breaks=seq(0,1, by = 0.25))
P_per_cumrel <- P_per_cumrel + coord_cartesian(expand=F)
if(show_plots){P_per_cumrel}
if(save_plots){ggsave(File_P_period_cum_rel, path = User_dir_save, height = Plot_div_height*2, width = P_period_width*1.5, units = "mm", dpi=300)}

#Combine period histogram of all authors with cum rel freq of categories
P_period_hist2 <- ggplot()
P_period_hist2 <- P_period_hist2 + geom_col(data=D_hist_per, aes(x, density), color=NA, fill="black")
P_period_hist2 <- P_period_hist2 + geom_step(data=D_hist_per, aes(x, cumsum(rest_density)), size=2, color=c_rest)
P_period_hist2 <- P_period_hist2 + geom_step(data=D_hist_per, aes(x, cumsum(off_density)), size=1.5, color=c_off)
P_period_hist2 <- P_period_hist2 + geom_step(data=D_hist_per, aes(x, cumsum(col_density)), size=1.5, color=c_col)
P_period_hist2 <- P_period_hist2 + geom_step(data=D_hist_per, aes(x, cumsum(off_col_density)), size=1.5, color=c_off_col)
P_period_hist2 <- P_period_hist2 + theme_bw()
P_period_hist2 <- P_period_hist2 + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = Line_width))
P_period_hist2 <- P_period_hist2 + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_line(color = "black", size = Line_width))
P_period_hist2 <- P_period_hist2 + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_period_hist2 <- P_period_hist2 + scale_x_continuous(limits = c(-0.5,Pub_period_max+0.5), breaks = seq(-200,200, by = Tick_pub_period))
P_period_hist2 <- P_period_hist2 + scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by=0.25))
if(show_plots){P_period_hist2}
if(save_plots){ggsave(File_P_period_hist2, path = User_dir_save, height = 160, width = Plot_width_AI, units = "mm", dpi=300)}


#----------------------------------------------------------------------------------------------------------------------------------------------------------
#3.11. Graphs showing annual counts of newcomers and established authors and of their pubs per category
#----------------------------------------------------------------------------------------------------------------------------------------------------------

Count_new_old_limits <- c(-800,800)
File_P_new_old_rest <- sprintf("%s_P_new_old_rest.tiff", Topic)

P_new_old_rest <- ggplot(data=D_rates)
P_new_old_rest <- P_new_old_rest + geom_segment(aes(x=PY_min, xend=PY_min, y = 0, yend=200), size=1, color="black")
P_new_old_rest <- P_new_old_rest + geom_col(aes(x=PY_bins, y = Pub_rates_new_rest_count*-1), size=1, color=c_rest, fill=NA)
P_new_old_rest <- P_new_old_rest + geom_col(aes(x=PY_bins, y = Author_rates_new_rest_count*-1), size=1, color=NA, fill=c_rest)
P_new_old_rest <- P_new_old_rest + geom_col(aes(x=PY_bins, y = (Author_rates_new_rest_count-Author_rates_mult_new_rest_count)*-1), size=1, color=NA, fill="grey80")
P_new_old_rest <- P_new_old_rest + geom_col(aes(x=PY_bins, y = Pub_rates_old_rest_count), size=1, color=c_rest, fill=NA)
P_new_old_rest <- P_new_old_rest + geom_col(aes(x=PY_bins, y = Author_rates_old_rest_count), size=1, color=NA, fill=c_rest)
P_new_old_rest <- P_new_old_rest + geom_hline(yintercept=0, color="black", size=1)
P_new_old_rest <- P_new_old_rest + theme_bw()
P_new_old_rest <- P_new_old_rest + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank())
#P_new_old_rest <- P_new_old_rest + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.ticks = element_line(color = "black", size = Line_width),axis.line = element_line(color = "black", size = Line_width))
P_new_old_rest <- P_new_old_rest + theme(axis.ticks = element_blank(), axis.line = element_blank())
P_new_old_rest <- P_new_old_rest + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_new_old_rest <- P_new_old_rest + scale_x_continuous(breaks = seq(1700, 2030, by = Tick_years))
P_new_old_rest <- P_new_old_rest + scale_y_continuous(limits = Count_new_old_limits, breaks = seq(-50000,50000, by = 100))
P_new_old_rest <- P_new_old_rest + coord_flip()
if(show_plots){P_new_old_rest}
if(save_plots){ggsave(File_P_new_old_rest, path = User_dir_save, height =  Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}

File_P_new_old_off <- sprintf("%s_P_new_old_off.tiff", Topic)

P_new_old_off <- ggplot(data=D_rates)
P_new_old_off <- P_new_old_off + geom_col(aes(x=PY_bins, y = Pub_rates_new_off_count*-1), size=1, color=c_off, fill=NA)
P_new_old_off <- P_new_old_off + geom_col(aes(x=PY_bins, y = Author_rates_new_off_count*-1), size=1, color=NA, fill=c_off)
P_new_old_off <- P_new_old_off + geom_col(aes(x=PY_bins, y = (Author_rates_new_off_count-Author_rates_mult_new_off_count)*-1), size=1, fill="grey80")
P_new_old_off <- P_new_old_off + geom_col(aes(x=PY_bins, y = Pub_rates_old_off_count), size=1, color=c_off, fill=NA)
P_new_old_off <- P_new_old_off + geom_col(aes(x=PY_bins, y = Author_rates_old_off_count), size=1, color=NA, fill=c_off)
P_new_old_off <- P_new_old_off + geom_hline(yintercept=0, color="black", size=1)
P_new_old_off <- P_new_old_off + theme_bw()
P_new_old_off <- P_new_old_off + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank())
#P_new_old_off <- P_new_old_off + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.ticks = element_line(color = "black", size = Line_width),axis.line = element_line(color = "black", size = Line_width))
P_new_old_off <- P_new_old_off + theme(axis.ticks = element_blank(), axis.line = element_blank())
P_new_old_off <- P_new_old_off + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_new_old_off <- P_new_old_off + scale_x_continuous(breaks = seq(1700, 2030, by = Tick_years))
P_new_old_off <- P_new_old_off + scale_y_continuous(limits = Count_new_old_limits, breaks = seq(-50000,50000, by = 100))
P_new_old_off <- P_new_old_off + coord_flip()
if(show_plots){P_new_old_off}
if(save_plots){ggsave(File_P_new_old_off, path = User_dir_save, height =  Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}

File_P_new_old_col <- sprintf("%s_P_new_old_col.tiff", Topic)

P_new_old_col <- ggplot(data=D_rates)
P_new_old_col <- P_new_old_col + geom_col(aes(x=PY_bins, y = Pub_rates_new_col_count*-1), size=1, color=c_col, fill=NA)
P_new_old_col <- P_new_old_col + geom_col(aes(x=PY_bins, y = Author_rates_new_col_count*-1), size=1, color=NA, fill=c_col)
P_new_old_col <- P_new_old_col + geom_col(aes(x=PY_bins, y = (Author_rates_new_col_count-Author_rates_mult_new_col_count)*-1), size=1, fill="grey80")
P_new_old_col <- P_new_old_col + geom_col(aes(x=PY_bins, y = Pub_rates_old_col_count), size=1, color=c_col, fill=NA)
P_new_old_col <- P_new_old_col + geom_col(aes(x=PY_bins, y = Author_rates_old_col_count), size=1, color=NA, fill=c_col)
P_new_old_col <- P_new_old_col + geom_hline(yintercept=0, color="black", size=1)
P_new_old_col <- P_new_old_col + theme_bw()
P_new_old_col <- P_new_old_col + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank())
#P_new_old_col <- P_new_old_col + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.ticks = element_line(color = "black", size = Line_width),axis.line = element_line(color = "black", size = Line_width))
P_new_old_col <- P_new_old_col + theme(axis.ticks = element_blank(), axis.line = element_blank())
P_new_old_col <- P_new_old_col + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_new_old_col <- P_new_old_col + scale_x_continuous(breaks = seq(1700, 2030, by = Tick_years))
P_new_old_col <- P_new_old_col + scale_y_continuous(limits = Count_new_old_limits, breaks = seq(-50000,50000, by = 100))
P_new_old_col <- P_new_old_col + coord_flip()
if(show_plots){P_new_old_col}
if(save_plots){ggsave(File_P_new_old_col, path = User_dir_save, height =  Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}

File_P_new_old_off_col <- sprintf("%s_P_new_old_off_col.tiff", Topic)

P_new_old_off_col <- ggplot(data=D_rates)
P_new_old_off_col <- P_new_old_off_col + geom_col(aes(x=PY_bins, y = Pub_rates_new_off_col_count*-1), size=1, color=c_off_col, fill=NA)
P_new_old_off_col <- P_new_old_off_col + geom_col(aes(x=PY_bins, y = Author_rates_new_off_col_count*-1), size=1, color=NA, fill=c_off_col)
P_new_old_off_col <- P_new_old_off_col + geom_col(aes(x=PY_bins, y = (Author_rates_new_off_col_count-Author_rates_mult_new_off_col_count)*-1), size=1, fill="grey80")
P_new_old_off_col <- P_new_old_off_col + geom_col(aes(x=PY_bins, y = Pub_rates_old_off_col_count), size=1, color=c_off_col, fill=NA)
P_new_old_off_col <- P_new_old_off_col + geom_col(aes(x=PY_bins, y = Author_rates_old_off_col_count), size=1, color=NA, fill=c_off_col)
P_new_old_off_col <- P_new_old_off_col + geom_hline(yintercept=0, color="black", size=1)
P_new_old_off_col <- P_new_old_off_col + theme_bw()
P_new_old_off_col <- P_new_old_off_col + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank())
#P_new_old_off_col <- P_new_old_off_col + theme(axis.ticks.length = unit(Tick_length, "cm"), axis.ticks = element_line(color = "black", size = Line_width),axis.line = element_line(color = "black", size = Line_width))
P_new_old_off_col <- P_new_old_off_col + theme(axis.ticks = element_blank(), axis.line = element_blank())
P_new_old_off_col <- P_new_old_off_col + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
P_new_old_off_col <- P_new_old_off_col + scale_x_continuous(breaks = seq(1700, 2030, by = Tick_years))
P_new_old_off_col <- P_new_old_off_col + scale_y_continuous(limits = Count_new_old_limits, breaks = seq(-50000,50000, by = 100))
P_new_old_off_col <- P_new_old_off_col + coord_flip()
if(show_plots){P_new_old_off_col}
if(save_plots){ggsave(File_P_new_old_off_col, path = User_dir_save, height =  Plot_height_PY, width = Plot_width_AI, units = "mm", dpi=300)}


end_time_tot <- Sys.time()

Time_diff_tot <- end_time_tot - start_time_tot

Time_diff_tot
