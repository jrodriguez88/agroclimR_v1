#########################################################
####       READ  DRATE.OUT  and param.out            ####
####     By https://github.com/jrodriguez88          ####
#########################################################

################################
#### Load Requeriments
################################
#knitr:::input_dir()
#library(ggplot2)
#library(tidyverse)
#library(stringr)
#library(magrittr)
#library(data.table)
#library(plyr)
#if(require(openxlsx)==FALSE){install.packages("openxlsx")}


# Work directory  :: #path    <- "C:/Users/nameUser/Desktop/workspace/"
#path    <- "C:/Users/jrespinosa/Dropbox/2017/ORYZA/4. Practica Parametros/CT21375/"
#setwd(path)
#path <- getwd()
#dir.create(paste0(path,"/_OutPut_Df"),showWarnings=F)
#dir.create(paste0(path,"/_OutPut_Graphic"),showWarnings=F)
#dir.create(paste0(path,"/_OutPut_Graphic/_1_Development_Rates_"),showWarnings=F)
#dir.create(paste0(path,"/_OutPut_Graphic/_2_Growth_"),showWarnings=F)
#dir.create(paste0(path,"/_OutPut_Graphic/_3_Partitioning_Factors"),showWarnings=F)
#dir.create(paste0(path,"/_OutPut_Graphic/_4_Partitioning_Factors"),showWarnings=F)

extract_drates_param <- function(exp_names) {

    #file <- "DRATE.OUT"
    #file <- "param.out"
    
    ################################
    ### Create EXP data.frame
    ################################
    # Make Master Table called "exp_df"
    
    #exp_names <- list.files("EXP",pattern = "\\.exp$")
    #exp_names <- str_subset(list.files("EXP",pattern = "\\.exp$"), cultivar)
    exp_df <- exp_names %>%
        str_sub(1,-5) %>% enframe() %>%
        separate(value, c("LOC_ID", "CULTIVAR","PROJECT", "TR_N"), sep = "_") %>%
        mutate(ID = paste0(LOC_ID, TR_N, PROJECT)) %>% select(-1)
    
    
    ################################
    ### Create DVR data.frame       
    ################################
    
    read_DVR_drate <- function(file){
        
        find_DVR <- file %>%
            read_lines() %>%
            str_detect(pattern = "crop development") %>%
            which() 
        
        
        DVR <- list()
        for (i in 1:length(find_DVR)){
            DVR[[i]] <- read_lines(file, skip = find_DVR[i], n_max = 4) %>%
                str_split(pattern = "=") %>% 
                sapply("[",2) %>%
                as.numeric() %>%
                matrix(ncol = 4) %>%
                as_tibble()%>%
                bind_cols(exp_df[i,])
            
        }
        
        #    pmap(list(file=file, skip=find_DVR, n_max=4), read_lines) %>% 
        #        map(., ~ str_split(., pattern = "=")) %>% map(., ~ bind_rows(.)) 
        
        DVR_df <- bind_rows(DVR)
        colnames(DVR_df) <- c("DVRJ", "DVRI", "DVRP", "DVRR", "LOC_ID", "CULTIVAR", "PROJECT", "TR_N","ID")
        
        return(DVR_df[c(9,5:8,1:4)])
        
    }
    DVR_df <<- read_DVR_drate("DRATE.OUT")
    #str(DVR)
    
    ################################
    ### Create PHEN data.frame
    ################################
    
    read_PHEN_drate <- function(file){
        find_TS <- file %>%
            read_lines() %>%
            str_detect(pattern = "TSTR") %>%
            which()
        
        TS <- list()
        for (i in 1:length(find_TS)){
            TS[[i]] <- read_lines(file, skip = find_TS[i], n_max = 1) %>%
                str_split(pattern = ",")
            
            TS[[i]] <- as.data.frame(do.call("rbind",TS[[i]]))
        } 
        
        GDD <- list()
        for (i in 1:length(find_TS)){
            GDD[[i]] <- read_lines(file, skip = find_TS[i]+2, n_max = 1) %>%
                str_split(pattern = ",")
            GDD[[i]] <- as.data.frame(do.call("rbind",GDD[[i]]))
        } 
        
        DAE <- list()
        for (i in 1:length(find_TS)){
            DAE[[i]] <- read_lines(file, skip = find_TS[i]+4, n_max = 1) %>%
                str_split(pattern = ",")
            DAE[[i]] <- as.data.frame(do.call("rbind",DAE[[i]]))
        } 
        PHEN <- list()
        for (i in 1:length(find_TS)) {
            PHEN[[i]] <- cbind(TS[[i]], GDD[[i]], DAE[[i]]) %>%
                bind_cols(exp_df[i,])
        }
        
        
        
        PHEN_df <- bind_rows(PHEN)%>%
            `colnames<-`(c("TSTR", "TSPI", "TSF",  "TSM",
                           "TGDDTR","TGDDPI","TGDDF","TGDDM",
                           "DAETR" , "DAEPI" , "DAEF"  , "DAEM" ,
                           "LOC_ID", "CULTIVAR", "PROJECT", "TR_N", "ID")) %>%
            mutate_at(1:12, as.character) %>% mutate_at(1:12, as.numeric)
        
        return(PHEN_df[c(17, 13:16, 1:12)])
        
    } 
    PHEN_df <<- read_PHEN_drate("DRATE.OUT")
    #str(PHEN_df)
    
    ###############################
    ### Create Biomass data.frame
    ###############################
    
    read_biomass_param <- function(file){
        
        find_BM <- file %>%
            read_lines() %>%
            str_detect(pattern ="biomass values") %>%
            which()
        end_BM <- file %>%
            read_lines()%>%
            str_detect(pattern = " Observed LAI values")%>%
            which()%>%-1
        
        BM <- list()
        for (i in 1:length(find_BM)){
            
            BM[[i]] <- suppressWarnings(fread(file, skip = find_BM[i], nrows = end_BM[i]-find_BM[i], showProgress = FALSE)) %>%
                as_tibble() %>%
                mutate(DVS  = as.numeric(DVS), 
                       WLVG = as.numeric(WLVG), 
                       WLVD = as.numeric(WLVD),
                       WST  = as.numeric(WST),
                       WSO  = as.numeric(WSO),
                       ID= exp_df$ID[i])
        }
        
        BM_df <- bind_rows(BM) %>% left_join(exp_df, by="ID")
        
        return(BM_df[c(6:10, 1:5)])
        
    }
    BMASS_df <<- read_biomass_param("param.out")
    #str(Biomass)
    
    ###############################
    ### Create Biomass Partition data.frame
    ###############################
    
    read_biomass_partition_param <- function(file){
        
        find_PF <- file %>%
            read_lines() %>%
            str_detect(pattern ="FSO") %>%
            which()%>%-1
        end_PF <- file %>%
            read_lines() %>%
            str_detect(pattern = "Calculated relative death rate leaves ") %>%
            which()%>%-1
        
        
        PF <- list()
        for (i in 1:length(find_PF)){
            
            PF[[i]] <- suppressWarnings(fread(file, skip = find_PF[i] , nrows = end_PF[i]-find_PF[i], showProgress = FALSE)) %>%
                mutate(DVSM  = as.numeric(DVSM), 
                       FLV = as.numeric(FLV), 
                       FST = as.numeric(FST),
                       FSO  = as.numeric(FSO),
                       ID= exp_df$ID[i])
        }
        
        PF_df <- bind_rows(PF) %>% left_join(exp_df, by="ID")
        
        return(PF_df)
        
    }
    BPART_df <<- read_biomass_partition_param("param.out")
    #str(BPartition_f)
    
    ###############################
    ### Create LAI data.frame
    ###############################
    
    read_LAI_param <- function(file){
        find_LAI <- file %>%
            read_lines() %>%
            str_detect(pattern = "Observed LAI") %>%
            which()
        
        End_LAI <- file %>%
            read_lines() %>%
            str_detect(pattern = "Calculated ORYZA1") %>%
            which() %>%
            -2
        LAI <- list()
        for (i in 1:length(find_LAI)){
            LAI[[i]] <- suppressMessages(fread(file, skip = find_LAI[i], nrows = End_LAI[i]-find_LAI[i])) %>%
                as_tibble()%>%
                mutate(ID=exp_df$ID[i])
            
            if (nrow(LAI[[i]])<1){ 
                LAI[[i]]<- data.frame(DVS=NA, LAI=NA) %>%
                    mutate(ID=exp_df$ID[i])
            }
            
            
        } #Extract LAI from param.out --> list()
        
        LAI_df <- bind_rows(LAI) %>% left_join(exp_df, by="ID") %>% select(
            c("DVS", "LAI","ID","LOC_ID", "CULTIVAR", "PROJECT", "TR_N")) %>% 
            na.omit()
        #        na.omit()
        #    `colnames<-`(c("DVS", "LAI","ID","LOC_ID", "CULTIVAR", "PROJECT", "TR_N"))
        
        #   as.numeric(gsub("([0-9]+).*$", "\\1", LAI_df$DVS))
        return(LAI_df[c(3:7, 1:2)])
        
    }
    LAI_df <<- read_LAI_param("param.out")
    #str(LAI)
    #LAI_SLA <- dplyr::left_join(LAI, Biomass, by="ID")
    #file <- "DRATE.OUT"
    
    
    
    ###############################
    ### Create FSTR data.frame
    ###############################
    
    read_FSTR_param <- function(file) {
        
        find_FSTR <- file %>%
            read_lines() %>%
            str_detect(pattern = "FSTR") %>%
            which()-1
        
        pmap(list(file=file, skip=find_FSTR, nrows=1), fread) %>%
            bind_rows() %>%
            bind_cols(exp_df)
    }
    FSTR_df <<- read_FSTR_param("param.out")
    
    
    ###############################
    ### Create DRLV data.frame
    ###############################
    
    read_DRLV_param <- function(file) {
        
        find_DRLV <- file %>%
            read_lines() %>%
            str_detect(pattern = "DRLV") %>%
            which()-1
        
        drlv_n <- file %>%
            read_lines() %>%
            str_detect(pattern = "Calculated fraction stem reserves") %>%
            which()-2
        
        drlv_list <- pmap(list(file=file, skip=find_DRLV, nrows=drlv_n - find_DRLV), fread) 
        names(drlv_list) <- exp_df$ID
        
        drlv_list %>% bind_rows(.id="ID") %>%
            left_join(exp_df, by="ID")
    }
    DRLV_df <<- read_DRLV_param("param.out")   
    
    ##############################
    ### Create csv files
    ##############################
    
    #file.names <-list("DVR_df", "PHEN_df", "BMASS_df", "BPART_df", "LAI_df")
    write.csv.df <- function(df){
        df.name <- deparse(substitute(df))
        write.csv(df, paste0(path,"//_OutPut_Df//",df.name,".csv"),row.names=F,quote =F)
    }
    #    write.csv.df(DVR_df)
    #    write.csv.df(PHEN_df)
    #    write.csv.df(BMASS_df)
    #    write.csv.df(BPART_df)
    #    write.csv.df(LAI_df)
        
    
}


#write.xlsx(PHEN_df,  file=paste0(path,"//_OutPut_Df//",DVR.m$CULTIVAR[1],".xlsx"), sheetName="sheet1", row.names=FALSE)
#write.xlsx(DVR_df,   file=paste0(path,"//_OutPut_Df//",DVR.m$CULTIVAR[1],".xlsx"), sheetName="sheet2", append=TRUE, row.names=FALSE)
#write.xlsx(BMASS_df, file=paste0(path,"//_OutPut_Df//",DVR.m$CULTIVAR[1],".xlsx"), sheetName="sheet3", append=TRUE, row.names=FALSE)
#write.xlsx(BPART_df, file=paste0(path,"//_OutPut_Df//",DVR.m$CULTIVAR[1],".xlsx"), sheetName="sheet4", append=TRUE, row.names=FALSE)
#write.xlsx(LAI_df,   file=paste0(path,"//_OutPut_Df//",DVR.m$CULTIVAR[1],".xlsx"), sheetName="sheet5", append=TRUE, row.names=FALSE)

#list_of_datasets <- list("DVR_df"=DVR_df, "PHEN_df="=PHEN_df, "BMASS_df"=BMASS_df, "BPART_df"=BPART_df, "LAI_df"=LAI_df)
#write.xlsx(list_of_datasets, file = "writeXLSX2.xlsx")
