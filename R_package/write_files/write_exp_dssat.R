#### Function to make/write Experimental file (X file) for DSSAT model
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/
# 2021

# load libraies

library(tidyverse)
library()


#inut_data_list <-
write_exp_dssat(path, id_name, crop, cultivar, soil, wth_file, planting_details, irri = T, fert = T, sim_dates = list(start_date, planting_date, emergence_date)){
  
  options(encoding = "UTF-8")
  
  general <- list(DETAILS = details,
                  PEOPLE = people,
                  addres = "CIAT",
                  site = "CALI")
  
  
  
  
}

# Globa; encoding
options(encoding = "UTF-8")

##############################
#*EXP.DETAILS:
#*############################


  
write_details <- function(name_exp, information){
  
  

  
  cat(paste0(information$DETAILS, "\n"), file = name_exp)
  cat("\n",file = name_exp)
  cat("*GENERAL\n@PEOPLE\n", file = name_exp)
  cat(paste(sprintf("%-12s", as.character(information$PEOPLE)), "\n", sep = ""), file = name_exp)
  cat("@ADDRESS\n", file = name_exp)
  cat(paste(sprintf("%-12s", as.character(information$ADDRESS)), "\n", sep = ""), file = name_exp)
  cat("@SITE\n", file = name_exp)
  cat(paste(sprintf("%-12s", as.character(information$SITE)), "\n", sep = ""), file = name_exp)
  
}

########################
#*TREATMENTS 
#*######################
#*
# IC <- 0         # 1 if its necesarry run the experimental with initial conditions, 0 if its not necessary to run
#                   the experimental with initial conditions

# MI <- 0         # 1 turn on field for irrigation level, 0 turn off field for irrigation level
# MF <- 0         # 1 turn on field for fertilizier level, 0 turn off field for fertilizier level
# MH <- 0         # 1 turn on field for harvest level, 0 turn off field for harvest level
# FL <- 1:200

make_treatments <- function(IC, MI, MF, MH){
  
  # Treatments
  
  # *TREATMENTS                        -------------FACTOR LEVELS------------
  # @N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM
  #  1 1 0 0 P1                         1  1  0  1  1  0  1  0  0  0  0  1  1
  
  treatments <- data.frame(N = 1:99, R = 1, O = 0, C = 0, TNAME = "CIAT",
                           CU = 1, FL = 1:99, SA = 0, IC, MP = 1,
                           MI, MF, MR = 0, MC = 0, MT = 0, ME = 0, MH, SM = 1)
  
  
  return(treatments)
  
}


write_treatments <- function(name_exp, information){
  
  cat("*TREATMENTS                        -------------FACTOR LEVELS------------\n", file = name_exp)
  cat("@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM\n", file = name_exp)
  for (i in 1:nrow(information)) {
    
    cat(paste(sprintf("%1$2d%2$2d%3$2d%4$2d",as.integer(information$N[i]),as.integer(information$R[i]),
                      as.integer(information$O[i]),as.integer(information$C[i])),
              " ",sprintf("%1$-25s%2$3d%3$3d%4$3d%5$3d%6$3d%7$3d%8$3d%9$3d%10$3d%11$3d%12$3d%13$3d%14$3d",information$TNAME[i],
                          as.integer(information$CU[i]),as.integer(information$FL[i]),as.integer(information$SA[i]),
                          as.integer(information$IC[i]),as.integer(information$MP[i]),as.integer(information$MI[i]),
                          as.integer(information$MF[i]),as.integer(information$MR[i]),as.integer(information$MC[i]),
                          as.integer(information$MT[i]),as.integer(information$ME[i]),as.integer(information$MH[i]),
                          as.integer(information$SM[i])),
              "\n", sep = ""), file = name_exp)
    
  }
  cat("\n", file = name_exp)
  
}

#####################################
#*CULTIVARS
#*###################################

# Parameters
# CR <- 'MZ'    # Crop Code, you need to search this parameter for de manual DSSAT (its different by crop)
# INGENO <- 'CI0027' # Cultivar indentifier, this is the code for cultivar to run depend of crop
# CNAME <- 'PIO 30F35HRB_'  # Whatever code to identify the cultivar ran, maybe no too long string

make_cultivars <- function(CR, INGENO, CNAME){
  
  
  cultivars <- data.frame(C = 1 , CR, INGENO, CNAME)
  
  return(cultivars)
  
}

write_cultivars <- function(name_exp, information){
  
  cat("*CULTIVARS\n", file = name_exp)
  cat("@C CR INGENO CNAME\n", file = name_exp)
  for (i in 1:nrow(information)) {
    cat(paste(sprintf("%2d",as.integer(information$C[i]))," ",sprintf("%2s", information$CR[i]),
              " ", sprintf("%6s",information$INGENO[i])," ",sprintf("%-12s",information$CNAME[i]),
              "\n", sep = ""), file = name_exp)
  }
  cat("\n", file = name_exp)
  
}

#################################
#*FIELDS
#*###############################

# Parameters
# WSTA <- 'CCCR8000' # Weather Station Code, its the same code to using in WTH file
# ID_SOIL <- 'CCBuga0001' # Id soil to using in the SOIL.SOl



make_fields <- function(WSTA, ID_SOIL){
  
  
  # *FIELDS
  # @L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME
  #  1 -99      CCBR1502   -99   -99 DR000   -99   -99     0 SL      30  CCBuga0001 Calibracion
  # 
  fields <- data.frame(L = 1:99, ID_FIELD = "USAID", WSTA, FLSA = -99, FLOB = -99, FLDT = "DR000",
                       FLDD = -99, FLDS = -99, FLST = -99, SLTX = -99, SLDP = -99, ID_SOIL,
                       FLNAME = "FIELD01", XCRD = -99, YCRD = -99, ELEV = -99, AREA = -99, SLEN=-99,
                       FLWR = -99, SLAS = -99, FLHST = -99, FHDUR=-99)
  
  
  return(fields)
  
}

write_fields <- function(name_exp, information){
  
  # fields
  cat("*FIELDS\n", file = name_exp)
  cat("@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME\n", file = name_exp)
  for (i in 1:nrow(information)) {
    
    cat(paste(sprintf("%2d",as.integer(information$L[i]))," ",sprintf("%-8s",information$ID_FIELD[i]),
              " ",sprintf("%-8s",information$WSTA[i]),sprintf("%6d",as.integer(information$FLSA[i])),
              sprintf("%6d",as.integer(information$FLOB[i])),sprintf("%6s",information$FLDT[i]),
              sprintf("%6d",as.integer(information$FLDD[i])),sprintf("%6s",as.integer(information$FLDS[i])),
              sprintf("%6d",as.integer(information$FLST[i]))," ",sprintf("%-4d",as.integer(information$SLTX[i])),
              sprintf("%6d",as.integer(information$SLDP[i])),"  ",sprintf("%-10s",information$ID_SOIL[i])," ",
              sprintf("%-12s",information$FLNAME[i]),"\n", sep=""),file=name_exp)
    
  }
  
  cat("\n", file = name_exp)
  
  
  cat("@L ..........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR\n",file=name_exp)
  # 
  # for (i in 1:nrow(information)) { 
  #   
  #   
  #   cat(paste(sprintf("%2d",as.integer(information$L[i]))," ",sprintf("%15.3f",information$XCRD[i])," ",
  #             sprintf("%15.3f",information$YCRD[i])," ",sprintf("%9d",as.integer(information$ELEV[i]))," ",
  #             sprintf("%17d",as.integer(information$AREA[i]))," ",sprintf("%5d",as.integer(information$SLEN[i]))," ",
  #             sprintf("%5d",as.integer(information$FLWR[i]))," ",sprintf("%5d",as.integer(information$SLAS[i]))," ",
  #             sprintf("%5d",as.integer(information$FLHST[i]))," ",sprintf("%5d",as.integer(information$FHDUR[i])),
  #             "\n",sep=""),file=name_exp)
  #   
  #   
  # }
  
  for (i in 1:nrow(information)) { 
    
    
    cat(paste(sprintf("%2d",as.integer(information$L[i]))," ",sprintf("%14.d",information$XCRD[i])," ",
              sprintf("%15.d",information$YCRD[i])," ",sprintf("%9d",as.integer(information$ELEV[i]))," ",
              sprintf("%17d",as.integer(information$AREA[i]))," ",sprintf("%5d",as.integer(information$SLEN[i]))," ",
              sprintf("%5d",as.integer(information$FLWR[i]))," ",sprintf("%5d",as.integer(information$SLAS[i]))," ",
              sprintf("%5d",as.integer(information$FLHST[i]))," ",sprintf("%5d",as.integer(information$FHDUR[i])),
              "\n",sep=""), file=name_exp)
    
    
  }
  
  
  cat("\n",file=name_exp)
  
}


###############################
#*PLANTING DETAILS
#*#############################

# input_pDetails <- list()
# input_pDetails$PDATE <- 80092 # Planting date
# input_pDetails$SDATE <- pmax(input_pDetails$PDATE - 20, 0)   ## Starting simulation. 20 before planting date
# input_pDetails$plant <- 'R'  # R = planting on reporting date
## Remember Simulation date starts 20 days before planting date
# input_pDetails$EDATE <- -99
# input_pDetails$PPOP <- 6.25
# input_pDetails$PPOE <- 6.25
# input_pDetails$PLME <- 'S'
# input_pDetails$PLDS <- 'R'
# input_pDetails$PLRS <- 80
# input_pDetails$PLRD <- 90
# input_pDetails$PLDP <- 4
## Variables como PLWT, PAGE, PENV, PLPH, SPRL con -99

make_pDetails <- function(input_pDetails){
  
  
  PDATE <- input_pDetails$PDATE 
  SDATE <- input_pDetails$SDATE    
  plant <- input_pDetails$plant   
  EDate <- input_pDetails$EDATE  
  PPOP <- input_pDetails$PPOP  
  PPOE <- input_pDetails$PPOE 
  PLME <- input_pDetails$PLME  
  PLDS <- input_pDetails$PLDS  
  PLRS <- input_pDetails$PLRS  
  PLRD <- input_pDetails$PLRD  
  PLDP <- input_pDetails$PLDP  
  
  
  planting <- data.frame( P = 1, PDATE, EDATE = -99, PPOP, PPOE, PLME, 
                          PLDS, PLRS = 80, PLRD, PLDP,
                          PLWT = -99, PAGE = -99, PENV = -99, PLPH = -99, SPRL = -99)
  
  
  return(planting)
  
  
}

write_pDetails <- function(name_exp, information){
  
  #planting details
  cat("*PLANTING DETAILS\n",file = name_exp)
  cat("@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME\n",file=name_exp)
  cat(paste(sprintf("%2d",as.integer(information$P))," ",sprintf("%5s",information$PDATE),
            " ",sprintf("%5s",information$EDATE)," ",sprintf("%5d",as.integer(information$PPOP)),
            " ",sprintf("%5s",as.numeric(as.character(information$PPOE)))," ",sprintf("%5s",information$PLME),
            " ",sprintf("%5s",information$PLDS)," ",sprintf("%5d",as.integer(information$PLRS)),
            " ",sprintf("%5d",as.integer(information$PLRD))," ",sprintf("%5d",as.integer(information$PLDP)),
            " ",sprintf("%5d",as.integer(information$PLWT))," ",sprintf("%5d",as.integer(information$PAGE)),
            " ",sprintf("%5d",as.integer(information$PENV))," ",sprintf("%5d",as.integer(information$PLPH)),
            " ",sprintf("%5d",as.integer(information$SPRL))," ",sprintf("%29s",information$PLNAME),
            "\n", sep = ""), file = name_exp)
  cat("\n", file = name_exp)
  
}


#########################################
#*IRRIGATION AND WATER MANAGEMENT
#*#######################################

#*IRRIGATION AND WATER MANAGEMENT
#@I  EFIR  IDEP  ITHR  IEPT  IOFF  IAME  IAMT IRNAME
#1     1    30    50   100 GS000 IR001    10 -99
#@I IDATE  IROP IRVAL
#1 97032   -99   -99



#*FERTILIZERS (INORGANIC) -- 
#*

#*FERTILIZERS (INORGANIC)
#@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME
#1     1 FE005 AP001     1    30   -99   -99   -99   -99   -99 fertApp
#1    25 FE006 AP001     1    10    20   -99   -99   -99   -99 fertApp
#1    40 FE028 AP001     1    10    30    10   -99   -99   -99 fertApp
#1    65 FE027 AP001     1    15    15    15   -99   -99   -99 fertApp



#####################################
#*SIMULATION CONTROLS
#*###################################

# Simulation Controls

# input_sControls <- list()
# input_sControls$NYERS <- 20 ## Years for simulation
# input_sControls$SMODEL <- 'MZCER046' # model to use
# input_sControls$WATER <- 'N'   ## Y = Utiliza balance Hidrico, N = No utiliza balance hidrico
# input_sControls$NITRO <-  'N'  ## Y = utiliza balance nitrogeno, N =  no utiliza balance nitrogeno
# input_sControls$PLANT <- 'R'  # R = planting on reporting date ## Add the other options
# input_sControls$IRRIG <- 'R'  ##  on reporting date, A automatically irragated, N Nothing, add the other options
# input_sControls$FERTI = 'N' ## add more options
# input_sControls$SDATE <- pmax(input_pDetails$PDATE - 20, 0)

make_sControls <- function(input_sControls, PDATE){
  
  ## 
  
  NYERS <- input_sControls$NYERS 
  SMODEL <-input_sControls$SMODEL
  WATER <- input_sControls$WATER 
  NITRO <- input_sControls$NITRO
  PLANT <- input_sControls$PLANT
  IRRIG <- input_sControls$IRRIG
  FERTI <- input_sControls$FERTI 
  SDATE <- input_sControls$SDATE
  
  
  sim_ctrl <- data.frame(N = 1, GENERAL = "GE", NYERS, NREPS = 1, START = "S", SDATE,
                         RSEED = 2150, SNAME = "simctr1", SMODEL,
                         OPTIONS = "OP", WATER, NITRO, SYMBI = "N",
                         PHOSP = "N", POTAS = "N", DISES = "N", CHEM = "N", TILL = "N",
                         CO2 = "N", METHODS = "ME", WTHER = "M", INCON = "M", LIGHT = "E",
                         EVAPO = "R", INFIL = "S", PHOTO = "C", HYDRO = "R",
                         NSWIT = 1, MESOM = "G", MESEV = "S", MESOL =2, MANAGEMENT = "MA",
                         PLANT, IRRIG,
                         FERTI, RESID = "R", HARVS = "M", OUTPUTS = "OU", FNAME = "N",
                         OVVEW = "N", SUMRY = "Y", FROPT = 1, GROUT = "Y", CAOUT = "N",
                         WAOUT = "N", NIOUT = "N", MIOUT = "N",
                         DIOUT = "N", VBOSE = "N", CHOUT = "N", OPOUT = "N")
  
  
  return(sim_ctrl)
  
}


write__sControls <- function(name_exp, information){
  
  #simulation controls
  cat("*SIMULATION CONTROLS\n", file = name_exp)
  cat("@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL\n", file = name_exp)
  cat(paste(sprintf("%2d",as.integer(information$N))," ",sprintf("%-11s",information$GENERAL),
            " ",sprintf("%5d",as.integer(information$NYERS))," ",sprintf("%5d",as.integer(information$NREPS)),
            " ",sprintf("%5s",information$START)," ",sprintf("%5s",information$SDATE),
            " ",sprintf("%5d",as.integer(information$RSEED))," ",sprintf("%-25s",information$SNAME),
            " ",sprintf("%-6s",information$SMODEL),"\n",sep=""),file=name_exp)
  cat("@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2\n",file=name_exp)
  cat(paste(sprintf("%2d",as.integer(information$N))," ",sprintf("%-11s",information$OPTIONS),
            " ",sprintf("%5s",information$WATER)," ",sprintf("%5s",information$NITRO),
            " ",sprintf("%5s",information$SYMBI)," ",sprintf("%5s",information$PHOSP),
            " ",sprintf("%5s",information$POTAS)," ",sprintf("%5s",information$DISES),
            " ",sprintf("%5s",information$CHEM)," ",sprintf("%5s",information$TILL),
            " ",sprintf("%5s",information$CO2),"\n",sep=""),file=name_exp)
  cat("@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL\n",file=name_exp)
  cat(paste(sprintf("%2d",as.integer(information$N))," ",sprintf("%-11s",information$METHODS),
            " ",sprintf("%5s",information$WTHER)," ",sprintf("%5s",information$INCON),
            " ",sprintf("%5s",information$LIGHT)," ",sprintf("%5s",information$EVAPO),
            " ",sprintf("%5s",information$INFIL)," ",sprintf("%5s",information$PHOTO),
            " ",sprintf("%5s",information$HYDRO)," ",sprintf("%5d",as.integer(information$NSWIT)),
            " ",sprintf("%5s",information$MESOM)," ",sprintf("%5s",information$MESEV),
            " ",sprintf("%5d",as.integer(information$MESOL)),"\n",sep=""),file=name_exp)
  cat("@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS\n",file=name_exp)
  cat(paste(sprintf("%2d",as.integer(information$N))," ",sprintf("%-11s",information$MANAGEMENT),
            " ",sprintf("%5s",information$PLANT)," ",sprintf("%5s",information$IRRIG),
            " ",sprintf("%5s",information$FERTI)," ",sprintf("%5s",information$RESID),
            " ",sprintf("%5s",information$HARVS),"\n",sep=""),file=name_exp)
  cat("@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT\n",file=name_exp)
  cat(paste(sprintf("%2d",as.integer(information$N))," ",sprintf("%-11s",information$OUTPUTS),
            " ",sprintf("%5s",information$FNAME)," ",sprintf("%5s",information$OVVEW),
            " ",sprintf("%5s",information$SUMRY)," ",sprintf("%5s",information$FROPT),
            " ",sprintf("%5s",information$GROUT)," ",sprintf("%5s",information$CAOUT),
            " ",sprintf("%5s",information$WAOUT)," ",sprintf("%5s",information$NIOUT),
            " ",sprintf("%5s",information$MIOUT)," ",sprintf("%5s",information$DIOUT),
            " ",sprintf("%5s",information$VBOSE)," ",sprintf("%5s",information$CHOUT),
            " ",sprintf("%5s",information$OPOUT),"\n",sep=""),file=name_exp)
  cat("\n", file = name_exp)
  
  
}

#######################################
#@  AUTOMATIC MANAGEMENT
#######################################

# PFRST <- -99
# PLAST <- -99
# system <- 'irrigation'  ## Solo se habilita con IC = 1 (irrigation es como condiciones inicial del suelo menos drasticas) 
# este campo para futuras simulaciones o proyectos


make_Amgmt <- function(PFRST, PLAST){
  # PSTMX PSTMN
  auto_mgmt <- data.frame(N = 1, PLANTING = 'PL', PFRST, PLAST, PH2OL = 50, PH2OU = 100,
                          PH2OD = 30, PSTMX = 40, PSTMN = 10, IRRIGATION = "IR", IMDEP =30, ITHRL = 50, 
                          ITHRU =100, IROFF = "GS000", IMETH = "IR001", IRAMT = 10, IREFF = 1,
                          NITROGEN = "NI", NMDEP = 30, NMTHR = 50, NAMNT = 25, NCODE = "FE001",
                          NAOFF = "GS000", RESIDUES = "RE", RIPCN = 100, RTIME = 1, RIDEP = 20, 
                          HARVEST = "HA", HFRST = 0, HLAST = 00001, HPCNP = 100, HPCNR = 0)
  
  return(auto_mgmt) 
}

write_Amgmt <- function(name_exp, information){
  
  
  cat("@  AUTOMATIC MANAGEMENT\n", file = name_exp)
  cat("@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN\n", file = name_exp)
  
  cat(paste(sprintf("%2d",as.integer(information$N))," ",sprintf("%-11s",information$PLANTING),
            " ",sprintf("%5s",information$PFRST)," ",sprintf("%5s",information$PLAST),
            " ",sprintf("%5d",as.integer(information$PH2OL))," ",sprintf("%5d",as.integer(information$PH2OU)),
            " ",sprintf("%5d",as.integer(information$PH2OD))," ",sprintf("%5d",as.integer(information$PSTMX)),
            " ",sprintf("%5d",as.integer(information$PSTMN)),"\n",sep=""),file=name_exp)
  
  cat("@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF\n",file=name_exp)
  cat(paste(sprintf("%2d",as.integer(information$N))," ",sprintf("%-11s",information$IRRIGATION),
            " ",sprintf("%5d",as.integer(information$IMDEP))," ",sprintf("%5d",as.integer(information$ITHRL)),
            " ",sprintf("%5d",as.integer(information$ITHRU))," ",sprintf("%5s",information$IROFF),
            " ",sprintf("%5s",information$IMETH)," ",sprintf("%5d",as.integer(information$IRAMT)),
            " ",sprintf("%5d",as.integer(information$IREFF)),"\n",sep=""),file=name_exp)
  
  cat("@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF\n",file=name_exp)
  cat(paste(sprintf("%2d",as.integer(information$N))," ",sprintf("%-11s",information$NITROGEN),
            " ",sprintf("%5d",as.integer(information$NMDEP))," ",sprintf("%5d",as.integer(information$NMTHR)),
            " ",sprintf("%5d",as.integer(information$NAMNT))," ",sprintf("%5s",information$NCODE),
            " ",sprintf("%5s",information$NAOFF),"\n",sep=""),file=name_exp)
  cat("@N RESIDUES    RIPCN RTIME RIDEP\n",file=name_exp)
  cat(paste(sprintf("%2d",as.integer(information$N))," ",sprintf("%-11s",information$RESIDUES),
            " ",sprintf("%5d",as.integer(information$RIPCN))," ",sprintf("%5d",as.integer(information$RTIME)),
            " ",sprintf("%5d",as.integer(information$RIDEP)),"\n",sep=""),file=name_exp)
  cat("@N HARVEST     HFRST HLAST HPCNP HPCNR\n",file=name_exp)
  cat(paste(sprintf("%2d",as.integer(information$N))," ",sprintf("%-11s",information$HARVEST),
            " ",sprintf("%5d",as.integer(information$HFRST))," ",sprintf("%5d",as.integer(information$HLAST)),
            " ",sprintf("%5d",as.integer(information$HPCNP))," ",sprintf("%5d",as.integer(information$HPCNR)),
            "\n",sep=""),file=name_exp)
  
  cat("\n", file = name_exp)
  
}








make_xfile_region <- function(dir_parameters, WSTA, filename, PDATE, SDATE, cultivar, ID_SOIL){ 
  
  
  
  
  
  
  ## Parameters necessary to write experimental file
  
  
  out_file <- filename     #./proof.MZX
  overwrite <- F
  details <- '*CIAT project Agroclimatic forecasts'
  people <- "Rodriguez-Espinoza, J. Mesa, J. and Ramirez-Villegas, J."
  
  
  
  
  IC <- 0  # Inital conditions
  MI <- 0  # input if you are going to use a irrigation, 1 = TRUE, 0 = FALSe 
  MF <- 0 # Fertilization field, 1 = TRUE, 0 = FALSE
  MH <- 0 # its necessary to include harvest date when you turn on this parameter
  
  # # depends of the inputs
  # if(file.exists(paste0(dir_parameters, 'irrigation.csv'))){
  #   
  #   irrigation_head <- read_csv(paste0(dir_parameters, 'irrigation_head.csv'))
  #   irrigation <- read_csv(paste0(dir_parameters, 'irrigation.csv'))
  #   MI <- 0
  #   
  # }
  
  
  
  CR <- 'MZ'    # Crop Code, you need to search this parameter for de manual DSSAT (its different by crop)
  INGENO <- cultivar # Cultivar indentifier, this is the code for cultivar to run depend of crop
  CNAME <- 'PIO 30F35HRB_'  # Whatever code to identify the cultivar to run, maybe no too long string
  
  # ICBL <- c(25, 45, 95)
  # SH20 <- -99
  # SNH4 <- c(4.2, 4.4, 4.5)    # estas variables se pueden investigar cuales utilizar cuando no se tiene condiciones iniciales
  # SNO3 <- c(11.9, 12.4, 7.6)  # estas variables se pueden investigar cuales utilizar cuando no se tiene condiciones iniciales
  # ICDAT <- -99 #  for now
  
  # input_fertilizer <- list()
  # input_fertilizer$FDATE = c(21, 21, 35, 35, 58)  ## Dias de la aplicacion +
  # input_fertilizer$FMCD = c('FE006', 'FE016', 'FE005', 'FE016', 'FE005') ## Investigar acerca de este parametro
  # input_fertilizer$FACD = 'AP002' ## Investigar acerca de este parametro
  # input_fertilizer$FDEP = 4       ## Profundidad de la aplicacion del nitrogeno
  # input_fertilizer$FAMN = c(33.3, 0, 63.9, 0, 63.9)
  # input_fertilizer$FAMP = c(29.1, 0, 0, 0, 0) ## Investigar mas acerca de este parametro
  # input_fertilizer$FAMK = c(0, 36, 0, 39.2, 0)
  # input_fertilizer$FAMC = 0
  # input_fertilizer$FAMO = 0
  # input_fertilizer$FOCD = 0
  # input_fertilizer$FERNAME = -99
  # input_fertilizer$FERTI = 'D' ## D = dias despues de la siembra, es necesario actualizar con las otras opciones que tiene este parametro
  # 
  
  
  ## doing a comment that explain all parameters
  
  
  input_pDetails <- as.data.frame(read_planting(dir_parameters)) %>%
    frame_list()
  
  
  ## IRRIGATION or RAINFED
  if(input_pDetails$IRR == 'YES'){
    IRR <- 'A'
  } else{
    
    IRR <- 'N'
  }
  
  input_pDetails$PDATE <- PDATE
  input_pDetails$SDATE <- SDATE
  
  # initial_conditions <- setNames(split(input_pDetails[,2], seq(nrow(input_pDetails))), input_pDetails[,1])
  
  # input_pDetails <- list()
  # input_pDetails$PDATE <- PDATE # Planting date
  # input_pDetails$SDATE <- SDATE
  # input_pDetails$plant <- 'R'  # R = planting on reporting date
  # input_pDetails$EDATE <- -99
  # input_pDetails$PPOP <- 6.25
  # input_pDetails$PPOE <- 6.25
  # input_pDetails$PLME <- 'S'
  # input_pDetails$PLDS <- 'R'
  # input_pDetails$PLRS <- 80
  # input_pDetails$PLRD <- 90
  # input_pDetails$PLDP <- 4
  
  
  ## Simulation Controls
  input_sControls <- list()
  input_sControls$NYERS <- 1 ## Years for simulation
  input_sControls$SMODEL <- 'MZCER046' # model to use
  input_sControls$WATER <- 'Y'   ## Y = Utiliza balance Hidrico, N = No utiliza balance hidrico
  input_sControls$NITRO <-  'Y'  ## Y = utiliza balance nitrogeno, N =  no utiliza balance nitrogeno
  input_sControls$PLANT <- 'R'  # R = planting on reporting date ## Add the other options
  input_sControls$IRRIG <- IRR  ##  R =  on reporting date, A automatically irragated, N Nothing, add the other options
  input_sControls$FERTI = 'N' ## add more options
  input_sControls$SDATE <- SDATE
  
  
  PFRST <- -99
  PLAST <- -99
  
  ## escritura del archivo experimental 
  options(encoding = "UTF-8")
  
  proof <- make_archive(out_file, overwrite = F,  encoding = "UTF-8") 
  
  write_details(proof, make_details(details, people))
  write_treatments(proof, make_treatments(IC, MI, MF, MH))  ## the parameter FL its to identify the run with a specific .WTH
  write_cultivars(proof, make_cultivars(CR, INGENO, CNAME))
  write_fields(proof, make_fields(WSTA, ID_SOIL))
  write_pDetails(proof, make_pDetails(input_pDetails))     
  write__sControls(proof, make_sControls(input_sControls))
  write_Amgmt(proof, make_Amgmt(PFRST, PLAST))
  close(proof)
  
  
  
  
}








