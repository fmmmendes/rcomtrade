#' Summary Target Path with COMTRADE files
#'
#' @param target
#'
#' @return
#' @export
#'
#' @examples
summary_target <- function(path){

    df <- data.frame(N = c(0,0,0,0,0,0))
    rownames(df) <- c("N of .DAT files: ",
                      "N of .CFG files: ",
                      "N of .HDR files: ",
                      "N of .INF files: ",
                      "N of pairs files (CFG and DAT): ",
                      "N of COMTRADE in ASCII format: ")

    fls <- list.files(path)

    df[1,1] <- sum(stringr::str_detect(string = fls,pattern = ".[Dd][Aa][Tt]"))
    df[2,1] <- sum(stringr::str_detect(string = fls,pattern = ".[Cc][Ff][Gg]"))
    df[3,1] <- sum(stringr::str_detect(string = fls,pattern = ".[Hh][Dd][Rr]"))
    df[4,1] <- sum(stringr::str_detect(string = fls,pattern = ".[Ii][Nn][Ff]"))

    fls_cfg_dat <- fls[stringr::str_detect(string = fls,
                                           pattern = ".[Dd][Aa][Tt]|.[Cc][Ff][Gg]")]

    df[5,1] <-  sum(duplicated(sub(pattern = ".[Dd][Aa][Tt]|.[Cc][Ff][Gg]",
                                   replacement = "",
                                   x = fls_cfg_dat)))

    df[6,1] <- sum(sapply(X = fls_cfg,FUN = is_ascii, path,simplify = TRUE))

    return(df)

}

#' Check if COMTRADE is ASCII formart
#'
#' This function verify the COMTRADE file formart inside the .CFG file
#'
#' @param files
#' @param path
#'
#' @return
#' @export
#'
#' @examples
is_ascii <- function(files, path){

    cfg <- files[stringr::str_detect(string = files,pattern = ".[Cc][Ff][Gg]")]
    #print(fls_cfg)
    tmp_path <- file.path(path,cfg)
    #print(tmp_path)
    con <- file(tmp_path, "r", blocking = FALSE)
    ln <- readLines(con)
    close(con)

    is_ascii <- any(stringr::str_detect(string = tail(ln),pattern = "ASCII"))

    return(is_ascii)

}

#' Config Reader
#'
#' This function will load a dat file and search for the corresponding
#' cfg file to extract all data info related to the distrubance record.
#'
#'
#'
#' This function is based on IEEE Std C37.111-1999
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
readCFG <- function(file){


    file_path <- dirname(file)
    file_pattern <- sub(pattern = ".[Dd][Aa][Tt]" ,
                        replacement ="" ,
                        x = basename(file))

    files <- list.files(file_path)

    # Dectect with the file pattern
    files <- files[grepl(pattern = file_pattern,x = files)]

    # Dectect with the file with .cfg extension
    file_cfg <- files[grepl(pattern = ".[Cc][Ff][Gg]",x = files)]

    if(length(file_cfg) == 1){

        # Load data
        config <- list()
        config$file_pattern <- file_pattern
        config$file_path <- file_path

        config$substation <- stringr::str_extract(string = file_pattern, pattern = "(([S][A-Z][A-Z][A-Z])|[S][A-Z][A-Z])")
        config$bay_name <- stringr::str_extract(string = file_pattern, pattern = "([P][0-9][0-9][0-9])")
        config$sp_id <- stringr::str_extract(string = file_pattern, pattern = "([S][P][0-9])")

        cfg_data <- read.csv(file = file.path(file_path,file_cfg),
                             header = F,
                             sep = ",",
                             na.strings = T ,
                             stringsAsFactors = F)

        An_col_names <- c("An","ch_id","ph","ccbm","uu","a","b",
                          "skew","min","max","primary","secondary",
                          "PS")

        Dn_col_names <- c("An","ch_id","ph","ccbm","y")

        # Station name, identification of the recording device,
        # and COMTRADE Standard revision year
        config$station_name <- cfg_data$V1[1]
        config$rec_dev_id <- cfg_data$V2[1]
        config$rev_year <- as.numeric(cfg_data$V3[1])

        # Number and type of channels
        config$TT <- as.numeric(cfg_data$V1[2])
        config$A <- as.numeric(strsplit(cfg_data$V2[2],split = "\\D",fixed = F,perl = TRUE)[[1]])[1]
        config$D <- as.numeric(strsplit(cfg_data$V3[2],split = "\\D",fixed = F,perl = TRUE)[[1]])[1]

        # Channel names, units, and conversion factors

        # Analog
        if(config$A != 0){

            row_start <- 3
            row_end <- row_start + config$A - 1
            cfg_data[seq(row_start,row_end),]

            # Analog channel index
            An <- as.numeric(cfg_data[seq(row_start,row_end),"V1"])
            # Channel identifier
            ch_id <- as.character(cfg_data[seq(row_start,row_end),"V2"])
            ch_id <- renameDupli(ch_id)
            # Channel phase identificaation
            ph <- as.character(cfg_data[seq(row_start,row_end),"V3"])
            # Circuit component being monitored
            ccbm <- as.character(cfg_data[seq(row_start,row_end),"V4"])
            # Channel units
            uu <- as.character(cfg_data[seq(row_start,row_end),"V5"])
            # Channel Mutiplier
            a <- as.numeric(cfg_data[seq(row_start,row_end),"V6"])
            # Channel offset adder
            b <- as.numeric(cfg_data[seq(row_start,row_end),"V7"])
            # channel time skew (micro secunds)
            skew <- as.numeric(cfg_data[seq(row_start,row_end),"V8"])
            # Range Minimum data value
            min <- as.numeric(cfg_data[seq(row_start,row_end),"V9"])
            # Range maximum data value
            max <- as.numeric(cfg_data[seq(row_start,row_end),"V10"])
            # Primary ratio
            primary <- as.numeric(cfg_data[seq(row_start,row_end),"V11"])
            if(is.null(cfg_data[seq(row_start,row_end),"V11"])){primary <- NA}
            # Secondary ratio
            secondary <- as.numeric(cfg_data[seq(row_start,row_end),"V12"])
            if(is.null(cfg_data[seq(row_start,row_end),"V12"])){secondary <- NA}
            # Primary or secondary data scaling identifier
            PS <- cfg_data[seq(row_start,row_end),"V13"]
            if(is.null(cfg_data[seq(row_start,row_end),"V13"])){PS <- NA}

            config$analog <- data.frame(An,ch_id,ph,ccbm,uu,a,b,
                                        skew,min,max,primary,secondary,PS)

        }

        # Digital
        if(config$D !=0){

            row_start <- 3 + config$A
            row_end <- row_start + config$D - 1
            cfg_data[seq(row_start,row_end),]

            # Digital channel index
            Dn <- as.numeric(cfg_data[seq(row_start,row_end),"V1"])
            # Channel identifier
            ch_id <- cfg_data[seq(row_start,row_end),"V2"]
            ch_id <- renameDupli(ch_id)
            # Channel phase identification
            ph <- cfg_data[seq(row_start,row_end),"V3"]
            # Circuit component being monitored
            ccbm <- cfg_data[seq(row_start,row_end),"V4"]
            # Circuit component being monitored
            y <- cfg_data[seq(row_start,row_end),"V5"]

            config$digital <- data.frame(Dn,ch_id,ph,ccbm,y)


        }

        row_start <- 3 + config$A + config$D
        row_end <- row_start + 6
        aux <- cfg_data[seq(row_start,row_end),]

        # Line frequency in Hz
        config$lf <- as.numeric(aux[1,"V1"])
        # Sampling rates
        config$nrates <- as.numeric(aux[2,"V1"])
        # Sample rate in Hertz
        config$samp <- as.numeric(aux[3,"V1"])
        # Last sample number at sample rate
        config$endsamp <- as.numeric(aux[3,"V2"])

        # Date and Time
        config$date <- base::strptime(x = aux[4,"V1"],format = "%d/%m/%Y", tz = "GMT")
        config$rec_start <- base::strptime(x = paste(aux[4,"V1"],aux[4,"V2"]),format = "%d/%m/%Y %H:%M:%OS",tz = "GMT")
        config$rec_end <- base::strptime(x = paste(aux[4,"V1"],aux[5,"V2"]),format = "%d/%m/%Y %H:%M:%OS",tz = "GMT")

        #config$rec_time_diff <- config$rec_end - config$rec_start

        # File type
        config$ft <- aux[6,"V1"]
        #Time Multiplier
        config$timemult <- as.numeric(aux[7,"V1"])

        return(config)

    }else{
        # Do nothing
        #print("This is not a Config (.cfg) File")
    }
}
#' DAT reader
#'
#' This function is based on IEEE Std C37.111-1999
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
readDAT <- function(file, file_type = "ASCII"){

    if(stringr::str_detect(file,".[Dd][Aa][Tt]")){

        if(file_type == "ASCII"){

            # Load data
            dat <- read.csv(file = file,header = F,sep = ",",na.strings = T,stringsAsFactors = F)

            colnames(dat) <- c("n","timestamp")

        }else if(file_type == "BINARY"){

            # Load Binary data
            dat <- readBin(con = file,what = "raw",n = 10^6)
            length(dat)

        }

        return(dat)

    }else{
        print("This is not a Data (.dat) File")
    }

}
#' COMTRADE Reader
#'
#' This function is based on IEEE Std C37.111-1999
#'
#' To use this function, choose a .dat to process. In the same folder,
#' the cfg file with the same filename must exist.
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
readCOMTRADE <- function(file){

    if(stringr::str_detect(file,".[Dd][Aa][Tt]")){

        # Check if there is a CFG file in the same folder
        # The CFG file must be associated to a DAT file
        path <- dirname(file)
        file_pattern <- sub(pattern = ".[Dd][Aa][Tt]" ,replacement ="" ,x = basename(file))

        files <- list.files(path)
        files <- files[grepl(pattern = file_pattern,x = files)]
        extension <- stringr::str_extract(string = files, pattern = ".[Cc][Ff][Gg]")
        extension <- extension[!is.na(extension)]

        if(any(grepl(pattern = ".[Cc][Ff][Gg]",x = files))){

            comtrade <- list()

            # To improve with grep to return the exact file name with cfg extention
            cfg_file <- paste0(file_pattern,extension)
            cfg_path <- file.path(path,cfg_file)

            #base::debug(readCFG)
            config <- readCFG(file = cfg_path)

            if(config$ft == "ASCII"){

                dat <- readDAT(file)

                if(ncol(dat) == 2 + config$A + config$D ){

                    # Sample number
                    colnames(dat)[1] <- "n"
                    # Time Stamp
                    colnames(dat)[2] <- "timestamp"

                    # Rename Columns with Analog Labels
                    colnames(dat)[c((2+1):(2+config$A))] <- as.character(config$analog$ch_id)

                    # Rename Columns with Digital Labels
                    colnames(dat)[c((2+1+config$A):(2+config$A+config$D))] <- as.character(config$digital$ch_id)

                    # Apply Formulat Y = aX + b
                    aux <- dat[c((2+1):(2+config$A))]
                    a <- config$analog$a
                    b <- config$analog$b
                    data_new <- aux ## Copy dataframe

                    for(i in 1:config$A){

                        data_new[i] <- as.vector(sapply(X = aux[i] , FUN = function(x){Y = a[i]*x+b[i]}))

                    }

                    dat[c((2+1):(2+config$A))] <- data_new

                    # Convert to seconds
                    dat$timestamp <- dat$timestamp/10^6
                    dat$timestamp <- config$rec_start + dat$timestamp
                    op <- options(digits.secs=3) ## Set 3 decimal digits

                }

                comtrade$config <- config
                comtrade$dat <- dat

            }else if(config$ft == "BINARY"){


                comtrade$config <- config
                comtrade$dat <- dat

                print("BINARY processing is not available.")

            }

            return(comtrade)

        }else{
            print("Config file (.CFG) is missing.")
        }

    }else{
        print("This is not a Data File (.DAT).")
    }

}
#' Time Mesurment with Digital Signals
#'
#' This function aims to measure time between signals. Operation times
#' can be achived by take the initial trip singal to the time of
#' circuit breaker open
#'
#'  To improve:
#'  * The process to detect changes on selecte points
#'  * Points must be search by regex due to the different syntax between ied's
#'
#' @param file - The comtrade list with config data and mesurement data
#'
#' @return
#' @export
#'
#' @examples
timeFromDigital <- function(comtrade){


    ied <- xlsx::read.xlsx(file =  "./ied/ied.xlsx",
                           sheetIndex = 1,
                           colIndex = 2:8,
                           stringsAsFactors = F)

    #jsonlite::write_json(ied,path = "./ied/ied.json")
    #jsonlite::read_json(path = "./ied/ied.json",simplifyVector = T)

    # Identify Device Record
    #file_path <- file.path(comtrade$config$file_path,comtrade$config$file_name)
    station_name <- comtrade$config$station_name
    bay_name <- comtrade$config$bay_name
    sp_id <- comtrade$config$sp_id

    ied <- ied %>% dplyr::filter(station == station_name) %>%
        filter(bay == bay_name)  %>% filter(sp == sp_id) %>%
        filter(d_pointers == "op_time_co")

    # IF there is pointers for this comtrade, then process data
    if(nrow(ied) == 1){

        pointers <- c(ied$d1,ied$d2)

        aux <- data.frame(n = comtrade$dat$n,
                          timestamp = comtrade$dat$timestamp,
                          d1 = comtrade$dat[,2+comtrade$config$A+pointers[1]],
                          d2 = comtrade$dat[,2+comtrade$config$A+pointers[2]])

        aux$status_change <- FALSE

        for(i in 2:nrow(aux)){

            if(any(aux[i,c("d1","d2")] != aux[i-1,c("d1","d2")])){

                aux[i,"status_change"] <- TRUE
            }
        }

        optime <- rbind(aux[1,],aux[aux$status_change == T,])

        optime$diff[1] <- 0

        for(i in 2:nrow(optime)){

            optime$diff[i] <- optime$timestamp[i] - optime$timestamp[i-1]
        }

        optime$cum_sum <- cumsum(optime$diff)

        # Filter Operating Time
        optime <- optime[optime$d1 == 1,]

        # Rename Pointers
        colnames(optime)[3] <- colnames(comtrade$dat)[2+comtrade$config$A+pointers[1]]
        colnames(optime)[4] <- colnames(comtrade$dat)[2+comtrade$config$A+pointers[2]]

        return(optime)

    }else{
        print("Pointers must be configured at ./ied/ied.xlsx")

    }
}
#' Calculate RMS from Analog Channels
#'
#' @param x.t
#'
#' @return
#' @export
#'
#' @examples
rmsVal <- function(x.t){

    y = x.t/sqrt(2)

    return(y)
}
#' Title
#'
#' @param file - The comtrade list with config data and mesurement data
#'
#' @return
#' @export
#'
#' @examples
tripCurrent <- function(comtrade, optime){

    ied <- xlsx::read.xlsx(file =  "./ied/ied.xlsx",sheetIndex = 1,colIndex = 2:8, stringsAsFactors = F)
    #jsonlite::write_json(ied,path = "./ied/ied.json")
    #jsonlite::read_json(path = "./ied/ied.json",simplifyVector = T)

    # Identify Device Record
    #file_path <- file.path(comtrade$config$file_path,comtrade$config$file_name)
    station_name <- comtrade$config$station_name
    bay_name <- comtrade$config$bay_name
    sp_id <- comtrade$config$sp_id

    ied <- ied %>% dplyr::filter(station == station_name) %>%
        filter(bay == bay_name)  %>% filter(sp == sp_id) %>%
        filter(d_pointers == "op_time_co")

    # IF there is pointers for this comtrade, then process data
    if(nrow(ied) == 1){

        # Extract Currents from Analog Channels
        aux <- comtrade$dat[optime$n[1]:optime$n[2],1:(2+comtrade$config$A)]
        string <- as.character(config$analog$uu)
        analog_ch_id <- grep("[Aa]", string,fixed = F)
        currents <- aux[,2+analog_ch_id]
        summary(currents)
        currents_abs <- apply(currents,MARGIN = 2,FUN = abs)
        currents_mean <- apply(currents_abs,MARGIN = 2,FUN = mean)
        #currents_max <- apply(currents_abs,MARGIN = 2,FUN = max)

        #rmsCurrent <- rmsVal(currents_mean)
        rmsCurrent <- rmsVal(currents_max)

        trip_current <- list()
        trip_current$rmsCurrent <- rmsCurrent
        trip_current$optime_start <- optime$timestamp[1]
        trip_current$optime_end <- optime$timestamp[2]

        return(trip_current)

    }else{
        print("Pointers must be configured at ./ied/ied.xlsx")
    }
}
#' Ampere Squared Secound Function
#'
#' This function aims to calculate the thermal energy
#' of the current flow during a fault
#'
#' @param comtrade
#' @param optime
#'
#' @return
#' @export
#'
#' @examples
AmpSqrSec <- function(comtrade,optime){


    ied <- xlsx::read.xlsx(file =  "./ied/ied.xlsx",sheetIndex = 1,colIndex = 2:8, stringsAsFactors = F)
    #jsonlite::write_json(ied,path = "./ied/ied.json")
    #jsonlite::read_json(path = "./ied/ied.json",simplifyVector = T)

    # Identify Device Record
    #file_path <- file.path(comtrade$config$file_path,comtrade$config$file_name)
    station_name <- comtrade$config$station_name
    bay_name <- comtrade$config$bay_name
    sp_id <- comtrade$config$sp_id

    ied <- ied %>% dplyr::filter(station == station_name) %>%
        filter(bay == bay_name)  %>% filter(sp == sp_id) %>%
        filter(d_pointers == "op_time_co")

    # IF there is pointers for this comtrade, then process data
    if(nrow(ied) == 1){

        # Extract Currents from Analog Channels
        aux <- comtrade$dat[optime$n[1]:optime$n[2],1:(2+comtrade$config$A)]
        string <- as.character(config$analog$uu)
        analog_ch_id <- grep("[Aa]", string,fixed = F)
        currents <- aux[,2+analog_ch_id]
        summary(currents)

        currents_abs <- apply(currents,MARGIN = 2,FUN = abs)
        currents_mean <- apply(currents_abs,MARGIN = 2,FUN = mean)
        currents_mean
        rmsCurrent <- rmsVal(currents_mean)
        t <- as.numeric(optime$timestamp[2] - optime$timestamp[1])

        amp_sqr_sec <- (rmsCurrent^2)*t
        return(amp_sqr_sec)

    }else{
        print("Pointers must be configured")
    }
}




#' CB Close Time
#'
#' This function capture the digital signal of CB Close
#' and return the time when the CB is changing the his
#' status from close to open.
#'
#' @param comtrade - The data source with the configuration info and data from disturbance record
#' @param Amp - Amplitude where the time should be measured
#'
#' @return
#' @export
#'
#' @examples
CBClose_t <- function(comtrade, Amp){

    # If data is ASCII format, then process
    if(comtrade$config$ft == "ASCII"){

        # Identify Digital Input (di) Label of CB Close
        di_ch_id <- comtrade$config$digital$ch_id

        di_Dn <- grep(pattern = "Q50.[Ff][Ee][Cc][Hh][Aa][Dd][Oo]|Q50.[Cc][Ll][Oo][Ss][Ee]|Q51.[52A]",x = di_ch_id,fixed = F)

        if(length(di_Dn) == 1){

            aux <- comtrade$dat[,c("n","timestamp")]
            aux$di_1 <- comtrade$dat[,2+comtrade$config$A+di_Dn]

            #t_cb_close <- aux[aux$di_1 == 1,"timestamp"][1]
            t_cb_not_close <- aux[aux$di_1 == Amp ,"timestamp"][1]

            # Rename Column
            colnames(aux)[length(aux)] <- colnames(comtrade$dat[2+comtrade$config$A+di_Dn])

            return(t_cb_not_close)

        }else{
            print("CB Close Status not found")
        }

    }else{
        print("BINARY processing is not available.")
    }

}
#' CBOpen time window capture
#'
#' This function takes a "snaphshot" to the disturbance record
#' during the circuit break status changing.
#'
#' @param comtrade -  Data Source with config info and disturbance record data
#' @param t_ref - Time reference
#' @param delay - A Delay to apply in miliseconds
#' @param t_window_ms - Time Window lengh in miliseconds
#'
#' @return
#' @export
#'
#' @examples
CBOpen_window <- function(comtrade,t_ref, delay_ms = 0.050 ,t_window_ms = 0.200){

    # If data is ASCII format, then process
    if(comtrade$config$ft == "ASCII"){

        if(is.POSIXct(t_ref)){

            t_start <- t_ref - delay_ms
            t_end <- t_start + t_window_ms

            window <- comtrade$dat %>% dplyr::filter(timestamp > t_start ,timestamp < t_end)

            return(window)

        }else{
            print("Time not available")
        }

    }else{
        print("BINARY processing is not available.")
    }

}

#' Rename Duplicated Columns Names
#'
#' This functions will fix the rename method by replace a unique name for each column
#'
#' @param strings
#'
#' @return
#' @export
#'
#' @examples
renameDupli <- function(strings){

    dup <- duplicated(strings)

    for(i in 1:length(dup)){

        if(dup[i]){
            strings[i] <- paste(strings[i],i,sep = "_")
        }
    }
    return(strings)
}
