# transform excel2R2json
# improvements: - "?" recognition
#               - add [] to output_certain and output_possible
#               - numerics should be stored as factors 

## the Consultant needs to fill in
input_file_name <- "test_ctcf.xlsx" # name of the excel sheet to read in
rcols <- c(6:16)                    # the columns (omit source, key + party-model: spare the "type")
row_num <- list(c(3:5), c(9:11), c(15:17), c(21:23), c(27:29), c(33:35), c(39:41), c(45:47), c(51:53), c(57:59), c(63:65), c(69:71), c(75:77))   # the row numbers containing the content
test_num <- list(2, 8, 14, 20, 26, 32, 38, 44, 50, 56, 62, 68, 74)             # the row containing the test name for each content 
## the first part of the data frame to be transformed into JSON, depending on the chosen data model
x <- data.frame(source = c("source1", "source2"),key=c("100", "101"), type=c("party", "party"))

## load required packages
library(xlsx)
library(XML)
library(jsonlite)

## initiate index for test name retrieval
test_idx <- 1

for(rrows in row_num){
  ## read excel sheet
  raw_exl <- read.xlsx(input_file_name, sheetIndex = 1, rowIndex=rrows, colIndex = rcols, encoding="UTF-8", as.data.frame = TRUE, check.names = FALSE, 
  colClasses = rep("character", 11))
  ## get the names of all the input columns 
  column_names <- names(raw_exl)

  ## before reading entities.config, remove first line in the config file as this would mess up xml
  raw_xml <- readLines("entities.config.ctcf", encoding = "UTF-8")
  xml <- xmlTreeParse(raw_xml, useInternal=TRUE, encoding = "UTF-8")
  
  ## parse all names and captions in xpath language
  fieldnames <- xpathSApply(xml, "//entity/field/@name")
  captionnames <- xpathSApply(xml, "//entity/field/@caption")
  entitynames <- captionnames

  ## retrieve the entities belonging to each caption; this is needed to display nested json objects
  xpath_exp1 <- "//../field[@caption=\'"
  xpath_exp2 <- "\']/ancestor::entity/@name"
  i <- 1
  for(element in entitynames){
    string2 <- paste(xpath_exp1, as.character(element), xpath_exp2, sep="")
    parent_node <-xpathSApply(xml, string2)
    ancestor <- getElement(parent_node[2], name="name")
    if(i < length(entitynames)){
      entitynames[i] <- ancestor
      i <- i + 1
    }
  }

  ## merge captions, fieldnames and their respective entities into a list and cleanse the list
  ## this mapping list will eventually be needed for the JSON transformation
  cfe_map <- cbind(captionnames, fieldnames, entitynames)
  cfe_map <- gsub("?Y", "?", cfe_map)
  cfe_map <- gsub("?Y", "?", cfe_map)
  cfe_map <- gsub("?Y", "?", cfe_map)
  cfe_map <- gsub("?Y", "?", cfe_map)
  cfe_map <- gsub("?Y", "?", cfe_map)
  cfe_map <- gsub("?Y", "?", cfe_map)
  cfe_map <- gsub("ä", "?", cfe_map)
  cfe_map <- gsub("ü", "?", cfe_map)
  cfe_map <- gsub("ö", "?", cfe_map)

  ## copy raw_exl
  mod_exl <- raw_exl

  ## overwrite the column captions of mod_exl with their respective field names
  for(entry in cfe_map[,1]){
    if(!is.na(match(entry, names(mod_exl)))){
      lookup <- grep(entry, cfe_map)
      names(mod_exl)[match(entry, names(mod_exl))] <- cfe_map[lookup,2]
    }
  }

  ## find out the entity belonging to each relevant element in raw_exl
  df = data.frame()
  for(each in names(raw_exl)){
    if(!is.na(match(each, cfe_map[,1]))){
      entity_pointer <- cfe_map[match(each, cfe_map[,1]),3]
      field_pointer <- cfe_map[match(each, cfe_map[,1]),2]
      df = rbind(df, cbind(entity_pointer, field_pointer))
    }
  }
  
  ## create a data.frame containing record info for further processing in JSON
  dstnct <- as.character(unique(df[,1]))
  x[,dstnct] <- NA

  clmn_rry = array()
  for (nq in dstnct) {
    plygrnd <- subset(df, df[,1] == nq)
    for (ntty in plygrnd[,2]) {
      clmn_fndr <- match(ntty, names(mod_exl))
      clmn_rry <- append(clmn_rry, clmn_fndr)
      clmn_rry <- clmn_rry[!is.na(clmn_rry)]
      dyb1 <- data.frame(mod_exl[1, clmn_rry])
      dyb2 <- data.frame(mod_exl[2, clmn_rry])
      colnames(dyb1) <- names(mod_exl[clmn_rry])
      colnames(dyb2) <- names(mod_exl[clmn_rry])
      x[[nq]][1] <- list(dyb1) 
      x[[nq]][2] <- list(dyb2)
    }
    clmn_rry <- NA
  }

  # now prepare transformation of data.frame object to json
  
  ## read in test name row and apply fuzzy search over test_name 
  ## to determine outcome of the test result
  test_name <- read.xlsx(input_file_name, sheetIndex = 1, rowIndex = test_num[[test_idx]], colIndex = 1, encoding="UTF-8", as.data.frame = FALSE, check.names = FALSE)
  test_name <- as.character(test_name)
  test_idx <- test_idx + 1
  
  sstring <- '{ "input": '

  if(agrepl("Certain", test_name) == TRUE){
    y <- paste0(sstring, toJSON(x, pretty=T), ', "output_certain": [', toJSON(x[1:2], pretty=T), '], "output_possible": []}')
  }
  if(agrepl("Possible", test_name) == TRUE){
    y <- paste0(sstring, toJSON(x, pretty=T), ', "output_certain": [], ', '"output_possible": [', toJSON(x[1:2], pretty=T), "]}")
  }
  if(agrepl("NonMatch", test_name) == TRUE){
    y <- paste0(sstring, toJSON(x, pretty=T), ', "output_certain": [], ', '"output_possible": []}')
  }
  
  ## write json output to destination
  out_name <- paste(test_name, ".json", sep="")
  out_file <- paste(getwd(), "/", out_name, sep="")
  write(y, out_file)
}

