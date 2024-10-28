
ezADI<-
function(inputFile=NULL, inputDat=NULL, adiFile="US_ADI_Example.csv", outputFile=NULL, batchSize=10)
{
  if(is.null(inputFile)&is.null(inputDat))
  {
    print("InputFile and InputDat are both missing:")
    stop()
  }
  if (!is.null(inputFile))
  {
    if(str_ends(inputFile, ".xlsx"))
    {
      inputDat <- read.xlsx(inputFile)
      otfl <- str_replace(inputFile, '.xlsx', '_geocode_ADI-res.xlsx')
    }
    if(str_ends(inputFile, ".csv"))
    {
      inputDat <- read.csv(inputFile, header =T)
      otfl <- str_replace(inputFile, '.csv', '_geocode_ADI-res.xlsx')
    }
    if(str_ends(inputFile, ".txt"))
    {
      inputDat <- read.table(inputFile, header = T, quote="\"", sep="\t")
      otfl <- str_replace(inputFile, '.txt', '_geocode_ADI-res.xlsx')
    }
  }
  if(!(all(c("ID", "Address") %in% names(inputDat)) | all(c("ID", "street", "city", "state", "zipcode") %in% names(inputDat))))
  {
    print("Data error: ")
    stop()
  }
  if(!"Address" %in% names(inputDat))
  {
    inputDat$Address<-paste(inputDat$street, inputDat$city, inputDat$state, inputDat$zipcode, sep=", ")
  }
  
  if(adiFile=="US_ADI_Example.csv")
  {
    adiFile <- paste0(path.package("ezADI"), "/",  adiFile)
  }
  adiDat<-read.csv(adiFile)
  adiDat$FIPS[nchar(adiDat$FIPS)==11]<-paste0("0", adiDat$FIPS[nchar(adiDat$FIPS)==11])
  
  geocodeQres<-NULL
  if(nrow(inputDat) <= batchSize)
  {
    geocodeQres<-geocode(inputDat, 'Address', method = "census", full_results = TRUE, 
                         api_options=list(census_return_type = "geographies"))
  }
  
  if(nrow(inputDat) > batchSize)
  {
    st<-seq(1, nrow(inputDat), by=batchSize)
    if(nrow(inputDat)%/%batchSize==0)
    {
      ed<-seq(0, nrow(inputDat), by=batchSize)[-1]
    }
    if(nrow(inputDat)%/%batchSize>0)
    {
      ed<-c(seq(0, nrow(inputDat), by=batchSize)[-1], nrow(inputDat))
    }
    for (i in 1:length(st))
    {
      thisgeocode<-geocode(inputDat[st[i]:ed[i], ], 'Address', method = "census", full_results = TRUE, 
                           api_options=list(census_return_type = "geographies"))
      geocodeQres<-rbind.data.frame(geocodeQres, thisgeocode)
    }
  }
  
  geocodeQres$FIPS<-paste0(geocodeQres$state_fips, geocodeQres$county_fips, 
                           geocodeQres$census_tract, substr(geocodeQres$census_block,1,1))
  
  adiRes<-merge(geocodeQres, adiDat, by="FIPS", by.y="FIPS", all.x = T)
  adiRes<-adiRes[, c(2:ncol(adiRes), 1)]
  nmtch_index <- adiRes$match_indicator%in%c("No_Match", "Tie")
  nadi<-sum(!is.na(adiRes$ADI_NATRANK))
  
  if(is.null(outputFile)&!is.null(inputFile))
  {
    outputFile <- otfl
  }
  if(!is.null(inputFile))
  {
    write.xlsx(adiRes, outputFile, overwrite = T)
  }
  return(list(adiRes=adiRes, matched=sum(!nmtch_index), no_match=sum(nmtch_index), problem_address=adiRes$Address[nmtch_index], nadi=nadi))
}