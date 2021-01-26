# Data gate keeper for loading mera data

Data_gate<-function(dattest){
  
  if(is.null(dattest)){
    
    return(NULL)
    
  }else{
    
    errlistD<-Data_self_consis(dattest)
    
    if(length(errlistD)==0){
      AM("No data self-consistency formating errors detected")
    }else{
      AM("! Data self-consistency formating errors detected !")
    }
    
    errlistQ<-MERA_Q_consis(dattest)
    
    if(length(errlistQ)==0){
      AM("Data file is compatible with MERA questionnaire")
    }else{
      AM("! Data file is not compatible with MERA questionnaire !")
    }
    
    return(c(errlistD,errlistQ))
    
  }

}

MERA_Q_consis<-function(dattest){
  
  errlist<-new('list')
  Year<-dattest@Year
  #if((min(Year)!=input$Syear)) errlist$S_year = "The first year in the data file must match the Fishery Start year in Fishery Question 1"
  #if(!(input$Lyear%in%Year)) errlist$L_year = "The last year in the data file must equal or exceed the Fishery End year in Fishery Question 1"
  
  errlist
}

Data_self_consis<-function(dattest){
  
  errlist<-new('list')
  
  Year<-dattest@Year
  Cat<-dattest@Cat[1,]
  Ind<-dattest@Ind[1,]
  SpInd<-dattest@SpInd[1,]
  CAL<-dattest@CAL[1,,]
  CALbins<-dattest@CAL_bins
  CAA<-dattest@CAA[1,,]
  ML<-dattest@Ind[1,]
  
  if(length(min(Year):max(Year))!=length(Year)) errlist$MissYr="The data file is missing years of data (the Year index and data must have an entry for each year 
                                                                - use NAs for catch and other data if these are missing for certain years"
  #if(any(is.na(Cat))) errlist$CatNA="You have specified missing (NA) values in your catch time series - this must have a value for each year of your dataset"
  if(!all(is.na(Cat))) if(length(Cat)!=length(Year)) errlist$CatLen="Catches (Cat) are not of the same length as the Year (Year) index"
  if(!all(is.na(Ind))) if(length(Ind)!=length(Year)) errlist$IndLen="Index (Ind) data are not of the same length as the Year (Year) index"
  if(!all(is.na(SpInd))) if(length(SpInd)!=length(Year)) errlist$SpIndLen="Spawning Index (SpInd) data are not of the same length as the Year (Year) index"
  if(!all(is.na(ML))) if(length(ML)!=length(Year)) errlist$ML="Mean Length (ML) data are not of the same length as the Year (Year) index"
  
  if(!all(is.na(CAL))) if(dim(CAL)[1]!=length(Year)) errlist$CALLen="Catch at length (CAL) data are not of the same length as the Year (Year) index"
  if(!all(is.na(CAA))) if(dim(CAA)[2]!=length(Year)) errlist$CAALen="Catch at age (CAA) data are not of the same length as the Year (Year) index"
  
  if(!all(is.na(CAL))) if(dim(CAL)[2]!=(length(CALbins)-1)) errlist$CALbins ="Catch at length (CAL) data do not match length of the CAL bin definitions 
                                                         (CALbins should be longer by 1 as these define the upper and lower breakpoints of the CAL data)"
  if(!all(is.na(CAL))) if(is.na(dattest@vbLinf))errlist$CALnoLinf ="Catch at length (CAL) data are provided without specifying the von Bertalanffy Linf parameter (asymptotic length)"
  
  errlist
  
}