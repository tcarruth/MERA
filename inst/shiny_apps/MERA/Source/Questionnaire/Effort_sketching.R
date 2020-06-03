

# Effort sketching functions

getyrs<-function(){
  suppressWarnings({ny<-as.numeric(input$nyears)})
  if(length(ny)==0){
    ny<-68
  }else if(is.na(ny)){
    ny<-68
  }
  Current_Year-(ny:1)
} 

# Get effort matrix
# matrix interpolation
mat_inter<-function(df){
  x<-df$x-min(df$x)+1
  approx(x,y=df$y,xout=1:max(x))$y
}

# linear interpolation of effort 
effort_mat<-function(){
  
  yrs<-getyrs()
  lyr<-yrs[length(yrs)]
  nseries<-eff_values$series
  nt<-length(yrs)
  effmat<-array(NA,c(nseries,nt))
 
  for(i in 1:nseries){
    
    df<-eff_values$df[eff_values$df$series==i,]
    
    if(i == nseries){
      
      if(nrow(df)==1){ # interpolation not possible only one datum
        df<-rbind(df,data.frame(x=lyr,y=df$y,series=df$series))
      }
      if(!(lyr%in%df$x)){ # did not specify last data point of last series
        df<-rbind(df,data.frame(x=lyr,y=df$y[nrow(df)],series=df$series[nrow(df)]))
      }
      
    }  
    effmat[i,]<-mat_inter(df)
    if(all(effmat[i,]==0))effmat[i,]<-0.5
    
  }
  
  effmat
  
}
