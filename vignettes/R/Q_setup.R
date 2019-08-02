
ny<-68
makeState<-function(x)rep(T,length(get(x)))

Fpanel_names<-c("M_list","D_list","h_list","FP_list","F_list","qh_list","q_list","LM_list","sel_list","dome_list","DR_list","PRM_list","sigR_list","Ah_list","Vh_list","A_list","V_list","Dh_list")
Mpanel_names<-c("M1_list","IB_list","IV_list","IBE_list","IVE_list","IBSL_list","IVSL_list")
Dpanel_names<-c("D1_list","CB_list","Beta_list","Err_list")
Slider_names<-c("loc","stmag","co")

MasterList<-list(Fpanel_names,Mpanel_names,Dpanel_names,Slider_names)

PanelState<-list(Fpanel=lapply(Fpanel_names, makeState),
                 Mpanel=lapply(Mpanel_names, makeState),
                 Dpanel=lapply(Dpanel_names, makeState),
                 Slider=lapply(Slider_names, makeState))

PanelState[[2]][[1]][c(3)]<-FALSE # no size limits allowed
PanelState[[3]][[1]][c(6,7,9)]<-FALSE # no comp or absolute biomass

PanelState[[1]][[18]]<-c(F,F,F,F,T) # Exception is the final fishery initial depletion
PanelState[[3]][[4]]<-c(F,F,F,T) # Exception is the final selection of the data menu - quality is a radio button default to data-poor

getinputnames<-function(x)strsplit(x,"_")[[1]][1]

inputnames<-list(Fpanel=lapply(Fpanel_names,getinputnames),
                  Mpanel=lapply(Mpanel_names,getinputnames),
                  Dpanel=lapply(Dpanel_names,getinputnames),
                  Slider=lapply(Slider_names,getinputnames))

randsel<-function(n){
  sel<-ceiling(runif(1)*n)
  bel<-ceiling(runif(1)-0.5)
  abv<-ceiling(runif(1)-0.5)
  seltest<-c((sel-bel),(sel+abv))
  if(seltest[1]<1)seltest[1]<-1
  if(seltest[2]>n)seltest[2]<-n
  out<-rep(FALSE,n)
  out[seltest[1]:seltest[2]]<-TRUE
  out
}

allwid<-unlist(inputnames)
allwid<-allwid[!allwid%in%c("loc","stmag","co")]
input<-new('list')
for(i in 1:length(allwid)){
  listy<-get(paste0(allwid[i],"_list"))
  input[[allwid[i]]]<-listy[randsel(length(listy))]
}

input$stmag<-1
input$loc<-1
input$co<-1
  