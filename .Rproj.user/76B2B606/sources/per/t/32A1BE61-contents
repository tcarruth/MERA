library(shinyalert)
library(shiny)
library(shinyjs)


#js_code <<- "shinyjs.browseURL = function(url) {window.open(url,'_blank')}"
js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"
shinyUI(

  fluidPage(
    useShinyjs(),
    extendShinyjs(text = js_code, functions = 'browseURL'),
    useShinyalert(),
    tags$head(
      tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),
      tags$style(HTML("hr {border-top: 1.4px solid #E3E1DE;}
                      h4 { font-size:15px;}
                      h5 { font-size:13px;}
                      h6 { font-size:11px;}
                      textarea{ font-size:13px;}
                      button{ font-size:13px}
                      input{ font-size:13px;}
                      title{ font-size:13px;}
                      caption{ font-size:13px;font-weight:normal}
                      label{ font-size:13px;}
                      legend{ font-size:13px;}
                      link{ font-size:13px;}
                      select{ font-size:13px;}
                      menu{ font-size:13px;}
                      option{ font-size:13px;}
                      output{ font-size:13px;}
                      tbody{ font-size:13px;}
                      style{ font-size:13px;}

                      [type = 'number'] {font-size:13px;height:30px;}
                      [type = 'text'] {font-size:13px;}
                      [type = 'textArea'] {font-size:13px;}
                      #Fpanelout{font-size: 13px;}
                      #Dpanelout{font-size: 13px;}
                      #Mpanelout{font-size: 13px;}
                      #email{font-size: 13px;}
                      #emailsend{font-size: 13px;}
                      #calculate{font-size: 13px;}
                      #Fcont{font-size: 13px;}
                      #FcontD{font-size: 13px;}
                      #Fback{font-size: 13px;}
                      #Build_OM_Q{font-size: 13px;}
                      #FbackD{font-size: 13px;}
                      #Build_OM{font-size: 13px;}
                      #Build_Eval{font-size: 13px;}
                      #Build_AI{font-size: 13px;}
                      #Save{font-size: 13px;}
                      #Load{font-size: 13px; height:10px;}
                      #Justification{font-size: 13px;}
                      #SessionID{font-size:11px;}


                      ")),

      tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Raleway|Cabin:400,700');

                      h2 {
                      font-family: 'Raleway', cursive;
                      font-weight: 500;
                      line-height: 1.1;
                      }

                      "))
    ),

    includeCSS("www/custom.css"),

    fluidRow(

      column(1,style="height:65px",
             h2("MERA")
      ),
      column(5,style="height:65px",
             h5("method evaluation and risk assessment    (MSC-DLMtool App v4.1.1)",style="padding:19px;")
      ),

      column(3,offset=2,style="padding:14px;height:65px",
             fluidRow(

               column(7,tags$a(img(src = "DLMtool.png", height = 45, width = 145),href="https://www.datalimitedtoolkit.org",target='_blank')),
               column(5,tags$a(img(src = "MSC4.png", height = 52, width = 136),href="https://www.msc.org/",target='_blank'))

            )
      )
    ),
    hr(),

    h4("Welcome to MERA, a tool for analyzing risk, guiding fishery improvement projects, and evaluating management strategies for certification.",style = "color:black"),
    h5("MERA links a straightforward graphical questionaire to the powerful OMx operating model of DLMtool and MSEtool  to conduct rapid management strategy evaluation (MSE) for multiple management procedures (MPs). ",style = "color:grey"),
    h5("In step A the fishery is characterized and any available data are loaded. In step B an operating model is constructed that is used in step C to evaluate feasible MPs or
     apply a suitable MP (Application). Finally, in step D indicators can be used to detect changes in system dynamics.",style = "color:grey"),
    h5("For further information see the ", a("MERA Manual.", href="https://dlmtool.github.io/DLMtool/MERA/MERA.html", target="_blank"),style = "color:grey"),
    h5("The DLMtool paper is also available ", a("here.", href="https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.13081", target="_blank"),style = "color:grey"),
    h5("For technical questions or bug reports please contact ", a("t.carruthers@oceans.ubc.ca", href="mailto:t.carruthers@ubc.ca", target="_blank"),style = "color:grey"),


    fluidRow(

      HTML("<br>"),
      column(12,
             h4("MODE"),
             hr()
             ),
      column(1),
      column(11,

             column(4,

               radioButtons("Mode",label=NULL,choices=c("Streamlined","Advanced"),selected="Streamlined")),

               column(8,
                      h5("MERA contains two modes of differing complexity",style = "color:grey"),
                      h5(" - Streamlined mode is an entirely questionnaire based description of fishery dynamics for calculation of biological risk where data are limited",style = "color:grey"),
                      h5(" - Advanced mode builds on the questionnaire to use data to condition operating models and identify exceptional circumstances ", style = "color:grey")
               )

      ),


      column(12,HTML("<br>")),

      column(12,style="height:60px",

            conditionalPanel(condition="output.Quest==0&input.Mode=='Advanced'",h4("STEP A1: CHARACTERIZE FISHERY SYSTEM")),
            conditionalPanel(condition="output.Quest==1&input.Mode=='Advanced'",h4("STEP A1: CHARACTERIZE FISHERY SYSTEM",style="color:green")),
            conditionalPanel(condition="output.Quest==0&input.Mode=='Streamlined'",h4("STEP A: CHARACTERIZE FISHERY IN QUESTIONNAIRE")),
            conditionalPanel(condition="output.Quest==1&input.Mode=='Streamlined'",h4("STEP A: CHARACTERIZE FISHERY IN QUESTIONNAIRE",style="color:green")),

            hr()
      ),

      column(1),
      column(11,
             fluidRow(

               column(width = 4, style="height:360px",
                      tabsetPanel( id = "tabs1",selected=1,
                        tabPanel(h4("Fishery",style = "color:black"),

                            conditionalPanel(width=4,condition="output.Fpanel==undefined|output.Fpanel==0",

                                 HTML("<br>"),
                                 h5("The Fishery panel contains a set of questions about the characteristics of the fish population and its fishery.",style="color:grey"),
                                 h5("These questions specify: ",style="color:grey"),
                                 h5(" - productivity and resilience of the population",style="color:grey"),
                                 h5(" - historical characteristics of its fishery",style="color:grey"),
                                 h5(" - vulnerability to fishing of various size classes",style="color:grey"),
                                 h5(""),
                                 h5("More detailed help on the Fishery questions can be found in the
                                         MERA User Guide: ", a("Section 4.", href="https://dlmtool.github.io/DLMtool/MERA/MERA.html#4_the_fishery_tab", target="_blank"),style="color:grey")),

                            conditionalPanel(width=4,condition="output.Fpanel==1",#|output.Fpanel==undefined",
                                fluidRow(
                                  column(width=12,h5("1. Fishery description")),
                                  column(width=4,style="height:40px;padding:19px",
                                         h5("Name:",style="font-weight:bold")),
                                  column(width=8,style="height:40px",
                                         textInput("Name", "", "e.g. Atlantic swordfish")),
                                  column(width=4,style="height:40px;padding:19px",
                                         h5("Species:",style="font-weight:bold")),
                                  column(width=8,style="height:40px",
                                         textInput("Species","","e.g. Xiphias gladius")),
                                  column(width=4,style="height:40px;padding:19px",
                                         h5("Location:",style="font-weight:bold")),
                                  column(width=8,style="height:40px",
                                         textInput("Region","","e.g. North Atlantic")),
                                  column(width=4,style="height:40px;padding:19px",
                                         h5("Agency:",style="font-weight:bold")),
                                  column(width=8,style="height:40px",
                                         textInput("Agency","","e.g. ICCAT")),
                                  column(width=4,style="height:40px;padding:19px",
                                         h5("No. years:",style="font-weight:bold")),
                                  column(width=8,style="height:40px",
                                         textInput("nyears", "", "e.g. 68 (since 1951)")),
                                  column(width=4,style="height:40px;padding:19px",
                                         h5("Author:",style="font-weight:bold")),
                                  column(width=8,style="height:40px",
                                         textInput("Author", "","Alex Doe (a.doe@gmail.com)"))
                                )
                             ),

                            conditionalPanel(width=4,condition="output.Fpanel==2",#|output.Fpanel==undefined",
                                checkboxGroupInput("M", label = h5("2. Longevity",style="color:black"),
                                    choices = M_list, selected = M_list),
                                actionLink("All_M","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==3",
                                checkboxGroupInput("D", label = h5("3. Stock depletion",style="color:black"),
                                     choices = D_list, selected = D_list),
                                actionLink("All_D","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==4",
                                checkboxGroupInput("h", label = h5("4. Resilience",style="color:black"),
                                        choices = h_list, selected = h_list),
                                actionLink("All_h","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==5",
                              column(6,
                                checkboxGroupInput("FP", label = h5("5. Historical effort pattern",style="color:black"),
                                        choices = FP_list, selected = FP_list),
                                actionLink("All_FP","UNKNOWN")),

                              column(6,
                                HTML("<br>"),
                                sliderInput("loc",label=h5("Skew"),min=0.2,max=1.8,value=1,step=0.1),
                                sliderInput("stmag",label=h5("Magnitude of recent change"),min=0.2,max=1.8,value=1,step=0.1))
                              ),

                            conditionalPanel(width=4,condition="output.Fpanel==6",
                                 checkboxGroupInput("F", label = h5("6. Inter-annual variability in historical effort",style="color:black"),
                                        choices = F_list, selected = F_list),
                                 actionLink("All_F","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==7",
                                 checkboxGroupInput("qh", label = h5("7. Historical fishing efficiency changes",style="color:black"),
                                                    choices = q_list, selected = q_list),
                                 actionLink("All_qh","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==8",
                                 checkboxGroupInput("q", label = h5("8. Future fishing efficiency changes",style="color:black"),
                                                    choices = q_list, selected = q_list),
                                 actionLink("All_q","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==9",
                                 checkboxGroupInput("LM", label = h5("9. Length at maturity",style="color:black"),
                                        choices = LM_list, selected = LM_list),
                                 actionLink("All_LM","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==10",
                                  checkboxGroupInput("sel", label = h5("10. Selectivity of small fish",style="color:black"),
                                        choices = sel_list, selected = sel_list),
                                  actionLink("All_sel","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==11",
                                 checkboxGroupInput("dome", label = h5("11. Selectivity of large fish",style="color:black"),
                                        choices = dome_list, selected = dome_list),
                                 actionLink("All_dome","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==12",
                                 checkboxGroupInput("DR", label = h5("12. Discard rate",style="color:black"),
                                        choices = DR_list, selected = DR_list),
                                 actionLink("All_DR","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==13",
                                 checkboxGroupInput("PRM", label = h5("13. Post-release mortality rate",style="color:black"),
                                         choices = PRM_list, selected = PRM_list),
                                 actionLink("All_PRM","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==14",
                                 checkboxGroupInput("sigR", label = h5("14. Recruitment variability",style="color:black"),
                                         choices = sigR_list, selected = sigR_list),
                                 actionLink("All_sigR","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==15",
                                  checkboxGroupInput("Ah", label = h5("15. Size of existing MPA",style="color:black"),
                                         choices = Ah_list, selected = Ah_list),
                                  actionLink("All_Ah","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==16",
                                  checkboxGroupInput("Vh", label = h5("16. Spatial mixing (movement) in/out of existing MPA",style="color:black"),
                                                    choices = Vh_list, selected = Vh_list),
                                  actionLink("All_Vh","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==17",
                                  checkboxGroupInput("A", label = h5("17. Size of future potential MPA",style="color:black"),
                                         choices = A_list, selected = A_list),
                                  actionLink("All_A","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==18",
                                  checkboxGroupInput("V", label = h5("18. Spatial mixing (movement) in/out of future potential MPA",style="color:black"),
                                         choices = V_list, selected = V_list),
                                  actionLink("All_V","UNKNOWN")),

                            conditionalPanel(width=4,condition="output.Fpanel==19",
                                  checkboxGroupInput("Dh", label = h5("19. Initial stock depletion",style="color:black"),
                                                    choices = Dh_list, selected = Dh_list[[5]]),
                                  actionLink("All_Dh","DEFAULT")),

                            value=1),


                        tabPanel(h4("Management",style = "color:black"),

                                 conditionalPanel(width=4,condition="output.Mpanel==undefined|output.Mpanel==0",

                                      HTML("<br>"),
                                      h5("The Management panel contains a set of questions about what fishery management options are available and how well management advice is followed.",style="color:grey"),
                                      h5("These questions are used to: ",style="color:grey"),
                                      h5(" - identify what management procedures are feasible given the types of management measures.",style="color:grey"),
                                      h5(" - determine the relative success of management procedures that provide various types of advice.",style="color:grey"),
                                      h5(""),
                                      h5("More detailed help on the Management questions can be found in the MERA manual
                                         : ", a("Section 5", href="https://dlmtool.github.io/DLMtool/MERA/MERA.html#5_the_management_tab", target="_blank"),style="color:grey")),


                                 conditionalPanel(width=4,condition="output.Mpanel==1",
                                                  checkboxGroupInput("M1", label = h5("1. Types of fishery management that are possible",style="color:black"),
                                                                     choices = M1_list, selected = M1_list),
                                                  actionLink("All_M1","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Mpanel==2",
                                                  checkboxGroupInput("IB", label = h5("2. TAC offset: consistent overages/underages",style="color:black"),
                                                                     choices = IB_list, selected = IB_list),
                                                  actionLink("All_IB","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Mpanel==3",
                                                  checkboxGroupInput("IV", label = h5("3. TAC implementation variability",style="color:black"),
                                                                     choices = IV_list, selected = IV_list),
                                                  actionLink("All_IV","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Mpanel==4",
                                                  checkboxGroupInput("IBE", label = h5("4. TAE offset: consistent overages/underages",style="color:black"),
                                                                     choices = IBE_list, selected = IBE_list),
                                                  actionLink("All_IBE","MATCH TAC IMPLEMENTATION")),

                                 conditionalPanel(width=4,condition="output.Mpanel==5",
                                                  checkboxGroupInput("IVE", label = h5("5. TAE implementation variability",style="color:black"),
                                                                     choices = IVE_list, selected = IVE_list),
                                                  actionLink("All_IVE","MATCH TAC IMPLEMENTATION")),

                                 conditionalPanel(width=4,condition="output.Mpanel==6",
                                                  checkboxGroupInput("IBSL", label = h5("6. Size limit offset: consistent overages/underages",style="color:black"),
                                                                     choices = IBSL_list, selected = IBSL_list),
                                                  actionLink("All_IBSL","MATCH TAC IMPLEMENTATION")),

                                 conditionalPanel(width=4,condition="output.Mpanel==7",
                                                  checkboxGroupInput("IVSL", label = h5("7. Size limit implementation variability",style="color:black"),
                                                                     choices = IVSL_list, selected = IVSL_list),
                                                  actionLink("All_IVSL","MATCH TAC IMPLEMENTATION")),


                                 value=2),

                        tabPanel(h4("Data",style = "color:black"),

                                 conditionalPanel(width=4,condition="output.Dpanel==undefined|output.Dpanel==0",

                                  HTML("<br>"),
                                  h5("The Data panel contains a set of questions about what types of data are available and the quality of the data that are available.",style="color:grey"),
                                  h5("These questions are used to: ",style="color:grey"),
                                  h5(" - identify what management procedures are feasible given the types of data available.",style="color:grey"),
                                  h5(" - determine the relative success of the management approaches that rely on differing types of data.",style="color:grey"),
                                  h5(""),
                                  h5("More detailed help on the data questions can be found in the MERA manual
                                         : ", a("Section 6", href="https://dlmtool.github.io/DLMtool/MERA/MERA.html#6_the_data_tab", target="_blank"),style="color:grey")),

                                 conditionalPanel(width=4,condition="output.Dpanel==1",
                                                  checkboxGroupInput("D1", label = h5("1. Types of data that are available",style="color:black"),
                                                                     choices = D1_list, selected = D1_list),
                                                  actionLink("All_D1","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Dpanel==2",
                                                  checkboxGroupInput("CB", label = h5("2. Catch reporting bias",style="color:black"),
                                                                     choices = CB_list, selected = CB_list),
                                                  actionLink("All_CB","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Dpanel==3",
                                                  checkboxGroupInput("Beta", label = h5("3. Hyperstability in indices",style="color:black"),
                                                                     choices = Beta_list, selected = Beta_list),
                                                  actionLink("All_Beta","UNKNOWN")),

                                 conditionalPanel(width=4,condition="output.Dpanel==4",
                                                  radioButtons("Err", label = h5("4. Overall data quality",style="color:black"),
                                                                     choices = Err_list, selected = "Err_bad"),
                                                  actionLink("All_Err","DEFAULT")),
                                 value=3)

                      )

               ),


               column(width = 7,style="height:360px",

                      HTML("<br><br><br>"),
                      #hr(),

                      # --------- Fishery panel guides ---------------------------------------------------------------------------------------------------------------

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==0",
                          h5("",style = "color:grey")
                      ),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==1",

                          column(12,
                            h5("Describe the fishery you are modelling and identify yourself and the relevant management agency.",style = "color:grey"),
                            h5("You can add additional introductory text or background references in the text box below.",style = "color:grey")

                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==2",
                          column(6,plotOutput("plotM",height=240)),

                          column(6,
                            h5("How long-lived is the fish species? This is a critical input determining stock productivity.",style = "color:grey"),
                            h5("The parameter M is the instantaneous natural mortality rate. For a review of data-limited methods of estimating M see",style = "color:grey"),
                            h5(a("Kenchington (2014).", href="http://onlinelibrary.wiley.com/doi/10.1111/faf.12027/abstract", target="_blank"),style = "color:grey"),

                            h5("The plot to the left shows survival rates at age for the longevity scenarios you have selected.",style = "color:grey"),
                            h5("The range in the maximum age (age at 2% survival) is plotted as vertical dashed lines.",style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==3",
                          column(6,plotOutput("plotD",height=240)),
                          column(6,
                            h5("Depletion (D), refers to current spawning stock biomass relative to the unfished level.",style = "color:grey"),
                            h5("Since depletion is a data-rich quantity it may not be readily quantified and it may be necessary to specify a wide range of uncertainty for this input to identify MPs that are suitably robust.",style = "color:grey"),
                            h5("In a data-limited situation, coarse information regarding depletion may be obtained from examining length compositions, historical versus current catch rates, or by use of so-called Robin-Hood approaches.",style = "color:grey"),
                            h5("For further information see ", a("Carruthers et al. (2014)", href="http://www.sciencedirect.com/science/article/pii/S0165783613003081", target="_blank"),style = "color:grey"),
                            h5("and ", a("Punt et al (2011)", href="https://academic.oup.com/icesjms/article/68/5/972/653125/Among-stock-comparisons-for-improving-stock", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==4",
                          column(6,plotOutput("ploth",height=240)),
                          column(6,
                            h5("How resilient to exploitation is the stock?",style = "color:grey"),
                            h5("This question controls recruitment compensation - the extent to which recruitment is reduced from unfished levels (R0) as the spawning stock becomes increasingly depleted below unfished levels (SSB0).",style = "color:grey"),
                            h5("Resilence is expressed in terms of steepness (h), which is the fraction of unfished recruitment at 1/5 of unfished spawning biomass.",style = "color:grey"),
                            h5("For a useful review of compensatory density dependence in fish populations see ", a("Rose et al. (2001).", href="http://hqczhzkgch48vzcc4czubj6v.wpengine.netdna-cdn.com/files/2012/07/Rose_etal_FishFisheries.pdf", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==5",
                          column(6,plotOutput("plotFP",height=240)),
                          column(6,
                            h5("What temporal pattern best describes the trend in historical annual fishing effort (e.g. boat-days per year, number of trips per year)?",style = "color:grey"),
                            h5("If more than one answer is given, historical fishing will be simulated subject to all trends in equal frequency.",style = "color:grey"),
                            h5("If a very specific pattern of effort is required, you can use the sliders to warp the effort patterns.",style = "color:grey"),
                            h5("This question specifies the possible range of mean trends, you will have an opportunity to adjust the extent of inter-annual variability and changes in fishing efficiency (catchability) in the following questions.",style = "color:grey"),
                            h5("Here is an introduction to fishing effort courtesy of the ", a("UN FAO.", href="http://www.fao.org/docrep/x5685e/x5685e04.htm", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==6",
                          column(6,plotOutput("plotF",height=240)),
                          column(6,
                            h5("The extent of interannual variability in historical exploitation rates around the mean trend(s) specified in Fishery question #5.",style = "color:grey"),
                            h5("Again, here is the introduction to effort and exploitation rate by the ", a("UN FAO.", href="http://www.fao.org/docrep/x5685e/x5685e04.htm", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==7",
                          column(6,plotOutput("plotqh",height=240)),
                          column(6,
                                h5("The annual percentage increase or decrease in historical fishing efficiency. In targeted fisheries gear efficiency may improve over time given techological improvements in the gear, changes in fishing behavior, fish distribution and information sharing among fishers, among other things. Conversely, non-target or bycatch species may be subject to declining fishing efficiency due to regulations or avoidance behaviors. The catchability (q) is the fraction of available fish caught per unit of effort. For example, a 2% per annum increase in fishing efficiency means that after 35 years twice as many fish will be caught for the same effort as today.",style = "color:grey"),
                                h5("The introduction to fishing efficiency by the FAO provides a ", a("basic summary.", href="http://www.fao.org/docrep/008/y6982e/y6982e06.htm", target="_blank"),style = "color:grey"),
                                h5(a("Arrenguin-Sanchez", href="https://drive.google.com/open?id=1ZrHbhotCJ5Vjw4JNloUSY94BVoM0iyfI", target="_blank")," provides a more comprehensive review of catchability.",style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==8",
                          column(6,plotOutput("plotq",height=240)),
                          column(6,
                                h5("The annual percentage increase or decrease in future fishing efficiency. In targeted fisheries gear efficiency may improve over time given techological improvements in the gear, changes in fishing behavior, fish distribution and information sharing among fishers, among other things. Conversely, non-target or bycatch species may be subject to declining fishing efficiency due to regulations or avoidance behaviors. The catchability (q) is the fraction of available fish caught per unit of effort. For example, a 2% per annum increase in fishing efficiency means that after 35 years twice as many fish will be caught for the same effort as today.",style = "color:grey"),
                                h5("The introduction to fishing efficiency by the FAO provides a ", a("basic summary.", href="http://www.fao.org/docrep/008/y6982e/y6982e06.htm", target="_blank"),style = "color:grey"),
                                h5(a("Arrenguin-Sanchez", href="https://drive.google.com/open?id=1ZrHbhotCJ5Vjw4JNloUSY94BVoM0iyfI", target="_blank")," provides a more comprehensive review of catchability.",style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==9",
                          column(6,plotOutput("plotLM",height=240)),
                          column(6,
                            h5("Size a maturity relative to asymptotic length (LM).",style = "color:grey"),
                            h5("Note 1: 'maturity' as used by this model (and most fish population dynamics models) is not really whether a fish has fully developed gonads, but rather the fraction of maximum spawning potential per weight. For example, some fishes mature early, but at small sizes they spawn infrequently and their recruits have poor survival (low spawning fraction).",style = "color:grey"),
                            h5("Note 2: asymptotic length is not the maximum length observed but rather the mean expected size of fish at their maximum age under unfished conditions",style = "color:grey"),
                            h5("An ICES workshop report provides ", a("an overview of maturity estimation.", href="http://www.ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/acom/2008/WKMOG/WKMOG08.pdf", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==10",
                          column(6,plotOutput("plotsel",height=240)),
                          column(6,
                            h5("Fishing gear selectivity relative to asymptotic length (S) (ascending limb selectivity). For example, if 50% of 40cm fish are caught and maximum length is 100cm, S = 0.4.",style = "color:grey"),
                            h5("The UN FAO provides an ", a("introduction to gear selectivity and how it may be quantified.", href="http://www.fao.org/docrep/w5449e/w5449e08.htm", target="_blank"),style = "color:grey"),
                            h5("For a more involved discussion on selectivity see the ", a("IATTC CAPAM workshop report.", href="https://swfsc.noaa.gov/publications/CR/2013/2013Crone.pdf", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==11",
                         column(6,plotOutput("plotdome",height=240)),
                         column(6,
                           h5("Fishing gear selectivity of the largest individuals (SL). For example, if only 20% of the longest fish are caught by the gear, SL = 0.2.",style = "color:grey"),
                           h5("Again, here is the UN FAO introduction to fishing gear selectivity ", a("introductory document.", href="http://www.fao.org/docrep/w5449e/w5449e08.htm", target="_blank"),style = "color:grey"),
                           h5("and here is the ", a("IATTC CAPAM workshop report.", href="https://swfsc.noaa.gov/publications/CR/2013/2013Crone.pdf", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==12",
                          column(6,plotOutput("plotDR",height=240)),
                          column(6,
                            h5("Discard rate (DR) is the fraction of fish that discarded both dead and alive",style = "color:grey"),
                            h5("The US National Marine Fisheries Service has a general guide to ", a("Understanding Fish Bycatch Discard and Escapee Mortality.", href="https://www.afsc.noaa.gov/quarterly/jfm03/featurejfm03.pdf", target="_blank"),style = "color:grey"),
                            h5("and one of the authors of that guide, Michael Davis also has a useful article: ", a("Key principles for understanding fish bycatch discard mortality.", href="https://drive.google.com/open?id=1ZtqB_MHapyagplv_enJ0o-_t4UKF5chh", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==13",
                          column(6,plotOutput("plotPRM",height=240)),
                          column(6,
                            h5("The post-release mortality rate (PRM) is the fraction of discarded fish that die after release",style = "color:grey"),
                            h5("The US National Marine Fisheries Service have a general guide to ", a("Understanding Fish Bycatch Discard and Escapee Mortality.", href="https://www.afsc.noaa.gov/quarterly/jfm03/featurejfm03.pdf", target="_blank"),style = "color:grey"),
                            h5("and one of the authors of that guide, Michael Davis also has a useful article: ", a("Key principles for understanding fish bycatch discard mortality.", href="https://drive.google.com/open?id=1ZtqB_MHapyagplv_enJ0o-_t4UKF5chh", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==14",
                           column(6,plotOutput("plotsigR",height=240)),
                           column(6,
                             h5("The interannual variability in recruitment is expressed here as the maximum inter-annual change. Recruitment is expected to change among years in response to spawning biomass levels. Additional variability may be driven by many factors including varying ocean conditions, amount of spawning habitat, food availability and predation.",style = "color:grey"),
                             h5("Recruitment variation is commonly described by the coefficient of variation in log-normal recruitment deviations (sigma R). An approximate rule of thumb is that 95% of recruitments fall in a range that is twice the sigma R. So given a sigma R of 10%, 95% of recruitments will fall within an interannual change of 20% of the mean recruitment predicted from spawning biomass.",style = "color:grey"),
                             h5("Edward Houde has authored a ", a("Comprehensive Review of Recruitment and Sources of Variability.", href="https://drive.google.com/open?id=19q_ct4Xd954H2jjotX3YKy0BJ-v53bt2", target="_blank"),style = "color:grey"),
                             h5("See also ", a("Chambers and Trippel (1997).", href="https://drive.google.com/open?id=1KhwP5URGPzW6ViJPiwprfY2tE2uucpDR", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==15",
                           column(6,plotOutput("plotAh",height=240)),
                           column(6,
                                  h5("The size of a existing spatial closure (Marine Protected Area, MPA). The size A, is the % of habitat that is protected.",style = "color:grey"),
                                  h5("The FAO provides a comprehensive ", a("review of Marine Protected Areas.", href="http://www.fao.org/docrep/015/i2090e/i2090e.pdf", target="_blank"),style = "color:grey")
                           )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==16",
                           column(6,plotOutput("plotVh",height=240)),
                           column(6,
                                  h5("Stock mixing in/out of existing spatial closure. The degree of the spatial mixing of the fish stock is represented as the probability (P) of a fish leaving area 1 (i.e. the marine protected area, MPA) between years",style = "color:grey"),
                                  h5("The FAO provides a comprehensive ", a("review of Marine Protected Areas.", href="http://www.fao.org/docrep/015/i2090e/i2090e.pdf", target="_blank"),style = "color:grey")
                           )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==17",
                           column(6,plotOutput("plotA",height=240)),
                           column(6,
                             h5("The size of a potential future spatial closure (Marine Protected Area, MPA). The size A, is the % of habitat that is protected.",style = "color:grey"),
                             h5("The FAO provides a comprehensive ", a("review of Marine Protected Areas.", href="http://www.fao.org/docrep/015/i2090e/i2090e.pdf", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==18",
                           column(6,plotOutput("plotV",height=240)),
                           column(6,
                            h5("Stock mixing in/out of a future spatial closure. The degree of the spatial mixing of the fish stock is represented as the probability (P) of a fish leaving area 1 (i.e. the marine protected area, MPA) between years",style = "color:grey"),
                            h5("The FAO provides a comprehensive ", a("review of Marine Protected Areas.", href="http://www.fao.org/docrep/015/i2090e/i2090e.pdf", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==1&output.Fpanel==19",
                           column(6,plotOutput("plotDh",height=240)),
                           column(6,
                                  h5("Initial depletion of the stock relative to asymptotic unfished levels (D1: spawning stock biomass in year 1 relative to equilibrium unfished conditions).",style = "color:grey"),
                                  h5("Many fisheries undertake large fluctuations in productivity. In some of these cases, a fishery may have began at a time when the stock was naturally low. This question provides an opportunity to specify this initial depletion. The default however is that the stock was at asymptotic unfished levels in the first year of the fishery.",style = "color:grey"),
                                  h5("For further information see ", a("Carruthers et al. (2014)", href="http://www.sciencedirect.com/science/article/pii/S0165783613003081", target="_blank"),style = "color:grey"),
                                  h5("and ", a("Punt et al (2011)", href="https://academic.oup.com/icesjms/article/68/5/972/653125/Among-stock-comparisons-for-improving-stock", target="_blank"),style = "color:grey")
                           )),


                      # -------- Management panel guides --------------------------------------------------------------------------------------------------------------

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==0",
                          h5("",style = "color:grey")
                      ),

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==1",
                         # h5("Possible modes of fishery management",style = "color:black"),
                          h5("Here you indicate which MPs are feasible given the management options that are available.", style = "color:grey"),
                          h5("Management procedures can provide management advice in terms of:",style="color:grey"),
                          h5(" - Total Allowable Catch (TAC, e.g. 20,000 metric tonnes).",style="color:grey"),
                          h5(" - Total Allowable Effort (TAE, e.g. 800 trap days per year).",style="color:grey"),
                          h5(" - Size limits (e.g. minimum size of 45cm).",style="color:grey"),
                          h5(" - Time-area closures (e.g. a permanent marine protected area or seasonal closure).",style="color:grey"),
                          h5(""),
                          h5("For more information see the ", a("UN FAO guide to fishery management types.", href="http://www.fao.org/docrep/005/y3427e/y3427e06.htm", target="_blank"),style = "color:grey"),
                          h5(""),
                          h5("Steffanson and Rosenberg describe and discuss fishery management types in ", a("their 2005 paper.", href="https://drive.google.com/open?id=1V5aMNf3raitNC515qyFfITDivgbXkU4X", target="_blank"),style = "color:grey")

                      ),

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==2",
                          column(7,plotOutput("plotIB",height=280)),
                          column(5,
                                 h5("What is the possible extent to which fishing operations may exceed (overages) or fall short (underages)
                                     of the specified Total Allowable Catch (TAC). For example, given a TAC of 1000 tonnes a 10% offset (overage) would on average lead to 1100 tonnes of fish taken.",style = "color:grey"),
                                 h5(""),
                                 h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey"),
                                 h5(""),
                                 h5("Fulton et al. also provide a discussion of implementation error in their ",a("2011 paper.", href="https://drive.google.com/open?id=1gzTMnk3Cg3PzgxDdKAyV52T9IIptUK7h", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==3",
                          column(7,plotOutput("plotIV",height=280)),
                          column(5,
                                h5("In the previous question you specified the range of the possible TAC offset (mean overage or underage).
                                   In this question you add the variability (V) in the implementation of TACs among years. For example, if on average there
                                   is no TAC offset, a V of 10% leads to annual overages/underages within 20% of the annual TAC recommendation (the black line in the figure opposite)
                                   for 95% of cases. The colored lines show the minimum and maximum variability superimposed on the lowest (dashed line) and highest
                                   (solid line) levels of overages/underages specified in the previous question.",style = "color:grey"),
                                h5(""),
                                h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey")
                      )),

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==4",
                         column(7,plotOutput("plotIB_E",height=280)),
                         column(5,
                                h5("What is the possible extent to which fishing operations may exceed (overages) or fall short (underages)
                                of the specified Total Allowable Effort (TAE)? For example, given a TAE of 2000 boat-days of fishing a 10% overage would on average lead to 2200 boat days of effort.",style = "color:grey"),
                                h5(""),
                                h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey"),
                                h5(""),
                                h5("Fulton et al. also provide a discussion of implementation error in their ",a("2011 paper.", href="https://drive.google.com/open?id=1gzTMnk3Cg3PzgxDdKAyV52T9IIptUK7h", target="_blank"),style = "color:grey")
                         )),

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==5",
                         column(7,plotOutput("plotIV_E",height=280)),
                         column(5,
                                h5("In the previous question you specified the range of possible TAE offset (mean overages/underages).
                                   In this question you add the variability (V) in the implementation of TAEs among years. For example, if on average there
                                   is no TAE offset, a V of 20% leads to annual TAE overages/underages within 40% of the annual TAE recommendation (the black line in the figure opposite)
                                   for 95% of cases. The colored lines show the minimum and maximum variability superimposed on the lowest (dashed line) and highest
                                   (solid line) levels of overages/underages specified in the previous question.",style = "color:grey"),
                                              h5(""),
                                h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey")
                         )),

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==6",
                         column(7,plotOutput("plotIB_SL",height=280)),
                         column(5,
                                h5("What is the possible extent to which fishing operations may exceed (catch larger) or fall short (catch smaller)
                                fish than the specified minimum size limit? For example, given a size limit of 20cm (e.g. escape hole size of a trap), a value of 20% would lead to a mean minimum size in the catch of 24cm.",style = "color:grey"),
                                h5(""),
                                h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey"),
                                h5(""),
                                h5("Fulton et al. also provide a discussion of implementation error in their ",a("2011 paper.", href="https://drive.google.com/open?id=1gzTMnk3Cg3PzgxDdKAyV52T9IIptUK7h", target="_blank"),style = "color:grey")
                         )),

                      conditionalPanel(condition="input.tabs1==2&output.Mpanel==7",
                         column(7,plotOutput("plotIV_SL",height=280)),
                         column(5,
                                h5("In the previous question you specified the range of possible mean violations of a minimum size limit.
                                   In this question you add variability (V) in size limit implementation among years. For example a size limit of 90cm is exceeded by an average of 10cm, a value of 5% leads to minimum catch sizes of between 90cm and 110cm (the black line in the figure opposite)
                                   for 95% of cases. The colored lines show the minimum and maximum variability superimposed on the lowest (dashed line) and highest
                                   (solid line) offset in size limit specified in the previous question.",style = "color:grey"),
                                              h5(""),
                                h5("The FAO provides a cursory introduction to uncertainties in fisheries management including implementation error ",a("here.", href="http://www.fao.org/docrep/003/v8400e/V8400E03.htm", target="_blank"),style = "color:grey")
                         )),






                      # -------- Data panel guides --------------------------------------------------------------------------------------------------------------------

                      conditionalPanel(condition="input.tabs1==3&output.Dpanel==0",
                          h5("",style = "color:grey")
                      ),

                      conditionalPanel(condition="input.tabs1==3&output.Dpanel==1",
                          h5("Management procedures (MPs) require various data. Data availability will determine what MPs are feasible.", style = "color:grey"),
                          h5("Annual catches are yearly reported landings (e.g. 135 tonnes in 1998, 159 tonnes in 1999, etc).", style = "color:grey"),
                          h5("Relative abundance indices may be fishery-dependent such as catch-per-unit-effort data, or fishery-independent such as an annual abundance survey.", style = "color:grey"),
                          h5("In the context of annual catches and relative abundance indices, 'historical' refers to data going back to 'unfished conditions' (pre industrial fishing) such that catches may be used to reconstruct stock trajectory and indices may infer current stock depletion.", style = "color:grey"),
                          h5("In contrast, 'recent' refers to data available for least 5 years from the present", style = "color:grey"),
                          h5("Effort data are annual observations of fishing effort such as boat days in 2002.", style = "color:grey"),
                          h5("Growth data refers to estimates for growth parameters such as von Bertalanffy growth parameter (K) and mean asymptotic length (L-infinity).", style = "color:grey"),
                          h5("Size and age composition data are samples of sizes and ages in the catch going back at least 2 years from the present", style = "color:grey"),
                          h5(""),
                          h5("If you require further guidance on what types of data are available in your fishery see the MERA manual: ", a("Section 6.", href="http://www.sciencedirect.com/science/article/pii/S0165783613003081", target="_blank"),style = "color:grey")

                      ),

                      conditionalPanel(condition="input.tabs1==3&output.Dpanel==2",
                          column(7,plotOutput("plotCB",height=280)),
                          column(5,
                                h5("Catch reporting bias includes a chronic misreporting of the catch over time.",style = "color:grey"),
                                h5("In some data-limited fisheries, incomplete monitoring of fishing operations may lead to under-reporting (and to a lesser extent over-reporting) of annual catches.",style = "color:grey"),
                                h5(""),
                                h5("For further discussion of catch under reporting see",a("Agnew et al. (2009).", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2646833/", target="_blank"),style = "color:grey")
                          )

                      ),

                      conditionalPanel(condition="input.tabs1==3&output.Dpanel==3",
                          column(7,plotOutput("plotBeta",height=280)),
                          column(5,
                                h5("Is the primary index of relative abundance proportional to real biomass? Indices of relative abundance derived from fishery
                                   catch-per-unit effort (CPUE) may decline faster than real abundance (hyperdepletion) in cases where, for example, the
                                   species is being avoided or there has been attrition of high-density sub-population structure during early commericial
                                   fishing. Conversely CPUE data may respond slower than real biomass changes (hyperstability) if the species is being targeted,
                                   there is range contraction of fishing toward high density areas as the stock declines or the population naturally forms aggregations.
                                   For example, purse-seine fisheries are often strongly hyperstable since the fish per aggregation may remain high even at low stock sizes.
                                   It may be generally assumed that a well designed fishery-independent survey is proportional to abundance but there are notable exceptions.",style = "color:grey"),
                                #h5(""),
                                #tagList("See ",a("Hilborn and Walters. (1992)", href="https://books.google.ca/books?id=Y0EGCAAAQBAJ&pg=PA190&lpg=PA190&dq=hyperstability+fisheries&source=bl&ots=v3jjRE1mwh&sig=XBbO2o7JvBqEwISAdQE83xMU5v0&hl=en&sa=X&ved=0ahUKEwiA__KW8-zZAhUJ3WMKHeL3CQ4Q6AEISjAF#v=onepage&q=hyperstability%20fisheries&f=false")),
                                h5("See ",a("Erisman et al. (1992)", href="https://drive.google.com/open?id=1jwhIGfTmXewKWGSTNyyjoo4TefW2JiNR", target="_blank"),style = "color:grey"),
                                h5("or ",a("Maunder et al. (2006)", href="https://drive.google.com/open?id=1chNF72tCB_fjTjbbhZk7EyxtRU810EIc", target="_blank"),style = "color:grey")
                          )
                      ),

                      conditionalPanel(condition="input.tabs1==3&output.Dpanel==4",
                          #column(7,plotOutput("plotErr",height=280)),
                          column(12,
                                h5("What is the overall quality of data that are available?",style = "color:grey"),
                                h5("Perfect Information: an unrealistic and idealized observation model for testing the theoretical performance of MPs.",style = "color:grey"),
                                h5("Good quality: annual catches and abundance indices are observed with low error (<20% CV) and length/age composition data are numerous (~100 independent observations per year).",style = "color:grey"),
                                h5("Data moderate: annual catches and abundance indices are observed with greater error (<30% CV) and length/age composition data are fewer (~40 independent samples per year).",style = "color:grey"),
                                h5("Data poor: annual catches and abundance indices are imprecisely observed (<50% CV) and length/age composition data are sparse (~15 independent samples per year).",style = "color:grey"),
                                h5(""),
                                h5("A description of the observation error model is included in ",a("Carruthers et al (2013)", href="https://drive.google.com/open?id=1EX6mu9HOb4jwlQF-Co6DQ-FJzwTcO7JP", target="_blank"),style = "color:grey"),
                                h5(" and a similar model was used by ",a("Carruthers et al. (2015).", href="https://drive.google.com/open?id=1xvNxp_3oUOhSaLARY_mI2cAiG2qFAgqN", target="_blank"),style = "color:grey")
                         )
                      ),


                      conditionalPanel(condition="input.tabs1==4|(input.tabs1==1 & (output.Fpanel==0&output.Dpanel==0&output.Mpanel==0))|(input.tabs1==2&(output.Fpanel==0&output.Dpanel==0&output.Mpanel==0))|(input.tabs1==3&(output.Fpanel==0&output.Dpanel==0&output.Mpanel==0))|(input.tabs1==5&(output.Fpanel==0&output.Dpanel==0&output.Mpanel==0))",
                                       column(12,
                                              h5("The Fishery, Management and Data questions specify the range of operating model simulations in the MSE.
                                                 The questions are presented in order of general importance and default to maxmum uncertainty.
                                                 At any stage you can select an analysis type and press 'CALCULATE'.
                                                 However it is desirable where possible to narrow the range of uncertainty in fishery dynamics.
                                                 As you work through the questions in the Fishery, Management and Data panels, you can narrow the range of simulated fisheries but you should provide justification for each selection in the justification box.",style = "color:grey")

                                        )
                      ),
                      conditionalPanel(condition="input.tabs1==5 & (output.Dpanel>0 | output.Fpanel>0 | output.Mpanel>0)",
                           column(12,

                             h5("Users can also determine the total number of simulations, the number of projected years and the management update interval (years between management recommendations in the projection).
                                 The burn-in is intended to represent a duration over which an MP has already been used. Burn-in is also the number of initial projected years correponding to some stock status performance indicators. ",style = "color:grey"),
                             h5("Users can also choose to exclude reference management procdures (e.g. zero catches, fishing at FMSY), activate parallel computation if more than 48 simulations are specified (which is much faster but there is no MSE progress bar).",style = "color:grey"),
                             h5("The Application step requires the selection of a single MP. Other options include the loading of custom DLMtool/MSEtool code (MPs, performance metrics and MSE controls)",style = "color:grey"),
                             h5(""),
                             h5("A more detailed guide to these options can be found in the MERA manual ",a("Section 7.", href="www.datalimitedtoolkit.org", target="_blank"),style = "color:grey"),
                             h5(""),
                             h5("NOTE: a few features are currently not available such as the ability to specify Low Trophic Level (LTL) species for an alternative
                                performance evaluation, the ability to upload indicator data and select variables for power analysis.",style = "color:grey")

                             )
                      )

               )

             )
        ),

       column(1),
       column(10,style="height:180px",
             fluidRow(

               column(width = 12,

                  conditionalPanel(condition="(input.tabs1==1 & (output.Fpanel!=0 & output.Fpanel!=undefined))|(input.tabs1==2 & (output.Mpanel!=0 & output.Mpanel!=undefined))|(input.tabs1==3 & (output.Dpanel!=0 & output.Dpanel!=undefined))",
                    HTML("<br>"),
                    textAreaInput("Justification", "", "< No reason for selection was provided >", height = "120px")
                  ),
                  conditionalPanel(condition="!((input.tabs1==1 & (output.Fpanel!=0 & output.Fpanel!=undefined))|(input.tabs1==2 & (output.Mpanel!=0 & output.Mpanel!=undefined))|(input.tabs1==3 & (output.Dpanel!=0 & output.Dpanel!=undefined)))",
                    HTML("<br>"),
                    textAreaInput("blank",h5("",style = "color:grey"), "", height = "120px")
                  )


               )


            )
        ),
        column(12,
          fluidRow(
          column(1),
          column(4,style="height:80px",
              fluidRow(

                 column(width = 2,
                   conditionalPanel(condition="(input.tabs1==1 & output.Fpanel>1)|(input.tabs1==2 & output.Mpanel>1)|(input.tabs1==3 & output.Dpanel>1)",
                      actionButton("Fback","< Back")
                   ),
                   conditionalPanel(condition="!((input.tabs1==1 & output.Fpanel>1)|(input.tabs1==2 & output.Mpanel>1)|(input.tabs1==3 & output.Dpanel>1))",
                                    actionButton("FbackD","< Back",style="color: #CFCFCF;  border-color: #CFCFCF") #background-color: #CFCFCF;
                   )

                 ),

                 column(width = 2,
                   conditionalPanel(condition="(input.tabs1==1 & output.Fpanel<19)|(input.tabs1==2 & output.Mpanel<7)|(input.tabs1==3 & output.Dpanel<4)",
                      actionButton("Fcont","Next >")
                   ),
                   conditionalPanel(condition="!((input.tabs1==1 & output.Fpanel<19)|(input.tabs1==2 & output.Mpanel<7)|(input.tabs1==3 & output.Dpanel<4))",
                      actionButton("FcontD","Next >",style="color: #CFCFCF;  border-color: #CFCFCF") #background-color: #CFCFCF;
                   )

                 ),

                 column(width=4,#style="height:180px",
                        conditionalPanel(condition="output.Fpanel>0|output.Ppanel>0|output.Dpanel>0|output.Fpanel!=undefined|output.Mpanel!=undefined|output.Dpanel!=undefined",
                                         textOutput("Fpanelout"),
                                         textOutput("Mpanelout"),
                                         textOutput("Dpanelout")

                        )
                 )



              )
           ),

           column(2),

           column(4,style="height:50px",

                 column(6,style="padding:10px",
                        fileInput("Load","Load  (.mera)",accept=c("mera",".mera"))
                 ),

                 column(2,

                          h5("Save",style="font-weight:bold"),
                          downloadButton("Save","",width=70)
                ),

                 column(4,
                       h5("Questionnaire Report",style="font-weight:bold"),
                       downloadButton("Build_OM"," ")
                 )
           )

          )
        )
       ), # end of Step 1 fluid row

       column(12,style="height:25px"),

       conditionalPanel(condition="input.Mode=='Advanced'",

         conditionalPanel(condition="output.CondOM==0",h4("STEP A2: LOAD AVAILABLE DATA (OPTIONAL)")),
         conditionalPanel(condition="output.CondOM==1",h4("STEP A2: LOAD AVAILABLE DATA (OPTIONAL)",style="color:green")),

         hr(),

         fluidRow(
            column(1),
            column(11,style="height:155px",

               fluidRow(
                 column(3,style="padding:7px;padding-left:14px",
                        fileInput("Load_Data","Load available data  (.csv)")
                 ),
                 column(1),
                 column(6,style="padding:19px",
                            h5("When formatted into a DLMtool/MSEtool csv data file, fishery data can be used to:",style = "color:grey"),
                            h5(" - condition operating models",style = "color:grey"),
                            h5(" - determine feasible MPs", style = "color:grey"),
                            h5(" - assess the fishery status", style = "color:grey"),
                            h5(" - test for exceptional circumstances.",style = "color:grey"),
                            h5("A description of the data object can be found ",a("here", href="https://dlmtool.github.io/DLMtool/cheat_sheets/Data", target="_blank"),style = "color:grey")

                 )
              )
            )
          ),

          fluidRow(
              column(1),
              column(6),
              column(4,
                    column(8),
                    column(4,
                           conditionalPanel(width=4,condition="output.Data==1",
                            h5("Data Report",style="font-weight:bold"),
                            downloadButton("Build_Data"," ")
                           )
                    )
              )
          )
        ),  # RA MSE conditional panel

        column(12,style="height:45px"),

        conditionalPanel(condition="output.MadeOM==0",h4("STEP B: BUILD OPERATING MODELS")),
        conditionalPanel(condition="output.MadeOM==1",h4("STEP B: BUILD OPERATING MODELS",style="color:green")),

        hr(),

        fluidRow(
           column(1),
           column(11,

              fluidRow(

                column(4,
                numericInput("nsim", label = "No. simulations", value=24,min=8,max=1000),

                conditionalPanel(condition="output.Data==1",
                                 selectInput("Cond_ops", label = "Conditioning Method", choices=c("None"),selected="None")

                                 ),

                actionButton("Build_OM_2",h5("BUILD OPERATING MODEL",style="color:red"))

                ),

                column(6,style="padding-left:8px",
                       style="padding:19px",
                       h5("Operating models are specified from the responses in the questionnaire (Step A)", style = "color:grey"),
                       h5("Alternatively, in MSE mode, users can use upload their data and condition models using
                          stochastic SRA ",a("(Walters et al. 2006)", href="https://drive.google.com/open?id=10kpQwOsvsDCojN2MyUYgRj9-IUQMQmAB", target="_blank"),style = "color:grey"),
                       h5("For demonstration purposes a small number of simulations (e.g. n = 24) is enough. For MP comparisons in 'Evaluation' mode,
                          100 simulations is generally sufficient to get convergence in performance rankings. In more detailed Application or Indicator
                          steps where specific MPs are tested, a larger number is recommended (e.g. n = 200) to get stable absolute performance", style = "color:grey")

                      )

              )
           )
        ),

        fluidRow(
          column(7),
          column(4,

                column(6,style="padding:10px",
                       conditionalPanel(condition="input.Mode=='Advanced'",

                       fileInput("Load_OM","Load  (.OM)"))
                ),

                column(2,
                       conditionalPanel(condition="output.MadeOM==1&input.Mode=='Advanced'",
                         h5("Save",style="font-weight:bold"),
                         downloadButton("Save_OM","",width=70)
                       )
                ),

                column(4,

                  conditionalPanel(condition="output.MadeOM==1",

                     h5("OM Report",style="font-weight:bold"),
                     downloadButton("Build_full_OM","")

                  ),

                  conditionalPanel(condition="output.CondOM==1",

                      h5("Conditioning Report",style="font-weight:bold"),
                      downloadButton("Build_Cond","")

                  )
                )

          )
        ),

        column(12,style="height:45px"),

        conditionalPanel(condition="output.Calc==0&input.Mode=='Advanced'",h4("STEP C1: EVALUATION (MULTI-MP)")),
        conditionalPanel(condition="output.Calc==1&input.Mode=='Advanced'",h4("STEP C1: EVALUATION (MULTI-MP)",style="color:green")),
        conditionalPanel(condition="output.Calc==0&input.Mode=='Streamlined'",h4("STEP C: EVALUATION (MULTI-MP)")),
        conditionalPanel(condition="output.Calc==1&input.Mode=='Streamlined'",h4("STEP C: EVALUATION (MULTI-MP)",style="color:green")),

        hr(),

        fluidRow(
          column(1),
          column(11,
             fluidRow(
                 column(4,conditionalPanel(condition="output.MadeOM==1",

                   column(6,numericInput("proyears", label = "Projected years", value=50,min=25,max=100)),
                   column(6,numericInput("interval", label = "Management interval", value=8,min=2,max=10)),

                   #column(4,checkboxInput("Demo", label = "Demo mode", value=TRUE)),
                   column(12,radioButtons('MPset',label="MP set",choices=c("Risk Assessment","Top 20","All","Demo"),selected="Demo",inline=T)),

                   column(12,h5("Additional options",style="font-weight:bold"),style="height:22px"),
                   #column(12,checkboxInput("Ex_Ref_MPs", label = "No ref. MPs", value = FALSE),
                   #        checkboxInput("Data_Rich", label = "Data-rich MPs", value = FALSE),
                   #
                   #checkboxInput("Parallel", label = "Parallel comp.", value = FALSE)),

                   column(4,checkboxInput("Ex_Ref_MPs", label = "No ref. MPs", value = FALSE),style="padding-top:0px"),
                   column(4,checkboxInput("Data_Rich", label = "Data-rich MPs", value = FALSE),style="padding-top:0px"),
                   column(4,checkboxInput("Parallel", label = "Parallel comp.", value = FALSE),style="padding-top:0px"),

                   column(12,actionButton("Calculate",h5("      RUN EVALUATION     ",style="color:red")))

                   ),
                   conditionalPanel(condition="output.MadeOM==0",

                          h5("Operating model not built yet (Step B)", style = "color:grey")

                  )
                ),

                column(6,

                       h5("Simulations can be run to test Multiple MPs over a certain number of projected years in which managment recommendations are updated every 'interval' years", style = "color:grey"),
                       h5("- Risk Assessment: a set of MPs reflecting status quo management including reference management approaches", style = "color:grey"),
                       h5("- Top 20: MPs that generally perform well in many cases but may not be appropriate for your operating model", style = "color:grey"),
                       h5("- All: an MSE is run for all available MPs (~100) which can take 20 minutes or more", style = "color:grey"),
                       h5("- Demo: a small selection of fast-running MPs for MERA demonstration purposes only", style = "color:grey"),
                       h5("Users may wish not to include reference MPs (No ref. MPs) that include perfect FMSY management and zero catches. Alternatively they may wish to test data-rich MPs that are slower to run", style = "color:grey"),
                       h5("In situations where operating models are built with more than 48 simulations it can be much faster to use parallel computing ('Parallel comp.)
                          although the progress bar will not longer work ",style="color:grey"),
                       h5("Documentation of the various MPs can be found as links in the results tables, below in the help section or online ",a("here", href="https://dlmtool.github.io/DLMtool/reference/index.html", target="_blank"),style = "color:grey")

               )
             )

             )

        ),


        fluidRow(
          column(1),
          column(6),
          column(4,
                 column(6,style="padding:10px",
                        fileInput("Load_Eval","Load  (.Eval)")
                 ),

                 column(2,
                        conditionalPanel(condition="output.Calc==1",
                           h5("Save",style="font-weight:bold"),
                           downloadButton("Save_Eval","",width=70)
                        )

                 ),
                 column(4,

                        conditionalPanel(condition="output.Calc==1",
                              h5("Evaluation Report",style="font-weight:bold"),
                              conditionalPanel(condition="input.Perf_type=='MSC continuity'",downloadButton("Build_Eval","")),
                              conditionalPanel(condition="input.Perf_type=='MSC'",downloadButton("Build_Eval_MSC",""))
                        )

                 )
          )
        ),

        column(12,style="height:15px"),

        conditionalPanel(condition="input.Mode=='Advanced'",
          conditionalPanel(condition="output.App==0",h4("STEP C2: APPLICATION (SINGLE MP)")),
          conditionalPanel(condition="output.App==1",h4("STEP C2: APPLICATION (SINGLE MP)",style="color:green")),

          hr(),

          fluidRow(
            column(1),
            column(11,
                   fluidRow(
                     column(4,
                       conditionalPanel(condition="output.MadeOM>0",
                        column(6,numericInput("proyears_app", label = "Projected years", value=50,min=25,max=100)),
                        column(6,numericInput("interval_app", label = "Management interval", value=8,min=2,max=10)),
                        column(6,selectInput("sel_MP", label = "Selected MP", choices=character(0),selected=character(0)),style="padding:10px"),
                        column(6,checkboxInput("Parallel_app", label = "Parallel comp.", value = FALSE)),

                        column(12,
                               actionButton("Calculate_app",h5("      RUN APPLICATION     ",style="color:red"))
                        )

                       ),
                       conditionalPanel(condition="output.MadeOM==0",
                            h5("Operating model not built yet (Step B)", style = "color:grey")
                      )
                     ),
                     column(6,
                         h5("In the application mode, a single MP is tested over a greater number of simulations.", style = "color:grey")
                     )
                   )
            )
          ),

          fluidRow(
            column(1),
            column(6),
            column(4,
                   column(6,style="padding:10px",
                          fileInput("Load_App","Load  (.App)")
                   ),

                   column(2,
                          conditionalPanel(condition="output.App==1",
                                           h5("Save",style="font-weight:bold"),
                                           downloadButton("Save_App","",width=70)
                          )

                   ),
                   column(4,

                          conditionalPanel(condition="output.App==1",
                                 column(12,style="height:50px",
                                        h5("Application Report",style="font-weight:bold"),
                                        conditionalPanel(condition="input.Perf_type=='MSC continuity'",downloadButton("Build_App","")),
                                        conditionalPanel(condition="input.Perf_type=='MSC'",downloadButton("Build_App_MSC",""))

                                 )
                          )

                   )
            )
          )
        ),

        conditionalPanel(condition="input.Mode=='Advanced'",
          column(12,style="height:15px"),

          conditionalPanel(condition="output.Ind==0",h4("STEP D: ANCILLARY INDICATORS (SINGLE MP - USES APPLICATION MSE OF STEP B2)")),
          conditionalPanel(condition="output.Ind==1",h4("STEP D: ANCILLARY INDICATORS (SINGLE MP - USES APPLICATION MSE OF STEP B2)",style="color:green")),

          hr(),

          fluidRow(
            column(1),
            column(11,#style="height:285px",

                   fluidRow(

                     column(3,style="padding:7px;padding-left:14px",

                            conditionalPanel(condition="output.DataInd==0",

                              h5("Compatible data file not loaded (at least three more years of data than LHYear)", style = "color:grey")

                            ),
                            conditionalPanel(condition="output.App==0",
                              h5("Application not run yet (Step C2)", style = "color:grey")
                            ),
                            conditionalPanel(condition="output.DataInd==1&output.App==1",
                                actionButton("Calculate_Ind",h5(" DETECT EXCEPTIONAL CIRCUMSTANCES  ",style="color:red"))
                            )

                     ),
                     column(1),
                     column(6,style="padding:19px",
                          h5("A similar data file to step A2 can be loaded here with extended data for years after operating model conditioning",style = "color:grey"),
                          h5("These data can be compared against the predicted data of the Application operating model and used to detect exceptional
                               circumstances using the method of ",a("Carruthers and Hordyk (2018)", href="https://drive.google.com/open?id=1Liif_ugfDbzIKZMBusHNemgfi3cohvtr", target="_blank"),style = "color:grey"),
                          h5("Resolution refers to the size of time block over which the indicator is evaluated. For example, the default, 6 years, calculates slopes and means in quantities such as catch and abundance indices over the first 6 years (you need new data for at least this many years)",style = "color:grey")
                     )
                 )
            )
          ),

          fluidRow(
            column(1),
            column(6),
            column(4,
                   column(8),
                   column(4,
                          conditionalPanel(width=4,condition="output.DataInd==1",
                                  h5("Indicator Report",style="font-weight:bold"),
                                  downloadButton("Build_AI"," ")
                          )
                   )
             )
          )
        ), # input.Mode conditional panel

        #conditionalPanel(width=12,condition="input.Mode=='Advanced'",

         #            column(12,style="height:15px"),

          #           h4("STEP E: ADVICE"),
    #           hr(),

    #           conditionalPanel(width=4,condition="output.Data==0",{


    #    fluidRow(
    # column(1),
    #  column(11,h5("Data not loaded (Step A2)", style = "color:grey")


    #   )
    #  )

    # }),

    # conditionalPanel(width=4,condition="output.Data==1",{


    #  fluidRow(
    #   column(1),
    #  column(11,

    #    fluidRow(
    #   column(2,
    #          actionButton("calcAdvice","Calculate Advice")
    #   )

    #   )
    # )

    # )


    # })

    # ), # end of ADVICE


        column(12,style="height:15px"),

        h4("RESULTS"),
        hr(),

        fluidRow(
          column(1),
          column(11,

            fluidRow(
              column(2,
                conditionalPanel(width=4,condition="(output.App==1|output.Calc==1)&(input.Res_Tab==1|input.Res_Tab==2)",

                  column(12,HTML("<br>")),
                  column(12,conditionalPanel(condition="output.Tweak==1",actionButton("Redo",h5(" REFRESH RESULTS ",style="color:red"))),style="height:45px"),
                  column(12,HTML("<br>","<br>")),
                  numericInput("burnin", label = "Burn-in years", value=10,min=5,max=20),
                  numericInput("ntop", label = "Number of top MPs to display", value=10,min=1,max=80),
                  #checkboxInput("LTL", label = "Low Trophic Level PIs", value = FALSE),
                  selectInput("Perf_type", label = "Performance metrics", choices=c("MSC","MSC continuity"),selected="MSC"),
                  column(12,conditionalPanel(condition="output.Data==1",checkboxInput("Fease", label = "Advanced data feasibility", value = FALSE))),
                  column(12,HTML("<br>","<br>")),
                  h5("Probability Thresholds",style="font-weight:bold"),
                  hr(),
                  conditionalPanel(condition="input.Perf_type=='MSC continuity'",
                    sliderInput("P111a","P.1.1.1a",min=0,max=100,value=70,step=5),
                    sliderInput("P111b","P.1.1.1b",min=0,max=100,value=50,step=5),
                    sliderInput("P112","P.1.1.2",min=0,max=100,value=70,step=5),
                    sliderInput("P121a","P.1.2.1a",min=0,max=100,value=80,step=5),
                    sliderInput("P121b","P.1.2.1b",min=0,max=100,value=50,step=5),
                    actionButton("Default_thres","Reset to default thresholds")
                  ),
                  conditionalPanel(condition="input.Perf_type=='MSC'",
                   sliderInput("P_STL","Stock Status - Limit",min=0,max=100,value=70,step=5),
                   sliderInput("P_STT","Stock Status - Target",min=0,max=100,value=50,step=5),
                   sliderInput("P_LTL","Harvest Strategy - Limit",min=0,max=100,value=80,step=5),
                   sliderInput("P_LTT","Harvest Strategy - Target",min=0,max=100,value=50,step=5),
                   actionButton("Default_thres_MSC","Reset to default thresholds")
                  )

                ), # end of app or eval control panel

                conditionalPanel(width=4,condition="output.Ind==1&input.Res_Tab==3",
                  #sliderInput("Ind_Res","Resolution (yrs)",min=3,max=15,value=3,step=1),
                  sliderInput("Ind_Alpha","Type I error (Prob false positive rejection, alpha)",min=0.01,max=0.25,value=0.05,step=0.01)
                ),# end of indicator control panel

                conditionalPanel(width=4,condition="output.Data==1 & input.Res_Tab==4",

                  conditionalPanel(width=4,condition="output.Calc==1 & output.App==1",

                        radioButtons("Advice_allMPs",label="Calculate advice for a set of MPs",
                                     choices = c("Custom MPs","Application MP","Evaluation MPs","All feasible MPs"),selected="Custom MPs")

                  ),

                  conditionalPanel(width=4,condition="output.Calc==0 & output.App==1",

                       radioButtons("Advice_nE",label="Calculate advice for a set of MPs",
                                    choices = c("Custom MPs","Application MP","All feasible MPs"),selected="Custom MPs")

                  ),

                  conditionalPanel(width=4,condition="output.Calc==1 & output.App==0",

                       radioButtons("Advice_nA",label="Calculate advice for a set of MPs",
                                    choices = c("Custom MPs","Evaluation MPs","All feasible MPs"),selected="Custom MPs")

                  ),

                  conditionalPanel(width=4,condition="output.Calc==0 & output.App==0",

                       radioButtons("Advice_nEA",label="Calculate advice for a set of MPs",
                                    choices = c("Custom MPs","All feasible MPs"),selected="Custom MPs")

                  ),

                  selectInput("Advice_MP1", label = "Custom MP 1", choices=c("curE","curC","curC75"),selected="curE"),
                  selectInput("Advice_MP2", label = "Custom MP 2", choices=c("curE","curC","curC75"),selected="curC"),
                  selectInput("Advice_MP3", label = "Custom MP 3", choices=c("curE","curC","curC75"),selected="curC75")

                )

              ),


              column(10,
               tabsetPanel( id = "Res_Tab",selected=1,
                tabPanel(h4("Evaluation",style = "color:black"),
                         conditionalPanel(condition="output.Calc==0&input.Mode=='Streamlined'",
                                          h5("Evaluation MSE not run yet (Step C)", style = "color:grey")
                         ),
                         conditionalPanel(condition="output.Calc==0&input.Mode=='Advanced'",
                                          h5("Evaluation MSE not run yet (Step C1)", style = "color:grey")
                         ),
                         conditionalPanel(condition="output.Calc==1",

                                fluidRow(
                                  column(width = 12,

                                         column(width = 12,h5("Performance Indicator Table",style="font-weight:bold")),
                                         conditionalPanel(condition="input.Perf_type=='MSC continuity'",
                                                            column(width=12,h5("< These performance metrics have been kept for App debugging reasons > The Performance Indicator Table includes the probabilities of each MP achieving the relevant MSC PI
                                                            thresholds for stock status (PI 1.1.1), rebuilding (PI 1.1.2) and harvest strategy (PI 1.2.1).  The MPs are presented in
                                                            order of projected long-term yield (relative to the MP of highest yield).
                                                            MPs that pass all PI thresholds are in green and those that do not are presented in red.  MPs that are
                                                            not available for use with current data are listed in black and the lacking data are listed in the last column to the
                                                            right."))),
                                         conditionalPanel(condition="input.Perf_type=='MSC'",
                                                          column(width=12,h5(" The Performance Indicator Table includes the 'Stock Status' metrics - the probabilities of each MP exceeding the limit (0.5 BMSY) and
                                                            the target (BMSY) biomass levels over the short term (burnin years). Also tabulated are the 'Harvest Strategy' metrics. These are similar but are evaluated over the long term (burnin-50 years).
                                                            MPs that pass all probability thresholds are in green and those that do not are presented in red.  MPs that are  not available for use with current data are listed in black and the lacking data are listed in the last column to the
                                                            right."))),
                                         column(width=12,h5("MPs colored green are feasible and pass all of the performance indicator thresholds. MPs colored red are feasible but
                                                            do not pass performance indicator thresholds. MPs colored black are not feasible. The column 'Reason not feasible'
                                                            explains the reason for this and can be due to data restrictions (D) controlled by data question 1, and/or
                                                            management restrictions (M) controled by management question 1.")),
                                         tableOutput('Ptable'),
                                         tableOutput('threshtable')

                                         ),
                                  column(width = 12,

                                         column(width = 12,h5("Performance Trade-offs",style="font-weight:bold")),

                                         conditionalPanel(condition="input.Perf_type=='MSC'",
                                                          column(width=12,h5("The trade-off performance indicator Table includes all those MPs that satsified performance thresholds above.
                                                                             Rebuilding is the probability that the stock rebuilds to BMSY levels after two mean generation times. Relative yield is the yield relative to the highest yield MP")),
                                                          tableOutput('Ptable2'))

                                  ),

                                  column(width = 4,

                                         column(width = 12,h5("Short-term stock status vs long term yield performance trade-off",style="color::grey")),
                                         plotOutput("P1_LTY",height="auto")

                                  ),
                                  column(width = 4,

                                         column(width = 12,h5("Long-term stock status vs long term yield performance trade-off",style="color::grey")),
                                         plotOutput("P2_LTY",height="auto")

                                  ),

                                  column(width = 4,

                                         column(width = 12,h5("Rebuilding performance vs long term yield trade-off",style="color::grey")),
                                         plotOutput("P3_LTY",height="auto")

                                  )
                                ), # end of fluid row

                                fluidRow(
                                  column(width = 12,h5("B/BMSY and Yield (relative to today) projection plots",style="font-weight:bold")),
                                  column(width=12,h5("Projections of biomass and yield relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations")),
                                  plotOutput("wormplot",height="auto"),

                                  column(width = 12,h5("Rebuilding analysis",style="font-weight:bold")),
                                  column(width=12,h5("Projections of biomass relative to MSY and unfished (B0) levels. The rebuilding analysis simulates the fishery currently in a depleted state even if the user-specified depletion in the operating model is higher.
                                                     In these cases, the rebuilding analysis provides added assurance whether a particular management procedure would be likely to rebuild the stock if the user-specified depletion level is overly optimistic and the stock status is more depleted in actuality, and thus in need of rebuilding.
                                                     The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations")),
                                  plotOutput("wormplot2",height="auto"),

                                  column(width = 12,h5("PI.1.1.1 uncertainties",style = "font-weight:bold")),
                                  column(width=12,h5("These plots show how many simulations could be assigned to each of the SG regions defined by PI.1.1.1 ")),
                                  plotOutput("PI111_uncertain",height="auto"),

                                  column(width = 12,h5("Cost of Current Uncertainties Analysis",style = "font-weight:bold")),
                                  column(width=12,h5("This is a post-hoc analysis to determine which question led to the largest uncertainty in long term yield.
                                                     The ranges in the answers of each question are divided into 8 separate 'bins'.
                                                     The variance in long term yield among these bins is represented in the bars below.
                                                     Note: this is not informative of MP performance, but should be used after an MP is selected to evaluate the cost or relevance of each question")),
                                  plotOutput("CCU",height="auto"),

                                  column(width = 12,h5("MSE convergence diagnostics",style = "font-weight:bold")),
                                  column(width=12,h5("Have enough simulations been carried out to interpret results? Ideally the lines in the graphs below should be flat and parallel to each other. If they are just parallel, the MP ranking may be stable but absolute MP performance is not")),
                                  plotOutput("Eval_Converge",height="auto")

                                ) # end of fluid row

                    ), value=1),

                    tabPanel(h4("Application",style = "color:black"),
                             conditionalPanel(condition="output.App==0&input.Mode=='Advanced'",
                                     h5("Application MSE not run yet (Step C2)", style = "color:grey")
                             ),
                             conditionalPanel(condition="output.App==0&input.Mode=='Streamlined'",
                                     h5("Application results are calculated in Advanced mode only", style = "color:grey")
                             ),
                             conditionalPanel(condition="output.App==1",

                                              column(width = 12,

                                                     fluidRow(
                                                       column(width = 12,h5("Performance Indicator Table",style="font-weight:bold")),
                                                       conditionalPanel(condition="input.Perf_type=='MSC continuity'",
                                                                        column(width=12,h5("< These performance metrics have been kept for App debugging reasons > The Performance Indicator Table includes the probabilities of each MP achieving the relevant MSC PI
                                                                                           thresholds for stock status (PI 1.1.1), rebuilding (PI 1.1.2) and harvest strategy (PI 1.2.1).  The MPs are presented in
                                                                                           order of projected long-term yield (relative to the highest yield MP).
                                                                                           MPs that pass all PI thresholds are in green and those that do not are presented in red.  MPs that are
                                                                                           not available for use with current data are listed in black and the lacking data are listed in the last column to the
                                                                                           right."))),
                                                       conditionalPanel(condition="input.Perf_type=='MSC'",
                                                                        column(width=12,h5("The Performance Indicator Table includes the 'Stock Status' metrics - the probabilities of each MP exceeding the limit (0.5 BMSY) and
                                                                                           the target (BMSY) biomass levels over the short term (burnin years). Also tabulated are the 'Harvest Strategy' metrics. These are similar but are evaluated over the long term (burnin-50 years).
                                                                                           MPs that pass all probability thresholds are in green and those that do not are presented in red.  MPs that are  not available for use with current data are listed in black and the lacking data are listed in the last column to the
                                                                                           right."))),
                             tableOutput('App_Ptable'),
                             tableOutput('App_threshtable'),

                             column(width = 12,h5("Biomass projection plots and risk assessment",style="font-weight:bold")),
                             column(width=12,h5("Projections of biomass that show the derivation of the various PI scores in the table above")),
                             plotOutput("MSC_PMs",height="auto"),

                             column(width = 12,h5("B/BMSY and Yield (relative to today) projection plots",style="font-weight:bold")),
                             column(width=12,h5("Projections of biomass and yield. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations.")),
                             plotOutput("App_wormplot",height="auto"),

                             column(width = 12,h5("F/FMSY and Yield (relative to MSY) projection plots",style="font-weight:bold")),
                             column(width=12,h5("Projections of fishing mortality rate and yield relative to MSY levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations.")),
                             plotOutput("App_wormplot2",height="auto"),

                             column(width = 12,h5("Rebuilding analysis",style="font-weight:bold")),
                             column(width=12,h5("Projections of biomass relative to MSY and unfished (B0) levels. The blue regions represent the 90% and 50% probability intervals, the white solid line is the median and the dark blue lines are two example simulations.")),
                             plotOutput("App_wormplot3",height="auto"),

                             column(width = 12,h5("PI.1.1.1 uncertainties",style = "font-weight:bold")),
                             column(width=12,h5("These plots show how many simulations could be assigned to each of the SG regions defined by PI.1.1.1 ")),
                             plotOutput("App_PI111_uncertain",height="auto"),

                             column(width = 12,h5("Cost of Current Uncertainties Analysis",style = "font-weight:bold")),
                             column(width=12,h5("This is a post-hoc analysis to determine which question led to the largest uncertainty in long term yield. The ranges in the answers of each question are divided into 8 separate 'bins'. The variance in long term yield among these bins is represented in the bars below.")),
                             plotOutput("App_CCU",height="auto"),

                             column(width = 12,h5("Value of information analysis",style = "font-weight:bold")),
                             column(width=12,h5("This is similar to cost-of-current uncertainties but identifies those data errors and biases that are most likely to impact the long-term yield performance of the MP.")),
                             plotOutput("App_VOI",height="auto")


                          )
                      )

                    ),
                    value=2),

                    tabPanel(h4("Indicators",style = "color:black"),

                             conditionalPanel(condition="output.Ind==0&input.Mode=='Advanced'",
                                      h5("Indicators not calculated yet (Step D)", style = "color:grey")
                             ),
                             conditionalPanel(condition="output.App==0&input.Mode=='Streamlined'",
                                              h5("Indicator results are calculated in Advanced mode only", style = "color:grey")
                             ),
                             conditionalPanel(condition="output.Ind==1",
                               column(width=12,h5("In this demonstration version of the ancillary indicators function, an example observed data point after 6 years (blue cross) is compared to the posterior predictive data of the operating model. The Mahalanobis distance is the multivariate distance from the posterior mean, taking account of data cross-correlation. When the observed data are within the 95th percentile the data are considered consistent with the operating model")),
                               hr(),
                               fluidRow(
                                 column(width = 12,h5("Posterior predictive data cross-correlation (statistics over the first 6 projected years)",style="color::grey")),
                                 column(width = 12,h5("CS = Catch Slope, CV = Catch Variability, CM = Catch Mean",style="color::grey")),
                                 column(width = 12,h5("IS = Index Slope, IM = Index Mean",style="color::grey")),
                                 column(width = 12,h5("MLS = Mean Length Slope, MLM = Mean Length",style="color::grey")),

                                 plotOutput("CC",height="auto"),
                                 column(width = 12,h5("Mahalanobis distance / quantile plot",style="color::grey")),
                                 plotOutput("mdist",height="auto")

                               )
                             ),

                             value=3),


                tabPanel(h4("Advice",style = "color:black"),
                         conditionalPanel(condition="output.AdCalc==0&input.Mode=='Advanced'",
                           h5("Data not loaded yet (Step A2)", style = "color:grey")
                         ),
                         conditionalPanel(condition="output.AdCalc==0&input.Mode=='Streamlined'",
                           h5("Advice is calculated in Advanced mode only", style = "color:grey")
                         ),
                         conditionalPanel(condition="output.AdCalc==1",
                           column(12,style="height:25px"),
                           DT::dataTableOutput('Advice'),
                           plotOutput("Advice_TAC",height="auto")
                         ),
                         value=4)

               )
              )
            )
          )
      ), # end of Results



     column(12,style="height:15px"),

      h4("HELP"),
      hr(),

      fluidRow(
        column(1),
        column(11,

               fluidRow(
                 column(2,

                   selectInput("help_MP", label = "Management Procedure", choices=c("AvC","AvC_MLL",      "BK" ,          "BK_CC" ,       "BK_ML",        "CC1" ,         "CC2",
                                                                       "CC3",          "CC4",          "CC5",          "CompSRA",      "CompSRA4010",  "curE",         "curE75",
                                                                       "DAAC",         "DBSRA",        "DBSRA_40",     "DBSRA4010",   "DCAC",        "DCAC_40",     "DCAC_ML",
                                                                       "DCAC4010",    "DCACs",       "DD",          "DD4010",      "DDe",         "DDe75",       "DDes",
                                                                       "DepF",        "DTe40",       "DTe50",       "DynF",        "EtargetLopt", "Fadapt",      "Fdem",
                                                                       "Fdem_CC",     "Fdem_ML",     "FMSYref",     "FMSYref50",   "FMSYref75",   "Fratio",      "Fratio_CC",
                                                                       "Fratio_ML",   "Fratio4010",  "GB_CC",       "GB_slope",    "GB_target",   "Gcontrol",    "HDAAC",
                                                                       "ICI",         "ICI2",        "Iratio",      "Islope1",     "Islope2",     "Islope4",     "IT10",
                                                                       "IT5",         "Itarget1",    "Itarget1_MPA","Itarget2",    "Itarget3",    "Itarget4",    "ItargetE1",
                                                                       "ItargetE2",   "ItargetE3",   "ItargetE4",   "ITe10",       "ITe5",        "ITM",         "L95target",
                                                                       "LBSPR",       "Lratio_BHI",  "Lratio_BHI2", "LstepCC1",    "LstepCC2",    "LstepCC3",    "LstepCC4",
                                                                       "LstepCE1",    "LstepCE2",    "Ltarget1",    "Ltarget2",    "Ltarget3",    "Ltarget4",    "LtargetE1",
                                                                       "LtargetE4",   "matlenlim",   "matlenlim2",  "MCD",         "MCD4010",     "minlenLopt1", "MRnoreal",
                                                                       "MRreal",      "NFref",       "Rcontrol",    "Rcontrol2",   "SBT1",        "SBT2",        "slotlim",
                                                                       "SPmod",       "SPMSY",       "SPslope",     "SPSRA",       "SPSRA_ML",    "YPR",         "YPR_CC",
                                                                       "YPR_ML")
                               ,selected=character(0))),

                 column(2, style="padding-top:25px",
                   actionButton("getMPhelp","Open documentation for MP")


                 ),
                 column(12,htmlOutput("MPdoc"))
               )
        )
      ),




      column(12,style="height:45px"),

      conditionalPanel(condition="input.Mode=='Advanced'",
            h4("ADVANCED"),
            hr(),


            fluidRow(
              column(1),
              column(11,

                     fluidRow(
                       column(2,
                            fileInput("Load_anything","Load DLMtool and MSEtool source code for OMs, MPs and PMs")
                       ),
                       column(1),
                       column(2,

                              checkboxInput("Debug","Debug mode",value=FALSE)
                       )
                     )
            ))
      ),
      column(12,style="height:100px"),
      hr(),

      column(8,style="height:40px"),
      column(2,style="height:40px; padding:9px",textOutput("SessionID")),
      column(2,style="height:40px", h6("copyright (c) NRDC 2018")),

        #column(12,style="height:100px"),
      #column(12, actionButton("debug", "Debug")),
      conditionalPanel(condition="input.Debug",
        column(12, textInput("Debug1", "Debug window", ""))
      )

     #) # end of fluid row
    ) # end of fluid page
  ) # end of server





