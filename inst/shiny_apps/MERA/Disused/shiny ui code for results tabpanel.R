
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




#), value=1),

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