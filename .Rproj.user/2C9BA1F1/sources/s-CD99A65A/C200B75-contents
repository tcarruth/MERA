# App design

# ----- Fishery answers -----------------------------------------------------------------------------

M_list<<-list("Very short-lived (0.6 < M)" = "M_60", "Short-lived      (0.4 < M < 0.6)" = "M_40_60",
              "Moderate life span  (0.2 < M < 0.4)" = "M_20_40", "Moderately long-lived (0.1 < M < 0.2)" = "M_10_20",
              "Long-lived          (0.05 < M < 0.1)" = "M_05_10","Very long-lived    (M < 0.05)" = "M_05")

M_mins<- c( 0.6,    0.4,      0.2,      0.1,      0.05,    0.025)
M_maxes<-c( 0.8,    0.6,      0.4,      0.2,      0.1,     0.05)

D_list<<-list("Crashed (D < 0.05)" = "D_05", "Very depleted (0.05 < D < 0.1)" = "D_05_10",
              "Depleted (0.1 < D < 0.15)" = "D_10_15", "Moderately depleted (0.15 < D < 0.3)" = "D_15_30",
              "Healthy (0.3 < D < 0.5)" = "D_30_50","Underexploited (0.5 < D)" = "D_50")

D_mins<- c( 0.01,  0.05,      0.1,      0.15,      0.3,     0.5)
D_maxes<-c( 0.05,   0.1,      0.15,     0.3,       0.5,     0.8)


h_list<<-list("Not resilient (h < 0.3)" = "h_30", "Low resilience (0.3 < 0.5)" = "h_30_50",
              "Moderate resilence (0.5 < h < 0.7)" = "h_50_70", "Resilient (0.7 < h < 0.9)" = "h_70_90",
              "Very Resilient (0.9 < h)" = "h_90")

h_mins<- c( 0.25,   0.3,      0.5,      0.7,      0.9)
h_maxes<-c( 0.3,    0.5,      0.7,      0.9,      0.99)


FP_list<<-list("Stable" = "FP_s","Gradual reduction" = "FP_gr" ,"Boom-bust" = "FP_bb","Gradual increases"="FP_gi",
               "Stable, recent increases"="FP_ri", "Stable, recent declines" = "FP_rd")

M1s<-   c(0.2,    0.3,    0.2,    0.15,    0.2,    0.2)
M2s<-   c(1.2,     0.3,   0.7,    1.4,    1.2,    0.7)
sd1s<-  c(0.075,    0.1,    0.1,   0.05,   0.075,   0.075)
sd2s<-  c(0.1,     0.5,    0.18,    0.4,    0.2,    0.2)
h2s<-   c(0,       0,      3,      8,      2.5,      0)

F_list<<-list("Not variable (CV ~ 10%)" = "F_10", "Variable (10% < CV < 25%)" = "F_10_25",
              "Highly variable (25% < CV < 50%)"="F_25_50")

F_mins<-   c(0.05,    0.1,    0.25)
F_maxes<-   c(0.1,     0.25,   0.5)

sel_list<<-list("Very small (S < 0.5)" = "sel_50","Small (0.5 < S < 0.75)"="sel_50_75",
                "Similar to maturity (0.75 < S < 1.25)" = "sel_75_125",
                "Large (1.25 < S < 1.5)" = "sel_125_150","Very large (1.5 < S)"="sel_150_200")

sel_mins<- c( 0.25,   0.5,         0.75,      1.25,      1.50)
sel_maxes<-c( 0.5,    0.75,        1.25,      1.50,      2)


dome_list<<-list("Asymptotic selectivity (SL = 1)" = "dome_100", "Declining selectivity with length (0.75 < SL < 1)"="dome_75_100",
                 "Dome-shaped selectivity (0.25 < SL < 0.75)" = "dome_25_75", "Strong dome-shaped selectivity (SL < 0.25)" = "dome_25")

dome_mins<- c(0.98,       0.75,         0.25,        0.05)
dome_maxes<-c(1,          1,            0.75,        0.25)

DR_list<<-list("Low (DR < 1%)"="DR_1","Low - moderate (1% < DR < 10%)"="DR_1_10","Moderate (10% < DR < 30%)"="DR_10_30",
               "Moderate - high (30% < DR < 50%)"="DR_30_50", "High (50% < DR < 70%)"="DR_50_70")

DR_mins<- c(  0,     0.01,    0.1,      0.3,      0.5)
DR_maxes<-c(  0.01,  0.1,     0.3,      0.5,      0.7)


PRM_list<<-list("Low (PRM < 5%)"="PRM_5","Low - moderate (5% < PRM < 25%)"="PRM_5_25","Moderate (25% < PRM < 50%)"="PRM_25_50",
               "Moderate - high (50% < PRM < 75%)"="PRM_50_75", "High (75% < PRM < 95%)"="PRM_75_95", "Almost all die (95% < PRM < 100%)"="PRM_95_100")

PRM_mins<- c(  0,     0.05,    0.25,      0.5,      0.75,  0.95)
PRM_maxes<-c(  0.05,  0.25,     0.5,      0.75,      0.95, 1)


sigR_list<<-list("Very low (sigma R < 0.1)"="sigR_10","Low (0.1 < sigma R < 0.3)"="sigR_10_30",
                 "Moderate (0.3 < sigma R < 0.6"="sigR_30_60", "High (0.6 < sigma R < 0.9"="sigR_60_90",
                 "Very high (0.9 < sigma R)"="sigR_90")

sigR_mins<- c(  0.05,     0.1,          0.3,         0.6,        0.9)
sigR_maxes<-c(  0.1,      0.3,          0.6,         0.9,        1.2)

q_list<<-list("Declining by 2-3% pa (halves every 25-35 years)"="q_d3_d2","Declining by 1-2% pa (halves every 35-70 years)"="q_d2_d1",
              "Stable -1% to 1% pa (may halve/double every 70 years)"="q_d1_1","Increasing by 1-2% pa (doubles every 35-70 years)"="q_1_2",
              "Increasing by 2-3% pa (doubles every 25-35 years)"="q_2_3")

q_mins<- c(-3,       -2,       -1,      1,      2)
q_maxes<-c(-2,       -1,       1,       2,      3)

A_list<<-list("None"="A_1","Small (A < 5%)"="A_1_5", "Small-moderate (5% < A < 10%)" = "A_5_10", "Moderate (10% < A < 20%)" = "A_10_20",
              "Large (20% < A < 30%)"="A_20_30","Very large (30% < A < 40%)" = "A_30_40", "Huge (40% < A < 50%)"="A_40_50")

A_mins<- c(0.005,  0.01,     0.05,    0.1,        0.2,       0.3,       0.4)
A_maxes<-c(0.01,  0.05,     0.1,     0.2,        0.3,       0.4,       0.5)

V_list<<-list("Very low (P < 1%)" = "P_1", "Low (1% < P < 5%)" = "P_1_5", "Moderate (5% < P < 10%)" = "P_5_10",
              "High (10% < P < 20%)" = "P_10_20", "Fully mixed" = "P_20")

V_mins<- c(0.005, 0.01,     0.05,    0.1,        0.2)
V_maxes<-c(0.01,  0.05,     0.1,     0.2,        0.5)


# ----- Management answers -----------------------------------------------------------------------------

M1_list<<-list("TAC" = "TAC", "TAE" = "TAE",
               "Size limit" = "size_limit", "Time-area closures" = "time_area_close")

IB_list<<-list("Large underages (40% - 70% of recommended)" = "IB_n30", "Underages (70% - 90% of recommended)" = "IB_n30_n10","Slight underages (90% - 100% of recommended)" = "IB_n10_0",
               "Taken exactly (95% - 105% of recommended)"="IB_n5_5","Slight overages (100% - 110% of recommended)"="IB_0_10","Overages (110% - 150% of recommended)"="IB_10_30","Large overages (150% - 200% of recommended)"="IB_30")

IB_mins<- c( 0.4,  0.7,      0.9,     0.95,      1,     1.1, 1.5)
IB_maxes<-c( 0.7,  0.9,      1,       1.05,      1.1,   1.5, 2)


IV_list<<-list("Constant (V < 1%)" = "IV_1", "Not variable (1% < V < 5%)" = "IV_1_5","Low variability (5% < V < 10%)" = "IV_5_10",
               "Variable (10% < V < 20%)"="IV_10_20","Highly variable (20% < V < 40%)"="IV_20_40")

IV_mins<-c(0.005,0.01,0.05,0.1,0.2)
IV_maxes<-c(0.01,0.05,0.1,0.2,0.4)


# ----- Data answers -----------------------------------------------------------------------------

D1_list<<-list("Historical annual catches (from unfished)" = "ann_cat","Recent annual catches (at least 5 recent years)" = "ann_cat_R", "Historical relative abundance index (from unfished)"= "ind",
               "Recent relative abundance index (at least 5 recent years)"= "ind_R",
               "Fishing effort" = "fis_eff","Size composition (length samples)" = "siz_com","Age composition (age samples)" = "age_com", "Growth (growth parameters)" = "growth",
               "Current biomass survey"="cur_bio_sur")

CB_list<<-list("Strong under-reporting (30% - 50%)" = "CB_n50_n30", "Under-reporting (10% - 30%)" = "CB_n30_n10","Slight under-reporting (less than 10%)" = "CB_n10_0",
               "Reported accurately (+/- 5%)" = "CB_n5_5","Slight over-reporting (less than 10%)" = "CB_0_10")

CB_mins<- c( 0.5,   0.7,      0.9,     0.95,      1)
CB_maxes<-c( 0.7,   0.9,      1,       1.05,      1.1)

Beta_list<<-list("Strong hyperdepletion (2 < Beta < 3)" = "Beta_200_300", "Hyperdepletion (1.25 < Beta < 2)" = "Beta_125_200","Proportional (0.8 < Beta < 1.25)" = "Beta_80_125",
                 "Hyperstability (0.5 < Beta < 0.8)" = "Beta_50_80","Strong hyperstability (0.33 < Beta < 0.5)"="Beta_33_50")

Beta_mins<- c( 2,                1.25,          0.8,           0.50,        0.33)
Beta_maxes<-c( 3,                2,             1.25,          0.8,         0.50)

Err_list<<-list("Perfect" = "Err_perf","Good (accurate and precise)" = "Err_good","Data moderate (some what inaccurate and imprecise)"="Err_Mod",
                "Data poor (inaccurate and imprecise)" = "Err_bad")

