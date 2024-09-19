############ MAIN ANALYSES PAPER TEMPORAL TRENDS SMN DCCSS LATER _ A.Westerveld ###############


##############################
#Open Data
##############################

path = file.path("PATH FILES")
Temporal_data = read_sav(path)

Select_cox <- select (Temporal_data, "NECESSARY VARIABLES")

## VARIABLES
#FU_KK_year = Follow up time since childhood cancer diagnosis
# SMNYesNo = event, developing subsequent malignant neoplasms yes or no'
# Sex = Sex of each participant
# Period_CC_diagnosis = de period of childhod caner diagnosis in 4 categories
# AgeDiagnosis_cat  = The age at childhood cancer diagnosis in categories
# th_rt_2inclrec = Receiving radiotherapy yes or no
# ct_gr_alkyl1_2 = Receiving alkylating agents yes or no
# ct_gr_anthra1_2 = Receiving anthracyclines yes or no
# ct_gr_epipod_2 = Receiving epipodophyllotixins yes or no
# ct_gr_platinum_2 = Receiving platinum agents yes or no
# ct_gr_vinca_2 = Receiving vinca-alkaloids yes or no


##############################
# COX / MEDIATION ANALYSIS
##############################


##############################
# TABLE 2
##############################

### TABLE 2 Base model – Not adjusted for any treatments*
modelBASIS <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat), data=Select_cox)
summary(modelBASIS)
(res.zph1 <- cox.zph(modelBASIS))
      
      ## for calculation p-trend
      modelTREND <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat), data=Select_cox)
      summary(modelTREND)
      (res.zph1 <- cox.zph(modelTREND))


### TABLE 2 Model adjusted for all treatments*
modelalleCTenRT <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat) + as.factor(th_rt_2inclrec)+ as.factor(ct_gr_alkyl1_2)+ as.factor(ct_gr_anthra1_2)
                         + as.factor(ct_gr_epipod_2) + as.factor(ct_gr_platinum_2) + as.factor(ct_gr_vinca_2), data=Select_cox)
summary(modelalleCTenRT)
(res.zph1 <- cox.zph(modelalleCTenRT))
      
      ## for calculation p-trend
      modelalleCTenRT_trend <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat) + as.factor(th_rt_2inclrec)+ as.factor(ct_gr_alkyl1_2)+ as.factor(ct_gr_anthra1_2)
                                     + as.factor(ct_gr_epipod_2) + as.factor(ct_gr_platinum_2) + as.factor(ct_gr_vinca_2), data=Select_cox)
      summary(modelalleCTenRT_trend)


### TABLE 2 Model adjusted for use of chemotherapy agents*
modelalleCT <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat) + as.factor(ct_gr_alkyl1_2)
                     + as.factor(ct_gr_vinca_2)+ as.factor(ct_gr_platinum_2) , data=Select_cox)
summary(modelalleCT)
      
      ## for calculation p-trend
      modelalleCT_trend <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat) + as.factor(ct_gr_alkyl1_2)+ as.factor(ct_gr_anthra1_2)+ as.factor(ct_gr_epipod_2)
                                 + as.factor(ct_gr_vinca_2)+ as.factor(ct_gr_platinum_2) , data=Select_cox)
      summary(modelalleCT_trend)


### TABLE 2 Model adjusted for use of anthracyclines and/or epipodophyllotoxins*
modelmetepianth <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2) + as.factor(AgeDiagnosis_cat)+ as.factor(ct_gr_epipod_2)+ as.factor(ct_gr_anthra1_2), data=Select_cox)
summary(modelmetepianth)
      
      ## for calculation p-trend
      modelmetepianth_trend <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2 + as.factor(AgeDiagnosis_cat)+ as.factor(ct_gr_epipod_2)+ as.factor(ct_gr_anthra1_2), data=Select_cox)
      summary(modelmetepianth_trend)



### TABLE 2 Model adjusted for use of radiotherapy only* 
modelmetRT <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat)+ as.factor(th_rt_2inclrec), data=Select_cox)
summary(modelmetRT)

      ## for calculation p-trend
      modelmetRT_trend <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat)+ as.factor(th_rt_2inclrec), data=Select_cox)
      summary(modelmetRT_trend)


##############################
# TABLE 3
##############################
      
      ##  OPEN  DATA WITH IRRADIATED SURVIVORS

### TABLE 3 MODEL  Irradiated survivors – not adjusted for treatment variables
modelBASIS <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat), data=Select_cox)
summary(modelBASIS)
(res.zph1 <- cox.zph(modelBASIS))

      ## for calculation p-trend
      modelTREND <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat), data=Select_cox)
      summary(modelTREND)
      (res.zph1 <- cox.zph(modelTREND))
      
    

### TABLE 3 MODEL   Irradiated survivors – adjusted for RT doses
modelmetRTdosis <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat)+ as.factor(RT_max_cat), data=Select_cox)
summary(modelmetRTdosis)
(res.zph1 <- cox.zph(modelmetRTdosis))
      
      ## for calculation p-trend     
      modelmetRTdosis_trend <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat)+ as.factor(RT_max_cat), data=Select_cox)
      summary(modelmetRTdosis_trend)
      


### TABLE 3 MODEL  Irradiated survivors – adjusted for RT body regions
modelmetRTVELD <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat)+ 
                          rt_body_tbi_2+
                          rt_body_hoofd_2+
                          rt_body_nek_2+
                          rt_body_borst_2+
                          rt_body_buik_2+
                          rt_body_bekken_2+
                          rt_body_bovenextr_2+
                          rt_body_onderextr_2+
                          rt_body_spinaal_2+
                          rt_body_testis_2+
                          rt_body_MIBG_2, data=Select_cox)
summary(modelmetRTVELD)
(res.zph1 <- cox.zph(modelmetRTVELD))
      
      ## for calculation p-trend       
      modelmetRTVELD_trend <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat)+ 
                                      rt_body_tbi_2+
                                      rt_body_hoofd_2+
                                      rt_body_nek_2+
                                      rt_body_borst_2+
                                      rt_body_buik_2+
                                      rt_body_bekken_2+
                                      rt_body_bovenextr_2+
                                      rt_body_onderextr_2+
                                      rt_body_spinaal_2+
                                      rt_body_testis_2+
                                      rt_body_MIBG_2, data=Select_cox)
      summary(modelmetRTVELD_trend)


      

### TABLE 3 MODEL   Irradiated survivors – adjusted for TBI*
modelmetTBI <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat)+ as.factor(rt_TBI), data=Select_cox)
summary(modelmetTBI)
(res.zph1 <- cox.zph(modelmetTBI))
      
      ## for calculation p-trend     
      modelmetTBI_trend <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat)+ as.factor(rt_TBI), data=Select_cox)
      summary(modelmetTBI_trend)
      

      
### TABLE 3 MODEL   Irradiated survivors – adjusted for SCT*
modelmetHSCT <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat)+ as.factor(ot_sct_bmt_2inclrec), data=Select_cox)
summary(modelmetHSCT)
(res.zph1 <- cox.zph(modelmetHSCT))
      
      ## for calculation p-trend      
      modelmetHSCT_trend <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat)+ as.factor(ot_sct_bmt_2inclrec), data=Select_cox)
      summary(modelmetHSCT_trend)
      

      
### TABLE 3 MODEL  Irradiated survivors –  adjusted for use of chemotherapy agents*
modelalleCT <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat) + as.factor(ct_gr_alkyl1_2)
                     + as.factor(ct_gr_vinca_2)+ as.factor(ct_gr_platinum_2) , data=Select_cox)
summary(modelalleCT)

      ## for calculation p-trend
      modelalleCT_trend <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat) + as.factor(ct_gr_alkyl1_2)+ as.factor(ct_gr_anthra1_2)+ as.factor(ct_gr_epipod_2)
                                 + as.factor(ct_gr_vinca_2)+ as.factor(ct_gr_platinum_2) , data=Select_cox)
      summary(modelalleCT_trend)
      

### TABLE 3 MODEL   Irradiated survivors –  adjusted for chemotherapy doses*
modelalleCTdose <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ as.factor(Period_CC_diagnosis2)+ as.factor(AgeDiagnosis_cat) + as.factor(cum_dose_alk_cat)+ 
                           as.factor(cum_dose_anth_cat)+ as.factor(cum_dose_epipodo_cat)
                         + as.factor(cum_dose_plat_cat)  + as.factor(ct_gr_vinca_2), data=Select_cox)
summary(modelalleCTdose)
(res.zph1 <- cox.zph(modelalleCTdose))
      

      ## for calculation p-trend
      modelalleCTdose_trend <- coxph(Surv(time=FU_KK_year,event=SMNYesNo) ~ sex+ Period_CC_diagnosis2+ as.factor(AgeDiagnosis_cat) + as.factor(cum_dose_alk_cat)+ 
                                       as.factor(cum_dose_anth_cat)+ as.factor(cum_dose_epipodo_cat)
                                     + as.factor(cum_dose_plat_cat)  + as.factor(ct_gr_vinca_2), data=Select_cox)
      summary(modelalleCTdose_trend)

      

      
      
##############################
# BOXPLOTS FIGURE 2 and 3
##############################
      
coefname <- names(coef(modelalleCT))
print (coefname)

# Extract coefficients and confidence intervals
coef_model <- coef(modelalleCT)
confint_model <- confint(modelalleCT)

# Extract names of coefficients
coef_names <- names(coef_model)
print(coef_names)

# Create a data frame to store HRs and CIs
hr_df <- data.frame(variable = character(), hr = numeric(), ci_lower = numeric(), ci_upper = numeric())

# Iterate through all levels of Period_CC_diagnosis2
for (name in coef_names) {
  if (grepl("Period_CC_diagnosis2", name)) {
    hr <- exp(coef_model[name])
    ci <- exp(confint_model[name, ])
    level <- sub("as.factor\\(Period_CC_diagnosis2\\)", "", name)
    hr_df <- rbind(hr_df, data.frame(variable = paste("Period_CC_diagnosis2", level), hr = hr, ci_lower = ci[1], ci_upper = ci[2]))
  }
}


# Add reference level HR (HR = 1, CI = 1) assuming reference level is 1
hr_df <- rbind(hr_df, data.frame(variable = "Period_CC_diagnosis2 1", hr = 1, ci_lower = 1, ci_upper = 1))

# Sort data frame by variable for better visualization
hr_df <- hr_df[order(hr_df$variable), ]


# Rename the levels of Period_CC_diagnosis2
hr_df$variable <- factor(hr_df$variable, levels = c("Period_CC_diagnosis2 1", "Period_CC_diagnosis2 2", "Period_CC_diagnosis2 3", "Period_CC_diagnosis2 4"),
                         labels = c("Before 1980\n(Reference)", "1980-\n1989", "1990-\n1999", "2000\and onwards"))



### BOXPLOT WITH FIXED y-AXIS:
pdf("NAME BOXPLOT", width=5, height=6)

plot <- ggplot(hr_df, aes(x = variable, y = hr)) +
  geom_boxplot(width = 0.2) +  # Adjust the width of the boxplot bars
  geom_point(size = 3, color = "#003a70") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "#003a70") +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.5)) +  # Set the y-axis limits and breaks
  labs(
    x = "Diagnosis period",
    y = "Hazard Ratio") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16 ),
    axis.title.x = element_text(size = 14 ),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, hjust = 0.5), # size and center labels
    axis.text.y = element_text(size = 12),
    axis.line = element_line(color = "black"), # adds axis line
    panel.grid.major.x = element_blank(),  # Remove major grid lines x axis
    panel.grid.minor.y = element_blank(),  # Remove minor grid lines y axis
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border entirely
    axis.ticks = element_line(color = "black")  # Add ticks on both axes
  )

print(plot)
dev.off()
