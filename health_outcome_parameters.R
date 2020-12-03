# health outcome parameters

p_pneum <- 0.126029

p_inpatient <- 0.2905679

p_seek_care <- 0.47

CFR_inpatient <- 0.016  # PERCH PIA
# CFR_inpatient_u <- rbeta(trials, 0.05*48, 0.552*259 - 0.05*48)

CFR_nr_care <- CFR_inpatient/0.51 * 0.49  # 49% of infants in LMIC with RSV-LRTI die outside of inpatient care setting
# CFR_nr_care_u <- CFR_inpatient_u/ 0.51 * 0.49
