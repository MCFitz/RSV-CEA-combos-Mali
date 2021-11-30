################################################################################
############# Code for Government Perspective Optimality Curve #################
################################################################################

# calculate NHBs from the gov perspective across WTP
gov_prep_llAb <- cbind(DALYS_lost_no_u, DALYS_lost_llAb_u, govcost_llAb, 0)
gov_NHB_l <- matrix(0, trials, length(WTP_sp))
for (l in 1: length(WTP_sp)){
  gov_NHB_l[,l] <- NHB_func(gov_prep_llAb, WTP_sp[l])
}

gov_prep_mVax <- cbind(DALYS_lost_no_u, DALYS_lost_mVax_u, govcost_mVax, 0)
gov_NHB_m <- matrix(0, trials, length(WTP_sp))
for (m in 1: length(WTP_sp)){
  gov_NHB_m[,m] <- NHB_func(gov_prep_mVax, WTP_sp[m])
}

gov_prep_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_pVax_u, govcost_pVax, 0)
gov_NHB_p <- matrix(0, trials, length(WTP_sp))
for (p in 1: length(WTP_sp)){
  gov_NHB_p[,p] <- NHB_func(gov_prep_pVax, WTP_sp[p])
}

gov_prep_joint_llAb_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_joint_llAb_pVax_u, govcost_joint_llAb_pVax, 0)
gov_NHB_lp <- matrix(0, trials, length(WTP_sp))
for (lp in 1: length(WTP_sp)){
  gov_NHB_lp[,lp] <- NHB_func(gov_prep_joint_llAb_pVax, WTP_sp[lp])
}

gov_prep_joint_mVax_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_joint_mVax_pVax_u, govcost_joint_mVax_pVax, 0)
gov_NHB_mp <- matrix(0, trials, length(WTP_sp))
for (mp in 1: length(WTP_sp)){
  gov_NHB_mp[,mp] <- NHB_func(gov_prep_joint_mVax_pVax, WTP_sp[mp])
}

gov_prep_pVax_older <- cbind(DALYS_lost_no_u, DALYS_lost_pVax_older_u, govcost_pVax_older, 0)
gov_NHB_p_older <- matrix(0, trials, length(WTP_sp))
for (po in 1: length(WTP_sp)){
  gov_NHB_p_older[,po] <- NHB_func(gov_prep_pVax_older, WTP_sp[po])
}

gov_prep_joint_llAb_pVax_older <- cbind(DALYS_lost_no_u, DALYS_lost_joint_llAb_pVax_older_u, govcost_joint_llAb_pVax_older, 0)
gov_NHB_lp_older <- matrix(0, trials, length(WTP_sp))
for (lpo in 1: length(WTP_sp)){
  gov_NHB_lp_older[,lpo] <- NHB_func(gov_prep_joint_llAb_pVax_older, WTP_sp[lpo])
}

gov_prep_joint_mVax_pVax_older <- cbind(DALYS_lost_no_u, DALYS_lost_joint_mVax_pVax_older_u, govcost_joint_mVax_pVax_older, 0)
gov_NHB_mp_older <- matrix(0, trials, length(WTP_sp))
for (mpo in 1: length(WTP_sp)){
  gov_NHB_mp_older[,mpo] <- NHB_func(gov_prep_joint_mVax_pVax_older, WTP_sp[mpo])
}

## Calculate probability of each strategy being optimal across WTP

# calculate NHB for no intervention at all
gov_prep_no <- cbind(DALYS_lost_no_u, DALYS_lost_no_u, 0, 0)
gov_NHB_no <- matrix(0, trials, length(WTP_sp))
for (no in 1: length(WTP_sp)){
  gov_NHB_no[,no] <- NHB_func(gov_prep_no, WTP_sp[no])}

# compare NHB across all strategies
gov_compare_NHB <- array(c(gov_NHB_no, gov_NHB_l, gov_NHB_m, gov_NHB_p, gov_NHB_lp, gov_NHB_mp, gov_NHB_p_older, gov_NHB_lp_older, gov_NHB_mp_older), dim = c(trials, length(WTP_sp), 9))
gov_win_NHB <- apply(gov_compare_NHB, MARGIN = c(1,2), which.max)

gov_pO_no <- rep(0, length(WTP_sp))
for(no in 1: length(WTP_sp)){
  gov_pO_no[no] <- sum(gov_win_NHB[,no] == 1)/trials
}

gov_pO_llAb <- rep(0, length(WTP_sp))
for(Ol in 1: length(WTP_sp)){
  gov_pO_llAb[Ol] <- sum(gov_win_NHB[,Ol] == 2)/trials
}

gov_pO_mVax <- rep(0, length(WTP_sp))
for(Om in 1: length(WTP_sp)){
  gov_pO_mVax[Om] <- sum(gov_win_NHB[,Om] == 3)/trials
}

gov_pO_pVax <- rep(0, length(WTP_sp))
for(Op in 1: length(WTP_sp)){
  gov_pO_pVax[Op] <- sum(gov_win_NHB[,Op] == 4)/trials
}

gov_pO_llAb_pVax <- rep(0, length(WTP_sp))
for(Olp in 1: length(WTP_sp)){
  gov_pO_llAb_pVax[Olp] <- sum(gov_win_NHB[,Olp] == 5)/trials
}

gov_pO_mVax_pVax <- rep(0, length(WTP_sp))
for(Omp in 1: length(WTP_sp)){
  gov_pO_mVax_pVax[Omp] <- sum(gov_win_NHB[,Omp] == 6)/trials
}

gov_pO_pVax_older <- rep(0, length(WTP_sp))
for(Opo in 1: length(WTP_sp)){
  gov_pO_pVax_older[Opo] <- sum(gov_win_NHB[,Opo] == 7)/trials
}

gov_pO_llAb_pVax_older <- rep(0, length(WTP_sp))
for(Olpo in 1: length(WTP_sp)){
  gov_pO_llAb_pVax_older[Olpo] <- sum(gov_win_NHB[,Olpo] == 8)/trials
}

gov_pO_mVax_pVax_older <- rep(0, length(WTP_sp))
for(Ompo in 1: length(WTP_sp)){
  gov_pO_mVax_pVax_older[Ompo] <- sum(gov_win_NHB[,Ompo] == 9)/trials
}

