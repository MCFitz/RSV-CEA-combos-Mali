################################################################################
############### Code for Donor Perspective Optimality Curve ####################
################################################################################

# calculate NHBs from the donor perspective across WTP
dnr_prep_llAb <- cbind(DALYS_lost_no_u, DALYS_lost_llAb_u, donorcost_llAb, 0)
dnr_NHB_l <- matrix(0, trials, length(WTP_sp))
for (l in 1: length(WTP_sp)){
  dnr_NHB_l[,l] <- NHB_func(dnr_prep_llAb, WTP_sp[l])
}

dnr_prep_mVax <- cbind(DALYS_lost_no_u, DALYS_lost_mVax_u, donorcost_mVax, 0)
dnr_NHB_m <- matrix(0, trials, length(WTP_sp))
for (m in 1: length(WTP_sp)){
  dnr_NHB_m[,m] <- NHB_func(dnr_prep_mVax, WTP_sp[m])
}

dnr_prep_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_pVax_u, donorcost_pVax, 0)
dnr_NHB_p <- matrix(0, trials, length(WTP_sp))
for (p in 1: length(WTP_sp)){
  dnr_NHB_p[,p] <- NHB_func(dnr_prep_pVax, WTP_sp[p])
}

dnr_prep_joint_llAb_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_joint_llAb_pVax_u, donorcost_joint_llAb_pVax, 0)
dnr_NHB_lp <- matrix(0, trials, length(WTP_sp))
for (lp in 1: length(WTP_sp)){
  dnr_NHB_lp[,lp] <- NHB_func(dnr_prep_joint_llAb_pVax, WTP_sp[lp])
}

dnr_prep_joint_mVax_pVax <- cbind(DALYS_lost_no_u, DALYS_lost_joint_mVax_pVax_u, donorcost_joint_mVax_pVax, 0)
dnr_NHB_mp <- matrix(0, trials, length(WTP_sp))
for (mp in 1: length(WTP_sp)){
  dnr_NHB_mp[,mp] <- NHB_func(dnr_prep_joint_mVax_pVax, WTP_sp[mp])
}

dnr_prep_pVax_older <- cbind(DALYS_lost_no_u, DALYS_lost_pVax_older_u, donorcost_pVax_older, 0)
dnr_NHB_p_older <- matrix(0, trials, length(WTP_sp))
for (po in 1: length(WTP_sp)){
  dnr_NHB_p_older[,po] <- NHB_func(dnr_prep_pVax_older, WTP_sp[po])
}

dnr_prep_joint_llAb_pVax_older <- cbind(DALYS_lost_no_u, DALYS_lost_joint_llAb_pVax_older_u, donorcost_joint_llAb_pVax_older, 0)
dnr_NHB_lp_older <- matrix(0, trials, length(WTP_sp))
for (lpo in 1: length(WTP_sp)){
  dnr_NHB_lp_older[,lpo] <- NHB_func(dnr_prep_joint_llAb_pVax_older, WTP_sp[lpo])
}

dnr_prep_joint_mVax_pVax_older <- cbind(DALYS_lost_no_u, DALYS_lost_joint_mVax_pVax_older_u, donorcost_joint_mVax_pVax_older, 0)
dnr_NHB_mp_older <- matrix(0, trials, length(WTP_sp))
for (mpo in 1: length(WTP_sp)){
  dnr_NHB_mp_older[,mpo] <- NHB_func(dnr_prep_joint_mVax_pVax_older, WTP_sp[mpo])
}

## Calculate probability of each strategy being optimal across WTP

# calculate NHB for no intervention at all
dnr_prep_no <- cbind(DALYS_lost_no_u, DALYS_lost_no_u, 0, 0)
dnr_NHB_no <- matrix(0, trials, length(WTP_sp))
for (no in 1: length(WTP_sp)){
  dnr_NHB_no[,no] <- NHB_func(dnr_prep_no, WTP_sp[no])}

# compare NHB across all strategies
dnr_compare_NHB <- array(c(dnr_NHB_no, dnr_NHB_l, dnr_NHB_m, dnr_NHB_p, dnr_NHB_lp, dnr_NHB_mp, dnr_NHB_p_older, dnr_NHB_lp_older, dnr_NHB_mp_older), dim = c(trials, length(WTP_sp), 9))
dnr_win_NHB <- apply(dnr_compare_NHB, MARGIN = c(1,2), which.max)

dnr_pO_no <- rep(0, length(WTP_sp))
for(no in 1: length(WTP_sp)){
  dnr_pO_no[no] <- sum(dnr_win_NHB[,no] == 1)/trials
}

dnr_pO_llAb <- rep(0, length(WTP_sp))
for(Ol in 1: length(WTP_sp)){
  dnr_pO_llAb[Ol] <- sum(dnr_win_NHB[,Ol] == 2)/trials
}

dnr_pO_mVax <- rep(0, length(WTP_sp))
for(Om in 1: length(WTP_sp)){
  dnr_pO_mVax[Om] <- sum(dnr_win_NHB[,Om] == 3)/trials
}

dnr_pO_pVax <- rep(0, length(WTP_sp))
for(Op in 1: length(WTP_sp)){
  dnr_pO_pVax[Op] <- sum(dnr_win_NHB[,Op] == 4)/trials
}

dnr_pO_llAb_pVax <- rep(0, length(WTP_sp))
for(Olp in 1: length(WTP_sp)){
  dnr_pO_llAb_pVax[Olp] <- sum(dnr_win_NHB[,Olp] == 5)/trials
}

dnr_pO_mVax_pVax <- rep(0, length(WTP_sp))
for(Omp in 1: length(WTP_sp)){
  dnr_pO_mVax_pVax[Omp] <- sum(dnr_win_NHB[,Omp] == 6)/trials
}

dnr_pO_pVax_older <- rep(0, length(WTP_sp))
for(Opo in 1: length(WTP_sp)){
  dnr_pO_pVax_older[Opo] <- sum(dnr_win_NHB[,Opo] == 7)/trials
}

dnr_pO_llAb_pVax_older <- rep(0, length(WTP_sp))
for(Olpo in 1: length(WTP_sp)){
  dnr_pO_llAb_pVax_older[Olpo] <- sum(dnr_win_NHB[,Olpo] == 8)/trials
}

dnr_pO_mVax_pVax_older <- rep(0, length(WTP_sp))
for(Ompo in 1: length(WTP_sp)){
  dnr_pO_mVax_pVax_older[Ompo] <- sum(dnr_win_NHB[,Ompo] == 9)/trials
}

