# Parameter preparation ---------------------------------------------------
# Throughout, assuming species i is assumed to be limited by resource k, and species j by l

# A "good" community where transgressive overyiedling, positive relationship between ND, CE, and NBE is guarateed 
comm_good <- list(
  # Q: species resource consumption matrix,rows for species i and j, columns for resource k and l
  Q = matrix(c(2.5, 2, 2, 4), nrow = 2, byrow = TRUE),
         
  # mu: mortality rate for each species
  mu=c(0.1,0.1),

  # K: half saturation constant in the Monod function (Equation S3.1),rows for species i and j, columns for resource k and l
  K = matrix(rep(0.5, 4), nrow = 2, byrow = TRUE)
)

# Prepare parameter sets for transgressive–overyielding and positive–NBE regions---------------------------------------------------

# transgressive overyileding is guaranteed
prepare_para_resource_trans <- function(n=10,lb=1.01,ub=0.99) {
  # Q: species resource consumption matrix,rows for species i and j, columns for resource k and l
  Q = comm_good$Q
             
  # mu: mortality rate for each species
  mu=comm_good$mu 

  # K: half saturation constant in the Monod function (Equation S3.1),rows for species i and j, columns for resource k and l
  K = comm_good$K

  # r_v: resource supply ratio
    # calculating the coexistence boundary for resource supply vector
    theta_i=Q[1,2]/Q[1,1]
    theta_j=Q[2,2]/Q[2,1]  
 
    # a list of resource supply vectors for simulation
    r_v_l=seq(from=theta_i*lb,to=theta_j*ub,length=n)

    # f: scalar for resource supply
    f_l=rep(2,n)

    # all in one
    par_l_tmp=lapply(1:n, function(x){
        list(Q=Q,K=K,mu=mu,r_v=c(1,r_v_l[x]),f=f_l[x])
    })

    return(par_l_tmp)

}

# transgressive overyileding will never happen
prepare_para_resource_notrans <- function(n=10,lb=1.01,ub=0.99) {
  # Q: species resource consumption matrix,rows for species i and j, columns for resource k and l
  Q = comm_good$Q
  theta_i=Q[1,2]/Q[1,1]
  Q[1,1]=1.5
  Q[1,2]=Q[1,1]*theta_i
             
  # mu: mortality rate for each species
  mu=comm_good$mu 

  # K: half saturation constant in the Monod function (Equation S3.1),rows for species i and j, columns for resource k and l
  K = comm_good$K
  A=Q*matrix(c(mu,mu),nrow=2,byrow = FALSE)
  A0=comm_good$Q*matrix(c(mu,mu),nrow=2,byrow = FALSE)

  # keep the same R*
  K[1,1]=K[1,1]*((A0[1,1]/(1-A0[1,1]))/(A[1,1]/(1-A[1,1])))
  R_st_tmp=K*(A/(1-A))

  # r_v: resource supply ratio
    # calculating the coexistence boundary for resource supply vector
    theta_i=Q[1,2]/Q[1,1]
    theta_j=Q[2,2]/Q[2,1]  
 
    # a list of resource supply vectors for simulation
    r_v_l=seq(from=theta_i*lb,to=theta_j*ub,length=n)

    # f: scalar for resource supply
    f_l=rep(2,n)

    # all in one
    par_l_tmp=lapply(1:n, function(x){
        list(Q=Q,K=K,mu=mu,r_v=c(1,r_v_l[x]),f=f_l[x])
    })

    return(par_l_tmp)

}

# another community with positive_nbe at the upper of coexistence region
prepare_para_resource_nbe <- function(n=10,lb=1.01,ub=0.99) {
  # Q: species resource consumption matrix,rows for species i and j, columns for resource k and l
  Q =matrix(c(5.5,2.2,1,1), nrow = 2, byrow = TRUE)
             
  # mu: mortality rate for each species
  mu=comm_good$mu 

  # K: half saturation constant in the Monod function (Equation S3.1),rows for species i and j, columns for resource k and l
  K = matrix(c(0.1,0.3,0.9,0.9), nrow = 2, byrow = TRUE)
  A=Q*matrix(c(mu,mu),nrow=2,byrow = FALSE)
  R_st_tmp=K*(A/(1-A))

  # r_v: resource supply ratio
    # calculating the coexistence boundary for resource supply vector
    theta_i=Q[1,2]/Q[1,1]
    theta_j=Q[2,2]/Q[2,1]  
  
    # a list of resource supply vectors for simulation
    r_v_l=seq(from=theta_i*lb,to=theta_j*ub,length=n)

    # f: scalar for resource supply
    f_l=rep(2,n)

    # all in one
    par_l_tmp=lapply(1:n, function(x){
        list(Q=Q,K=K,mu=mu,r_v=c(1,r_v_l[x]),f=f_l[x])
    })

    return(par_l_tmp)

}


# Prepare parameter sets for variation in the resource-supply ratios ---------------------------------------------------
#
# n:  number of resource-supply vectors to simulate
# lb: lower multiplier for the coexistence boundary (> 1)
# ub: upper multiplier for the coexistence boundary (< 1)

# theoretically, variation of NBE with increasing resource l depends only on the value of Tij:
# Tij=(q_ik*mu_i * q_il*mu_i)/(q_jk*mu_i* q_jl*mu_j); see Equation S3.2e
# this pattern does not dependens on the monoculture yield difference

# Scenario in Main text, where NBE decreases with tan(\theta_ij) as Tij<1
prepare_para_resource_main <- function(n=10,lb=1.01,ub=0.99) {
  # Q: species resource consumption matrix,rows for species i and j, columns for resource k and l
  Q = comm_good$Q
             
  # mu: mortality rate for each species
  mu=comm_good$mu 

  # K: half saturation constant in the Monod function (Equation S3.1),rows for species i and j, columns for resource k and l
  K = comm_good$K

  # check
  A=Q*matrix(c(mu,mu),nrow=2,byrow = FALSE)
  Tij=A[1,1]*A[1,2]/(A[2,1]*A[2,2])

  # r_v: resource supply ratio
    # calculating the coexistence boundary for resource supply vector
    theta_i=Q[1,2]/Q[1,1]
    theta_j=Q[2,2]/Q[2,1]  
    # the threshold for resource supply ratio to ensure species i has higher monoculture yield
    theta_eta=A[2,2]/A[1,1]

    if(theta_eta*0.99<theta_i*lb){
      stop("All N1_mono < N2_mono under the current boundary.", call. = FALSE)
    }else{
      # a list of resource supply vectors for simulation
      r_v_l=seq(from=theta_i*lb,to=pmin(theta_j*ub,theta_eta*0.99),length=n)

      # f: scalar for resource supply
      f_l=rep(2,n)

      # all in one
      par_l_tmp=lapply(1:n, function(x){
        list(Q=Q,K=K,mu=mu,r_v=c(1,r_v_l[x]),f=f_l[x])
      })

      return(par_l_tmp)

    } 

}

# Scenario in Appendix, where NBE increases with tan(\theta_ij) as Tij>1
prepare_para_resource_sup <- function(n,lb=1.01,ub=0.99) {
  # Q: species resource consumption matrix,rows for species i and j, columns for resource k and l
  Q = comm_good$Q
  Q[2,2]=2.4

  # mu: mortality rate for each species
  mu=comm_good$mu 

  # check
  A=Q*matrix(c(mu,mu),nrow=2,byrow = FALSE)
  Tij=A[1,1]*A[1,2]/(A[2,1]*A[2,2])

  # K: half saturation constant in the Monod function (Equation S3.1),rows for species i and j, columns for resource k and l
  K = comm_good$K
  R_st_tmp=K*(A/(1-A))

  # r_v: resource supply ratio
    # calculating the coexistence boundary for resource supply vector
    theta_i=Q[1,2]/Q[1,1]
    theta_j=Q[2,2]/Q[2,1]  
    # the threshold for resource supply ratio to ensure species i has higher monoculture yield
    theta_eta= A[2,2]/ A[1,1]

    if(theta_eta*0.99<theta_i*lb){
      stop("All N1_mono < N2_mono under the current boundary.", call. = FALSE)
    }else{
      # a list of resource supply vectors for simulation
      r_v_l=seq(from=theta_i*lb,to=pmin(theta_j*ub,theta_eta*0.99),length=n)

      # f: scalar for resource supply
      f_l=rep(2,n)

      # all in one
      par_l_tmp=lapply(1:n, function(x){
        list(Q=Q,K=K,mu=mu,r_v=c(1,r_v_l[x]),f=f_l[x])
      })

      return(par_l_tmp)

    }

}

# Prepare parameter sets for variation in the qil  ---------------------------------------------------
#
# n:  number of qil to simulate
# lb: lower multiplier for qil
# ub: upper multiplier for qil

# Scenario in Main text, where ND increases NBE as qik>qjk
prepare_para_nonlim_main <- function(n=10,lb=0.25,ub=0.75) {
  # Q: species resource consumption matrix,rows for species i and j, columns for resource k and l
  Q = comm_good$Q
  theta_j=Q[2,2]/Q[2,1]

  # make sure theta_i < theta_j, and R*_il<R*_jl
  qil_l=rev(seq(from=lb*theta_j*Q[1,1],to=pmin(Q[2,2]*0.99,ub*theta_j*Q[1,1]),length=n))
             
  # mu: mortality rate for each species
  mu=comm_good$mu

  # K: half saturation constant in the Monod function (Equation S3.1),rows for species i and j, columns for resource k and l
  K = comm_good$K

  # r_v: resource supply ratio 
  # find a supply ratio that guarantee coexistence across all combinations and species i has higher monoculture yield
  Q_tmp=Q
  Q_tmp[1,2]= max(qil_l)
  A=Q_tmp*matrix(c(mu,mu),nrow=2,byrow = FALSE)
  theta_i_max=Q_tmp[1,2]/Q_tmp[1,1]# maximum theta_i_max
  R_st_tmp=K*(A/(1-A))
  # the threshold for resource supply ratio to ensure species i has higher monoculture yield
  theta_eta= A[2,2]/ A[1,1]

  if(theta_eta*0.99<theta_i_max){
      stop("All N1_mono < N2_mono under the current boundary.", call. = FALSE)
   }else{
      r_v=c(1,mean(c(theta_i_max,pmin(theta_eta*0.99,theta_j))))

      # f: scalar for resource supply
      f_l=rep(2,n)

      # all in one
      par_l_tmp=lapply(1:n, function(x){
        Q_tmp=Q
        Q_tmp[1,2]=qil_l[x]
        list(Q=Q_tmp,K=K,mu=mu,r_v=r_v,f=f_l[x])
      })
      return(par_l_tmp)
   }

}

# Scenario in Appendix, where ND decreases NBE as qik<qjk
# I make the resource regions pattern here the same as prepare_para_nonlim_main
# codes have to be modified for more general conditions
  
prepare_para_nonlim_sup <- function(n=10,lb=0.25,ub=0.75) {
  # Q: species resource consumption matrix,rows for species i and j, columns for resource k and l
  Q = comm_good$Q
  theta_j=Q[2,2]/Q[2,1]
  Q[2,1]=Q[1,1]*1.1
  # keep the same thetaj
  Q[2,2]=Q[2,1]*theta_j

  # make sure theta_i < theta_j, and R*_il<R*_jl
  qil_l=rev(seq(from=lb*theta_j*Q[1,1],to=pmin(Q[2,2]*0.99,ub*theta_j*Q[1,1]),length=n))
    
  # mu: mortality rate for each species
  mu=comm_good$mu

  # K: half saturation constant in the Monod function (Equation S3.1),rows for species i and j, columns for resource k and l
  K = comm_good$K
  # r_v: resource supply ratio 
  # find a supply ratio that guarantee coexistence across all combinations and species i has higher monoculture yield
  Q_tmp=Q
  Q_tmp[1,2]= max(qil_l)
  A=Q_tmp*matrix(c(mu,mu),nrow=2,byrow = FALSE)
  A0=comm_good$Q*matrix(c(mu,mu),nrow=2,byrow = FALSE)

  # make sure R*_ik>R*_jk
  K[2,1]=K[1,1]*0.75*((A[1,1]/(1-A[1,1]))/(A[2,1]/(1-A[2,1])))
  K[2,2]=K[2,2]*((A0[2,2]/(1-A0[2,2]))/(A[2,2]/(1-A[2,2])))
  #R_st_tmp=K*(A/(1-A))

  theta_i_max=Q_tmp[1,2]/Q_tmp[1,1]# maximum theta_i_max
  theta_j=Q_tmp[2,2]/Q_tmp[2,1]
  # the threshold for resource supply ratio to ensure species i has higher monoculture yield
  # here I try to make the resource regions pattern the same as prepare_para_nonlim_main
  theta_eta=comm_good$Q[2,2]/ comm_good$Q[1,1]

  r_v=c(1,mean(c(theta_i_max,pmin(theta_eta*0.99,theta_j))))

  # f: scalar for resource supply
  f_l=rep(2,n)

  # all in one
  par_l_tmp=lapply(1:n, function(x){
     Q_tmp=Q
     Q_tmp[1,2]=qil_l[x]
     list(Q=Q_tmp,K=K,mu=mu,r_v=r_v,f=f_l[x])
  })
  return(par_l_tmp)
}


# Prepare parameter sets for variation in the qik  ---------------------------------------------------
#
# n:  number of qik to simulate
# lb: lower multiplier for qik
# ub: upper multiplier for qik
# parameter setting here can be complicated due to change simutanenous change in monoculture yield and resoure consumption ratio

# Scenario in Main text, where ND decreases CE (a special case)
prepare_para_lim_main <- function(n=10,lb=0.25,ub=0.75) {
  # Q: species resource consumption matrix,rows for species i and j, columns for resource k and l
  Q = matrix(c(0.68, 0.17, 0.26, 1.66), nrow = 2, byrow = TRUE)# base
  qik_l= seq(lb, ub, length=n)
             
  # mu: mortality rate for each species
  mu=c(0.05, 0.125)

  # K: half saturation constant in the Monod function (Equation S3.1),rows for species i and j, columns for resource k and l
  K = matrix(c(3.97, 0.0414,0.076, 0.0866), nrow = 2, byrow = TRUE)

  # resource supply point
  # find a supply point that guarantee coexistence across all combinations and species i has higher monoculture yield
  
  R_init=c(0.29, 0.77)

  # all in one
  # Note that in this case, resource supply ratio also varies across combinations so it has to be back–calculated 
  par_l_tmp=lapply(1:n, function(x){
     Q_tmp=Q
     Q_tmp[1,1]= qik_l[x]

     # calculate R star to find mixture resource equilibrium
     A_tmp=Q_tmp*matrix(c(mu,mu),nrow=2,byrow = TRUE)
     R_st_tmp=K*(A_tmp/(1-A_tmp))
     C=R_init- diag(R_st_tmp)

     list(Q=Q_tmp,K=K,mu=mu,r_v=c(1,C[2]/C[1]),f=R_init[1]- diag(R_st_tmp)[1])
  })
  return(par_l_tmp)
}

# Scenario in Appendix, where ND increases CE 
prepare_para_lim_sup <- function(n=10,lb=0.25,ub=0.5) {
  Q = comm_good$Q
  theta_j=Q[2,2]/Q[2,1]
 
  # make sure theta_i < theta_j and R*_ik>R*_jk
  qik_l=seq(from=pmax(Q[2,1]*1.25,Q[1,2]/(ub*theta_j)),to=Q[1,2]/(lb*theta_j),length=n)
             
  # mu: mortality rate for each species
  mu=comm_good$mu

  # K: half saturation constant in the Monod function (Equation S3.1),rows for species i and j, columns for resource k and l
  K = comm_good$K

  # r_v: resource supply ratio 
  # find a supply point that guarantee coexistence across all combinations and species i has higher monoculture yield
  # this resource supply has to located within two lines (see below)

  # lower boundary determined by the maximum consumption ratio of i
  Q_tmp=Q
  Q_tmp[1,1]= min(qik_l)
  A=Q_tmp*matrix(c(mu,mu),nrow=2,byrow = FALSE)
  theta_i_max=Q_tmp[1,2]/Q_tmp[1,1]# maximum theta_i_max
  R_st_tmp_max=K*(A/(1-A))

  # upper boundary: the threshold for resource supply ratio to ensure species i has higher monoculture yield
  # determined by maximum qik, i.e. lowest monoculture yield
  Q_tmp=Q
  Q_tmp[1,1]= max(qik_l)
  A=Q_tmp*matrix(c(mu,mu),nrow=2,byrow = FALSE)
  R_st_tmp=K*(A/(1-A))
  theta_eta = A[2,2]/ A[1,1]

  # the qualified resource suplly point located at the right side of the intersction point
  poin_seek=point_seek_visualize(x0=R_st_tmp_max[1,1],y0=R_st_tmp_max[2,2],z0=theta_i_max,
             x1=R_st_tmp[1,1],    y1=R_st_tmp[2,2],z1=theta_eta,delta = 1.5)

  R_init=unlist(poin_seek[["right_point"]])

  # all in one
  # Note that in this case, resource supply ratio also varies across combinations so it has to be back–calculated 

  par_l_tmp=lapply(1:n, function(x){
     Q_tmp=Q
     Q_tmp[1,1]= qik_l[x]

     # calculate R star to find mixture resource equilibrium
     A_tmp=Q_tmp*matrix(c(mu,mu),nrow=2,byrow = TRUE)
     R_st_tmp=K*(A_tmp/(1-A_tmp))
     C=R_init- diag(R_st_tmp)
     list(Q=Q_tmp,K=K,mu=mu,r_v=c(1,C[2]/C[1]),f=R_init[1]- diag(R_st_tmp)[1])
  })
  return(par_l_tmp)
}

point_seek_visualize <- function(x0, y0, z0,
                               x1, y1, z1,
                               delta = 1,
                               plot_range = 2) {
  
  
  # intersection
  xc <- (y0 - y1 + z1 * x1 - z0 * x0) / (z1 - z0)
  yc <- y0 + z0 * (xc - x0)
  
  # at the right side
  xp <- xc + delta
  yp <- yc + (z0 + z1) / 2 * delta
  
  intersection <- c(x = xc, y = yc)
  right_point  <- c(x = xp, y = yp)
  
  # visual
  xx <- seq(
    xc - plot_range,
    xp + plot_range,
    length.out = 300
  )
  
  line_data <- rbind(
    data.frame(
      x = xx,
      y = y0 + z0 * (xx - x0),
      line = "Line a"
    ),
    data.frame(
      x = xx,
      y = y1 + z1 * (xx - x1),
      line = "Line b"
    )
  )
  
  point_data <- data.frame(
    x = c(xc, xp),
    y = c(yc, yp),
    point = c("Intersection", "Right point")
  )
  

  p <- ggplot2::ggplot(
    line_data,
    ggplot2::aes(x = x, y = y, color = line)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(
      data = point_data,
      ggplot2::aes(x = x, y = y, shape = point),
      inherit.aes = FALSE,
      size = 3
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = xc,
        y = yc,
        xend = xp,
        yend = yp
      ),
      inherit.aes = FALSE,
      linetype = "dashed",
      color = "grey40"
    ) +
    ggplot2::annotate(
      "text",
      x = xc,
      y = yc,
      label = "Intersection",
      vjust = -1
    ) +
    ggplot2::annotate(
      "text",
      x = xp,
      y = yp,
      label = "Right point",
      vjust = -1
    ) +
    ggplot2::labs(
      x = "x",
      y = "y",
      color = "Line",
      shape = "Point"
    ) +
    ggplot2::theme_minimal()
  
  # 5. 返回结果列表
  list(
    intersection = intersection,
    right_point = right_point,
    line_data = line_data,
    point_data = point_data,
    plot = p
  )
}

# Model dynamics ----------------------------------------------------------

# Two-species, two-resource consumer-resource model defined by Equations
# 1-3 and 6-8.
CR_model <- function(t, state, parms) {

  # parameters
  n <- parms$n
  mu <- parms$mu
  D  <- parms$D
  K  <- parms$K
  R0 <- parms$R0
  Q  <- parms$Q
  
  # initial state
  Ni <- state[1:n]
  Rk <- state[(n+1):(2*n)]
  # Avoid negative numerical drift
  Ni <- pmax(Ni, 0)
  Rk <- pmax(Rk, 0)

  # calculate growth rate following Monod function: G_i(R) = (R_k/(K+Rk)) / Q[i,k]
  G <- numeric(n) 
  lim_k <- integer(n)
  for (i in 1:n) {
    # growth rate under single resource limitation
    fR <- Rk/(K[i,]+Rk) / Q[i, ]       
    lim_k[i] <- which.min(fR)
    # growth rate under Liebig's law of the minimum
    G[i] <- fR[lim_k[i]]
  }
  
  # Species dynamics
  dNi <- Ni * (G - mu)
  
  # Resource dynamics (same number of resources as species)
  cons_k <- numeric(n)  # total consumption by all species
  for (k in 1:n) {
    cons_k[k] <- sum(G * Q[, k] * Ni)
  }
  dRk <- D * (R0 - Rk) - cons_k
  
  list(c(dNi, dRk), limit= lim_k)
}

# Biodiversity effect partitioning ----------------------------------------------------------

LH.partition <- function(mono, mix) {
  ww=rep(1,length(mono))
  n <- length(mono)
  NBE <- sum(mix, na.rm = TRUE) -
    sum(mono * ww, na.rm = TRUE) / sum(ww, na.rm = TRUE)
  
  if (any(is.na(mono) | mono == 0)) {
    data.frame(
      NBE = NBE,
      CE = 0,
      SE = NBE,
      tr = sum(mix, na.rm = TRUE) - max(mono, na.rm = TRUE)
    )
  } else {
    pi <- ww/sum(ww, na.rm = TRUE)
    zi <- mix/mono
    
    delta.zi <- zi - pi
    
    CE <- n * mean(delta.zi, na.rm = TRUE) * mean(mono, na.rm = TRUE)
    SE_LH <- n*cov(delta.zi,mono)*(n-1)/n
    trs=sum(mix)-max(mono)
    data.frame(NBE=NBE,CE=CE,SE=SE_LH,tr=trs)
  }
  
}

# Simulation helpers ------------------------------------------------------

# simulation function across a series of parameter sets
  # input (par_l): a list of parameters for each simulation (see prepare_para_resource() for details)
  # output: a data frame contaning both theoretical predictions and simulation results for each parameter set in par_l
simulation_CR=function(par_l){
  if (!is.list(par_l) || length(par_l) == 0L) {
    stop("`par_l` must be a non-empty list of parameter sets.", call. = FALSE)
  }

  # non-parallel version

  simu_list= lapply(seq_along(par_l), function(ss) {
    # retrieve input parameters for the current simulation
      par =par_l[[ss]]
      Q=par[["Q"]]# species resource consumption matrix 
      n=nrow(Q) # number of species
      K=par[["K"]]# half saturation constant in the Monod function (Equation S3.1)
      mu=par[["mu"]]# mortaltiy rate 
      r_v=par[["r_v"]]# resource supply vector
      f=par[["f"]]# scalar for resource supply
      D  <- rep(1,n) # dilution rates

    # calculate R^* based on the Monod function (Equation 6, Equation S3.1)
      A=Q*matrix(c(mu,mu),nrow=2,byrow = FALSE)/D
      if (any(!is.finite(A)) || any(A <= 0) || any(A >= 1)) {
        stop(
          "All elements of Q * mu / D must be finite and between 0 and 1.",
          call. = FALSE
        )
      }
      R_star=K*(A/(1-A))
    # resource supply vector based on the resource supply ratio and scalar f
      C=r_v*f

    # ----------------------------------------------------------------------
    #  analytical predictions for coexistence and biodiversity effects
    # ----------------------------------------------------------------------

    # monoculture equilibrium biomass
      N_mono_pred=C/diag(A)

    # mixture equilibrium
      # competition outcome
      theta_i=Q[1,2]/Q[1,1]; theta_j=Q[2,2]/Q[2,1]
      theta_ij=r_v[2];theta_ref=(theta_i*theta_j)/(theta_ij)
      ND=-1*log(sqrt(theta_i/theta_j))
      FDij=log(sqrt(theta_ref/theta_ij))

      # equilibrium biomass in mixture
      if(ND>abs(FDij)){
        N_mix_pred= solve(t(A), C) 
        # scaled biodiversity effect
        CE_pred=(theta_j+theta_i-theta_ref-theta_ij)/(theta_j-theta_i)
        SE_pred=((N_mono_pred[1]-N_mono_pred[2])/(N_mono_pred[1]+N_mono_pred[2]))*(theta_ref-theta_ij)/(theta_j-theta_i)
        NBE_pred=CE_pred+SE_pred
      }else{
        if(ND<0){
        # priority effects, the equilibrium biomass in mixture is not unique and depends on initial conditions
          N_mix_pred=NA
        }else if(FDij>ND){
        # species i wins
          N_mix_pred=c(N_mono_pred[1],0)
        }else{
        # species j wins
          N_mix_pred=c(0,N_mono_pred[2])
        }
        # scaled biodiversity effect
        CE_pred=0
        SE_pred=sum(N_mix_pred)-mean(N_mono_pred)
        NBE_pred=SE_pred
      }
    
  
    # ----------------------------------------------------------------------
    #  simulation
    # ----------------------------------------------------------------------

    # back-calculated supply point
    R_init=diag(R_star)+C

    Tmax <- 100 * max(1 /min(D), 1 /min(mu))
    times <- seq(0, Tmax, length.out = 2001)
    # collect parameters for the ODE solver
    parms <- list(n = n, mu = mu, D = D,K=K, R0 = R_init, Q = Q)
    
    # monoculture simulation
    out_mono_df=bind_rows(lapply(seq_len(n), function(ind){
      # set initial biomass of focal species as half of the predicted monoculture biomass, and the other species as 0
      N_init_sp=rep(N_mono_pred[ind]/2,n)
      idx=1:n
      N_init_sp[!idx==ind]=0

      # initial state for the ODE solver
      state_mono <- c(setNames(N_init_sp, paste0("n", 1:n)),setNames(R_init, paste0("R", 1:n)))

      # run the ODE solver for monoculture
      out_mono_tmp <- deSolve::ode(y = state_mono, times = times, func = CR_model, parms = parms,  method = "ode45", atol = 1e-8, rtol = 1e-8)
      
      # calculate the mean of the last 500 time points
      out_mono_tmp <- as.data.frame(out_mono_tmp) %>% 
        arrange(desc(time)) %>% 
        slice_head(n = 500) %>%
        summarise_all(mean)
      
      # rename columns for monoculture results
      out_mono_tmp=out_mono_tmp[grepl(paste(c("sp",ind,"R"),collapse = "|"),colnames(out_mono_tmp))]
      colnames(out_mono_tmp)[c(1,4)]=paste(gsub(ind,"",colnames(out_mono_tmp))[c(1,4)],"mono",sep="_")
      colnames(out_mono_tmp)[c(2:3)]=paste(colnames(out_mono_tmp)[c(2:3)],"mono",sep="_")
      return(out_mono_tmp %>%  mutate(sp=ind))
    }))

    # mixture simulation
    # initial biomass 
    N_init <- if (all(is.finite(N_mix_pred))) {
      rep(mean(N_mix_pred), n)
    } else {
      N_mono_pred / 2
    }
    state_mix <- c(setNames(N_init, paste0("n", 1:n)), setNames(R_init, paste0("R", 1:n)))
    
    # run the ODE solver for mixture
    out_mix <- deSolve::ode(y = state_mix, times = times, func = CR_model, parms = parms , method = "ode45", atol = 1e-8, rtol = 1e-8)
    
    # calculate the mean of the last 500 time points 
    out_mix_df <- as.data.frame(out_mix) %>%
      tidyr::pivot_longer(
        cols = -time,
        names_to = c(".value", "sp"),
        names_pattern = "^([A-Za-z]+)([0-9]+)$"
      ) %>% 
      dplyr::mutate(sp=as.numeric(sp))%>%
      group_by(sp) %>% 
      arrange(sp,desc(time)) %>% 
      slice_head(n = 500) %>%
      select(!time) %>% 
      ungroup() %>% 
      group_by(sp) %>% 
      summarise_all(mean)  

    # calculate simulated CE,SE,NBE based on monoculture and mixture results
    out_fi=out_mix_df %>% 
      left_join(out_mono_df,by="sp")
    
    out_nbe=out_fi %>% 
        dplyr::group_modify(~ LH.partition(.x$n_mono, .x$n)) %>% 
        dplyr::mutate(CE=ifelse(is.infinite(CE)|is.nan(CE),NA,CE),
              SE=ifelse(is.infinite(SE)|is.nan(SE),NA,SE)) %>% 
        ungroup() %>% 
        mutate(across(c("CE","SE","NBE","tr"),~.x/mean(out_fi$n_mono)))
    
    # organizing results for output
    out=ifelse(any(out_fi$n<0.001, na.rm = TRUE),"exclusion","coexist")
    out_fi=out_fi %>% 
      dplyr::select(sp,n,R,n_mono,R1_mono,R2_mono) %>% 
      tidyr::pivot_wider(names_from = "sp",values_from = n:R2_mono,names_sep = "")
    
    # ----------------------------------------------------------------------
    #  output
    # ----------------------------------------------------------------------

    N_pred=as.data.frame(N_mono_pred) %>% 
      cbind(as.data.frame(N_mix_pred)) %>% 
      dplyr::mutate(sp=1:2) %>% 
      tidyr::pivot_wider(names_from = "sp",values_from =c(N_mix_pred,N_mono_pred),names_sep = "")
    
    q <- as.data.frame(t(as.vector(Q)))
    names(q) <- paste0("q",rep(c("i","j"), times = n), 
                         rep(c("k","l"), each  = n))   
    
    R_init=c(R_init,r_v[[2]])
    names(R_init)=c("Rk0","Rl0","r_v")
    names(mu)=c("mu_i","mu_j")
    # all output in one data frame
    re_all=q %>% 
      cbind(t(R_init))%>% 
      cbind(t(mu))%>% 
      mutate(ID=ss,
      thetai=theta_i,
      thetaj=theta_j) %>% 
      cbind(out_nbe) %>%
      cbind(data.frame(
        ND=ND,
        FDij=FDij,
        CE_pred=CE_pred,
        SE_pred=SE_pred,
        NBE_pred=NBE_pred)) %>% 
      cbind(out_fi) %>% 
      cbind(N_pred) %>% 
      mutate(outcome=out)
    
    return(list(R_star,Q,re_all))
    }
  )
}
