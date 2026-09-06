# Figures and file outputs ----------------------------------------------------

# Resource diagram (classic) ----------------------------------------------------------

# classic resource diagram with consumption vectors, ZNGIs, supply points, and coexistence region
# input
  #data
    #R_st: R star, 2*2 matrix or a list of 2*2 matrices
    #Q: resource consumption matrix, 2*2 matrix or a list of 2*2 matrices
    #sim_re: simulation results
  # plot settings
    #spcol: color for species
    #mult: extension across x and y axis compared with the resource supply point
    #trc:whether add arrow tractories
    #trc_var: "point" if tractory along supply points; "vec" if tractory along supply vectors
    #trc_var_col: variable to set arrow color
    #trc_col: color palette for tractory arrows
    #tagind: add tags for which simulation
    #taglabel: tag labels

 CR_diag<- function(Rst,Q,sim_re, 
          spcol=c("#0070C0","#D55E00"),mult=c(1.1,1.1),
          trc_var="point",trc_var_col="NBE",trc_col=c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F","#9E0142"),
          tagind=c(2,5,7),taglabel=c("d","e","f"),color_bar=FALSE){
    
              # define resource supply point
              df_suppoint <-sim_re %>% 
                select(ID,Rk0,Rl0,r_v)

              # Define plot limits
              x_max= max(df_suppoint$Rk0)*mult[[1]]
              y_max= max(df_suppoint$Rl0)*mult[[2]]

              # Define resource consumption vectors
              df_vec <-bind_rows(lapply(1:length(Q), function(ind){
                # derive the angle for line color if there are multiple species
                theta_i=Q[[ind]][1,2]/Q[[ind]][1,1]; theta_j=Q[[ind]][2,2]/Q[[ind]][2,1]
                # define start and end of vectors
                data.frame(
                  x_start =rep(max(Rst[[ind]][,1]),2),
                  y_start =rep(max(Rst[[ind]][,2]),2),
                  x_end   = rep(x_max,2),
                  col=unlist(spcol),theta=c(theta_i,theta_j),
                  sp=c("i","j")
                ) %>% 
                  mutate(ID=as.numeric(names(Q)[ind])) %>% 
                  mutate(y_end = y_start+(x_end-x_start)*c(theta_i,theta_j))
              }))%>% 
                ungroup() %>% 
                distinct() %>% 
                group_by(sp) %>% 
                mutate(max=max(theta),
                      min=min(theta)) %>% 
                ungroup() %>% 
                # define color gradient
                mutate(col_scal=ifelse(max==min,1,(theta-min)/(1.12*(max-min))+0.1)) %>% 
                ungroup() %>% 
                mutate(color= scales::alpha(col,round(col_scal, 1)))

              ### define isoclines
              df_zngi= bind_rows(lapply(1:length(Rst), function(ind){
                theta_i=Q[[ind]][1,2]/Q[[ind]][1,1]; theta_j=Q[[ind]][2,2]/Q[[ind]][2,1]
                data.frame(
                  ## horizontal line i,j; vertical line i,j
                  xmin = rep(Rst[[ind]][,1],2),
                  ymin=rep(Rst[[ind]][,2],2),
                  xmax=c(rep(x_max,2),Rst[[ind]][,1]),
                  ymax=c(Rst[[ind]][,2],rep(max(df_vec$y_end)*mult[2],2)),
                  theta=rep(c(theta_i,theta_j),2),
                  col=rep(spcol,2),
                  sp=rep(c("i","j"),2)
                ) 
              })) %>% 
                ungroup() %>% 
                distinct() %>% 
                group_by(sp) %>% 
                mutate(max=max(theta),
                      min=min(theta)) %>% 
                ungroup() %>% 
                # define color gradient
                mutate(col_scal=ifelse(max==min,1,(theta-min)/(1.12*(max-min))+0.1)) %>% 
                ungroup() %>% 
                mutate(color= scales::alpha(col,round(col_scal, 1)))
              
              # assemble all elements
              base_plot=ggplot() +
                #isoclines
                geom_segment(data=df_zngi,aes(x=xmin,y=ymin,xend=xmax,yend=ymax,col=color),linewidth=0.5)+
                # consumption vectos
                geom_segment(data=df_vec,
                            aes(x = x_start, y = y_start,
                                xend = x_end, yend = y_end,col=color),linetype="dashed",
                            linewidth = 0.5) +
                geom_point(data=df_suppoint,
                            aes(x = Rk0, y = Rl0),
                            col="black",size=1) +
                scale_color_identity()+
                theme_classic()+
                scale_x_continuous(expand =expansion(mult=c(0.1,0.01)))+
                scale_y_continuous(expand =expansion(mult=c(0.1,0.01)))+
                theme(panel.border = element_rect(linewidth = 0.75),
                      plot.margin = margin(10, 30, 10, 2, unit = "pt"))+
                labs(x="Resource k",y="Resource l")
              
            # add trajactory of required
            if(!is.null(trc_var)){
              # chose the species
              trc_def=unlist(strsplit(trc_var, split = ";"))
              # define trajectory
              if(trc_def[[1]]=="r_v"){
              ## along resource supply point, following order of increase resource l
              df_trc=df_suppoint %>%
                mutate(
                  x = Rk0,
                  y = Rl0
                ) %>%
                arrange(r_v)  %>%
                mutate(
                    ar_xend = lead(x),
                    ar_yend = lead(y))
               
              }else{
                # along resource consumption vector point
                if(grepl("i",trc_def[[1]])){
                  df_trc=df_vec %>%
                    filter(sp=="i")
                }else{
                  df_trc=df_vec %>%
                    filter(sp=="j")
                }
                
                # define trajectory coordinate (right under resource supply point)
                df_trc=df_trc%>%
                  ungroup() %>%
                    left_join(
                      sim_re %>% select(ID, Rk0,qik:qjl),
                      by = "ID"
                    ) %>%
                    rename(var = all_of(trc_def[[1]]))%>%
                    mutate(x=Rk0,
                          y=y_start+(x-x_start)*theta) 
                
                # define direction
                if(trc_def[[2]]=="desc"){
                  df_trc=df_trc %>%
                    arrange(desc(var))
                }else{
                  df_trc=df_trc %>%
                    arrange(var) 
                }  

                # connect trajectory
                df_trc=df_trc %>%
                  mutate(
                    ar_xend = lead(x),
                    ar_yend = lead(y)
                  )
            }
              df_trc=df_trc%>%
                ungroup()%>%
                left_join(sim_re %>% 
                          rename(trc_var_col = all_of(trc_var_col))%>%
                          select(ID,trc_var_col),by="ID") 

              # add tag if required
              if(!is.null(tagind)){
                df_tag=data.frame(ID=tagind,tag=taglabel)
                df_trc=df_trc%>%
                  left_join(df_tag,by="ID")%>%
                  mutate(tag=ifelse(is.na(tag),"",tag))
              }else{
                df_trc$tag=""
              }

              # add trajactory
              tag_plot=base_plot+
                ggnewscale::new_scale_color() +
                geom_segment(data=df_trc%>%filter(!is.na(ar_xend)),aes(x = x, y = y, xend = ar_xend, yend = ar_yend,col=trc_var_col),
                  linewidth = .5,lineend = "butt",arrow = grid::arrow(length = grid::unit(0.1, "cm"),type = "closed"), na.rm = TRUE) +
                scale_color_gradientn(colors=trc_col)+
                ggrepel::geom_text_repel(data=df_trc,aes(x=x,y=y,label=tag,color=trc_var_col),vjust=0.5,size=5,hjust=-0.75,
                    show.legend = FALSE)
              if(color_bar==FALSE){
                tag_plot=tag_plot+
                guides(col="none")
              }
                
              return(tag_plot)

            }else{
              return(base_plot)
            } 
          }

# Resource diagram (regions) ----------------------------------------------------------

# classic resource diagram with consumption vectors, ZNGIs, supply points, and coexistence region
# input
  #data
    #R_st: R star, 2*2 matrix 
    #Q: resource consumption matrix, 2*2 matrix 
    #sim_ss: simulation results for one combination
  # plot settings
    #spcol: color for species 
    #reg_col: color palette for resource regions
    #mult: extension across x and y axis compared with the resource supply point
    
CR_region <- function(Rst,sim_ss, 
          mult=1.1,
          reg_col=c("#0070C0","#D55E00","#BFBFBF","#009E73"),
          lim=NULL){
          
          # define color
          df_col=data.frame(sp=c("i","j","overlap","unsed"),reg_col=c(unlist(reg_col)))
          
          #retrieve monoculture equilibrium resource levels, row for species, col for resource
          R_mono=matrix(c(sim_ss[,c( "R1_mono1","R2_mono1","R1_mono2","R2_mono2"  )]),nrow=2,ncol=2,byrow=TRUE)
          
          # retrive resource supply point
          R_init=as.numeric(sim_ss[1,c( "Rk0","Rl0")])

          # define resource regions 
          df_RCbox= data.frame(
            sp  = c("i","j","unsed","overlap"),
            ## bottom left
            x_bl   = unlist(c(R_mono[,1], R_mono[,1])),
            y_bl   = unlist(c(R_mono[,2],rev(R_mono[,2]))),
            ## top right
            x_tr = unlist(c(R_mono[2,1], R_init[1],R_mono[2,1],R_init[1])),
            y_tr = unlist(c(R_init[2],R_mono[1,2],R_mono[1,2],R_init[2])),
            alp=c(0.8,0.8,1,0.6)
          ) %>%
            left_join(df_col,by="sp")
          
          # define consumption vectors
          df_vec <- data.frame(
            sp  = c("i","j"),
            xstart    = rep(R_init[1],2),
            ystart    = rep(R_init[2],2),
            xend = unlist(R_mono[,1]),
            yend = unlist(R_mono[,2])
          ) %>%
            left_join(df_col,by="sp") 

          # define reference lines
          df_line_ref <- data.frame(
            sp  = c("i","j"),
            xstart = unlist(c(Rst[1,1], R_mono[2,1])),
            ystart = unlist(c(R_mono[1,2],Rst[2,2])),
            xend = unlist(c(R_init[1],R_mono[2,1])),
            yend = unlist(c(R_mono[1,2],R_init[2]))
          )%>%
            left_join(df_col,by="sp")

          # define isoclines: only show two lines
          df_zngi<- data.frame(
            xmin = rep(Rst[1,1],2),
            ymin=rep(Rst[2,2],2),
            xmax=c(ifelse(is.null(lim),max(df_RCbox$x_tr)*mult,lim[[3]]),Rst[1,1]),
            ymax=c(Rst[2,2],ifelse(is.null(lim),max(df_RCbox$y_tr)*mult,lim[[4]])),
            color=rev(reg_col[1:2])
          )

          # assemble all elements
          base_plot=ggplot() +
            # regions
            geom_rect(data=df_RCbox,aes(xmin=x_bl,xmax=x_tr,ymin=y_bl,ymax=y_tr,fill=reg_col,alpha=alp),linewidth = 0,col="white")+
            # isoclines
            geom_segment(
              data = df_zngi,
              aes(x = xmin, y = ymin, xend = xmax, yend = ymax,colour = color),linewidth = 0.45) +
            scale_color_identity()+
            scale_fill_identity()+
            scale_alpha_identity()+
            # ref lines
            geom_segment(
              data = df_line_ref,
              aes(x = xstart, y = ystart, xend = xend, yend = yend,colour = reg_col),
              linewidth = 0.4, linetype="dashed") +
            # vectors
            geom_segment(
              data = df_vec,
              aes(x = xstart, y = ystart, xend = xend, yend = yend,
                  colour = reg_col),linewidth = 0.5,arrow = grid::arrow(length = grid::unit(0.2, "cm"))
            ) +
            ## suppy point
            geom_point(
              data = df_vec %>% distinct(xstart,ystart),
              aes(x = xstart, y = ystart),size = 1.5,color="black"
            ) +
            theme_classic()+
            labs(x="Resource k",y="Resource l")+
            theme(
                  #panel.border = element_rect(size=0.75),
                  axis.title = element_text(size=10),
                    plot.margin  = margin(2, 25, 2, 25, unit = "pt")    
            )
          if(!is.null(lim)){
            return(base_plot+
            scale_x_continuous(limits = c(lim[1],lim[3]*mult),expand =expansion(mult=c(0.1,0.1)))+
            scale_y_continuous(limits = c(lim[2],lim[4]*mult),expand =expansion(mult=c(0.1,0.1)))
         
            )
          }else{
            return(base_plot+
            scale_x_continuous(expand =expansion(mult=c(0.1,0.05)))+
            scale_y_continuous(expand =expansion(mult=c(0.1,0.05)))
             )
          }

          }

# bi-partition diagram (ND–FD/ CE–SE) ----------------------------------------------------------
bi_diagram <- function(x,y,ID,ord,col,col_label="",dir="desc",mult=1.1,trc_col=c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F","#9E0142"),
          tagind=c(1,6,9),taglabel=c("d","e","f"),xlab,ylab,equal_axis=TRUE,region_fill){
  
  # collect data
  data=data.frame(x=x,y=y,ID=ID,col=col,ord_var=ord)
  
  # define plot limits
  if(equal_axis){
     # define coexistence region
    xmax=max(c(abs(data$y),abs(data$x)))
    df_co=data.frame(x=seq(0,xmax*mult,length=10)) %>% 
    mutate(ymax=x,ymin=-x,col=region_fill) 
  }else{
    # define positive NBE regiion
    xmax=max(abs(data$x))
    df_co=data.frame(x=seq(0,xmax*mult,length=10)) %>% 
    mutate(ymax=pmax(max(abs(data$y)),xmax*0.25),ymin=-x,col=region_fill) 
  }
  # define direction
  if(dir=="desc"){
    df_trc=data %>%
      arrange(desc(ord_var))
  }else{
    df_trc=data %>%
      arrange(ord_var) 
   }  

  # connect trajectory
  df_trc=df_trc %>%
    mutate(
      ar_xend =lead(x),
      ar_yend =lead(y))

  # assemble all elements
  base_plot=ggplot()+
    geom_ribbon(data=df_co,aes(x=x,ymin=ymin,ymax=ymax,fill=col),show.legend = FALSE)+
    geom_vline(xintercept = 0,linetype="dotted")+
    geom_hline(yintercept = 0,linetype="dotted")+
    scale_fill_identity()+
    geom_segment(
    data=df_trc%>%filter(!is.na(ar_xend)),
      aes(x = x, y = y, xend = ar_xend, yend = ar_yend,col=col),linewidth = .65,
      lineend = "butt",arrow = grid::arrow(length = grid::unit(0.1, "cm"),type = "closed"), na.rm = TRUE)+
    scale_color_gradientn(colors=trc_col,name =col_label)+

    theme_classic()+
    labs(x=xlab,
        y=ylab)+
    scale_y_continuous(expand=expansion(mult = c(0.05, 0.05)))+
    scale_x_continuous(expand=expansion(mult = c(0, 0.)))+
    theme(panel.border = element_rect(linewidth = 0.75),
          plot.margin = margin(10, 30, 10, 2, unit = "pt"),)

  # add tag if required
  if(!is.null(tagind)){
    df_tag=data.frame(ID=tagind,tag=taglabel)
    df_trc=df_trc%>%
      left_join(df_tag,by="ID")%>%
      mutate(tag=ifelse(is.na(tag),"",tag))
    
    tag_plot=base_plot+
      ggrepel::geom_text_repel(data=df_trc,aes(x=x,y=y,label=tag,color=col),vjust=0.5,size=5,hjust=-0.5,
          show.legend = FALSE)
    return(tag_plot)

  }else{
    return(base_plot)
  }
}

# Dual y-axis plot  ----------------------------------------------------------
 du_plot <- function(df, left_var=c("ND","FDij"),right_var=c("CE","SE","NBE"),
    exa,exa_label,dir,y_labels=c("Niche and fitness differences","Biodiversity effects"),x_label="Resource supply",
    colors=c(ND  = "#009E73", FDij  = "#0070C0", spacer = "transparent", CE  = "#66C2A5",SE  = "#3288BD",NBE = "#D53E4F"),
    linetypes=c(ND = "dashed",FDij = "dashed", spacer = "blank",CE = "solid",SE = "solid",NBE = "solid")){
      
      # reshape data
      left_data <- df %>%
        select(all_of(c("x","ID",left_var))) %>%
        pivot_longer(
          cols = -c(x,ID),
          names_to = "variable",
          values_to = "value"
        )%>%
        rbind( data.frame(
              x = NA_real_,
              ID = NA_real_,
              value = NA_real_,
              variable = "spacer"
            ))

      right_data <- df %>%
        select(all_of(c("x","ID",right_var))) %>%
        pivot_longer(
          cols = -c(x,ID),
          names_to = "variable",
          values_to = "value"
        )%>%
        rbind( data.frame(
              x = NA_real_,
              ID = NA_real_,
              value = NA_real_,
              variable = "spacer"
            ))

      # pick break values
      df_x=df%>%
        filter(ID%in%exa)%>%
        arrange(ID)%>%
        distinct(x)
      bk_val=df_x$x

      # scaling right variables to the same range as left
      scale_factor <- max(abs(left_data$value), na.rm = TRUE) /
        max(abs(right_data$value), na.rm = TRUE)

      right_data <- right_data %>%
        mutate(plot_value = value * scale_factor)
      
      # define legend order
      legend_order  <- c("ND", "FDij", "spacer","CE", "SE", "NBE")
      legend_labels <- expression(ND,FD[ij], "", CE,SE,NBE)

      # plot
      base_plot=ggplot() +
        geom_hline(
          yintercept = 0,
          linetype = "dotted",
          linewidth = 0.3,
          color =alpha( "grey40",0.5)
        ) +
        geom_vline(
          xintercept = bk_val,
          linetype = "dotted",
          color =alpha( "grey40",0.75),
          linewidth = 0.4
          
        ) +
        geom_line(
          data = left_data,
          aes(
            x = x,
            y = value,
            color = variable,
            linetype = variable
          ),
          linewidth = 0.65,
          na.rm = TRUE
        ) +
        geom_line(
          data = right_data,
          aes(
            x = x,
            y = plot_value,
            color = variable,
            linetype = variable
          ),
          linewidth = 0.65,
          na.rm = TRUE
        ) +
        scale_y_continuous(
          name =y_labels[1],
          sec.axis = sec_axis(
            transform = ~ . / scale_factor,
            name =y_labels[2]
          )
        ) +
        labs(
          x =x_label,
          color = NULL,
          linetype = NULL
        ) +
        theme_classic() +
        scale_color_manual(
          name = NULL,
          values = colors,
          breaks = legend_order,
          labels = legend_labels,
          drop = FALSE
        ) +
        scale_linetype_manual(
          name = NULL,
          values = linetypes,
          breaks = legend_order,
          labels = legend_labels,
          drop = FALSE
        ) +
         guides(
          color = guide_legend(
            nrow = 2,
            byrow = TRUE
          ),
          linetype = guide_legend(
            nrow = 2,
            byrow = TRUE
          )
        ) +
        theme(legend.position = "inside",
              legend.position.inside = c(0.05, 0.15),
              legend.justification = "left",
              legend.box.just = "left",
              legend.margin = margin(0),
              legend.background = element_rect(
                fill = "transparent",
                color = NA
              ),
              legend.box.background = element_rect(
                fill = "transparent",
                color = NA
              ),
              legend.key = element_rect(
                fill = "transparent",
                color = NA
              ),
              legend.key.width = grid::unit(0.8,"cm"),
              axis.text.x.top = element_text(size=13),
              axis.line.x.top = element_blank(),
              axis.ticks.x.top = element_blank(),
              plot.margin =margin (0, 25, 5, 2, unit = "pt") )

      if(dir=="desc"){
        return(base_plot+
                 scale_x_reverse(
          # Duplicate x-axis at the top
          sec.axis = dup_axis(
            name = NULL,
            breaks = bk_val,
            labels = exa_label
          )
        ))
      }else{
        return(base_plot+
                 scale_x_continuous(
          # Duplicate x-axis at the top
          sec.axis = dup_axis(
            name = NULL,
            breaks = bk_val,
            labels = exa_label
          )
        ))
      }
 }

# RC_graph: Wrap everything together ----------------------------------------------------------

# input:
  # simu_result: simulation results retrived from simulation_CR
  # exa: example simulation ID
  # fig_labs: labels for subplots
  # trc_pal: color palatte for trajactories

RC_graph <- function(simu_result,exa=c(2,6,10),fig_labs=letters[1:6],dir="desc",
    x_var="thetai",x_label="Species resource consumption",
    trc_pal=c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F","#9E0142")
    ){
    # retrive outputs for visual
    var_names=c("Rst","Q")
    var_l=lapply(1:2,function(ind){
        tmp=lapply(simu_result,function(x){x[[ind]]})
        names(tmp)=unlist(lapply(simu_result,function(x){x[[3]]$ID}))
        return(tmp)
    })
    names(var_l)=var_names
    sim_re=bind_rows(lapply(simu_result,function(x){x[[3]]}))

    # resource digram
    CR_vec=CR_diag(Rst=var_l$Rst,Q=var_l$Q, sim_re=sim_re, 
            spcol=c("#0070C0","#D55E00"),mult=c(1.1,1.1),trc_col=trc_pal,
            trc_var=paste(x_var,dir,sep=";"),trc_var_col="NBE",
            tagind=exa,taglabel=fig_labs[4:6])

    # ND-FD diagram
    ND_FD_diag=bi_diagram(x=sim_re$ND,y=sim_re$FDij,ID=sim_re$ID,col=sim_re$NBE,
            ord=unlist(sim_re[,x_var]),dir=dir,mult=1.2,
            trc_col=trc_pal,tagind=exa,taglabel=fig_labs[4:6],region_fill=alpha("#009E73",0.1),
            xlab="Niche difference (ND)",ylab=expression(paste("Fitness difference (",FD[ij],")")))+
            theme(legend.position = "none")

    # CE-SE diagram
    CE_SE_diag=bi_diagram(x=sim_re$CE,y=sim_re$SE,ID=sim_re$ID,col=sim_re$NBE,col_label="NBE",
            ord=unlist(sim_re[,x_var]),dir=dir,mult=1.2,
            trc_col=trc_pal,tagind=exa,taglabel=fig_labs[4:6],
            xlab="Complementary effect (CE)",ylab="Selection effect (SE)",
            equal_axis=FALSE,region_fill=alpha("#D53E4F",0.075))+
            theme(panel.border = element_rect(linewidth = 0.75),
                      plot.margin = margin(10, 5, 10, 2, unit = "pt"))

    # resource regions

    # define the same axis limits
    xmax=max(unlist(lapply(1:3,function(ind){sim_re[exa[[ind]],c( "Rk0")]})))*1.05
    ymax=max(unlist(lapply(1:3,function(ind){sim_re[exa[[ind]],c( "Rl0")]})))*1.05
    xmin=min(unlist(lapply(1:3,function(ind){var_l$Rst[exa[[ind]]][[1]][1,1]})))
    ymin=min(unlist(lapply(1:3,function(ind){var_l$Rst[exa[[ind]]][[1]][2,2]})))

    CR_regions=lapply(1:3,function(ind){
        CR_region(Rst=var_l$Rst[exa[[ind]]][[1]], sim_ss=sim_re[exa[[ind]],], 
            mult=1.025,
            reg_col=c("#0070C0","#D55E00","#BFBFBF","#009E73"),
            lim=c(xmin,ymin,xmax,ymax))})

    # parameter–driven change in all variables

    # prepare data for visual
    df= bind_rows(lapply(simu_result,function(x){x[[3]]}))%>%
        rename(x = all_of(x_var))%>%
        select(x,ID,NBE:SE,ND:FDij)

    grad_plot= du_plot(df,left_var=c("ND","FDij"),right_var=c("CE","SE","NBE"),
      y_labels=c("Niche and fitness differences","Biodiversity effects"),
      x_label=x_label,
      exa=exa,exa_label=fig_labs[4:6],dir=dir)+
      labs(tag="(g)")+
      theme(plot.tag = element_text(face="bold",size=16))

    # put all together
    blank= ggplot() +theme_void()

    row_top <- ggpubr::ggarrange(
          blank,CR_vec, ND_FD_diag,CE_SE_diag,
          nrow = 1,ncol = 4,widths = c(0.125, 1, 1, 1.20),
          labels = c("",paste0("(", fig_labs[1:3], ")")),

          # Move labels upward to avoid the y-axis titles
          label.x = c( 0.01, 0.01, 0.01),
          label.y = c( 1., 1, 1),
          hjust = 0.5, vjust = 0.7,
          font.label = list(size = 16, face = "bold"),align = "h")

    row_middle <- ggpubr::ggarrange(
          blank,CR_regions[[1]],CR_regions[[2]],CR_regions[[3]],blank,
          nrow = 1,ncol = 5,widths = c(0.2, 1, 1,1, 0.25),

          labels = c("",paste0("(", fig_labs[4:6], ")"),"" ),
          label.x = c(0, 0.01, 0.01, 0.01, 0),
          label.y = c(1, 1., 1.,1., 1),
          hjust = -0.2,vjust = 1,font.label = list(size = 14, face = "bold"), align = "hv")
    
    figure_core <- ggpubr::ggarrange(blank,row_top,blank,row_middle,ggpubr::ggarrange(grad_plot),
          nrow = 5,ncol = 1,
          # Give g more height and reduce the middle-row space
          heights = c(0.05,0.975,0.05, 0.7, 1.025),align = "v")
    return(figure_core)
}


# transgressive_overyielding/ positive NBE region
bef_region <- function(simu_result,var="trans",col=alpha("#5E4FA2",0.1)){

 # retrive outputs for visual
    var_names=c("Rst","Q")
    var_l=lapply(1:2,function(ind){
        tmp=lapply(simu_result,function(x){x[[ind]]})
        names(tmp)=unlist(lapply(simu_result,function(x){x[[3]]$ID}))
        return(tmp)
    })
    names(var_l)=var_names
    sim_re=bind_rows(lapply(simu_result,function(x){x[[3]]}))%>%
      mutate(trans=(n1+n2-pmax(n_mono1,n_mono2))/((n_mono1+n_mono2)/2))%>%
      rename(y = all_of(var))
  
   # resource digram
    CR_vec=CR_diag(Rst=var_l$Rst,Q=var_l$Q, sim_re=sim_re, 
            spcol=c("#0070C0","#D55E00"),mult=c(1.1,1.1),
            trc_col=c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#FEE08B", "#FDAE61", "#F46D43", "#D53E4F","#9E0142"),
            trc_var="r_v;inc",trc_var_col="y",
            tagind=NULL,taglabel=NULL,color_bar = TRUE)+
            theme(plot.margin = margin(10, 15, 10, 10, unit = "pt"))
    
    theta_i=unique(sim_re$thetai)
    theta_j=unique(sim_re$thetaj)

    if(var=="trans"){
      CR_vec=CR_vec+
        labs(color=expression(widehat(paste(Delta,"Y"))))
      v_posNBE=c(theta_i,theta_j)
    }else{
      
      CR_vec=CR_vec+
        labs(color=var)

      # predicted positive NBE region

      mu=c(unique(sim_re$mu_i),unique(sim_re$mu_j))
      A=unique(var_l$Q)[[1]]*matrix(c(mu,mu),nrow=2,byrow = FALSE)
      ## theoretical prediction for positive NBE region
      xi=sum(A[1,]/A[2,])-2 
       ## threshold for positive NBE region
      v_thres=-(theta_i+theta_j-2*(A[1,1]/A[2,2])*theta_i*theta_j)/(xi)
       if (xi>0){
         v_posNBE=c(pmax(v_thres,theta_i),theta_j)
       }else{
         v_posNBE=c(theta_i,pmin(v_thres,theta_j))
       }
      
    }
           
    
    # define positive NBE/transgressive overyielding region
     Rst=unique(var_l$Rst)[[1]]
     Rinit_x=unique(sim_re$Rk0)
     df_nbe=data.frame(x=seq(Rst[1,1],Rinit_x*1.1,length=10)) %>% 
        mutate(ymax=Rst[2,2]+v_posNBE[2]*(x-Rst[1,1]),
            ymin=Rst[2,2]+v_posNBE[1]*(x-Rst[1,1])) 
    # find transgressive overyielding region in simulation if there is any
    if(any(sim_re$y>0)){
      return( CR_vec+
      geom_ribbon(data=df_nbe,aes(x=x,ymin=ymin,ymax=ymax),show.legend = FALSE,fill=col))
     
    }else{
      return(CR_vec)
    }
    
}

  
