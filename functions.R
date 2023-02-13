library(imager)
library(tidyverse) 
library(tidymodels) 
library(sp) 
library(scales)
library(cowplot)
devtools::install_github("sharlagelfand/dmc") 
library(dmc)

change_resolution <- function(image_df, x_size)
{ if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  
  sp_dat <- image_df 
  gridded(sp_dat) = ~x+y
 
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
}

process_image<-function(image_file_name, k_list){
  
  #Import image
  im<-imager::load.image(image_file_name)
  
  #Convert image into data frame, create new variable 'col' which is the RGB code of color at each point.
  tidy_dat<-as.data.frame(im,wide = 'c')%>%rename(R=c.1,G=c.2,B=c.3)%>%mutate(col=rgb(R,G,B))
  
  #Map each chosen value of k with their k-means result.
  dat<-select(tidy_dat,c(-x,-y,-col))
  kclusts<-tibble(k=k_list) %>% 
    mutate(kclust=map(k,~kmeans(x=dat,centers = .x,nstart = 4)),glanced=map(kclust,glance),)
  clusterings<-kclusts%>%unnest(cols = c(glanced))
  
  #Create a list, the first element of the list is the data frame of image, the second element of the list is the result of k-means for each selection of k.
  cluster_info<-list(tidy_dat,clusterings)
  return(cluster_info)
}

scree_plot<-function(cluster_info){
  
  #Plot the scree plot.
  G1<-ggplot(cluster_info[[2]],aes(k,tot.withinss))+geom_line()+geom_point()
  
  #Plot the ratio of change instead.
  nclust = length(cluster_info[[2]]$k) 
  ratio = rep(NA, nclust-1)
  for (kk in 2:nclust) {
    ratio[kk-1] = cluster_info[[2]]$tot.withinss[kk]/cluster_info[[2]]$tot.withinss[kk-1] 
  }
  plot_data <- data.frame(k = cluster_info[[2]]$k[2:nclust],ratio) 
  G2<-ggplot(plot_data, aes(x=k, y = ratio)) + geom_line()
  #Put two plots together
  plot_grid(G1,G2)
}

color_strips<-function(cluster_info){
  #Want to show all the color strips provided by the most complex clustering.
  #The most complex clustering is the last one in the list.
  opt <- tail(cluster_info[[2]],1)
  
  #Extract the information of the most complex clustering.
  opt_kclust<-opt$kclust
  
  #Extract the information of cluster centers, create new column 'col' which indicates the RGB code of each cluster center.
  center <- data.frame(opt_kclust[[1]]$centers) %>% mutate(col = rgb(R,G,B))
  
  #Create new data frame 'dmc' that maps the RGB information of each cluster center to their DMC information. The data frame includes six variables: 'dmc', 'name', 'hex', 'red', 'green', 'blue'.
  dmc <- map(center$col, ~dmc(.x, visualize = FALSE)) %>%tibble() %>%unnest(cols = c(.))
 
   #Show the color strips with the DMC color closest to the cluster centers' color. 'hex' is the DMC code for colors.
  show_col(dmc$hex)
}

make_pattern<-function(cluster_info,k, x_size, black_white = FALSE, background_colour = NULL){
 
  #Select the optimal clustering according to scree plot or color strips.
  selected_cluster<-cluster_info[[2]][cluster_info[[2]]$k==k,]
 
  #Extract the information of optimal clustering.
  opt_kclust <- selected_cluster$kclust[[1]]
  
  #Extract the information of optimal clustering centers, create new column 'col' which indicates the RGB code of each cluster center.
  center <- data.frame(opt_kclust$centers) %>% mutate(col = rgb(R,G,B))
  
  #Create new data frame 'dmc' that maps the clustering centers' RGB informationn to DMC information. The data frame includes six variables: 'dmc', 'name', 'hex', 'red', 'green', 'blue'.
  dmc <- map(center$col, ~dmc(.x, visualize = FALSE)) %>%tibble() %>%unnest(cols = c(.))
  
  #Update the data frame of image with clustering centers' information and combine them with DMC information.
  opt_dat <- augment(opt_kclust, cluster_info[[1]]) %>% rename(cluster = .cluster)
  opt_dat <- dmc[array(opt_dat$cluster),] %>%select(hex) %>%cbind(opt_dat, .)
 
  #Update the list cluster_info.
  new_cluster_info<-list(opt_dat,selected_cluster)
 
  #Change resolution
  simple<-change_resolution(new_cluster_info[[1]],x_size)
  
  #Set background cluster according to the selected background_color
  background<-as.character(simple[simple$hex==background_colour,][1,]$cluster)
  
  #If no background color is claimed & want black_white, return black and white plot with background.
  #If no background color is claimed & don't want black_white, return colorful plot with background.
  #If background color is claimed & want black_white, return black and white plot without background.
  #If background color is claimed & don't want black_white, return colorful plot without background.
  if(is.null(background_colour)){
    if(isTRUE(black_white)){
      ggplot(simple, aes(x = x, y = y, shape=cluster)) + geom_point() + scale_y_reverse()
    }
    else{
    ggplot(simple, aes(x = x, y = y, color=hex , shape=cluster)) + geom_point() + scale_y_reverse()
    }
  }
  else{
    #Change the hex code and cluster code to NA if the color is claimed to be background color, ggplot will automatically ignore them.
    simple$hex[simple$hex==background_colour]<-NA
    simple$cluster[simple$cluster==background]<-NA
    if(isTRUE(black_white)){
      ggplot(simple, aes(x = x, y = y, shape=cluster)) + geom_point() + scale_y_reverse()
    }
    else{
      ggplot(simple, aes(x = x, y = y, color=hex, shape=cluster)) + geom_point() + scale_y_reverse()
    }
  }
}
