################################################################
#                                                              #
#           Visualization via Stacked Bar Plots                #
#                                                              #
################################################################


# load libraries
library(here) # lock the root directory
library(dplyr) # many functions to be used in pipe
library(ggplot2) # functions for plotting
library(reshape2) # reshape matrix
library(RColorBrewer) # set colors for plotting
library(hash)


# parameters to be changed by users
out.dir <- "stacked_bar_plots" # dir to save the HD images of stacked bar plots
csv.dir <- "deconvolution_results" # all the deconvolution results, of which each subdir represents a method
sample.list <- c("18_64", "1_1", "2_5", "2_8", "2_3", "T4857") # the list of samples; the first three are CT, while
# the other three are AD

layer.hash <- hash(c(
  "X" = "X", 
  "PERLayer1" = "L1", 
  "PERLayer2" = "L2", 
  "PERLayer3" = "L3",
  "PERLayer4" = "L4", 
  "PERLayer5" = "L5", 
  "PERLayer6" = "L6", 
  "PERWhiteMatter" = "WM"
)) # hash table from layer names to labels


# basic parameter setting for input
here::i_am("visualize_stacked_bar_plot.R") # set the root directory
# setwd(csv.dir)
csv.subdirs <- list.dirs(recursive = T) # get all subdirs recursively
csv.subdirs <- csv.subdirs[-1] # remove the current directory
csv.subdirs <- csv.subdirs[which(csv.subdirs != paste0("./", csv.dir))] # remove the csv directory
csv.files <- Reduce(c, lapply(csv.subdirs, function(dir) {
  paste0(dir, "/", sample.list, "_v1.csv")
})) # all the csv files
message (length(csv.files), " csv files were identified in ", here(), ".\n")
# csv.files <- csv.list[unlist(lapply(csv.list, function(x) {
#   grepl(".csv$", x)
# }))] # all the csv files
# message (length(csv.files), " .csv files were retained.\n")
# %>% lapply(., function(x) {
#   y <- substring(x, 3)
# }) %>% unlist # all the subdirs
# subdir.list <- subdir.list[-1] # remove the current directory



# generate stacked bar plots
for (i in seq_along(csv.files)) {
  csv <- csv.files[i] # file path
  path.array <- strsplit(csv, split = "/")
  image.path <- paste0("./", out.dir, "/", path.array[[1]][3], "/", 
                      substring(path.array[[1]][4], 1, 
                                nchar(path.array[[1]][4]) - 4), ".tiff")
  # the path to the stacked bar plot
  
  pct.df <- read.csv(csv) # read the csv file
  colnames(pct.df) <- lapply(colnames(pct.df), function(x) {
    layer.hash[[x]]
  }) %>% unlist # rename the layer labels
  pct.df[, 1] <- lapply(pct.df[, 1], function(x) {
    substring(x, 4)
  }) %>% unlist # rename the cell subpopultions
  summary.df <- melt(pct.df) # reshape the matrix
  colnames(summary.df) <- c("Cell types", "Layer", "Percent")
  # display.brewer.all()
  # https://www.r-graph-gallery.com/40-rcolorbrewer-get-a-longer-palette.html
  color.list <- colorRampPalette(brewer.pal(8, "Dark2"))(nrow(pct.df))
  
  # save the image
  p <- ggplot(summary.df, aes(fill = `Cell types`, y = Percent, x = Layer)) + 
    geom_bar(position = "fill", stat = "identity") + theme_classic() + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 20), 
          axis.text.y = element_text(size = 20), axis.title = element_text( size = 20), 
          axis.title.y = element_blank(), legend.text = element_text(size = 20), 
          legend.title = element_text(size = 20)) + 
    scale_fill_manual(values = color.list)
    ggsave(p, filename = image.path,
           device ="tiff", dpi = 300 , width = 10.5, height = 10)
}








# load data
# the data format is as follows (.csv):
#
##########################################################################
# "","PERLayer1","PERLayer2","PERLayer3"                                 #
# "EC.OPC",0.0711661106604723,0.121351634808745,0.0763463438996696       #
# "EC.Inh.1",0.0346118076990167,0.0351052092831525,0.0821011741642907    #
# "EC.Micro",0.290471592730411,0.0154787349071103,0.043049979445711      #
##########################################################################
#