

plot_supervised <- function(x) {


#packages
library(data.table)
library(dplyr)
library(tibble)
library(uwot)
  #densvis has odd dependencies, for mac, I'm not sure this is necessary
#library(densvis)
library(Rfast)
#library(reticulate)
library(optparse)
library(randomcoloR)
library(stringr)
library(parallel)
library(plotly)

files <- list.files(path = "./Betas/", recursive = TRUE, full.names = TRUE)
files <- lapply(x, function(x) files[grepl(x, files)])
gc()

#serial
beta <- lapply(files, function(x) fread(x))
beta <- lapply(beta, function(x) x <- x %>% column_to_rownames("V1"))
beta <- do.call("cbind", beta)

beta_t <- Rfast::transpose(as.matrix(beta))
rownames(beta_t) <- colnames(beta)
colnames(beta_t) <- rownames(beta)

#feature selection
beta_SD <- beta_t[,Rfast::colVars(as.matrix(beta_t), std = TRUE, parallel = TRUE)>0.24]

PC <- prcomp(beta_SD, center = TRUE, scale = FALSE)

cumulative_pro_PC <- as.data.frame(cumsum(PC$sdev^2 / sum(PC$sdev^2)))
names(cumulative_pro_PC) <- "variance"
PCs_keep <- length(cumulative_pro_PC[cumulative_pro_PC$variance<=0.8,])

umap <- uwot::umap(PC$x[,1:PCs_keep],
                   #n_components = 2,
                   #pca = 25,
                   n_neighbors = 10,
                   metric = "correlation",
                   #y = anno_neuro$Combined_class_match,
                   spread = 1,
                   min_dist = 0,
                   local_connectivity = 1,
                   bandwidth = 1)

#plot(umap, pch = 19)

rownames(umap) <- rownames(beta_SD)
umap <- as.data.frame(umap)
umap <- umap %>% rownames_to_column("ID")
names(umap) <- c("ID","x","y")

#add UMAP coordinates to annotations
anno_base$umap_supSD_x <- umap$x[match(anno_base$idat_filename,umap$ID)]
anno_base$umap_supSD_y <- umap$y[match(anno_base$idat_filename,umap$ID)]
anno_base_supervised <- anno_base[!is.na(anno_base$umap_supSD_x),]

m <- list(
  l = 100,
  r = 200,
  b = 100,
  t = 50)

if(!is.na(anno_base_supervised$NIH_labels)) {
key <- rownames(anno_base_supervised)
p <- ggplot(data=anno_base_supervised, aes(x=anno_base_supervised[["umap_supSD_x"]],y=anno_base_supervised[["umap_supSD_y"]],key=key)) +
            geom_point(aes(color=Combined_class_match_dkfz),  size=2, alpha=1) +
            theme_classic() +
            theme(axis.text.y = element_text(size=9, color="black"),
                  axis.text.x = element_text(size=9, color="black"),
                  axis.ticks.y = element_line(color="black", size = 0.5),
                  axis.ticks.x = element_line(color="black", size = 0.5),
                  axis.ticks.length = unit(2,"mm"),
                  panel.border = element_rect(colour = "black", fill=NA, size=1),
                  panel.grid.major = element_line(colour="grey", size=0.5),
                  axis.line = element_blank(),
                  legend.text=element_text(size=10),
                  legend.title=element_text(size=11),
                  legend.key.size = unit(0.5, 'lines'),
                  axis.title.x = element_text(size=14, color="black"),
                  axis.title.y = element_text(size=14, color="black")) +
            labs(x = ("umap_1"), y = "umap_2") +
            #theme(legend.position="right") +
            scale_color_manual("Methylation class", values = mycols, guide = guide_legend(override.aes = list(shape = 15))) +
            scale_x_continuous(breaks = seq(-100, 100, by=5)) +
            scale_y_continuous(breaks = seq(-100, 100, by=5))
          #guides(color = guide_legend(override.aes = list(shape = c(15))))
          #coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
ggplotly(p, source = "B") %>% 
  add_annotations(x = subset(p$data, !is.na(NIH_labels))[["umap_supSD_x"]],
                  y = subset(p$data, !is.na(NIH_labels))[["umap_supSD_y"]],
                  text = subset(p$data, !is.na(NIH_labels))$NIH_labels,
                  showarrow = TRUE,
                  arrowcolor='red',
                  arrowhead = 6,
                  arrowsize = 1,
                  xref = "x",
                  yref = "y",
                  font = list(color = 'black',
                              family = 'arial',
                              size = 14)) %>%
  config(scrollZoom = TRUE) %>%
  layout(width = 800, dragmode = "pan", xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
         showlegend = T,
         margin = m,
         legend = list(orientation = "v",
                       yanchor = "top",
                       itemclick = "toggleothers",
                       tracegroupgap = 2,
                       itemwidth = 75,
                       itemsizing = "constant")) %>% 
  toWebGL()


} else {

key <- rownames(anno_base_supervised)
p <- ggplot(data=anno_base_supervised, aes(x=anno_base_supervised[["umap_supSD_x"]],y=anno_base_supervised[["umap_supSD_y"]],key=key)) +
  geom_point(aes(color=Combined_class_match_dkfz),  size=2, alpha=1) +
  theme_classic() +
  theme(axis.text.y = element_text(size=9, color="black"),
        axis.text.x = element_text(size=9, color="black"),
        axis.ticks.y = element_line(color="black", size = 0.5),
        axis.ticks.x = element_line(color="black", size = 0.5),
        axis.ticks.length = unit(2,"mm"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_line(colour="grey", size=0.5),
        axis.line = element_blank(),
        legend.text=element_text(size=10),
        legend.title=element_text(size=11),
        legend.key.size = unit(0.5, 'lines'),
        axis.title.x = element_text(size=14, color="black"),
        axis.title.y = element_text(size=14, color="black")) +
  labs(x = ("densmap_1"), y = "densmap_2") +
  #theme(legend.position="right") +
  scale_color_manual("Methylation class", values = mycols, guide = guide_legend(override.aes = list(shape = 15))) +
  scale_x_continuous(breaks = seq(-100, 100, by=5)) +
  scale_y_continuous(breaks = seq(-100, 100, by=5))
#guides(color = guide_legend(override.aes = list(shape = c(15))))
#coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
ggplotly(p, source = "B") %>% 
                    # add_annotations(x = subset(p$data, !is.na(NIH_labels))[["umap_supSD_x"]],
                    #                 y = subset(p$data, !is.na(NIH_labels))[["umap_supSD_y"]],
                    #                 text = subset(p$data, !is.na(NIH_labels))$NIH_labels,
                    #                 showarrow = TRUE,
                    #                 arrowcolor='red',
                    #                 arrowhead = 6,
                    #                 arrowsize = 1,
                    #                 xref = "x",
                    #                 yref = "y",
                    #                 font = list(color = 'black',
                    #                             family = 'arial',
                    #                             size = 14)) %>%
                    config(scrollZoom = TRUE) %>%
                    layout(width = 800, dragmode = "pan", xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
                           showlegend = T,
                           margin = m,
                           legend = list(orientation = "v",
                                         yanchor = "top",
                                         itemclick = "toggleothers",
                                         tracegroupgap = 2,
                                         itemwidth = 75,
                                         itemsizing = "constant")) %>% 
                    toWebGL()
}
}
