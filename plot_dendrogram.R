read <- function(x) { fread(x)
}

plot_dendrogram <- function(x) {

#packages
library(data.table)
library(dplyr)
library(tibble)
library(uwot)
library(parallel)
library(plotly)
library(cluster)
library(factoextra)
library(dendextend)
library(purrr)
library(randomcoloR)

files <- list.files(path = "./Betas/", recursive = TRUE, full.names = TRUE)
files <- lapply(x, function(x) files[grepl(x, files)])
gc()

#serial
beta <- lapply(files, function(x) fread(x))
beta <- lapply(beta, function(x) x <- x %>% column_to_rownames("V1"))

# #parallel
# cl <- makeCluster(30)
# clusterExport(cl, c("fread", "read"))
# 
# beta <- parLapply(cl, files, read)
# beta <- lapply(beta, function(x) x <- x %>% column_to_rownames("V1"))
# stopCluster(cl)

beta <- do.call("cbind", beta)

beta_t <- Rfast::transpose(as.matrix(beta))
rownames(beta_t) <- colnames(beta)
colnames(beta_t) <- rownames(beta)

#feature selection
beta_SD <- beta_t[,Rfast::colVars(as.matrix(beta_t), std = TRUE, parallel = TRUE)>0.27]
  

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(beta_SD, method = x)$ac
}

best_method <- map_dbl(m, ac)
best_method <- best_method[which.max(best_method)]

avg_silh <- fviz_nbclust(beta_SD, FUN = hcut, method = "silhouette")
n_clust <- avg_silh$data
max_cluster <- as.numeric(n_clust$clusters[which.max(n_clust$y)])

# gap_stat <- clusGap(beta_SD, FUN = hcut, K.max = 10, B = 10)
# fviz_gap_stat(gap_stat)

hc3 <- agnes(beta_SD, method = names(best_method[which.max(best_method)]))

# Cut tree into k groups
sub_grp <- cutree(hc3, k = max_cluster)
clusters <- as.data.frame(sub_grp)
anno_base$sample_idat <- paste0(anno_base$Sample,"_",anno_base$idat_filename)
clusters$class <- anno_base$Combined_class_match_dkfz[match(rownames(clusters), anno_base$idat_filename)]
clusters$color <- mycols[match(clusters$class, names(mycols))]
clusters$case <- anno_base$NIH_labels[match(rownames(clusters), anno_base$idat_filename)]
clusters$sample_idat <- anno_base$sample_idat[match(rownames(clusters), anno_base$idat_filename)]

#fviz_cluster(list(data = beta_SD, cluster = sub_grp), ggtheme = theme_minimal())

# fviz_dend(hc3,
#           k = max_cluster,
#           h = NULL,
#           k_colors = "aaas",
#           palette = NULL,
#           show_labels = TRUE,
#           cases = cases,
#           color_labels_by_k = TRUE,
#           label_cols = NULL,
#           labels_track_height = 3,
#           repel = FALSE,
#           lwd = 0.7,
#           type = "rectangle",
#           #phylo_layout = "layout.auto",
#           rect = TRUE,
#           rect_border = "aaas",
#           rect_lty = 0,
#           rect_fill = TRUE,
#           lower_rect = 0,
#           horiz = TRUE,
#           cex = 0.8,
#           main = "Cluster Dendrogram",
#           xlab = "",
#           ylab = "Height",
#           sub = NULL,
#           ggtheme = theme_classic()
# )

dend <- as.dendrogram(hc3)
clusters <- clusters %>% rownames_to_column("sample")
clusters <- clusters[match(order.dendrogram(dend),rownames(clusters)),]
labels_colors(dend) <- clusters$color
#dend <- color_branches(dend, k = max_cluster)
dend <- dend %>% set("branches_lwd", 0.5)

#plot(dend)

library(ggplot2)
library(ggdendro)
dendr <- dendro_data(hc3, type="rectangle") # convert for ggplot
clust.df <- data.frame(label=rownames(beta_SD), cluster=factor(sub_grp))
dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by="label")
rect <- aggregate(x~cluster,label(dendr),range)
rect <- data.frame(rect$cluster,rect$x)
names(rect)[1] <- "cluster"
ymax <- mean(hc3$height[length(hc3$height)-((max_cluster-2):(max_cluster-1))])

# ggplot() + 
#   geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
#   geom_text(data=label(dendr), aes(x, y, label=label, hjust=0, color=cluster), 
#             size=3) +
#   geom_rect(data=rect, aes(xmin=X1-.3, xmax=X2+.3, ymin=0, ymax=ymax), 
#             color="red", fill=NA)+
#   geom_hline(yintercept=0.33, color="blue")+
#   coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
#   theme_dendro()

# #random color scale
# cl <- colors(distinct = TRUE)
# set.seed(20) # to set random generator seed
# mycols2 <- sample(cl, length(unique(clusters$sub_grp)))
# names(mycols2) <- unique(clusters$sub_gr)


# create your own color palette based on `seedcolors`
mycols2 <- randomColor(max_cluster, luminosity="bright")

# Dendrogram
# scale_x_continuous() for p1 should match scale_x_discrete() from p2
# scale_x_continuous strips off the labels. I grab them from df2
# scale _y_continuous() puts a little space between the labels and the dendrogram
p1 <- ggplot() +
      geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) +
      geom_rect(data=rect, aes(xmin=X1-.3, xmax=X2+.3, ymin=0, ymax=ymax, fill = cluster), 
            alpha = 0.5) +
      theme_classic() +
      theme(axis.line = element_blank(),
            legend.position = "right") +
      scale_x_continuous(expand = c(0, 0.5)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      scale_fill_manual(values = mycols2)

cols1 <- as.vector(clusters$color)
names(cols1) <- clusters$class

rownames(clusters) <- factor(rownames(clusters), order.dendrogram(dend))

# Tiles and labels
p2 <- ggplot(clusters, aes(x = sample_idat, y = 1, fill = class)) +
              geom_tile() +
              #geom_text(aes(label=case), angle = 90, vjust = 1) +
              theme_classic() +
              scale_fill_manual(values = cols1) +
              theme(axis.title = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    axis.line = element_blank(),
                    legend.position = "right")

ply1 <- ggplotly(p1, source = "C") %>% config(scrollZoom = TRUE) %>%
         layout(dragmode = "pan", xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
         showlegend = F)
ply2 <- ggplotly(p2, source = "B") %>% config(scrollZoom = TRUE) %>%
         layout(dragmode = "pan", xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
         showlegend = F)

subplot(ply1, ply2, nrows=2, heights = c(0.93,0.07), shareX = TRUE)


}
