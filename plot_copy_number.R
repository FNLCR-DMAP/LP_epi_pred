path_prefix="/data/mdata/Shiny_NCI_demo/"
#path_prefix="./"

CNA_plot <- function(x) {


#packages
library(foreach)
library(dplyr)
library(tibble)
library(GenomicRanges)
library(splitstackshape)
library(data.table)
library(tidyr)
library(bigreadr)
library(stringr)
library(plotly)
print("starting CNA")
lines <- read.delim(paste0(path_prefix,"plot_vertical_lines.txt"))
print(paste0(path_prefix,"Segments"))
print(x)
file <- list.files(path = paste0(path_prefix,"Segments"), pattern = x, recursive = TRUE, full.names = TRUE)
print(file)
segments <- fread(file)
print(paste0(path_prefix,"Bins"))
file <- list.files(path = paste0(path_prefix,"Bins"), pattern = x, recursive = TRUE, full.names = TRUE)
print(file)
bins <- fread(file)

#bins
bins$position <- c((bins$Start + bins$End)/2)
names(bins)[5] <- "log2ratio"
names(bins)[1] <- "chr"
bins$chr <- sub("^chr","",bins$chr)
bins$chr[bins$chr=="X"] <- 23
bins$chr[bins$chr=="Y"] <- 24
bins$chr <- as.integer(bins$chr)
bins <- bins[order(bins$chr, bins$position),]

#segments
segments <- segments[,c(2:4,8)]
colnames(segments)[1] <- "chr"
colnames(segments)[2] <- "posStart"
colnames(segments)[3] <- "posEnd"
colnames(segments)[4] <- "MedianLog2Ratio"
segments$chr <- sub("^chr","",segments$chr)
segments$chr[segments$chr=="X"] <- 23
segments$chr[segments$chr=="Y"] <- 24
segments$chr <- as.integer(segments$chr)
segments <- segments[order(segments$chr, segments$posStart),]

segments$CNV[segments$MedianLog2Ratio <= -0.6] <- "deletion"
segments$CNV[segments$MedianLog2Ratio > -0.6 & segments$MedianLog2Ratio <= -0.1] <- "loss"
segments$CNV[segments$MedianLog2Ratio >= 0.1 & segments$MedianLog2Ratio < 0.6] <- "gain"
segments$CNV[segments$MedianLog2Ratio >= 0.6] <- "amplification"

## new chr position
position.offsets <- cumsum(c(0, head(tapply(bins$position, bins$chr, max), -1))) ## left ends
chrs1 <- sort(unique(bins$chr))
names(position.offsets) <- chrs1
NewPos1 <- bins$position + rep(position.offsets, tapply(bins$position, bins$chr, length))
nchr <- length(chrs1)

segments$x1 <- position.offsets[segments$chr] + segments$posStart
segments$x2 <- position.offsets[segments$chr] + segments$posEnd

segments$width <- segments$posEnd-segments$posStart
position.offsets <- cumsum(c(0, head(tapply(segments$posStart, segments$chr, max), -1))) ## left ends
chrs1 <- sort(unique(segments$chr))
names(position.offsets) <- chrs1
segments$start_new <- segments$posStart + rep(position.offsets, tapply(segments$posStart, segments$chr, length))
segments$end_new <- segments$start_new + segments$width

## colors
cols1 <- rep(NA,length(bins$log2ratio))
if (nchr %% 2 == 0) {
  cols1[bins$chr %in% seq(1, nchr-1,  by=2)] <- "#abb0d4"
  cols1[bins$chr %in% seq(2, nchr, by=2)] <- "#ced0e4"
} else {
  cols1[bins$chr %in% seq(1, nchr,  by=2)] <- "#abb0d4"
  cols1[bins$chr %in% seq(2, nchr-1, by=2)] <- "#ced0e4"
}

## segment colors
cols.seg <- c("blue","lightblue","lightred","red")
names(cols.seg) <- c("deletion","loss","gain","amplification")

data <- data.frame(NewPos1, bins$log2ratio)
lines$chr <- sub("^chr","",lines$chr)
lines$chr[lines$chr=="X"] <- 23
lines$chr[lines$chr=="Y"] <- 24
lines_cumulative <- as.data.frame(c(0, cumsum(as.numeric(lines[,"size"]))))
names(lines_cumulative) <- "chr"
lines_cumulative$pq <- lines_cumulative$chr + lines[,"pq"]
lines_cumulative <- lines_cumulative[1:24,]
lines_cumulative$center <- c(lines$size/2)
lines_cumulative$center <- c(lines_cumulative$chr + lines_cumulative$center)
rownames(lines_cumulative)[23:24] <- c("X","Y")


library(scales)
p <- ggplot(data, aes(x=NewPos1,y=bins$log2ratio)) +
  geom_point(aes(color = bins.log2ratio), size=1, alpha=1) +
  geom_vline(xintercept = lines_cumulative$chr) +
  geom_vline(xintercept = head(lines_cumulative$pq, -1), linetype="dotted") +
  geom_hline(yintercept=0, linetype = "dotted") +
  
  geom_segment(data=segments, aes(x=x1,           # plot horizontal lines (segments)
                                   xend=x2,
                                   y=MedianLog2Ratio,
                                   yend=MedianLog2Ratio), col="black") +

  theme_classic() +
  theme(axis.text.y = element_text(size=9, color="black"),
        axis.text.x = element_text(size=12, color="black"),
        axis.ticks.y = element_line(color="black", size = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(2,"mm"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        legend.text=element_text(size=10),
        legend.title=element_text(size=11),
        legend.key.size = unit(0.5, 'lines'),
        axis.title.x = element_text(size=14, color="black"),
        axis.title.y = element_text(size=14, color="black")) +
  coord_cartesian(ylim = c(-0.8, 0.8), clip = "off") +
  scale_x_continuous(breaks = lines_cumulative$center, labels = rownames(lines_cumulative)) +
  scale_color_gradient2(low="blue", mid="lightgrey", high="red", 
                        limits = c(-0.7, 0.7), oob = scales::squish) +
  labs(x = "Chromosome", y = "log2ratio") +
  theme(legend.position="none")
ggplotly(p) %>% 
   # add_annotations(x = subset(p$data, !is.na(NIH_labels))[[x]],
   #                 y = subset(p$data, !is.na(NIH_labels))[[y]],
   #                 text = subset(p$data, !is.na(NIH_labels))$NIH_labels,
   #                 showarrow = TRUE,
   #                 arrowcolor='red',
   #                 arrowhead = 6,
   #                 arrowsize = 1,
   #                 xref = "x",
   #                 yref = "y",
   #                 font = list(color = 'black',
   #                            family = 'arial',
   #                             size = 14)) %>%
  config(scrollZoom = TRUE) %>%
  layout(dragmode = "pan") %>% 
  toWebGL()

}
