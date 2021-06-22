require(data.table)
library(config)
path_prefix=config::get("path_prefix")

#amplifications and deletions (ALL)
CNA_all <- fread(paste0(path_prefix,"Del_amp_conumee_bins_neuro_baselinecorrection_preprocessRaw_XYincl_sub30_GMAF1p_minwidth3.txt"),
                 stringsAsFactors = FALSE, check.names = FALSE, na.strings = "")
CNA_deleterious <- CNA_all[rowSums(is.na(CNA_all[,7:13]))!=7,]

#breakpoints
CNA_breakpoint_fusions <- fread(paste0(path_prefix,"Breakpoints_conumee_neuro_baselinecorrection_preprocessIRaw_XYincl_sub30_GMAF1p_minwidth3_10kb_possiblefusions.txt"),
                                stringsAsFactors = FALSE, check.names = FALSE)
names(CNA_breakpoint_fusions) <- c("sample","matches")
CNA_breakpoint_fusions$matches <- gsub("\\.", "-", CNA_breakpoint_fusions$matches)
CNA_breakpoint_fusions <- CNA_breakpoint_fusions[!CNA_breakpoint_fusions$matches=="",]

CNA_breakpoint_genes <- fread(paste0(path_prefix,"./Conumee_baselinecorrection_preprocessRaw_XYincl_sub30_GMAF1p_minwidth3_breakpoint_genes_10kb.txt"))
CNA_breakpoint_genes <- CNA_breakpoint_genes[!CNA_breakpoint_genes$gene=="",]
