#' Plot Heatmap
#'
#' This function plots a Heapmap of a datamatrix where the first column are the genesymbols
#'
#' @param data A dataset where only the first column is character (where to put your GeneSymbols)
#' @param title A short name to put in the title after "Heatmap of "
#' @param scale Does the data need to be scaled? Values= "column"/"row"/"none"
#' @param cexTitle Size of the title
#' @export HeatMapUEB
#' @import gplots
#' @examples
#' HeatMapUEB(data=hmdata,"Normalized Data",0.8,scale="column")

HeatMapUEB <- function (data, title, cexTitle, scale) {
  rnames <- data[,1]
  #Ho transforment en una matrix
  mat.Data<-data.matrix(data[,-1])
  #afegim els rownames
  rownames(mat.Data) <- rnames
  #parámetre dels colors baixa expressió "blue" i alta "red"
  my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 299)
  par(cex.main=cexTitle) #tamany del títol
  heatmap.2(na.omit(mat.Data),
            main = paste0("Heatmap of ",title),
            key = TRUE,
            scale=scale,
            na.rm = TRUE,
            cexRow = 0.3 ,
            cexCol = 0.5 ,
            notecol="black",      # change font color of cell labels to black
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            margins =c(12,9),     # widens margins around plot
            col=my_palette,       # use on color palette defined earlier
            Rowv=TRUE)
}