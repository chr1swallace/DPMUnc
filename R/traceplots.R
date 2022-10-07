
## calculate_quantiles <- function(datasets, filename, block_size=100) {
##   data_mat = read_numeric(datasets, filename)
##   df = data.frame(data_mat)
##   colnames(df) = paste0("seed", 1:ncol(df))
##   df["block"] = rep(1:(nrow(df) / block_size), each=block_size)
##   df = df%>% tidyr::pivot_longer(cols=1:(ncol(df) - 1))

##   quantiled = df %>%
##     group_by(block, name) %>%
##     summarise(value = quantile(value, c(0.1, 0.25, 0.5, 0.75, 0.9)),
##               quantile = c(0.1, 0.25, 0.5, 0.75, 0.9),
##               count = n())

##   ## write.table(quantiled, "quantiled.csv")
##   return(quantiled)
## }

## median_traceplot <- function(label, quantiled) {

##   cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
##   if (length(unique(quantiled$name)) > 8) {
##       # Using RColorBrewer::brewer.pal(10, "Spectral")
##       cbbPalette <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")
##   }

##   g = ggplot(quantiled, aes(x=block, y=value, colour=name)) +
##     geom_vline(xintercept=max(quantiled["block"])/ 2, colour="red") +
##     scale_color_manual(values=cbbPalette) +
##     labs(y=label) +
##     geom_line(data=subset(quantiled,quantile==0.5), alpha=0.5)

##   return(g)
## }

## quantile_traceplot <- function(label, quantiled) {

##   cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
##   if (length(unique(quantiled$name)) > 8) {
##       # Using RColorBrewer::brewer.pal(10, "Spectral")
##       cbbPalette <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")
##   }

##   g = ggplot(quantiled, aes(x=block, y=value, colour=name)) +
##     geom_vline(xintercept=max(quantiled["block"])/ 2, colour="red") +
##     scale_color_manual(values=cbbPalette) +
##     labs(y=label) +
##     geom_line(data=subset(quantiled,quantile==0.5)) +
##     geom_line(data=subset(quantiled,quantile==0.1), alpha = 0.2, linetype='dashed') +
##     geom_line(data=subset(quantiled,quantile==0.9), alpha = 0.2, linetype='dashed') +
##     geom_line(data=subset(quantiled,quantile==0.25), alpha = 0.2) +
##     geom_line(data=subset(quantiled,quantile==0.75), alpha = 0.2)

##   return(g)
## }
## ##' @param data list of data required for making traceplots, as returned by
## ##'   read_trace_data()
## ##' @param what either "median" - plot only the median of each block_size
## ##'   samples or "quantile" - plot .1, .25, .5, .75, .9 quantiles
## ##' @rdname traceplots
## traceplots=function(data, what=c("median","quantile")) {
##   what=match.arg(what)
##   FUN=if(what=="median") { median_traceplot } else { quantile_traceplot }
##   g_alpha = FUN("alpha", data$quant_alpha)
##   g_K = FUN("K", data$quant_K)
##   g_latent = FUN("pLatents", data$quant_latent)

##   grid.arrange(g_alpha + theme(legend.position = "none"),
##                    g_K + theme(legend.position = "none"),
##                    g_latent + theme(legend.position = "none"),
##                    get_only_legend(g_latent),
##                    top = textGrob(paste0("Median traceplot - "),gp=gpar(fontsize=20,font=3)),
##                    ncol=4,
##                    widths=c(2,2,2,1))
## }
