
#' Volcano
#'
#' @param dataset
#' @param type (p-value adjustment. Either p, xiao, or q)
#' @param threshold (cut-off)
#'
#' @return a volcano plot

volcano <- function (dataset, type, threshold) {

    dataset%>%
        mutate(color = case_when(
            logFC >= 0 & {{type}} <= {{threshold}} ~ "Upregulated",
            logFC <= 0 & {{type}} <= {{threshold}} ~ "Downregulated",
            TRUE ~ "Unchanged")) %>%
        ggplot(aes(x=logFC, y=-log10(P.Value), label=row.names(dataset)))+
        geom_point(aes(color = color, alpha=color), size = 3)+
        theme(panel.background = element_rect(fill = "transparent", colour = NA),
              plot.background = element_rect(fill = "transparent", colour = NA),
              panel.grid.minor=element_blank(),
              panel.grid.major = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.text=element_text(size=10),
              text = element_text(size = 12),
              legend.title = element_blank(),
              legend.key = element_blank(),
              plot.title = element_text(hjust = 0.5, face="bold"),
              axis.text.x = element_text(color="black", size=10),
              axis.text.y = element_text(color="black", size=10),
              legend.position = "none")+
        geom_text_repel(point.size=3, size=3, min.segment.length = Inf, force=0.3)+
        scale_color_manual(breaks = c("Upregulated", "Downregulated", "Unchanged"),
                           values=c("dodgerblue3", "firebrick3", "gray50"))+
        scale_alpha_manual(breaks = c("Upregulated", "Downregulated", "Unchanged"),
                           values=c(1, 1, 0.1))+
        xlab("Log2fold change (post-pre)") + ylab("-log10(p)")
}


#' Volcano for interaction plots
#'
#' @param dataset
#' @param type (p-value adjustment. Either p, xiao, or q)
#' @param threshold (cut-off)
#'
#' @return a volcano plot

volcano_interaction <- function (dataset, type, threshold) {

    dataset%>%
        mutate(color = case_when(
            interaction >= 0 & {{type}} <= {{threshold}} ~ "Upregulated",
            interaction <= 0 & {{type}} <= {{threshold}} ~ "Downregulated",
            TRUE ~ "Unchanged")) %>%
        ggplot(aes(x=interaction, y=-log10(P.Value), label=row.names(dataset)))+
        geom_point(aes(color = color, alpha=color), size = 3)+
        theme(panel.background = element_rect(fill = "transparent", colour = NA),
              plot.background = element_rect(fill = "transparent", colour = NA),
              panel.grid.minor=element_blank(),
              panel.grid.major = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.text=element_text(size=10),
              text = element_text(size = 12),
              legend.title = element_blank(),
              legend.key = element_blank(),
              plot.title = element_text(hjust = 0.5, face="bold"),
              axis.text.x = element_text(color="black", size=10),
              axis.text.y = element_text(color="black", size=10),
              legend.position = "none")+
        geom_text_repel(point.size=4, size=3, min.segment.length = 0.1, force=0.3)+
        scale_color_manual(breaks = c("Upregulated", "Downregulated", "Unchanged"),
                           values=c("dodgerblue3", "firebrick3", "gray50"))+
        scale_alpha_manual(breaks = c("Upregulated", "Downregulated", "Unchanged"),
                           values=c(1, 1, 0.1))+
        xlab("Log2 difference (type II - type I)") + ylab("-log10(p)")
}



#' Volcano with continuous color based on type
#'
#' @param dataset
#' @param type
#' @param threshold
#'
#' @return a volcano plot

volcano_con <- function (dataset, type) {

    xiao_gradient <- colorRampPalette(c((colorRampPalette(c("gray","gray", "gray", "gray","gray", "#F94040"))(50)), rev(colorRampPalette(c("gray","gray","gray","gray","gray", "#5757F9"))(50))))

    dataset%>%
        dplyr::mutate(color = ifelse(logFC>0, {{type}}, {{type}}*-1)) %>%
        ggplot(aes(x=logFC, y=-log10(P.Value), label=row.names(dataset)))+
        geom_point(aes(color = color), size = 3)+
        theme(panel.background = element_rect(fill = "transparent", colour = NA),
              plot.background = element_rect(fill = "transparent", colour = NA),
              panel.grid.minor=element_blank(),
              panel.grid.major = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.text=element_text(size=10),
              text = element_text(size = 12),
              legend.title = element_blank(),
              legend.key = element_blank(),
              plot.title = element_text(hjust = 0.5, face="bold"),
              axis.text.x = element_text(color="black", size=10),
              axis.text.y = element_text(color="black", size=10))+
        geom_text_repel(point.size=4, size=3, min.segment.length = 0.1, force=0.3)+
        xlab("Log2fold change") + ylab("-log10(p)")+
        scale_colour_gradientn(colors=xiao_gradient(100))
}

