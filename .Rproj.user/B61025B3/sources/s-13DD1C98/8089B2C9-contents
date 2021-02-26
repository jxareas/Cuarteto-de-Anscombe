require(ggplot2)
require(datasets)
require(dplyr)

get_anscombe_plots <- function()
{
        q <- datasets::anscombe
        l <- list()
        p <- list()
        
        for(i in 1:4)
                l[[i]] <- q[, c(paste0("x", i), paste0("y", i))]
        
        for(k in 1:4)
        {
                p[[k]] <- local({
                        k <- k
                        ggplot(data = l[[k]], mapping = aes(x = unlist(l[[k]][1], use.names = F), y = unlist(l[[k]][2], use.names = F), col = unlist(l[[k]][1], use.names = F))) +
                                geom_point() +
                                geom_abline(slope = 1, col = "red") +
                                geom_vline(xintercept = mean(unlist(l[[k]][1]), na.rm = TRUE), col = "red") +
                                geom_hline(yintercept = mean(unlist(l[[k]][2]), na.rm = TRUE), col = "darkred") +
                                labs(
                                        title = paste("Anscombe", k),
                                        x = "Eje X",
                                        y = "Eje Y",
                                        caption = "Cuarteto de Anscombe",
                                        color = "valor"
                                ) + theme_minimal()
                })
        }
        
        return(p);
        
        
}