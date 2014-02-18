set . seed (2954) # make examples reproducible
parse1 <- parseEval(c1 = c(45 ,14), c2 = c(45 ,14), c3 = c(45 ,14), cipi12 = c(0 ,12),
                    cipi23 = c (0 ,12) , voweld =230 , var_anchor = 5 ,types = 3 ,words = 30,
                    RE_rsd = 0.230, CC_rsd =.080, LE_rsd =.140)

require ( lattice , quietly = T )
require ( latticeExtra , quietly = T )
require ( gridExtra , quietly = T )
p1_s <- xyplot((parse_s * 100) ~ as.factor(anchorindex), groups = edge, type = "o", pch =20,
               cex = 0.4, lwd = .5, main = "(a.1)", xlab = "Anchorindex" , ylab = "Median RSD (%)",
               scales = list(x = list(cex = .48)),
               auto.key = list(space = "top", text = c("CC to anchor", "LE to anchor", "RE to anchor"),
                               points =F, lines =T, columns =2, cex =.7, padding.text =2),
               data = parse1$Plot_1)

p1_c <- xyplot ((parse_c * 100) ~ as.factor(anchorindex), groups = edge, type = "o", pch =20,
                cex =0.4, lwd =.5, main = "(b.1)", xlab = "Anchorindex" , ylab = "Median RSD (%)",
                scales = list(x = list(cex =.48)),
                auto.key = list(space = "top", text = c("CC to anchor", "LE to anchor", "RE to anchor"),
                                points =F, lines =T, columns =2, cex =.7, padding.text =2),
                data = parse1$Plot_1)

p2_s <- xyplot(parse_s ~ as.factor(anchorindex), type = "o", pch =20, lty =1, cex =0.5,
               main = "(a.2)", xlab = "Anchorindex", ylab = "Hits (F-Statistics > 98.503)",
               scales = list(x = list(cex =.48)), data = parse1$Plot_2)

p2_c <- xyplot(parse_c ~ as.factor(anchorindex), type = "o", pch =20, lty =1, cex =0.5,
               main = "(b.2)", xlab = "Anchorindex", ylab = "Hits (F-Statistics > 98.503)",
               scales = list(x = list(cex =.48)), data = parse1$Plot_2)

rs_3 <- xyplot(rs_median ~ as.factor(anchorindex),
               panel = function(...) {
                 panel.xyplot (...)
                 panel.abline(h = 1.0, lty = 2, lwd = .5)
                 },
               type = "o", pch =20, cex =0.4, lwd =.5, subset = parse == "simp", main = "(a.3)",
               xlab = "Anchorindex", ylab = expression("Median" ~ R^2),
               scales = list(x = list(cex =.48)), data = parse1$Plot_3$median)

rs_4 <- xyplot(rs_median ~ as.factor(anchorindex),
               panel = function(...) {
                 panel.xyplot(...)
                 panel.abline(h = 1.0, lty = 2, lwd = .5)
                 },
               type = "o", pch =20, cex =0.4, lwd =.5, subset = parse == "comp", main = "(b.3)",
               xlab = "Anchorindex" , ylab = expression("Median" ~ R^2),
               scales = list(x = list(cex =.48)), data = parse1$Plot_3$median)

# open pdf - device simplex diagnostics
pdf(file = "diag_simple.pdf", paper = "a4", width =0 , height =0)
grid.arrange(p1_s, p2_s, rs_3, as.table = FALSE, ncol =1)
dev.off ()
# open pdf - device complex diagnostics
pdf(file = "diag_complex.pdf" , paper = "a4", width =0 , height =0)
grid.arrange(p1_c, p2_c, rs_4, as.table = FALSE, ncol =1)
dev.off ()
# open pdf - device both diagnostics
pdf(file = "diag_allh.pdf", paper = "a4r", width =0 , height =0)
grid.arrange(p1_s, p2_s, rs_3, p1_c, p2_c, rs_4, as.table = FALSE, ncol =3)
dev.off()
# open pdf - device both diagnostics
pdf(file = "diag_allv.pdf", paper = "a4", width =0 , height =0)
grid.arrange(p1_s, p1_c, p2_s, p2_c, rs_3, rs_4, as.table = FALSE, ncol =2)
dev.off()
rm(p1_s, p1_c, p2_s, p2_c, rs_3, rs_4, parse1)