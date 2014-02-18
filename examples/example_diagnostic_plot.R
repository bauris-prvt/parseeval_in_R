require(lattice)
require(latticeExtra)
require(gridExtra)
# bw theme for publication
#ltheme <- canonical.theme(color = FALSE) ## in-built B&W theme 
#ltheme$strip.background$col <- "transparent" ## change strip bg 
#lattice.options(default.theme = ltheme) ## set as default
p1_s<-xyplot((parse_s*100)~as.factor(anchorindex), groups=edge, type="o", pch=20, cex=0.4, lwd=.5, main="(a.1)", xlab="Anchorindex", ylab="Median RSD (%)", scales=list(x=list(cex=.48)), auto.key=list(space="top", text=c("CC to anchor", "LE to anchor", "RE to anchor"), points=F, lines=T, columns=2, cex=.7, padding.text=2), data=parse1$Plot_1)
p1_c<-xyplot((parse_c*100)~as.factor(anchorindex), groups=edge, type="o", pch=20, cex=0.4, lwd=.5, main="(b.1)",xlab="Anchorindex", ylab="Median RSD (%)", scales=list(x=list(cex=.48)), auto.key=list(space="top", text=c("CC to anchor", "LE to anchor", "RE to anchor"), points=F, lines=T, columns=2, cex=.7, padding.text=2), data=parse1$Plot_1)
p2_s<-xyplot(parse_s~as.factor(anchorindex), type="o", pch=20, lty=1, cex=0.5, main="(a.2)", xlab="Anchorindex", ylab="Hits (F-Statistics > 98.503)", scales=list(x=list(cex=.48)), data=parse1$Plot_2)
p2_c<-xyplot(parse_c~as.factor(anchorindex), type="o", pch=20, lty=1, cex=0.5, main="(b.2)", xlab="Anchorindex", ylab="Hits (F-Statistics > 98.503)", scales=list(x=list(cex=.48)), data=parse1$Plot_2)
rs.3<-xyplot(rs_median~as.factor(anchorindex),
             panel=function(...) {
               panel.xyplot(...)
               panel.abline(h = 1.0, lty = 2, lwd = .5)
             },
             type="o", pch=20, cex=0.4, lwd=.5, subset=parse=="simp", main="(a.3)", xlab="Anchorindex", ylab=expression("Median"~R^2), scales=list(x=list(cex=.48)), data=parse1$Plot_3$median)
rs.4<-xyplot(rs_median~as.factor(anchorindex),
             panel=function(...) {
               panel.xyplot(...)
               panel.abline(h = 1.0, lty = 2, lwd = .5)
             },
             type="o", pch=20, cex=0.4, lwd=.5, subset=parse=="comp", main="(b.3)", xlab="Anchorindex", ylab=expression("Median"~R^2), scales=list(x=list(cex=.48)), data=parse1$Plot_3$median)
# open pdf-device simplex diagnostics
pdf(file="diag_simple.pdf", paper="a4", width=0, height=0)
grid.arrange(p1_s, p2_s, rs.3, as.table=FALSE, ncol=1)
dev.off()
# open pdf-device complex diagnostics
pdf(file="diag_complex.pdf", paper="a4", width=0, height=0)
grid.arrange(p1_c, p2_c, rs.4, as.table=FALSE, ncol=1)
dev.off()
# open pdf-device both diagnostics
pdf(file="diag_allh.pdf", paper="a4r", width=0, height=0)
grid.arrange(p1_s, p2_s, rs.3, p1_c, p2_c, rs.4, as.table=FALSE, ncol=3)
dev.off()
# open pdf-device both diagnostics
pdf(file="diag_allv.pdf", paper="a4", width=0, height=0)
grid.arrange(p1_s, p1_c, p2_s, p2_c, rs.3, rs.4, as.table=FALSE, ncol=2)
dev.off()
rm(p1_c,p1_s,p2_c,p2_s,rs.3,rs.4)
