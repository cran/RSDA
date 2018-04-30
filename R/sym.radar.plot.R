#' Internal sym.radar.plot the distence between two rows
#' @keywords internal
sym.radar.plot. <- function(dat, indivs, vars, dat.min, dat.max, rad.main, seg = 3,
    use.legend = T) {
    # Extra la parte de datos de la estructura simbólica de RSDA
    dat <- dat$data
    if (!missing(indivs)) {
        dat <- dat[indivs, ]
    }
    if (!missing(vars)) {
        if (length(vars) <= 2)
            stop("You must specify 3 or more variables in vars")
        vars <- sort(c(2 * vars - 1, 2 * vars))
        dat <- dat[, vars]
    }
    if (missing(dat.max)) {
        dat.max = apply(dat[, seq(2, ncol(dat), 2)], 2, max)
    }
    if (missing(dat.min)) {
        dat.min = apply(dat[, seq(1, ncol(dat), 2)], 2, min)
    }
    # Cantidad de columnas (2 por variable)
    n <- ncol(dat)
    # Cantidad de variables (la mitad del total de columnas)
    m <- n/2
    # Cantidad de observaciones
    k <- nrow(dat)
    datos.L <- dat[, seq(1, n, 2)]
    datos.R <- dat[, seq(2, n, 2)]
    colnames(datos.R) <- colnames(datos.L)
    legend.names <- row.names(dat)
    temp <- rbind(datos.L, datos.R)
    dat <- temp[order(row.names(temp)), ]
    rm(list = c(c("temp"), c("datos.L"), c("datos.R")))
    # Crea la paleta de colores, tomando 1 color para cada individuo
    colors <- rainbow(2 * k)
    # Grafica el sistema coordenado
    plot(c(-1.4, 1.4), c(-1.4, 1.4), type = "n", frame.plot = FALSE, axes = F, xlab = "",
        ylab = "", main = rad.main, asp = 1)
    if (use.legend)
        legend("topright", legend.names, lty = 1, col = colors[seq(1, 2 * k - 1, 2)],
            bty = "n", cex = 0.75)
    # Posiciones de los vértices al rededor de un círculo unitario expresados en
    # radianes Esto se utiliza para dibujar la telaraña central
    theta <- seq(90, 360 + 90, length = m + 1) * pi/180
    theta <- theta[1:m]
    xx <- cos(theta)
    yy <- sin(theta)
    # Genera un pequeño desplazamiento para que se vean mejor los ejes
    xx.s <- xx + cos(theta) * 0.05
    yy.s <- yy + sin(theta) * 0.05
    # Genera otro desplazamiento para las etiquetas
    xx.s.l <- xx + cos(theta) * 0.2
    yy.s.l <- yy + sin(theta) * 0.2
    CGap <- 1
    # Genera el interior (aspecto de telaraña) del gráfico de radar
    for (i in 0:seg) {
        polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + CGap), lty = 1,
            lwd = 1, border = c("black"))
    }
    text(xx * (CGap)/(seg + CGap), yy * (CGap)/(seg + CGap), dat.min, col = c("grey"),
        cex = 0.8)
    # Dibuja los ejes de cada una de las variables
    arrows(rep(0, length(xx.s)), rep(0, length(yy.s)), xx.s, yy.s, lwd = 1, lty = 1,
        length = 0, col = c("black"))
    # Coloca los máximos de cada variable
    text(xx.s + cos(theta) * 0.07, yy.s + sin(theta) * 0.07, dat.max, col = c("grey"),
        cex = 0.8)
    # Coloca el nombre de cada variable
    axis.labs <- names(dat)
    text(xx * 1.4, yy * 1.4, axis.labs, cex = 0.8)
    ############### GRAFICA CADA INDIVIDUO ############### Para cada observación (fila)
    alpha.x <- 0.05
    for (i in seq(1, 2 * k - 1, 2)) {
        # Encuentra el factor de escala la i-ésima observación
        scale.int <- CGap/(seg + CGap) + (dat[i, ] - dat.min)/(dat.max - dat.min) *
            seg/(seg + CGap)
        # Aplica la escala a las coordenadas
        xx.int <- xx * scale.int
        yy.int <- yy * scale.int
        # Encuentra el factor de escala la i-ésima observación
        scale.ext <- CGap/(seg + CGap) + (dat[i + 1, ] - dat.min)/(dat.max - dat.min) *
            seg/(seg + CGap)
        # Aplica la escala a las coordenadas
        xx.ext <- xx * scale.ext
        yy.ext <- yy * scale.ext
        poly.coords <- data.frame(x = as.numeric(c(xx.ext, NA, xx.int)), y = as.numeric(c(yy.ext,
            NA, yy.int)))
        polypath(poly.coords[, 1], poly.coords[, 2], col = adjustcolor(colors[i], alpha.f = 0.8 -
            alpha.x), rule = "evenodd")
        # Coloca los puntos de los vértices internos
        points(xx.int, yy.int, pch = 20, col = c("black"))
        # Coloca los puntos de los vértices externos
        points(xx.ext, yy.ext, pch = 20, col = c("black"))
        alpha.x <- alpha.x + 0.1
    }
}


#' Radar Plot For Symbolic Interval Variables
#'
#' @param x A symbolic object.
#' @param dat.min <documentar>
#' @param dat.max <documentar>
#' @param rad.main <documentar>
#' @param seg <documentar>
#' @param use.legend a logical value indicating whether use a legend.
#' @param main the main title (on top).
#'
#' @return A radar plot.
#' @export
#'
sym.radar.plot <- function(x, dat.min, dat.max, rad.main, seg = 3, use.legend = T,
    main = "") {
    sym.radar.plot.(dat = x, use.legend = use.legend, rad.main = main)
}
