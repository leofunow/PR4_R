if ("quantmod" %in% rownames(installed.packages()) == FALSE) { install.packages("quantmod") }
library(quantmod)
if ("stringr" %in% rownames(installed.packages()) == FALSE) { install.packages("stringr") }
library(stringr)
# Мы хотим загрузить акции с данными наименованиями в yahoo
downloadable_stocks <- c("ATVI", "^IXIC")
# Функция получения фреймов с данными
quantmod::getSymbols(Symbols = downloadable_stocks,
                     src = "yahoo",
                     from = as.Date.character("1900-01-01"))
# Функция get() позволяет получить содержимое объекта по его названию-строке
# Мы можем и не знать названия акций в скрипте, но всё равно работать с ними
# при пользовательском вводе названий
df <- data.frame(get(downloadable_stocks[1]))
# Применяем регулярное выражение для поиска и удаления ненужных символов
downloadable_stocks <- stringr::str_remove(downloadable_stocks,
                                           "[:punct:\\^]")
# Удалим полученные объекты
rm(list = downloadable_stocks)


out_of_trend <- function(x, dt, method = c("Arifm", "Geom", "Garm")) {
  if (length(x) < 3 ||
    dt > ceiling(length(x) / 2) - 1 ||
    is.numeric(x) == FALSE ||
    is.numeric(dt) == FALSE)
    return("error")
  else {
    x <- x + min(x) + 1
    y <- c()
    if (method == "Garm") {
      for (t in (1 + dt):(length(x) - dt))
      {
        y[t - dt] <- log(2 * x[t - dt] * x[t + dt] / (x[t] * (x[t - dt] + x[t + dt])))
      }
    }
    else if (method == "Geom") {
      for (t in (1 + dt):(length(x) - dt))
      {
        y[t - dt] <- log(x[t - dt] * x[t + dt] / (x[t] * x[t]))
      }
    }
    else {
      for (t in (1 + dt):(length(x) - dt))
      {
        y[t - dt] <- log((x[t - dt] + x[t + dt]) / (x[t] * 2))
      }
    }
    return(y)
  }
}

t <- seq(0, 10, 0.1)
x <- 2 * t + 3 + sin(2 * t)
print(mean(x))
xn <- out_of_trend(x, 1)
print(xn)
print(mean(xn))

Alter_Johns <- function(y) {
  a <- c()
  n <- length(y)
  for (t in 0:n - 1) {
    a[t] <- 0
    for (i in 1:(n - t))
      a[t] <- a[t] + abs(y[i + t] - y[i])
    a[t] <- a[t] / (n - t)
  }
  return(a)
}

Alter_Johns(xn)
install.packages("spatialEco")
# Ищем локальные минимумы
library(spatialEco)
local.min.max(xn)$minima
# which.min(Alter_Johns(xn))

tmp <- out_of_trend(df[, 1], 2)
res6 <- Alter_Johns(tmp)
par(mar = c(1, 1, 1, 1))
plot(seq_along(res6), res6)

# ЧАСТЬ 3
A <- cbind(c(4, 0), c(0, 9))
f <- c(4, 2)

SIM <- function(A, u0, f, n_iter = 10e5, eps = 10e-7) {
  A <- A / max(A)
  f <- f / max(f)
  B <- diag(1, 2) - A
  for (i in 1:n_iter) {
    assign(paste0("u",(i) %% 2), B %*% get(paste0("u",(i-1) %% 2)) + f)
    if(abs(u0-u1)<eps)
      return(get(paste0("u",(i) %% 2)))
  }
  return(get(paste0("u",(i) %% 2)))
}

print(SIM(A, c(0.2, -0.3), f))