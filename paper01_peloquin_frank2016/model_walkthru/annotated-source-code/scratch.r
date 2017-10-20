

dat_post_df <- rsa_matrix_to_df(dat_post, response_colname="pragmatics")

dat_prepost <- full_join(dat_pre_df, dat_post_df, by=c("quantity", "word")) %>% 
  melt(id.vars=c("quantity", "word")) %>% 
  rename(type = variable) %>% 
  mutate(type = as.character(type))

ggplot(dat_prepost, aes(x=quantity, y=value, col=word)) + 
  geom_line() + facet_wrap(~type)




dat_pre_df <- as.data.frame(dat_pre) %>% 
  mutate(quantity = as.numeric(gsub("row", "", rownames(dat_pre)))) %>% 
  melt(id.vars="quantity", variable.name="word") %>% 
  mutate(word = as.character(word)) %>% 
  rename(semantics = "value") %>% 
  arrange(quantity, word)





# d <- rsa_data$pf2
# d %>% select(scale, words, stars, expt1=e6, expt2=e11, speaker.p) %>% 
#   mutate(scale = gsub("ellent", "", scale)) %>% 
#   mutate(words = gsub("ellent", "", words)) %>% head









m
row(m)
(xx <- matrix(c(1,2,3,4,5,6,4,4), ncol=2))
# both same shape, row has idx along cols; col has idx along rows 
row(xx); col(xx) 
# 
split(xx, f=xx[,1])
split(xx, f=xx[,2])

split(m, f=rownames(m))
split(costsAsMatrix, row(costsAsMatrix))


mapply(rep, 1:4, 4:1)
mapply(rep, times = 1:4, x = 4:1)

mapply(rep, 1:4, times=4:1, each=4:1)


mapply(rep, times = 1:4, MoreArgs = list(x = 42))
mapply(rep, times = 1:4, MoreArgs = list(x = 42, each=2))

mapply(function(x, y) seq_len(x) + y,
       c(a =  1, b = 2, c = 3),  # names from first
       10)#c(A = 10, B = 0, C = -10))



word <- function(C, k) paste(rep.int(C, k), collapse = "")
utils::str(mapply(word, LETTERS[1:6], 6:1, SIMPLIFY = FALSE))





### EXAMPLES WITH `outer()` --------
x <- 1:9; names(x) <- x
# Multiplication & Power Tables
x %o% x
y <- 2:8; names(y) <- paste(y,":", sep = "")
outer(y, x, "^") # or * or + 
outer(month.abb, 1999:2003, FUN = "paste")

## three way multiplication table:
x %o% x %o% y[1:3]


### EXAMPLES WITH `inner()` --------
x <- 1:4
(z <- x %*% x)    # scalar ("inner") product (1 x 1 matrix)
?drop(z)             # as scalar

y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
y %*% z
y %*% x
x %*% z



(z <- crossprod(1:4))    # = sum(1 + 2^2 + 3^2 + 4^2)
drop(z)                  # scalar
x <- 1:4; names(x) <- letters[1:4]; x
tcrossprod(as.matrix(x)) # is
identical(tcrossprod(as.matrix(x)),
          crossprod(t(x)))
tcrossprod(x)            # no dimnames

m <- matrix(1:6, 2,3) ; v <- 1:3; v2 <- 2:1
stopifnot(identical(tcrossprod(v, m), v %*% t(m)),
          identical(tcrossprod(v, m), crossprod(v, t(m))),
          identical(crossprod(m, v2), t(m) %*% v2))





# We use rep.int as rep is primitive
vrep <- Vectorize(rep.int)
vrep(1:4, 4:1)
vrep(times = 1:4, x = 4:1)

vrep <- Vectorize(rep.int, "times")
vrep(times = 1:4, x = 42)

f <- function(x = 1:3, y) c(x, y)
vf <- Vectorize(f, SIMPLIFY = FALSE)
f(1:3, 1:3)
vf(1:3, 1:3)
vf(y = 1:3) # Only vectorizes y, not x

# Nonlinear regression contour plot, based on nls() example
require(graphics)
SS <- function(Vm, K, resp, conc) {
  pred <- (Vm * conc)/(K + conc)
  sum((resp - pred)^2 / pred)
}
vSS <- Vectorize(SS, c("Vm", "K"))
Treated <- subset(Puromycin, state == "treated")

Vm <- seq(140, 310, length.out = 50)
K <- seq(0, 0.15, length.out = 40)
SSvals <- outer(Vm, K, vSS, Treated$rate, Treated$conc)
contour(Vm, K, SSvals, levels = (1:10)^2, xlab = "Vm", ylab = "K")

# combn() has an argument named FUN
combnV <- Vectorize(function(x, m, FUNV = NULL) combn(x, m, FUN = FUNV),
                    vectorize.args = c("x", "m"))
combnV(4, 1:4)
combnV(4, 1:4, sum)





