function (n, alpha = 1) 
{
  if ((n <- as.integer(n[1L])) > 0L) {
    even.n <- n%%2L == 0L
    k <- n%/%2L
    l1 <- k + 1L - even.n
    l2 <- n - k + even.n
    c(if (l1 > 0L) hsv(h = 6/12, s = seq.int(0.5, ifelse(even.n, 
      0.5/k, 0), length.out = l1), v = 1, alpha = alpha), 
      if (l2 > 1) hsv(h = 10/12, s = seq.int(0, 0.5, length.out = l2)[-1L], 
        v = 1, alpha = alpha))
  }
  else character()
}
