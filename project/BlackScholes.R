black_n_scholes <- function (s, k, r, t, v) {
call_put <- c(2)
s <- c(90,120,200,300)
#calculo de la distribucion para el call
call <- ((log(s[1]/k)+(r+v^2/2)*t)/(v*sqrt(t)))
#calculo de la distribucion para el put
put <- (call)-v*sqrt(t)
#calculo del valor real del call (en la misma medida de s y k)
call_put[1] <- s[1]*pnorm(call) - k*exp(-r*t)*pnorm(put)
#calculo del valor real del put (en la misma medida de s y k)
call_put[2] <- k*exp(-r*t)*pnorm(-put) - s[1]*pnorm(-call)
print(call_put)
print(call)
#grafica de la distribucion
x <- (seq(0,call,0.025))

plot(x,dlnorm(x),type = 'l',ylim=c(0,1),ylab='Proporcion de cambio')

}

black_n_scholes(3200,100,0.01,3,0.4)
