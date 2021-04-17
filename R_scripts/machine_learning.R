
df <- data.frame(value = c(1, 3, 4, 6, 8, 4,1, 3, 4, 6, 8, 4,1, 3, 4, 6, 8, 4, 
                           140, 150, 200, 300, 160, 220,140, 150, 200, 300, 160, 220,140, 150, 200, 300, 160, 220), 
                 label = c(0, 0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
df <- data.frame(value = c(1, 3, 4, 6, 8, 4,1, 3, 4, 6, 8, 4,1, 3, 4, 6, 8, 4, 
                           4, 5, 20, 30, 16, 22,14, 15, 20, 30, 16, 22,14, 15, 20, 30, 16, 22), 
                 label = c(0, 0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

lg <- glm(factor(label) ~ value, family = binomial(link = "logit"), data = df)
summary(lg)


df <- data.frame(value = c(1, 3, 4, 6, 8, 4,1, 3, 4, 6, 8, 4,1, 3, 4, 6, 8, 4, 
                           7, 8, 10, 14, 15, 13,7, 8, 10, 14, 15, 13,7, 8, 10, 14, 15, 13), 
                 label = c(0, 0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

lg <- glm(factor(label) ~ log(value), family = binomial(link = "logit"), data = df)
summary(lg)

predict(lg, newdata = data.frame(value = c(150, 200, 300, 160, 220,140,200)), type = "response")
predict(lg, newdata = data.frame(value = c(15, 20, 30, 16, 22,14,20)), type = "response")
predict(lg, newdata = data.frame(value = c(5, 2, 3, 1, 2,4,2)), type = "response")
