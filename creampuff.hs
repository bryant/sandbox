{-cream puff case: 2 x 3
large case: 3 x 4, 13.75" x 20"

cream puff: 2.29" x 1.67" = 0.0582 m x 0.0424 m, 1.33 oz = 3.77e-2 kg
-}
k = 0.5 * rho * a * c
    where
    rho = 1.293 -- kg m**-3
    a = 0.0582 * 0.0424  -- m**2
    c = 1

mg = m * 9.81 where m = 0.0377  -- kg

v t = sqrt(mg / k) * tanh(t * sqrt(k / mg))

d t = mg / k * log(cosh(t * sqrt(k / mg)))

--assuming dropped from fifth floor, 3 m per floor:

t_final = arccosh(exp(15 * k / mg)) / sqrt(k / mg)

arccosh t = log(t + sqrt(t + 1) * sqrt(t - 1))

v_final = v t_final
