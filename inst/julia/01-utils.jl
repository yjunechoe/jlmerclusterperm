function z_value(mod)
  coef(mod) ./ stderror(mod)
end

function get_rng_counter()
  Int(rng.ctr1)
end
