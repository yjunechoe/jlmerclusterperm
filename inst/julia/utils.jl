Term(:a) + Term(:b)
@formula(y ~ 1 + a)

a = :aa
b = :bb
c = :xx
FormulaTerm(Term(a), Tuple(Term.([b, c])))
