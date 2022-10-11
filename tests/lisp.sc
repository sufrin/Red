import RedLisp.Test._
import RedLisp.Syntax._

run()

run(one, ff)

run(sset, a, Quote(b))

run(a)

run(sset, c, Quote(c))

run(iif, tt, a, b)

run(iif, ff, a, b)

run(iifs, ss(ff, a), b)

run(iifs, ss(ff, a), ss(tt, Quote(v("Woohoo"))) )

run(iifs, ss(c, c), ss(tt, Quote(v("Woohoo"))) )

run(ddef, f, ss(a), a)

run(f, c)

run(f, ss(f, c))

run(sset, g, ss(ffun, ss(a, b), b))

run(g)

run(g, a, c)

run(g, a)

run(sset, e, ss(ffun, ss(a, b), a))

run(llist, a, c, ss(llist, llist))

run(v("toString"), ss(v("'"), v("Heironymous Bosch")))

run(sset, d, ss(v("toString"), ss(v("'"), v("Heironymous Bosch"))))

run(d)

run(llist, a, c, ss(ccons, d, nil))

run(v("isString"), d)

run(v("printenv"), d)


