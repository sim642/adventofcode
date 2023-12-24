from z3 import *

s = Solver()

x, y, z = Reals("x y z")
vx, vy, vz = Reals("vx vy vz")

t0, t1, t2 = Reals("t0 t1 t2")

# s.add(19 + t0 * -2 == x + t0 * vx)
# s.add(13 + t0 * 1 == y + t0 * vy)
# s.add(30 + t0 * -2 == z + t0 * vz)
#
# s.add(18 + t1 * -1 == x + t1 * vx)
# s.add(19 + t1 * -1 == y + t1 * vy)
# s.add(22 + t1 * -2 == z + t1 * vz)
#
# s.add(20 + t2 * -2 == x + t2 * vx)
# s.add(25 + t2 * -2 == y + t2 * vy)
# s.add(34 + t2 * -4 == z + t2 * vz)

s.add(262130794315133 + t0 * 57 == x + t0 * vx)
s.add(305267994111063 + t0 * -252 == y + t0 * vy)
s.add(163273807102793 + t0 * 150 == z + t0 * vz)

s.add(290550702673836 + t1 * -74 == x + t1 * vx)
s.add(186986670515285 + t1 * 19 == y + t1 * vy)
s.add(231769402282435 + t1 * -219 == z + t1 * vz)

s.add(275698513286341 + t2 * -59 == x + t2 * vx)
s.add(162656001312879 + t2 * -24 == y + t2 * vy)
s.add(183065006152383 + t2 * -225 == z + t2 * vz)

print(s.check())

m = s.model()
print(m)

