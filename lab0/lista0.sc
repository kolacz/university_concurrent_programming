// Andrzej Kołacz 280009
// lista 0, zadanie 1

import scala.annotation.tailrec

val epsilon = 10E-35

def root3(a :Double): Double =
{
  @tailrec
  def root3aux(acc :Double): Double =
  {
    if (math.abs(acc*acc*acc - a) > epsilon * math.abs(a))
      root3aux(acc+(a/(acc*acc) - acc)/3)
    else
      acc
  }
  root3aux(if (a > 1) a/3 else a)
}

math.abs(root3(8.0) - 2.0)   < 10E-70
math.abs(root3(-512) - (-8)) < 10E-70
math.abs(root3(0)) == 0