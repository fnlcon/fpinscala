package fpinscala


package object handlingerrors {
  def variance(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)
      .flatMap(m => Some(xs.map(x => math.pow(x - m, 2))))
      .flatMap(ys => Some(ys.sum / xs.length))

  def lift[A,B](f: A => B): Option[A] => Option[B] =
    _ map f
}

