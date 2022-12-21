package object proyecto {

  import math.pow
  import math.abs

  //Puede tomar cualquier valor real
  type DistributionValues = Vector [ Double ]

  //Toma valores entre 0 y 1.
  type Frequency = Vector [ Double ]

  def rhoER(d:(Frequency,DistributionValues)):Double=
  {
    val K = 10
    val alpha = 1.6
    val a = (for (i <- (0 to d._1.length-1)) yield for (j <- (0 to d._1.length-1))
      yield pow(d._1(i), 1+alpha)*d._1(j)*abs(d._2(i)-d._2(j))).flatten

    a.reduceLeft(_+_)*K
  }

  type SpecificBeliefConf = Vector [ Double ]

  type GenericBeliefConf = Int => SpecificBeliefConf

  type Discretization = Vector[Double]

  /*
  def inI(y:Double, i:Vector[Double]): Vector[Int] =
  {
    if
  }*/

/*
  def rho(d_k: Discretization, sb: SpecificBeliefConf): Double = {
    val limits = 0 +: d_k :+ 1


  }
*/



}
