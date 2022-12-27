package object proyecto {

  import math.pow
  import math.abs

  //Puede tomar cualquier valor real
  type DistributionValues = Vector[Double]

  //Toma valores entre 0 y 1.
  type Frequency = Vector[Double]

  def rhoER(d: (Frequency, DistributionValues)): Double = {
    val K = 10
    val alpha = 1.6
    val a = (for (i <- (0 to d._1.length - 1)) yield for (j <- (0 to d._1.length - 1))
      yield pow(d._1(i), 1 + alpha) * d._1(j) * abs(d._2(i) - d._2(j))).flatten

    a.reduceLeft(_ + _) * K
  }

  type SpecificBeliefConf = Vector[Double]

  type GenericBeliefConf = Int => SpecificBeliefConf

  type Discretization = Vector[Double]


  //Función que a partir de una lista de dobles, crea un vector de intervalos.
  def createIntervals(d_k: Discretization): Vector[(Double, Double)] = {
    d_k match {
      case e +: Vector() => Vector()
      case e +: es => (e, es.head) +: createIntervals(d_k.tail)
      case _ => throw new Error("ERROR")
    }
  }

  //Función que determina en que indice de la lista de intervalos inter, se encuentra el valor doble v.
  def indexInterval(v: Double, inter: Vector[(Double, Double)], i: Int): Int = {
    if (inter.tail == Vector()) i //Si es el último intervalo, la única posibilidad es que esté ahí.
    else {
      if (v >= inter.head._1 && v < inter.head._2) i
      else indexInterval(v, inter.tail, i + 1)
    }

  }


  //Función que construye un vector de enteros, el cual representa las apariciones de los valores de sb en los intervalos de inter.
  def counterIntervals(sb: SpecificBeliefConf, inter: Vector[(Double, Double)], counts: Vector[Int]): Vector[Int] = {
    val indexToUpdate = indexInterval(sb.head, inter, 0)
    if (sb.tail != Vector())
      counterIntervals(sb.tail, inter, counts updated(indexToUpdate, counts(indexToUpdate) + 1))
    else
      counts updated(indexToUpdate, counts(indexToUpdate) + 1)
  }

  def rho(d_k: Discretization, sb: SpecificBeliefConf): Double = {

    val intervals = createIntervals(0.0 +: d_k :+ 1.0) // [0,a), [a, b),..., [y,z),[z,1]

    val counter = counterIntervals(sb, intervals, for (a <- Vector.tabulate(intervals.length)(x => x)) yield 0)

    val cantB = sb.length

    val y = for (a <- Vector.tabulate(counter.length)(x => x) if counter(a) != 0) yield ((intervals(a)._2 - intervals(a)._1) / 2) + intervals(a)._1

    val pi = for (a <- Vector.tabulate(counter.length)(x => x) if counter(a) != 0) yield (counter(a).toDouble / cantB)

    rhoER((pi, y))

  }





}
