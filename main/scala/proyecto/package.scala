import org.apache.commons.math3.stat.Frequency

package object proyecto {

  import scala.collection.parallel.immutable.ParVector
  import scala.collection.parallel.CollectionConverters._
  import common._
  import math.pow
  import math.abs




  //---------------------------------- Ejercicio 2.1 Medida de Esteban & Ray ------------------------------------//

  //Ejercicio 2.1.1

  //Puede tomar cualquier valor real
  type DistributionValues = Vector[Double]

  //Toma valores entre 0 y 1.
  type Frequency = Vector[Double]

  def rhoER(d: (Frequency, DistributionValues)): Double = {
    val K = 10
    val alpha = 1.6
    val a = {
      for {
        i <- 0 to d._1.length - 1
        j <- 0 to d._1.length - 1
      } yield pow(d._1(i), 1 + alpha) * d._1(j) * abs(d._2(i) - d._2(j))
    }
    a.sum * K
  }

  //------------------------------ Ejercicio 2.2 Elementos Estaticos del Modelo ----------------------------------//

  //Ejercicio 2.2.1
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
    if (sb.isEmpty == true) counts
    else {
      val indexToUpdate = indexInterval(sb.head, inter, 0)
      if (sb.tail != Vector())
        counterIntervals(sb.tail, inter, counts updated(indexToUpdate, counts(indexToUpdate) + 1))
      else
        counts updated(indexToUpdate, counts(indexToUpdate) + 1)
    }
  }

  def rho(d_k: Discretization, sb: SpecificBeliefConf): Double = {

    val intervals = createIntervals(0.0 +: d_k :+ 1.0) // [0,a), [a, b),..., [y,z),[z,1]

    val counter = counterIntervals(sb, intervals, for (a <- Vector.tabulate(intervals.length)(x => x)) yield 0)

    val cantB = sb.length

    val y = for (a <- Vector.tabulate(counter.length)(x => x) if counter(a) != 0) yield ((intervals(a)._2 - intervals(a)._1) / 2) + intervals(a)._1

    val pi = for (a <- Vector.tabulate(counter.length)(x => x) if counter(a) != 0) yield (counter(a).toDouble / cantB)

    rhoER((pi, y))

  }

  //------------------------------ Ejercicio 2.3 Elementos Dinamicos del Modelo ----------------------------------//

  //Ejercicio 2.3.1
  type WeightedGraph = (Int, Int) => Double
  type SpecificWeightedGraph = (WeightedGraph , Int )
  type GenericWeightedGraph = Int => SpecificWeightedGraph

  def showWeightGraph(swg : SpecificWeightedGraph):IndexedSeq[IndexedSeq[Double]]=  {
    val (a, b) = swg
    for {
    i <- (1 to b)
  } yield (for {
    j <- (1 to b)
  } yield a(i, j) )

  }

  //Ejercicio 2.3.2
  def confBiasUpdate(b:SpecificBeliefConf, swg: SpecificWeightedGraph):SpecificBeliefConf ={
    val CB = for(i <- 0 to b.length-1) yield {
      val A_i = for(j <- 0 to b.length-1 if swg._1(j,i) > 0) yield j
      val nb = for(j <- 0 to A_i.length - 1) yield (1 - (b(j)-b(i)).abs) * swg._1(j,i) * (b(j) - b(i))
      b(i) + nb.sum / A_i.length
    }
    CB.toVector
  }

  //Ejercicio 2.3.3
  type FunctionUpdate = (SpecificBeliefConf,SpecificWeightedGraph) => SpecificBeliefConf
  def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph, b0: SpecificBeliefConf, t: Int): IndexedSeq[SpecificBeliefConf] = {
    if(t == 1) IndexedSeq(fu(b0,swg))
    else {
      val newB = fu(b0,swg)
      newB +: simulate(fu,swg,newB,t-1)
    }
  }

  //------------------------ Ejercicio 2.4 Acelerando la simulacion con Paralelizacion -----------------------------//

  type DistributionValuesPar = ParVector[Double]
  type FrequencyPar = ParVector[Double]
  type DistributionPar = (FrequencyPar,DistributionValuesPar)

  //Ejercicio 3.4.1
  def rhoERPar(d: (FrequencyPar, DistributionValuesPar)): Double = {
    val K = 10
    val alpha = 1.6
    val a = {
      for {
        i <- 0 to d._1.length - 1
        j <- 0 to d._1.length - 1
      } yield pow(d._1(i), 1 + alpha) * d._1(j) * abs(d._2(i) - d._2(j))
    }
    a.sum * K
  }

  //Ejercicio 2.4.2
  def rhoPar(d_k: Discretization, sb: SpecificBeliefConf): Double = {

    val intervals = createIntervals(0.0 +: d_k :+ 1.0) // [0,a), [a, b),..., [y,z),[z,1]

    val result1 = parallel(
      counterIntervals(sb take (sb.length.toFloat / 2).ceil.toInt, intervals, for (a <- Vector.tabulate(intervals.length)(x => x)) yield 0),
      counterIntervals(sb drop (sb.length.toFloat / 2).ceil.toInt, intervals, for (a <- Vector.tabulate(intervals.length)(x => x)) yield 0))

    val counter = (for (i <- Vector.tabulate(result1._2.length)(x => x)) yield result1._1(i) + result1._2(i)) ++ (if (result1._1.length != result1._2.length) Vector(result1._1.last) else Vector())

    val cantB = sb.length

    val result2 = parallel(for (a <- Vector.tabulate(counter.length)(x => x) if counter(a) != 0) yield ((intervals(a)._2 - intervals(a)._1) / 2) + intervals(a)._1, for (a <- Vector.tabulate(counter.length)(x => x) if counter(a) != 0) yield (counter(a).toDouble / cantB))

    rhoERPar((result2._2.par, result2._1.par))

  }

  //Ejercicioo 2.4.3
  def confBiasUpdatePar(b: SpecificBeliefConf, swg: SpecificWeightedGraph): SpecificBeliefConf = {
    val CB = for (i <- 0 to b.length - 1) yield {
      val A_i = {
        if(i<20)
        {
          for (j <- 0 to b.length - 1 if swg._1(j, i) > 0) yield j
        }
        else
        {
          val b1 = b take (b.length/2)
          val b2 = b drop (b.length/2)

          lazy val primerA_i= for (j <- 0 to b1.length - 1 if swg._1(j, i) > 0) yield j
          lazy val segundoA_i = for (j <- b1.length to b2.length + b1.length - 1 if swg._1(j, i) > 0) yield j

          val parA_i = parallel(primerA_i,segundoA_i)

          parA_i._1++parA_i._2
        }

      }
      val nb = for (j <- 0 to A_i.length - 1) yield (1 - (b(j) - b(i)).abs) * swg._1(j, i) * (b(j) - b(i))
      b(i) + nb.sum / A_i.length
    }
    CB.toVector
  }


}
