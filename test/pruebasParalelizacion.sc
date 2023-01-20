import org.scalameter.measure
import proyecto._

import org.scalameter._
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParVector

val standardConfig = config(
  Key.exec.minWarmupRuns := 20,
  Key.exec.maxWarmupRuns := 40,
  Key.exec.benchRuns := 25,
  Key.verbose := false
)withWarmer(Warmer.Default())

// función que genera una distribución con frecuencias aleatorias (entre 0 y 1) y decisiones representadas (0,...,n-1)

def distribucion (n:Int):(Vector[Double],Vector[Double]) =
{
  val frecuencias = for(i<- 0 until n) yield Math.random()
  val decisiones = for(i<- 0 until n) yield i.toDouble
  (frecuencias.toVector,decisiones.toVector)
}

//------------------------------------------------- FUNCION rhoER-------------------------------------------------

/*
for(i <- 1 to 5) yield {
  val d = distribucion(math.pow(10,i).toInt)
  val dPar = (d._1.par,d._2.par)

  val s = standardConfig measure {rhoER(d)}
  val p = standardConfig measure {rhoERPar(dPar)}
  val a = s.value / p.value

  (math.pow(10,i).toInt,s,p,a)
}*/

//------------------------------------------------- FUNCION rho ---------------------------------------------------

def b1(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i: Int) => {if (i <= nags / 2) 0.6 else 0.4})
}

def b2(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i:Int) => 0)
}

def b3(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i: Int) => math.random())
}

val d1=Vector ( 0.2 , 0.4 , 0.6 , 0.8 )
val d2 = Vector(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
val d3 = (for(i <- 1 to 98 by 2) yield i/100.toDouble).toVector
val d4 = (for(i <- 1 to 99) yield i/100.toDouble).toVector

val b_nags = for(i <- 1 to 6) yield b3(math.pow(10,i).toInt)

/*
val rhoTest = for(i <- 1 to 6) yield {
  val s = standardConfig measure{rho(d4,b_nags(i-1))}
  val p = standardConfig measure{rhoPar(d4,b_nags(i-1))}
  val a = s.value/p.value
  (b_nags(i-1).length,s,p,a)
}
 */

//---------------------------------------------- FUNCION confBiasUpdate ----------------------------------------------


def i1(nags: Int): SpecificWeightedGraph = {
  ((i: Int, j: Int) => if (i == j) 1.0
  else if (i < j) 1.0 / (j - i).toDouble
  else 0.0, nags)
}
def i2(nags: Int): SpecificWeightedGraph = {
  ((i: Int, j: Int) => if (i == j) 1.0
  else if (i < j) (j - i).toDouble / nags.toDouble
  else (nags - (i - j)).toDouble / nags.toDouble, nags)
}

val i_nags = for(i <- 1 to 5) yield i2(math.pow(10,i).toInt)

/*
val confBiasTest = for(i <- 1 to 5) yield {
  val s = standardConfig measure{confBiasUpdate(b_nags(i-1),i_nags(i-1))}
  val p = standardConfig measure{confBiasUpdatePar(b_nags(i-1),i_nags(i-1))}
  val a = s.value/p.value
  (b_nags(i-1).length,s,p,a)
}*/

//----------------------------- Verificando aceleración final en simulate ------------------------------------

val n = 3 // Representa math.pow(10, n+1) agentes, 0 <= n <= 3, mas de 10000 tarda en dar respuesta

val s1 = standardConfig measure {for(b <- simulate(confBiasUpdate,i_nags(n),b_nags(n),2)) yield rho(d2,b)}
val p1 = standardConfig measure {for(b <- simulate(confBiasUpdatePar,i_nags(n),b_nags(n),2)) yield rhoPar(d2,b)}
val a1 = s1.value/p1.value
(b_nags(n).length,s1,p1,a1)

val s2 = standardConfig measure {for(b <- simulate(confBiasUpdate,i_nags(n),b_nags(n),10)) yield rho(d2,b)}
val p2 = standardConfig measure {for(b <- simulate(confBiasUpdatePar,i_nags(n),b_nags(n),10)) yield rhoPar(d2,b)}
val a2 = s2.value/p2.value
(b_nags(n).length,s2,p2,a2)

val s3 = standardConfig measure {for(b <- simulate(confBiasUpdate,i_nags(n),b_nags(n),50)) yield rho(d2,b)}
val p3 = standardConfig measure {for(b <- simulate(confBiasUpdatePar,i_nags(n),b_nags(n),50)) yield rhoPar(d2,b)}
val a3 = s3.value/p3.value
(b_nags(n).length,s3,p3,a3)

val s4 = standardConfig measure {for(b <- simulate(confBiasUpdate,i_nags(n),b_nags(n),100)) yield rho(d2,b)}
val p4 = standardConfig measure {for(b <- simulate(confBiasUpdatePar,i_nags(n),b_nags(n),100)) yield rhoPar(d2,b)}
val a4 = s4.value/p4.value
(b_nags(n).length,s4,p4,a4)

val s5 = standardConfig measure {for(b <- simulate(confBiasUpdate,i_nags(n),b_nags(n),500)) yield rho(d2,b)}
val p5 = standardConfig measure {for(b <- simulate(confBiasUpdatePar,i_nags(n),b_nags(n),500)) yield rhoPar(d2,b)}
val a5 = s5.value/p5.value
(b_nags(n).length,s5,p5,a5)
