import proyecto._
import proyecto.SpecificBeliefConf
import scala.collection.parallel.immutable._
import scala.collection.immutable._
import scala.collection.parallel.CollectionConverters._

def genDist (n:Int):(Vector[Double],Vector[Double]) =
{
  val frecuencias = for(i<- 0 until n) yield Math.random()
  val decisiones = for(i<- 0 until n) yield i.toDouble
  (frecuencias.toVector,decisiones.toVector)
}

//----------------------------- FUNCION rhoER y rhoERPar ---------------------------------------

val d1 = genDist(10)
val d2 = genDist(100)
val d3 = genDist(1000)
val d4 = genDist(5000)
val d5 = genDist(10000)

val dPar1 = (d1._1.par,d1._2.par)
val dPar2 = (d2._1.par,d2._2.par)
val dPar3 = (d3._1.par,d3._2.par)
val dPar4 = (d4._1.par,d4._2.par)
val dPar5 = (d5._1.par,d5._2.par)

rhoER(d1)
rhoER(d2)
rhoER(d3)
rhoER(d4)
rhoER(d5)

rhoERPar(dPar1)
rhoERPar(dPar2)
rhoERPar(dPar3)
rhoERPar(dPar4)
rhoERPar(dPar5)

//-------------------------------- FUNCION rho y rhoPar ----------------------------

// Funcion para generar creencias con valores aleatorios
def genRandSBC(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i: Int) => math.random())
}

//Funcion para generar creencias con valores extremos
def gen1_0SBC(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i: Int) => if(math.random() > 0.5) 0 else 1)
}

val bR_100 = genRandSBC(100)
val bR_1000 = genRandSBC(1000)
val bR_10000 = genRandSBC(10000)
val bE_10000 = gen1_0SBC(10000)
val bR_100000 = genRandSBC(100000)
val bE_1M = gen1_0SBC(1000000)

val d1=Vector ( 0.2 , 0.4 , 0.6 , 0.8 )
val d2 = Vector(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
val d3 = (for(i <- 1 to 98 by 2) yield i/100.toDouble).toVector
val d4 = (for(i <- 1 to 99) yield i/100.toDouble).toVector

rho(d1,bR_1000)
rho(d1,bR_1000)
rho(d2,bR_1000)
rho(d2,bE_10000)
rho(d3,bR_100000)
rho(d4,bE_1M)

rhoPar(d1,bR_100)
rhoPar(d1,bR_1000)
rhoPar(d2,bR_1000)
rhoPar(d2,bE_10000)
rhoPar(d3,bR_100000)
rhoPar(d4,bE_1M)

//------------------- FUNCION confBiasUpdate y confBiasUpdatePar -----------------------------

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

def i3(nags: Int) : SpecificWeightedGraph = {
  ((i:Int, j:Int) => if(i == j) 1.0
  else if (i<j) (i * 2)/nags.toDouble
  else (j*2)/nags.toDouble, nags)
}

val i1_100 = i1(100)
val i1_1000 = i1(1000)
val i1_10000 = i1(10000)

val i2_100 = i2(100)
val i2_1000 = i2(1000)
val i2_10000 = i2(10000)

val i3_100 = i3(100)
val i3_1000 = i3(1000)
val i3_10000 = i3(10000)

confBiasUpdate(bR_100,i1_1000)
confBiasUpdate(bR_100,i1_1000)
confBiasUpdate(bR_100,i1_1000)

confBiasUpdate(bR_1000,i2_1000)
confBiasUpdate(bR_1000,i2_1000)
confBiasUpdate(bR_1000,i2_1000)

confBiasUpdate(bR_100,i3_100)
confBiasUpdate(bR_1000,i3_1000)
confBiasUpdate(bR_10000,i3_10000)

//-------------------------------------------------------------

confBiasUpdatePar(bR_100,i1_1000)
confBiasUpdatePar(bR_100,i1_1000)
confBiasUpdate(bR_100,i1_1000)

confBiasUpdatePar(bR_1000,i2_1000)
confBiasUpdatePar(bR_1000,i2_1000)
confBiasUpdatePar(bR_1000,i2_1000)

confBiasUpdatePar(bR_100,i3_100)
confBiasUpdatePar(bR_1000,i3_1000)
confBiasUpdatePar(bR_10000,i3_10000)

//----------------------------------------- FUNCION simulate -----------------------------------

for {
  b <- simulate(confBiasUpdate, i1_100, bR_100, 10)
} yield (b,rho (d1,b))

for {
  b <- simulate(confBiasUpdatePar, i1_100, bR_100, 10)
} yield (b,rhoPar(d1,b))

//----------------------------------------------------

for {
  b <- simulate(confBiasUpdate, i2_1000, bR_1000, 20)
} yield (b,rho (d2,b))

for {
  b <- simulate(confBiasUpdatePar, i2_1000, bR_1000, 20)
} yield (b,rhoPar(d2,b))


//---------------------------------------------------

for {
  b <- simulate(confBiasUpdate, i3_100, bR_100, 10)
} yield (b,rho(d2,b))

for {
  b <- simulate(confBiasUpdatePar, i3_100, bR_100, 10)
} yield (b,rhoPar(d2,b))


//--------------------------------------------------

for {
  b <- simulate(confBiasUpdate, i2_100, bR_100, 50)
} yield (b,rho (d1,b))

for {
  b <- simulate(confBiasUpdatePar, i2_100, bR_100, 50)
} yield (b,rhoPar(d1,b))