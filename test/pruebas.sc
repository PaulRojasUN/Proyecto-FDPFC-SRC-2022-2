import proyecto._

val pi1= Vector (0.4 , 0.6 )
val pi2=Vector ( 0.5 , 0.5 )
val pi3=Vector ( 0.6 , 0.4 )
val pi4=Vector ( 0.1 , 0.9 )
val pi5=Vector ( 0.9 , 0.1 )

val py1 = Vector(3.0,9.5)
val pp2= Vector(0.6, 0.4)


val y=Vector ( 1.0 , 5.0 )
val y2=Vector(2.9,3.1)

rhoER((pi1,y))
rhoER((pi2,y))
rhoER((pi3,y))
rhoER((pi4,y))
rhoER((pi5,y))

rhoER((pi3, y2))

val d1=Vector ( 0.2 , 0.4 , 0.6 , 0.8 )
val d2=Vector ( 0.1 , 0.4 , 0.7 , 0.9 )

def b0(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i:Int) => {0.6})
}

def b1(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i: Int) => {if (i <= nags / 2) 0.6 else 0.4})
}

def b2(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i:Int)=> if (i <= nags/2) 0.3 else 0.9)
}

def b3(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i:Int) => (i+1).toDouble/nags.toDouble)
}


val b0_10= b0 ( 10 )
val b0_20 = b0 ( 20 )
val b1_10= b1 ( 10 )
val b1_20 = b1 ( 20 )
val b2_10= b2 ( 10 )
val b2_20 = b2 ( 20 )
val b3_10= b3 ( 10 )
val b3_20 = b3( 20 )

/*  eg 2.2.1
  val py1 = Vector(0.25,0.95)
  val pp2 = Vector(0.6, 0.4)
  rhoER((pp2, py1)) ==  1 .1297078329082348 == rho (d2,b2_10 )

*/