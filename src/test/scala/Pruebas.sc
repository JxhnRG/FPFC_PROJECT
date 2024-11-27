import Comete._
import Benchmark._
import Opinion._

//Definimos los casos de las frecuencias
val pi_max = Vector(0.5, 0.0, 0.0, 0.0, 0.5)
val pi_min = Vector(0.0, 0.0, 1.0, 0.0, 0.0)
val pi_der = Vector(0.4, 0.0, 0.0, 0.0, 0.6)
val pi_izq = Vector(0.6, 0.0, 0.0, 0.0, 0.4)
val pi_int1 = Vector(0.0, 0.5, 0.0, 0.5, 0.0)
val pi_int2 = Vector(0.25, 0.0, 0.5, 0.0, 0.25)
val pi_int3 = Vector(0.25, 0.25, 0.0, 0.25, 0.25)
val pi_cons_centro = pi_min
val pi_cons_der = Vector(0.0, 0.0, 0.0, 0.0, 1.0)
val pi_cons_izq = Vector(1.0, 0.0, 0.0, 0.0, 0.0)

//Definimos la Escala likert
val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

val cmt1 = rhoCMT_Gen(1.2, 1.2)

cmt1(pi_max, likert5)
cmt1(pi_min, likert5)
cmt1(pi_der, likert5)
cmt1(pi_izq, likert5)
cmt1(pi_int1, likert5)
cmt1(pi_int2, likert5)
cmt1(pi_int3, likert5)
cmt1(pi_cons_centro, likert5)
cmt1(pi_cons_der, likert5)
cmt1(pi_cons_izq, likert5)

val cmt1_norm = normalizar(cmt1)

cmt1_norm(pi_max, likert5)
cmt1_norm(pi_min, likert5)
cmt1_norm(pi_der, likert5)
cmt1_norm(pi_izq, likert5)
cmt1_norm(pi_int1, likert5)
cmt1_norm(pi_int2, likert5)
cmt1_norm(pi_int3, likert5)
cmt1_norm(pi_cons_centro, likert5)
cmt1_norm(pi_cons_der, likert5)
cmt1_norm(pi_cons_izq, likert5)

val sb_ext = allExtremeBelief(100)
val sb_cons = consensusBelief(0.2)(100)
val sb_unif = uniformBelief(100)
val sb_triple = allTripleBelief(100)
val sb_midly = midlyBelief(100)

val rho1 = rho(1.2, 1.2)
val rho2 = rho(2.0, 1.0)
val rhoPar1 = rhoPar(1.2,1.2)
val rhoPar2 = rhoPar(2.0,1.0)
val dist1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

//Pruebas secuenciales
rho1(sb_ext, dist1)
rho2(sb_ext, dist1)
rho1(sb_ext, dist2)
rho2(sb_ext, dist2)
rho1(sb_cons, dist1)
rho2(sb_cons, dist1)
rho1(sb_cons, dist2)
rho2(sb_cons, dist2)
rho1(sb_unif, dist1)
rho2(sb_unif, dist1)
rho1(sb_unif, dist2)
rho2(sb_unif, dist2)
rho1(sb_triple, dist1)
rho2(sb_triple, dist1)
rho1(sb_triple, dist2)
rho2(sb_triple, dist2)
rho1(sb_midly, dist1)
rho2(sb_midly, dist1)
rho1(sb_midly, dist2)
rho2(sb_midly, dist2)

//Pruebas paralelas
rhoPar1(sb_ext, dist1)
rhoPar2(sb_ext, dist1)
rhoPar1(sb_ext, dist2)
rhoPar2(sb_ext, dist2)
rhoPar1(sb_cons, dist1)
rhoPar2(sb_cons, dist1)
rhoPar1(sb_cons, dist2)
rhoPar2(sb_cons, dist2)
rhoPar1(sb_unif, dist1)
rhoPar2(sb_unif, dist1)
rhoPar1(sb_unif, dist2)
rhoPar2(sb_unif, dist2)
rhoPar1(sb_triple, dist1)
rhoPar2(sb_triple, dist1)
rhoPar1(sb_triple, dist2)
rhoPar2(sb_triple, dist2)
rhoPar1(sb_midly, dist1)
rhoPar2(sb_midly, dist1)
rhoPar1(sb_midly, dist2)
rhoPar2(sb_midly, dist2)

val i1_10 = i1(10)
val i2_10 = i2(10)
val i1_20 = i1(20)
val i2_20 = i2(20)

showWeightedGraph(i1_10)
showWeightedGraph(i2_10)

//Pruebas secuenciales
val sbu_10 = uniformBelief(10)
confBiasUpdate(sbu_10, i1_10)
rho1(sbu_10, dist1)
rho1(confBiasUpdate(sbu_10, i1_10), dist1)

val sbm_10 = midlyBelief(10)
confBiasUpdate(sbm_10, i1_10)
rho1(sbm_10, dist1)
rho1(confBiasUpdate(sbm_10, i1_10), dist1)

//Pruebas Paralelas
val sbu_10 = uniformBelief(10)
confBiasUpdatePar(sbu_10, i1_10)
rho1(sbu_10, dist1)
rho1(confBiasUpdatePar(sbu_10, i1_10), dist1)

val sbm_10 = midlyBelief(10)
confBiasUpdatePar(sbm_10, i1_10)
rho1(sbm_10, dist1)
rho1(confBiasUpdatePar(sbm_10, i1_10), dist1)

for {
  b <- simulate(confBiasUpdate, i1_10, sbu_10, 2)
} yield (b, rho1(b, dist1))
for {
  b <- simulate(confBiasUpdate, i1_10, sbm_10, 2)
} yield (b, rho1(b, dist1))

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
val sbms = for {
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val polSec = rho(1.2, 1.2)
val polPar = rhoPar(1.2, 1.2)
val cmp1 = compararMedidasPol(sbms, likert5, polSec, polPar)

cmp1.map(t => t._6)

val i132768 = i1(32768)
val i232768 = i2(32768)
compararFuncionesAct(sbms.take(sbms.length / 2), i232768, confBiasUpdate, confBiasUpdatePar)


val sbes = for {
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield allExtremeBelief(nags)

val sbts = for {
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield allTripleBelief(nags)

val evolsSec = for {
  i <- 0 until sbms.length
} yield simEvolucion(Seq(sbms(i), sbes(i), sbts(i)),
  i232768, 10, polSec, confBiasUpdate, likert5,
  "Simulacion Secuencial " ++ i.toString ++ "-" ++ sbms(i).length.toString
)