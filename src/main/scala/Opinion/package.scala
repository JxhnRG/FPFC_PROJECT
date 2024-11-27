import Comete._
import common._

import scala.collection.parallel.CollectionConverters._
package object Opinion {
  // Si n es el número de agentes, estos se identifican
  // como los números enteros entre 0 y n-1
  // O sea el conjunto de Agentes A es
  // implícitamente el conjunto { 0,1,2, ... , n-1 }

  // Si b : BeliefConf, para cada i en Int, b[i] es un numero
  // entre 0 y 1 que indica cuanto cree el agente i en
  // la veracidad de la proposición p
  // Si existe i:b(i) < 0 o b(i) > 1 b esta mal definida

  type SpecificBelief = Vector[Double]
  // Si b:SpecificBelief, para cada i en Int, b[i] es un
  // número entre 0 y 1 que indica cuanto cree el
  // agente i en la veracidad de la proposición p
  // El número de agentes es b.length
  // Si existe i : b(i) < 0 o b(i) > 1 b esta mal definida.
  // Para i en Int\A, b(i) no tiene sentido

  type GenericBeliefConf = Int => SpecificBelief
  // si gb: GenericBelief, entonces gb(n) = b tal que
  // b: SpecificBelief

  type AgentsPolMeasure = (SpecificBelief, DistributionValues) => Double
  // Si rho: AgentsPolMeasure y sb: SpecificBelief
  // y d: DistributionValues,
  // rho (sb, d) es la polarización de los agentes
  // de acuerdo a esa medida

  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
    // rho es la medida de polarizacion de agentes basada
    // en comete
    (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {

      val k = distributionValues.length

      val firstInterval = (0.0, distributionValues(1)/2)
      val middleIntervals = (1 to k-2).map(i =>
        ((distributionValues(i)+distributionValues(i-1))/2, (distributionValues(i)+distributionValues(i+1))/2)).toVector
      val lastInterval = ((distributionValues(k-2)+1)/2, 1.0)

      val intervals = firstInterval +: middleIntervals :+ lastInterval

      val emptyClassification = (0 until k).map(i => i -> Vector.empty[Double]).toMap
      val classification = specificBelief.groupBy(a => intervals.zipWithIndex.indexWhere {
        case ((start, end), i) =>
          if(i == k-1) (start <= a && a <= end)
          else (start <= a && a < end)
      })
      val finalClassification = (emptyClassification ++ classification).toSeq.sortBy(_._1)

      val numAgents = specificBelief.length
      val frequency = finalClassification.map{ case (i, values) => (values.length.toDouble)/numAgents}.toVector

      val cmt= rhoCMT_Gen(alpha, beta)
      val cmtnorm= normalizar(cmt)
      cmtnorm((frequency, distributionValues))
    }
  }
  type WeightedGraph = ( Int , Int ) => Double
  type SpecificWeightedGraph = (WeightedGraph , Int )
  type GenericWeightedGraph =
    Int => SpecificWeightedGraph
  type FunctionUpdate =
    ( SpecificBelief , SpecificWeightedGraph )=> SpecificBelief


  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    val (f, n) = swg
    (0 until n).par.map(i =>
      (0 until n).par.map(j => f(i + 1, j + 1)).toIndexedSeq
    ).toIndexedSeq
  }


  def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    // Crear una nueva colección para almacenar las creencias actualizadas
    sb.zipWithIndex.map { case (belief, i) =>
      // Identificar los agentes influyentes
      val influentAgents = (0 until swg._2).filter(j => swg._1(j, i) > 0)

      // Verificar si hay agentes influyentes
      if (influentAgents.isEmpty) {
        // Si no hay agentes influyentes, la creencia permanece igual
        belief
      } else {
        // Calcular la suma ponderada de influencias
        val sum = influentAgents.map { j =>
          val beta = 1 - math.abs(sb(j) - belief)
          val influenceGraph = swg._1(j, i)
          beta * influenceGraph * (sb(j) - belief)
        }.sum

        // Actualizar la creencia con el promedio de influencias
        belief + sum / influentAgents.length
      }
    }
  }

  def simulate(
                fu: FunctionUpdate,
                swg: SpecificWeightedGraph,
                b0: SpecificBelief,
                t: Int
              ): IndexedSeq[SpecificBelief] = {
    (0 until t).scanLeft(b0)((b, _) => fu(b, swg))
  }
  // Versiones paralelas
  def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
    (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
      val numAgents = specificBelief.length
      val k = distributionValues.length

      val firstInterval = (0.0, distributionValues(1)/2)
      val middleIntervals = (1 to k-2).par.map(i =>
        ((distributionValues(i)+distributionValues(i-1))/2, (distributionValues(i)+distributionValues(i+1))/2)).toVector
      val lastInterval = ((distributionValues(k-2)+1)/2, 1.0)

      val intervals = firstInterval +: middleIntervals :+ lastInterval

      val emptyClassification = (0 until k).map(i => i -> Vector.empty[Double]).toMap
      val classification = specificBelief.par.groupBy(a => intervals.zipWithIndex.indexWhere {
        case ((start, end), i) =>
          if(i == k-1) (start <= a && a <= end)
          else (start <= a && a < end)
      })
      val finalClassification = (emptyClassification ++ classification).toSeq.sortBy(_._1)

      val frequency = finalClassification.map{ case (i, values) => values.knownSize.toDouble/numAgents}.toVector

      val  cmt = rhoCMT_Gen(alpha, beta)
      val cmtnorm = normalizar(cmt)
      cmtnorm((frequency, distributionValues))
    }
  }


}
