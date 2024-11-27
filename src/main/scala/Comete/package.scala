package object Comete {
  // Tipo para los valores reales de una distribución
  type DistributionValues =  Vector[Double]
  // Pi_k es una frecuencia de longitud k
  // Si Pi_k.length=K, 0<= Pi_k(i) <= 1, 0<=i<=k-1
  // Pi_k.sum == 1
  type Frequency = Vector[Double]
  // (Pi, dv) es una distribución si Pi es una frecuencia
  // y dv son los valores de distribución y Pi y dv
  // son de la misma longitud
  type Distribution = (Frequency, DistributionValues)

  type PolMeasure = Distribution => Double

  //Definimos la función que encontrara el punto mínimo
  // de funciones convexas
  def min_p(f: Double => Double, min: Double, max: Double, prec: Double): Double = {
    if ((max - min) < prec) (max + min) / 2
    else {
      //Divide el intervalo en 10 partes
      val div = (max - min) / 10
      //Se genera los puntos de las divisiones
      val intervals = for {
        i <- 0 to 10
      } yield min + i * div
      //Se evalua la fucion en cada punto
      val values = intervals.map ( x => f(x))
      //Se ncuentra el punto donde la función tiene el menor valor
      val valueMin = intervals(values.indexOf(values.min))
      //Se calculan los nuevos límites del intervalo
      val newMin = if (valueMin - div < min) min else valueMin - div
      val newMax = if (valueMin + div > max) max else valueMin + div
      //La función se llama recursivamente con los nuevos intervalos
      min_p(f, newMin, newMax, prec)
    }
  }

  def rhoCMT_Gen(alpha: Double, beta: Double): PolMeasure = {
    (distribution: Distribution) => {
      val (pi, y) = distribution
      val k = pi.length

      def paux(p: Double): Double = {
        (for {
          i <- 0 until k
        } yield math.pow(pi(i), alpha) * math.pow(math.abs(y(i) - p), beta)).sum
      }

      val f: Double => Double = (p: Double) => paux(p)
      val min = min_p(f, 0.0, 1.0, 0.0001)
      //Se redondean los resultados a 3 decimales
      math.round(paux(min) * 1000) / 1000.0
    }
  }

  def normalizar(m: PolMeasure): PolMeasure = {
    //Se definen la fecuencias y valores del peor caso dinamicamente
    def frecuencia(n: Int ): Frequency = {
      (0 until n).map(i => if (i == 0 || i == n - 1) 0.5 else 0.0).toVector
    }
    def valores(n: Int): DistributionValues = {
      (0 until n).map(i => if (i == n - 1)  1.0 else 0.0).toVector
    }
    //Se define un funcion anonima que calcula la polarización de la
    // distribución que se pase  como argumento y la dive por la del peor caso
    (x: Distribution) => {
      //Se busca el tamaño del primer elemento de la tupla Distribuction
      // que corresponde al vector de las frecuencias y como lo ideal es que el vector de los valores
      // de la distribucion tenga el mimso tamaño tomaremos solamente ese valor para n
      val n = x._1.length
      val frecuencias = frecuencia(n)
      val valoresDistribucion = valores(n)

      // Crear la distribución del peor caso
      val peorCaso: Distribution = (frecuencias, valoresDistribucion)

      //Se clacula la polarización del peor caso
      val polarizacionPeorCaso = m(peorCaso)
      //Se redondean los resultados a 3 decimales
      math.round((m(x) / polarizacionPeorCaso) * 1000) / 1000.0
    }
  }
}
