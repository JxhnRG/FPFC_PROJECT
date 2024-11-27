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
      //Se genera los puntos en las divisiones
      val intervals = for {
        i <- 0 to 10
      } yield min + i * div
      //Se evalua la fucion en cada punto
      val values = intervals.map ( x => f(x))
      //Se ncuentra el punto donde la función tiene el menor valor
      val valueMin = intervals(values.indexOf(values.min))
      // Calcula los nuevos límites del intervalo
      val newMin = if (valueMin - div < min) min else valueMin - div
      val newMax = if (valueMin + div > max) max else valueMin + div
      //La función se llama recursivamente con el nuevo intervalo
      min_p(f, newMin, newMax,prec)
      }
    }
  // Creamos la función que nos va a devolver la medida de polarización
  def rhoCMT_Gen(alpha: Double, beta: Double): PolMeasure = {
    // Función que recibe una distribución
    (distribution: Distribution) => {
      // Creamos una tuple que sea una distribución
      val (frequencies, values) = distribution
      //Creamos una función Auxiliar que reciba un Double y devuelva la medida de polarización
      def rhoaux(p:Double):Double = {
        // Creamos tuples (pi,yi) y luego les cambiamos el valor por la formula que calcula la polarizacion
        frequencies.zip(values).map{case (pi,yi) =>  Math.pow(pi, alpha) * Math.pow(Math.abs(yi - p), beta)}.sum
      }
      // Definimos f como una función que recibe un p y es igual rhoaux
      val f = (p:Double) => rhoaux(p)
      // Definimos el valor mínimo de de la funcion rhoaux usando la funcion de min_p
      val min = min_p(f, 0.0, 1.0, 0.001)
      // Si f(min) es menor que 0.001 nos devuelve 0,0
      if (f(min) < 1e-2) 0.0
      // Si no Devuelve rhoaux(min) aproximado con tres decimales
      else BigDecimal(rhoaux(min)).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    }
  }
  def normalizar(m: PolMeasure): PolMeasure = {
    //Se definen la fecuencias y valores del peor caso
    val peorCaso: Distribution = (
      Vector(0.5, 0.0, 0.0, 0.0, 0.5), // Frecuencias del peor caso con 0.5 en extremos
      Vector(0.0, 0.25, 0.5, 0.75, 1.0) // Valores distribucion de likert5
    )

    //Se clacula la polarización del peor caso
    val polarizacionPeorCaso = m(peorCaso)

    //Se define un funcion anonima que calcula la polarización de la
    // distribución que se pase  como argumento y la dive por la del peor caso
    (x: Distribution) => BigDecimal(m(x) / polarizacionPeorCaso).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    }
}
