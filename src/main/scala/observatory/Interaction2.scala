package observatory


/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 extends Interaction2Interface {

  private val temperatureColor  = List[(Temperature, Color)] (
    (-60, Color(0, 0, 0)), (-50, Color(33, 0, 107)),
    (-27, Color(255, 0, 255)), (-15, Color(0, 0, 255)),
    (0, Color(0, 255, 255)), (12, Color(255, 255, 0)),
    (32, Color(255, 0, 0)), (60, Color(255, 255, 255)))

  private val deviationColor = List[(Temperature, Color)](
    (-7, Color(0, 0, 255)), (-2, Color(0, 255, 255)),
    (0, Color(255, 255, 255)), (2, Color(255, 255, 0)),
    (4, Color(255, 0, 0)),(7, Color(0, 0, 0))
  )

  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] = {
    val (deviation, temperature) = (LayerName.Deviations ,LayerName.Temperatures)

    List (
        Layer(temperature, temperatureColor, Range(1975, 1989)),
        Layer(deviation, deviationColor, Range(1990, 2015))
    )

  }

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = {
    Signal(selectedLayer.apply().bounds)
  }

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year] = {
   /* Doesn't work
   val bound = selectedLayer.apply().bounds
      if(bound.start > sliderValue.apply()){
        Signal(bound.start)
      }else if(bound.end < sliderValue.apply()){
        Signal(bound.end)
      }else{
        Signal(sliderValue.apply())
      }*/

    Signal(sliderValue()
      .min(selectedLayer.apply().bounds.end)
      .max(selectedLayer.apply().bounds.start))
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {
     Signal(s"target/${selectedLayer.apply().layerName.id}/${selectedYear.apply()}/{z}/{x}-{y}.png")
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {
    Signal(selectedLayer.apply().layerName.id.capitalize + " ("+ selectedYear.apply() + ")")
  }

}

// Interface used by the grading infrastructure. Do not change signatures
// or your submission will fail with a NoSuchMethodError.
trait Interaction2Interface {
  def availableLayers: Seq[Layer]
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range]
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year]
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
}

sealed abstract class LayerName(val id: String)
object LayerName {
  case object Temperatures extends LayerName("temperatures")
  case object Deviations extends LayerName("deviations")
}

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Temperature, Color)], bounds: Range)

