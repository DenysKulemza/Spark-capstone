package observatory

import Math.{abs, sqrt, asin, cos, sin, toRadians, PI, pow}
import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  val earthRadius = 6371
  val p = 3
  val width = 360
  val height = 180


  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val distance = temperatures.map(p=>(computeDistance(location, p._1), p._2))

    val getDistance = distance.minBy(_._1)

    if(getDistance._1 <= 1){
      getDistance._2

    }else {
      val firstSum = distance.map(x=> x._2 / pow(x._1, p)).sum
      val secondSum =  distance.map(x => 1.0 / pow(x._1, p)).sum

      firstSum / secondSum

    }

  }

  /**
    *
    * @param firstCoordinate     coordinate of first location
    * @param secondCoordinate    coordinate of second location
    * @return                    boolean
    */
  def checkAntipodes(firstCoordinate: Location, secondCoordinate: Location): Boolean = {
    (firstCoordinate.lat == -secondCoordinate.lat) && (abs(firstCoordinate.lon - secondCoordinate.lon) == 180)
  }

  /**
    *
    * @param firstCoordinate  coordinate of first location
    * @param secondCoordinate coordinate of second location
    * @return                 distance
    */
  def computeDistance(firstCoordinate: Location, secondCoordinate: Location): Double = {

    if(firstCoordinate == secondCoordinate) 0.0
    else if(checkAntipodes(firstCoordinate, secondCoordinate)) earthRadius * PI
    else {
      val firstLat = toRadians(firstCoordinate.lat)
      val firstLon = toRadians(firstCoordinate.lon)

      val secondLat = toRadians(secondCoordinate.lat)
      val secondLon = toRadians(secondCoordinate.lon)

      val xDelta = abs(secondLon - firstLon)
      val yDelta = abs(secondLat - firstLat) //CHANGE

      val result = 2 * asin(sqrt(pow(sin(yDelta / 2), 2)
        + cos(firstLat)*cos(secondLat) * pow(sin(xDelta / 2), 2)))


      earthRadius * result

    }

  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val equalTemperature = points.find(_._1 == value)

    equalTemperature match {
      case Some(_) => equalTemperature.head._2
      case None => linearInterpolation(points, value)
    }
  }

  /**
    *
    * @param points refer to temperature and equality color
    * @param value  some temperature
    * @return       color
    */
  private def linearInterpolation(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    val (min, max) = points.toList.sortBy(_._1).partition(_._1 < value)

    if(min.isEmpty) max.minBy(_._1)._2
    else {
      if(max.isEmpty){
        min.maxBy(_._1)._2
      }else{
        val (x0, x1) = (min.maxBy(_._1)._1, max.minBy(_._1)._1)
        val (minColor, maxColor) = (min.maxBy(_._1)._2, max.minBy(_._1)._2)

        val weight1 = (x1 - value) / (x1 - x0)
        val weight2 = (value - x0) / (x1 - x0)

        Color(
          interpolate(minColor.red, maxColor.red, weight1, weight2),
          interpolate(minColor.green, maxColor.green, weight1, weight2),
          interpolate(minColor.blue, maxColor.blue, weight1, weight2)
        )
      }
    }
  }

  /**
    *
    * @param minColor            color of min temperature
    * @param maxColor            color of max temperature
    * @param weight1      first weight of linear interpolation
    * @param weight2      second weight of linear interpolation
    * @return             number of RGB
    */
  private def interpolate(minColor: Int, maxColor: Int, weight1: Double, weight2: Double): Int = {
    (weight1 * minColor + weight2 * maxColor).round.toInt
  }


  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val cord = for (w <- 90 until -90 by -1; h <- -180 until 180) yield (w, h)

    val pixel = cord
      .map(coord => Location(coord._1, coord._2))
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(col => Pixel(col.red, col.green, col.blue, 255)).toArray

     Image(width, height, pixel)

  }

}

