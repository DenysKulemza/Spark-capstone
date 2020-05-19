package observatory

import Math.{PI, atan, pow, sinh}

import com.sksamuel.scrimage.{Image, Pixel}
import Visualization.{interpolateColor, predictTemperature}


/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  val (width, height) = (256, 256)

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val (x, y, zoom) = (tile.x, tile.y, tile.zoom)

    val lon = x / pow(2, zoom) * 360 - 180
    val lat = atan(sinh(PI - (y / pow(2, zoom)) * 2 * PI)) * (180 / PI)

    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val offsetX = tile.x * pow(2, 8).toInt
    val offsetY = tile.y * pow(2, 8).toInt
    val zoom = tile.zoom

    val coord = for(w <- 0 until  width; h <- 0 until height) yield (w, h)

    val pixel = coord.map(c => tileLocation(Tile(tile.x + offsetX, tile.y + offsetY, 8 + zoom))) // PAR DELETED
      .map(predictTemperature(temperatures, _))
      .map(interpolateColor(colors, _))
      .map(colors => Pixel(colors.red, colors.green, colors.blue, 127))
      .toArray

      Image(width, height, pixel)

  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {

    for(zoom <- 0 to 3;
        x <- 0 until  pow(2, zoom).toInt;
        y <- 0 until  pow(2, zoom).toInt;
        (year, data) <- yearlyData) generateImage(year, Tile(x, y, zoom), data)
  }
}
