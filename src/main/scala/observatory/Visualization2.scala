package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import Interaction.tileLocation
import Visualization.interpolateColor


/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  val (width, height) = (256, 256)

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
      val (x, y) = (point.x, point.y)

    (d00 * (1 - x) * (1 - y)) + (d10 * x * (1 - y)) + (d01 * (1 - x) * y) + (d11 * x * y)

  }

  def interpolate(grid: GridLocation => Temperature, location: Location): Temperature = {
    val (x0, y0)  = (location.lat.ceil.toInt, location.lon.ceil.toInt)
    val (x1, y1) = (location.lat.floor.toInt, location.lon.floor.toInt)

    val d00 = GridLocation(x1, y0)
    val d01 = GridLocation(x1, y1)
    val d10 = GridLocation(x0, y0)
    val d11 = GridLocation(x0, y1)

    val pointX = x1 - location.lat.toInt
    val pointY = y1 - location.lon.toInt

    val cellPoint = CellPoint(pointX, pointY)

    bilinearInterpolation(cellPoint, grid(d00), grid(d01), grid(d10), grid(d11))

  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {

    val offsetX = (tile.x * Math.pow(2, 8)).toInt
    val offsetY = (tile.y * Math.pow(2, 8)).toInt
    val zoom = tile.zoom

    val cord = for (w <- 0 until  width; h <- 0 until  height) yield (w, h)

    val pixel = cord
      .map(c => Tile(c._1 + offsetX, c._2 + offsetY, 8 + zoom))
      .map(tileLocation)
      .map(interpolate(grid, _))
      .map(interpolateColor(colors, _))
      .map(c => Pixel(c.red, c.green, c.blue, 127))
      .toArray

    Image(width, height, pixel)
  }

}
