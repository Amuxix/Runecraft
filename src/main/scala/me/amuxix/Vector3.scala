package me.amuxix

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

object Vector3 {
	implicit def Tuple32Vector3[T: Integral](tuple: (T, T, T)): Vector3[T] = {
		Vector3(tuple._1, tuple._2, tuple._3)
	}
	implicit val encoder: Encoder[Vector3[Int]] = deriveEncoder
	implicit val decoder: Decoder[Vector3[Int]] = deriveDecoder
}

/**
  * Created by Amuxix on 22/11/2016.
  */
case class Vector3[T : Integral](x: T, y: T, z: T) {
  import Integral.Implicits._
	def +(vector: Vector3[T]): Vector3[T] = Vector3(x + vector.x, y + vector.y, z + vector.z)
	def -(vector: Vector3[T]): Vector3[T] = Vector3(x - vector.x, y - vector.y, z - vector.z)
	def *(constant: T): Vector3[T] = Vector3(x * constant, y * constant, z * constant)
	def /(constant: T): Vector3[T] = Vector3(x / constant, y / constant, z / constant)

  def toIntVector(implicit ev: T =:= Double): Vector3[Int] = Vector3(x.toInt, y.toInt, z.toInt)
  def toDoubleVector(implicit ev: T =:= Int): Vector3[Double] = Vector3(x.toDouble, y.toDouble, z.toDouble)

  override def toString: String = s"($x, $y, $z)"
}
