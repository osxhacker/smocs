/**
 * Created on: Apr 2, 2013
 */
package com.tubros.constraints.examples.cargo.model

import scala.language.postfixOps

import scalaz._


/**
 * The '''CargoVessel''' type embodies the concept of a transport which is
 * capable of moving some amount [[com.tubros.constraints.examples.cargo.Cargo]]
 * from one place to another.
 *
 * @author svickers
 *
 */
final case class CargoVessel (
	val name : String,
	val tonnage : Int,
	val capacity : Volume,
	val items : Seq[Cargo]
	)
{
	/// Class Imports
	import syntax.std.boolean._
	
	
	/// Instance Properties
	lazy val isOverWeight = weight > tonnage;
	lazy val isOverLoaded = loaded > (capacity area);
	lazy val hasRoom = !(isOverWeight || isOverLoaded);
	lazy val loaded = items map (_ area) sum;
	lazy val weight = (items map (_ weight) sum);
	
	
	def load (item : Cargo) : Option[CargoVessel] =
		hasRoom option (copy (items = items :+ item));
}


object CargoVessel
{
	def apply (name : String, tonnage : Int, capacity : Volume) : CargoVessel =
		new CargoVessel (name, tonnage, capacity, Seq.empty[Cargo]);
}
