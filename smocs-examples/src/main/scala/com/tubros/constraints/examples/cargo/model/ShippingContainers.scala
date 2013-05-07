/**
 * Created on: Apr 2, 2013
 */
package com.tubros.constraints.examples.cargo.model

import scalaz._


/**
 * The '''ShippingContainers''' type models the supported
 * [[com.tubros.constraints.examples.cargo.model.CargoVessel]]s available to
 * the program for solving the constraint problem.
 *
 * @author svickers
 *
 */
final case class ShippingContainers (
	planes : Int,
	ships : Int,
	trucks : Int
	)
{
	/// Class Imports
	import std.anyVal._
	import syntax.equal._
	import ShippingContainers._

	
	/// Instance Properties
	val isEmpty = (planes + ships + trucks) === 0;
	
	
	def vessels () : Seq[CargoVessel] =
		Plane (planes) ++ Ship (ships) ++ Truck (trucks);
}


object ShippingContainers
{
	/// Class Types
	object Plane
	{
		def apply (amount : Int) : Seq[CargoVessel] =
			List.fill (amount) (
				CargoVessel (
					name = "plane",
					tonnage = 50,
					capacity = Volume (10, 5, 20)
					)
				);
	}
	
	
	object Ship
	{
		def apply (amount : Int) : Seq[CargoVessel] =
			List.fill (amount) (
				CargoVessel (
					name = "ship",
					tonnage = 2000,
					capacity = Volume (50, 20, 100)
					)
				);
	}
	
	
	object Truck
	{
		def apply (amount : Int) : Seq[CargoVessel] =
			List.fill (amount) (
				CargoVessel (
					name = "truck",
					tonnage = 100,
					capacity = Volume (10, 10, 10)
					)
				);
	}
	
	
	/// Implicit Conversions
	implicit def ShowShippingContainers : Show[ShippingContainers] =
		new Show[ShippingContainers] {
			override def shows (sc : ShippingContainers) : String =
				"Running LoadVessels with %d planes, %d ships, and %d trucks".format (
					sc.planes,
					sc.ships,
					sc.trucks
					);
			}
}
