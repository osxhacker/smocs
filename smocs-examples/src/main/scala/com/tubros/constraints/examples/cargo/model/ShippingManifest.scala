/**
 * Created on: Apr 2, 2013
 */
package com.tubros.constraints.examples.cargo.model

import scalaz._


/**
 * The '''ShippingManifest''' type represents the various
 * [[com.tubros.constraints.examples.cargo.model.Cargo]] being shipped in a
 * particular run.
 *
 * @author svickers
 *
 */
final case class ShippingManifest (
	val anchors : Int,
	val guitars : Int,
	val pianos : Int,
	val plates : Int
	)
{
	/// Class Imports
	import std.anyVal._
	import syntax.equal._
	import ShippingManifest._
	
	
	/// Instance Properties
	val isEmpty = (anchors + guitars + pianos + plates) === 0;
	
	
	def cargo () : Seq[Cargo] =
		(
			Anchor (anchors) ++
			Guitar (guitars) ++
			Piano (pianos) ++
			Plate (plates)
		);
	
	
	def loaded (schedule : Schedule) : ShippingManifest =
		copy (
			anchors = anchors - schedule.anchors.size,
			guitars = guitars - schedule.guitars.size,
			pianos = pianos - schedule.pianos.size,
			plates = plates - schedule.plates.size
			);
}


object ShippingManifest
{
	/// Class Types
	object Anchor
	{
		val volume = Volume (1, 2, 2);
		
		
		def apply (amount : Int) : Seq[Cargo] =
			List.fill (amount) (Cargo ('anchors, "20.00", 50, volume));
	}
	
	
	object Guitar
	{
		val volume = Volume (2, 3, 3);
		
		
		def apply (amount : Int) : Seq[Cargo] =
			List.fill (amount) (Cargo ('guitars, "100.00", 4, volume));
	}
	
	
	object Piano
	{
		val volume = Volume (5, 4, 2);
		
		
		def apply (amount : Int) : Seq[Cargo] =
			List.fill (amount) (Cargo ('pianos, "1000.00", 200, volume));
	}
	
	
	object Plate
	{
		val volume = Volume (1, 1, 1);
		
		
		def apply (amount : Int) : Seq[Cargo] =
			List.fill (amount) (Cargo ('plates, "2.00", 1, volume));
	}
	
	
	/// Implicit Conversions
	implicit def ShowShippingManifest : Show[ShippingManifest] =
		new Show[ShippingManifest] {
			override def shows (sm : ShippingManifest) : String =
				"Shipping %d anchors, %d guitars, %d pianos, and %d plates".format (
					sm.anchors,
					sm.guitars,
					sm.pianos,
					sm.plates
					);
			}
}
