/**
 * Created on: Apr 2, 2013
 */
package com.tubros.constraints.examples.cargo.model


/**
 * The '''Volume''' type objectifies the concept of how much space a unit of
 * [[com.tubros.constraints.examples.cargo.Cargo]] occupies in the physical
 * world.
 *
 * @author svickers
 *
 */
final case class Volume (
	val height : Int,
	val width : Int,
	val length : Int
	)
{
	/// Instance Properties
	lazy val area = height * width * length;
}
