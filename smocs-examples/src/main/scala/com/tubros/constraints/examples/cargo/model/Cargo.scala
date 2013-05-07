/**
 * Created on: Apr 2, 2013
 */
package com.tubros.constraints.examples.cargo.model

import scala.language.postfixOps


/**
 * The '''Cargo''' type defines the model used to represent a piece of cargo
 * being shipped in the example application.
 *
 * @author svickers
 *
 */
final case class Cargo (
	val name : Symbol,
	val value : BigDecimal,
	val weight : Int,
	val volume : Volume
	)
{
	/// Instance Properties
	lazy val area = volume area;
}


object Cargo
{
	def apply (
		name : Symbol,
		value : String,
		weight : Int,
		volume : Volume
		)
		: Cargo = new Cargo (name, BigDecimal (value), weight, volume);
}
