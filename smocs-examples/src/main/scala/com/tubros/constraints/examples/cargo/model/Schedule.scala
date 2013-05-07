/**
 * Created on: Apr 2, 2013
 */
package com.tubros.constraints.examples.cargo.model

import scalaz._


/**
 * The '''Schedule''' type is a `model` type which describes the
 * [[com.tubros.constraints.examples.cargo.model.Cargo]] destined for a
 * particular '''vessel'''.
 *
 * @author svickers
 *
 */
final case class Schedule (
	val vessel : CargoVessel,
	val anchors : Seq[Cargo],
	val guitars : Seq[Cargo],
	val pianos : Seq[Cargo],
	val plates : Seq[Cargo]
	)
{

}


object Schedule
{
	/// Implicit Conversions
	implicit def showSchedule = new Show[Schedule] {
		override def shows (schedule : Schedule) =
			"%s has %s anchors, %s guitars, %s pianos, and %s plates".format (
				schedule.vessel.name,
				schedule.anchors.size,
				schedule.guitars.size,
				schedule.pianos.size,
				schedule.plates.size
				);
		}
}