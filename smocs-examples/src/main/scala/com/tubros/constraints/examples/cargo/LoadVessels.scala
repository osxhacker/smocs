/**
 * Created on: Apr 2, 2013
 */
package com.tubros.constraints.examples
package cargo

import scalaz._

import model._


/**
 * The '''LoadVessels''' type is a [[scala.App]] which serves as the entry-point
 * for the a simple
 * [[http://en.wikipedia.org/wiki/Bin_packing_problem Bin packaging problem]]
 * example program.
 * 
 * There's not much error checking done on the command line arguments, as this
 * is not a production-ready example.  Instead, the intent is to solve a
 * real-world problem ''without'' introducing logic which a production system
 * would need to have.  Ideally, this will lead to a more discoverable example.
 *
 * @author svickers
 *
 */
object LoadVessels
	extends App
		with Timer
{
	/// Class Imports
	import syntax.show._
	
	
	/// Instance Properties
	val options = CommandLineArguments (args);
	
	lazy val usage = """
		|usage: LoadVessels vessel1=N | cargo1=M [vessel2=O | cargo2=P ...]
		|
		|Where 'vessel' parameters are one of:
		|	ships=N           (high capacity)
		|	trucks=N          (medium capacity)
		|	planes=N          (low capacity)
		|
		|And 'cargo' parameters are one of:
		|	anchors=N         (high weight, small size)
		|	guitars=N         (low weight, medium size)
		|	pianos=N          (high weight, large size)
		|	plates=N          (low weight, small size)
		|
		|And 'N' is any positive integer (which can be different for each param)
		""".stripMargin;
	
	lazy val planes = options getOrElse ("planes", 0);
	lazy val ships = options getOrElse ("ships", 0);
	lazy val trucks = options getOrElse ("trucks", 0);
	
	lazy val anchors = options getOrElse ("anchors", 0);
	lazy val guitars = options getOrElse ("guitars", 0);
	lazy val pianos = options getOrElse ("pianos", 0);
	lazy val plates = options getOrElse ("plates", 0);
	
	lazy val containers = ShippingContainers (planes, ships, trucks);
	lazy val manifest = ShippingManifest (anchors, guitars, pianos, plates);
	
	
	/// Constructor Body
	if (containers.isEmpty || manifest.isEmpty)
		println (usage);
	else
		run (containers, manifest);
	
	
	private def run (
		containers : ShippingContainers,
		manifest : ShippingManifest
		)
		: Unit =
	{
		println ("\n\n%s\n%s".format (containers.shows, manifest.shows));
		
		// Run the solver to find the best configuration
		time {
			val results = ProduceShippingSchedules (containers, manifest);
			
			println ("\nResult:\n");
			
			results foreach {
				result =>
					
				val report = result match {
					case \/- (schedule) =>
						println (schedule.shows);
						
					case -\/ (error) =>
						println ("Failed to find an answer: %s".format (error));
					}
				}
			}
		
		println ("\n\n");
	}
}
