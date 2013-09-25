/**
 * Created on: Sep 19, 2013
 */
package com.tubros.constraints.examples

import scala.util.control.Exception._


/**
 * The '''CommandLineArguments''' type provides the `examples` with a consistent
 * way to parse CLI-based programs.
 *
 * @author svickers
 *
 */
object CommandLineArguments
{
	/// Class Types
	type Options = Map[String, Int]
	val Options = Map;
	
	private object NaturalNumber
	{
		def unapply (s : String) : Option[Int] =
			catching (classOf[NumberFormatException]) opt s.toInt filter (_ > 0);
	}
	
	
	def apply (args : Array[String]) : Options =
		nextOption (Options.empty, args.toList);
	
	
	private def nextOption (options : Options, args : List[String])
		: Options =
		args match {
			case Nil =>
				options;
				
			case param :: tail =>
				nextOption (add (options, param), tail);
			}
	
	
	private def add (options : Options, param : String) : Options =
		param split ('=') match {
			case Array (name, NaturalNumber (value)) =>
				options + (name -> value);
				
			case _ =>
				options;
			}
}
