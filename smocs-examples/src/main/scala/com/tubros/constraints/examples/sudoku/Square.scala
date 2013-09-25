/**
 * Created on: Sep 19, 2013
 */
package com.tubros.constraints.examples.sudoku

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints.api._


/**
 * The '''Square''' type defines one
 * [[com.tubros.constraints.examples.sudoku.Puzzle]] row/column position and
 * can optionally have a value assigned to it.
 *
 * @author svickers
 *
 */
final case class Square (
	val row : Char,
	val column : Int,
	val assignment : Option[Int] = None
	)
{
	/// Class Imports
	import Scalaz._
	
	
	/// Instance Properties
	val isAssigned = assignment.isDefined;
	val isUnassigned = !isAssigned;
	val name = VariableName (s"$row$column");
	
	
	def assign (n : Int) = copy (assignment = n.some);
	
	
	def occupies (other : Square) : Boolean =
		row === other.row && column === other.column;
}


object Square
{
	/// Implicit Conversions
	implicit object OrderSquare
		extends Ordering[Square]
	{
		/// Class Imports
		import Scalaz._
		
		
		override def compare (a : Square, b : Square) : Int =
			((a.row cmp b.row) |+| (a.column cmp b.column)).toInt;
	}
	
	
	implicit object ShowSquare
		extends Show[Square]
	{
		/// Class Imports
		import Cord._
		
		
		override def shows (a : Square) : String =
			s"${a.row}${a.column} -> ${a.assignment}";
	}
	
	
	implicit def equalSquare : Equal[Square] = Equal.equalA;
}