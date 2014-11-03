/**
 * Created on: Sep 19, 2013
 */
package com.tubros.constraints.examples.sudoku

import scala.language.higherKinds

import scalaz.{
	Ordering => _,
	_
	}


/**
 * The '''Puzzle''' type reifies a Sudoku puzzle.
 *
 * @author svickers
 *
 */
final case class Puzzle (
	val squares : Set[Square]
	)
{
	/// Class Imports
	import std.anyVal._
	import syntax.equal._
	import syntax.std.option._
	
	
	/// Instance Properties
	val rowSize = 9;
	lazy val isComplete : Boolean = !squares.exists (_.isUnassigned);
	lazy val isEmpty : Boolean = squares.forall (_.isUnassigned);
	lazy val rows : Seq[Seq[Square]] =
		squares.toSeq.sorted.grouped (rowSize).toSeq;
	
	
	def + (value : Square) : Puzzle =
		copy (squares = squares.filterNot (value.occupies) + value);
	
	
	def ++[C[X] <: Traversable[X]] (values : C[Square]) : Puzzle =
	{
		val setOfValues = values.toSet;

		copy (
			squares = squares.filterNot {
				s =>
					
				setOfValues.exists (_.occupies (s));
				} ++ setOfValues
			);
	}
	
		
	def apply (row : Char, column : Int) : Option[Square] =
		squares.find (_.occupies (Square (row, column))).filter (_.isAssigned);
	
	
	def row (id : Char) : Set[Square] = squares filter (_.row === id);
	
	
	def column (n : Int) : Set[Square] = squares filter (_.column === n);
	
	
	def update (square : Square, value : Int) : Puzzle =
		this + square.assign (value);
}


object Puzzle
{
	/// Class Imports
	import std.anyVal._
	import syntax.enum._
	
	
	/// Instance Properties
	val rowNames = 'A' |=> 'I';
	val columnNames = 1 |=> 9;
	val columnOffsets = 0 to 8;
	
	
	/**
	 * The apply method provides functional-style creation of '''Puzzle'''
	 * instances.
	 */
	def apply () : Puzzle =
	{
		val squares = for {
			row <- rowNames
			col <- columnNames
			} yield Square (row, col, None);
			
		return (Puzzle (squares.toIterable.toSet));
	}

		
	/// Implicit Conversions
	implicit object ShowPuzzle
		extends Show[Puzzle]
	{
		/// Class Imports
		import Cord._
		import syntax.show._
		
		
		override def show (a : Puzzle) : Cord =
			Cord (
				"Puzzle(\n",
				mkCord ("\n", mkRows (a) : _*),
				"\n)"
				);
		
		
		private def mkRows (p : Puzzle) : Seq[Cord] =
			p.rows map (r => mkCord (" | ", r map (fromSquare) : _*));
		
		
		private def fromSquare (s : Square) : Cord = s.show;
	}
	
	
	implicit def equalPuzzle : Equal[Puzzle] = Equal.equalA;
}
