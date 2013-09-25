/**
 * Created on: Sep 19, 2013
 */
package com.tubros.constraints.examples.sudoku

import scalaz.{
	Ordering => _,
	_
	}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


/**
 * The '''PuzzleSpec''' type serves as an exemplar for using the
 * [[com.tubros.constraints.examples.sudoku.Puzzle]]] domain type.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class PuzzleSpec
	extends ProjectSpec
{
	/// Class Imports
	import syntax.show._
	
	
	"A Puzzle" should "be created 'empty'" in
	{
		val empty = Puzzle ();
		
		empty should be ('empty);
	}
	
	it should "produce a nicely formatted representation" in
	{
		val expected = """Puzzle(
			|A1 -> None | A2 -> None | A3 -> None | A4 -> None | A5 -> None | A6 -> None | A7 -> None | A8 -> None | A9 -> None
			|B1 -> None | B2 -> None | B3 -> None | B4 -> None | B5 -> None | B6 -> None | B7 -> None | B8 -> None | B9 -> None
			|C1 -> None | C2 -> None | C3 -> None | C4 -> None | C5 -> None | C6 -> None | C7 -> None | C8 -> None | C9 -> None
			|D1 -> None | D2 -> None | D3 -> None | D4 -> None | D5 -> None | D6 -> None | D7 -> None | D8 -> None | D9 -> None
			|E1 -> None | E2 -> None | E3 -> None | E4 -> None | E5 -> None | E6 -> None | E7 -> None | E8 -> None | E9 -> None
			|F1 -> None | F2 -> None | F3 -> None | F4 -> None | F5 -> None | F6 -> None | F7 -> None | F8 -> None | F9 -> None
			|G1 -> None | G2 -> None | G3 -> None | G4 -> None | G5 -> None | G6 -> None | G7 -> None | G8 -> None | G9 -> None
			|H1 -> None | H2 -> None | H3 -> None | H4 -> None | H5 -> None | H6 -> None | H7 -> None | H8 -> None | H9 -> None
			|I1 -> None | I2 -> None | I3 -> None | I4 -> None | I5 -> None | I6 -> None | I7 -> None | I8 -> None | I9 -> None
			|)""".stripMargin;
		
		Puzzle ().shows should be === (expected);
	}
	
	it should "be empty when constructed" in
	{
		Puzzle () should be ('empty);
		Puzzle () should not be ('complete);
	}

	it should "be able to assign a square" in
	{
		val puzzle = Puzzle ();
		val updated = puzzle.update (Square ('D', 3), 2);

		updated ('D', 3).flatMap (_.assignment) should be === (Some (2));
	}
	
	it should "be able to assign multiple squares" in
	{
		val puzzle = Puzzle () ++ (
			Square ('A', 1, Some (42)) ::
			Square ('B', 2, Some (99)) ::
			Nil
			);
		
		puzzle ('A', 1) should be === (Some (Square ('A', 1, Some (42))));
		puzzle ('A', 2) should be === (None);
		puzzle ('B', 2) should be === (Some (Square ('B', 2, Some (99))));
		puzzle ('B', 3) should be === (None);
	}
}
