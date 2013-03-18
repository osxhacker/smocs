/**
 * Created on: Mar 13, 2013
 */
package com.tubros.constraints.core.spi.solver
package runtime

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._


/**
 * The '''SymbolTableSpec''' type defines the unit tests responsible for
 * certifying the [[com.tubros.constraints.core.spi.solver.runtime.SymbolTable]]
 * abstraction for use in CSP implementations.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SymbolTableSpec
	extends ProjectSpec
{
	"A SymbolTable" should "be able to be constructed with nothing in it" in
	{
		val table = SymbolTable.empty;
		
		table must not be === (null);
		table ('unknown) should be ('empty);
		table.contains ('anyName) should be === (false);
	}
	
	it should "support symbol name addition" in
	{
		val entry = VariableName ('aName);
		val oneEntry = SymbolTable.empty addSymbol (entry);

		oneEntry.contains (entry) should be === (true);
		oneEntry (entry) should be === (Set (entry));
	}
	
	it should "support dependant definitions" in
	{
		val dependant = (
			SymbolTable.empty
				addSymbol ('a)
				addSymbol ('b)
				addDerivedSymbol ('c, Set ('a, 'b))
				);
		
		dependant ('c) should not be ('empty);
		dependant.contains ('c) should be === (true);
		dependant ('c) should be === (Set ('a, 'b));
	}
	
	it should "support nested dependant definitions" in
	{
		val nested = (
			SymbolTable.empty
				addDerivedSymbol ('d, Set ('c))
				addDerivedSymbol ('c, Set ('a, 'b))
				addSymbol ('b)
				addSymbol ('a)
				);
		
		nested.contains ('d) should be === (true);
		nested ('d) should not be ('empty);
		nested ('d) should be === (Set ('a, 'b));
	}
}
