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
		
		table shouldNot be (null);
		table ('unknown) should be ('empty);
		table.contains ('anyName) shouldBe (false);
	}
	
	it should "support symbol name addition" in
	{
		val entry = VariableName ('aName);
		val oneEntry = SymbolTable.empty addSymbol (entry);

		oneEntry.contains (entry) shouldBe (true);
		oneEntry (entry) shouldBe (Set (entry));
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
		dependant.contains ('c) shouldBe (true);
		dependant ('c) shouldBe (
			Set (VariableName ('a), VariableName ('b))
			);
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
		
		nested.contains ('d) shouldBe (true);
		nested ('d) should not be ('empty);
		nested ('d) shouldBe (
			Set (VariableName ('a), VariableName ('b))
			);
	}
	
	it should "be able to determine what symbols are dependants" in
	{
		val dependant = (
			SymbolTable.empty
				addSymbol ('a)
				addSymbol ('b)
				addDerivedSymbol ('c, Set ('a, 'b))
				);
		
		dependant.isDerived ('a) shouldBe (false);
		dependant.isDerived ('c) shouldBe (true);
	}
	
	it should "be able to resolve dependants available when expected" in
	{
		val dependant = (
			SymbolTable.empty
				addSymbol ('a)
				addSymbol ('b)
				addDerivedSymbol ('c, Set ('a, 'b))
				);
		
		dependant.derivedFrom (Set[VariableName] ('a, 'b)) shouldBe (
			Set (VariableName ('c))
			);
	}
	
	it should "not resolve a dependant unless all roots are available" in
	{
		val dependant = (
			SymbolTable.empty
				addSymbol ('a)
				addSymbol ('b)
				addDerivedSymbol ('c, Set ('a, 'b))
				);
		
		dependant.derivedFrom (Set[VariableName] ('a)) should be ('empty);
	}
	
	it should "not resolve a dependant unless all participants are available" in
	{
		val dependant = (
			SymbolTable.empty
				addSymbol ('a)
				addSymbol ('b)
				addSymbol ('d)
				addDerivedSymbol ('c, Set ('a, 'e))
				addDerivedSymbol ('e, Set ('d))
				);
		
		dependant.derivedFrom (Set[VariableName] ('a, 'b)) should be ('empty);
	}
}
