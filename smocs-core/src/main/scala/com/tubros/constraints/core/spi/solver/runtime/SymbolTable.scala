/**
 * Created on: Mar 9, 2013
 */
package com.tubros.constraints.core.spi.solver.runtime

import scala.language.postfixOps

import scalax.collection.GraphPredef._
import scalax.collection.GraphTraversal.VisitorReturn._
import scalax.collection.constrained._
import scalax.collection.constrained.constraints.Acyclic

import scalaz._

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._


/**
 * The '''SymbolTable''' type defines the contract for resolving
 * [[com.tubros.constraints.api.VariableName]]s and the
 * [[com.tubros.constraints.core.internal.runtime.VariableDescriptor]]s which
 * capture their meta-data.
 *
 * @author svickers
 *
 */
trait SymbolTable
{
	def addDerivedSymbol (name : VariableName, definedBy : Set[VariableName])
		: SymbolTable;
	
	
	def addSymbol (name : VariableName) : SymbolTable;
	
	
	def apply (name : VariableName) : Set[VariableName];
	
	
	def contains (name : VariableName) : Boolean;
	
	
	def isEmpty : Boolean;
}


object SymbolTable
{
	/// Class Types
	private case class DefaultSymbolTable (
		private val symbolGraph : DAG[VariableName]
		)
		extends SymbolTable
	{
		/// Class Imports
		import syntax.std.boolean._
		
		
		override def addDerivedSymbol (
			name : VariableName,
			definedBy : Set[VariableName]
			)
			: SymbolTable =
			copy (symbolGraph = symbolGraph ++ definedBy.map (p => name ~> p));
		
		
		override def addSymbol (name : VariableName) : SymbolTable =
			copy (symbolGraph = symbolGraph + name);
		
		
		override def apply (name : VariableName) : Set[VariableName] =
			symbolGraph.find (name).fold (Set.empty[VariableName]) {
				node =>
					
				node.outNeighbors.isEmpty.fold (
					Set (node.value),
					findLeaves (node)
					);
				}
		
		
		override def contains (name : VariableName) : Boolean =
			symbolGraph contains (name);
		
		
		override def isEmpty : Boolean = symbolGraph isEmpty;
		
		
		private def findLeaves (node : symbolGraph.NodeT) : Set[VariableName] =
		{
			def collector (dest : collection.mutable.Set[VariableName])
				(cur : symbolGraph.NodeT)
			{
				val outbound = cur.outNeighbors;
				
				outbound.isEmpty.fold (
					dest += cur.value,
					outbound foreach (collector (dest))
					);
				}
			
			val leaves = new collection.mutable.HashSet[VariableName];
			
			collector (leaves) (node);
			
			return (leaves.toSet);
		}
	}
	
	
	/// Instance Properties
	implicit private val config : Config = Acyclic;
	
	
	def empty : SymbolTable = DefaultSymbolTable (DAG[VariableName] ());
}
