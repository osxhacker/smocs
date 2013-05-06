/**
 * Created on: Mar 9, 2013
 */
package com.tubros.constraints.core.spi.solver.runtime

import scala.language.postfixOps

import scalaz._

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.config.GraphConfig
import scalax.collection.constrained._
import scalax.collection.constrained.constraints.Acyclic

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
	/// Instance Properties
	/**
	 * The isEmpty property indicates whether or not there are any symbolic
	 * [[com.tubros.constraints.api.VariableName]]s known in this instances.
	 */
	def isEmpty : Boolean;
	
	/**
	 * The size property reports to the caller how many distinct
	 * [[com.tubros.constraints.api.VariableName]]s are currently managed by
	 * the '''SymbolTable'''.
	 */
	def size : Int;
	
	
	/**
	 * A '''name''' which is '''definedBy''' one or more other
	 * [[com.tubros.constraints.api.VariableName]]s is managed by the
	 * addDerivedSymbol method.
	 */
	def addDerivedSymbol (name : VariableName, definedBy : Set[VariableName])
		: SymbolTable;
	
	
	/**
	 * The addSymbol method is available for '''name'''s which are atomic in
	 * their definition.
	 */
	def addSymbol (name : VariableName) : SymbolTable;
	
	
	/**
	 * The apply method resolves all constituent
	 * [[com.tubros.constraints.api.VariableName]]s contributing to the
	 * definition of an arbitrary '''name''', if any.
	 */
	def apply (name : VariableName) : Set[VariableName];
	
	
	/**
	 * The contains method provides the expected behaviour of other like-named
	 * methods in Scala containers, in that the '''name''' is verified to be
	 * known to the '''SymbolTable''' or not.
	 */
	def contains (name : VariableName) : Boolean;
	
	
	/**
	 * The derivedFrom method resolves what, if any, derived
	 * [[com.tubros.constraints.api.VariableName]]s are ''completely'' defined
	 * by the '''names''' given.  Note that this includes derived
	 * [[com.tubros.constraints.api.VariableName]]s which only need a subset of
	 * those specified.
	 */
	def derivedFrom (names : Set[VariableName]) : Set[VariableName];
	
	
	/**
	 * The isDerived method indicates whether or not the given '''name'''
	 * references a [[com.tubros.constraints.api.solver.Variable]] which is
	 * defined in terms of one or more ''other'' 
	 * [[com.tubros.constraints.api.solver.Variable]]s.
	 */
	def isDerived (name : VariableName) : Boolean;
}


object SymbolTable
{
	/// Class Types
	private case class DefaultSymbolTable (
		private val symbolGraph : DAG[VariableName],
		override val size : Int
		)
		extends SymbolTable
	{
		/// Class Imports
		import std.anyVal._
		import syntax.equal._
		import syntax.std.boolean._
		
		
		/// Instance Properties
		override lazy val isEmpty : Boolean = (size === 0);
		
		
		override def addDerivedSymbol (
			name : VariableName,
			definedBy : Set[VariableName]
			)
			: SymbolTable =
		{
			require (
				!definedBy.isEmpty,
				"Derived symbol %s' must have symbols which define it".format (
					name
					)
				);
			
			copy (
				symbolGraph = symbolGraph ++ definedBy.map (p => name ~> p),
				size = size + 1
				);
		}
		
		
		override def addSymbol (name : VariableName) : SymbolTable =
			copy (symbolGraph = symbolGraph + name, size = size + 1);
		
		
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
		
		
		override def derivedFrom (names : Set[VariableName])
			: Set[VariableName] =
		{
			def definers (sink : collection.mutable.Set[VariableName])
				(cur : symbolGraph.NodeT)
			{
				val inbound = cur.inNeighbors;
				
				names.contains (cur.value).unless {
					sink += cur.value;
					}
				
				inbound foreach (definers (sink));
			}
			
			val derived = new collection.mutable.HashSet[VariableName];
			
			names flatMap (name => symbolGraph.find (name)) foreach {
				node =>
					
				definers (derived) (node);
				}
			

			return (derived.filter (hasAllDefiningSymbols (names)).toSet);
		}
		
		
		override def isDerived (name : VariableName) : Boolean =
			symbolGraph.find (name).filterNot (_.outgoing.isEmpty).isDefined;
		
		
		@inline
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
		
		
		@inline
		private def hasAllDefiningSymbols (available : Set[VariableName])
			(name : VariableName)
			: Boolean =
		{
			val roots = apply (name);
			
			return (roots.forall (available.contains));
		}
	}
	
	
	/// Instance Properties
	implicit private val config : GraphConfig = Acyclic;
	
	
	val empty : SymbolTable = DefaultSymbolTable (Graph.empty[VariableName, DiEdge], 0);
}

