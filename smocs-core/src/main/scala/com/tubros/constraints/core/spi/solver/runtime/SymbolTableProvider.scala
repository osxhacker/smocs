/**
 * Created on: Mar 11, 2013
 */
package com.tubros.constraints.core.spi.solver.runtime


/**
 * The '''SymbolTableProvider''' type defines the ability to provide a
 * [[com.tubros.constraints.core.spi.solver.runtime.SymbolTable]] in a manner
 * which fully insulates clients from ''how'' this is achieved.  Effectively,
 * it is a functional-style model of the FACTORY pattern.
 *
 * @author svickers
 *
 */
trait SymbolTableProvider
{
	def symbols : SymbolTable;
}
