(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
****************************************************************************)
(****************************************************************************
* This file is part of Teyjus.
*
* Teyjus is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Teyjus is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************)
(**********************************************************************
*Table
**********************************************************************)
module SymbolTable : Map.S with type key = Symbol.symbol

type 'a symboltable = 'a SymbolTable.t

val find : SymbolTable.key -> 'a SymbolTable.t -> 'a option
val add : SymbolTable.key -> 'a -> 'a SymbolTable.t -> 'a SymbolTable.t
val remove : SymbolTable.key -> 'a SymbolTable.t -> 'a SymbolTable.t
val iter : (SymbolTable.key -> 'a -> unit) -> 'a SymbolTable.t -> unit
val fold : (SymbolTable.key -> 'a -> 'b -> 'b) -> 'a SymbolTable.t -> 'b -> 'b
val empty : 'a symboltable

val printTable : ('a -> string) -> 'a SymbolTable.t -> unit 
