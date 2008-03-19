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
val dualArgs : (Arg.key * Arg.key * Arg.spec * Arg.doc) list ->
  (Arg.key * Arg.spec * Arg.doc) list

val versionspec : Arg.key * Arg.key * Arg.spec * Arg.doc

val getModName : string -> string
  
val error : string -> 'a

val inputName : string ref
val setInputName : ?filter:(string -> string) -> string -> unit
val ensureInputName : unit -> unit
