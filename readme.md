# Almique - Python SME to VHDL transpiler #

Almique is a Python SME (Synchronous Message Exchange) to VHDL transpiler intended
to aid quick prototyping and testing of hardware designs. This work is performed
as a master project at the Niels Bohr Institute, University of Copenhagen

## Synchronous Message Exchange ##
Synchronous Message Exchange (SME) is a globally synchronous message passing
model desigend to address some of the the productivity issues associated with
traditional hardware design workflows. SME has a direct equivalence in the CSP
model, but only supports a single broadcasting (`any-to-any` type) channel type
where message propagation is is globally synchronous, which mimics the clocked
signal propagation that is found in hardware.

## References ##
 * Synchronous Message Exchange for Hardware Designs, Brian Vinter and Kenneth Skovhede, *Communicating Sequential Processes* 2014
 * Bus Centric Synchronous Message Exchange, Brian Vinter and Kenneth Skovhede, *Communicating Sequential Processes* 2015

## Licensing ##
Copyright 2016 Truls Asheim

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
