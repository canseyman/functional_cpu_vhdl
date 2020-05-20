--Authoren : Can

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library WORK;

-- Can Begin
entity System is
--  Port ( );
end System;

architecture Functional of System is

begin
p_main : PROCESS
use WORK.cpu_defs_pack.all;
use work.cpu_trace_pack.ALL;
use std.textio.all;
file TraceFile : Text is out "Trace.txt";
file DumpFile : Text is out "Dump.txt";
file MemoryFile : Text is in "Memory.txt";
file IOInputFile : Text is in "IOInput.txt";
file IOOutputFile: Text is out "IOOutput.txt";
variable l : line;

variable Memory : mem_type;

variable Reg : reg_type;
variable INSTR, data : data_type := 0; --=> 2.1.2.2
variable OP : opcode_type;
variable X, Y, Z : reg_addr_type; 
variable PC : addr_type :=0; -- => 2.1.3.2. 
variable Zero, Carry, Negative, Overflow : Boolean := FALSE; -- =>  3.2.1, 3.2.2, 3.2.4.2., 3.2.5.7., 3.2.6.3., 3.2.7.4.
begin
init_memory_mnemonic(MemoryFile ,memory);
print_header( TraceFile );
print_tail(TraceFile);
loop
Instr := Memory(PC); 
OP := Instr / (2**reg_addr_width)**3;
X := (Instr / (2**reg_addr_width)**2) mod 2**reg_addr_width;
Y := (Instr / 2**reg_addr_width) mod 2**reg_addr_width;
Z := Instr mod 2**reg_addr_width;
write_PC_CMD(l, PC, Op, X, Y, Z);

PC := (PC+1) mod 2**addr_width; -- => 2.1.3.3. 

case OP is
-- => 3.1.2.1. Begin
when code_nop => write_noParam(l); null; -- => 3.3.1.1.
when code_stop => write_noParam(l); write_regs (l, Reg, Zero, Carry, Negative, Overflow); writeline( TraceFile, l); print_tail( TraceFile ); dump_memory( Memory, DumpFile );  wait; -- => 3.3.1.2.
-- => 3.1.2.1. End

when code_add =>  write_noParam(l); EXEC_ADDC(Reg(Y), Reg(Z), FALSE, Reg(X), Zero, Carry, Negative, Overflow); -- => 3.3.1.3 
when code_addc => write_noParam(l); EXEC_ADDC(Reg(Y), Reg(Z), Carry, Reg(X), Zero, Carry, Negative, Overflow); -- => 3.3.1.4 
when code_sub =>  write_noParam(l); EXEC_SUBC(Reg(Y), Reg(Z), FALSE, Reg(X), Zero, Carry, Negative, Overflow); -- => 3.3.1.5 
when code_subc => write_noParam(l); EXEC_SUBC(Reg(Y), Reg(Z), Carry, Reg(X), Zero, Carry, Negative, Overflow); -- => 3.3.1.6


when code_not => write_noParam(l); Data := "NOT"( Reg(Y) ); Reg(X) := Data; Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow); -- => 3.3.1.7
when code_and => write_noParam(l); Data := "AND"(Reg(Y), Reg(Z)) ; Reg(X) := Data; Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow);  -- => 3.3.1.7
when code_or => write_noParam(l); Data := "OR"(Reg(Y), Reg(Z)) ; Reg(X) := Data; Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow);
when code_xor => write_noParam(l); Data := "XOR"(Reg(Y), Reg(Z)) ; Reg(X) := Data; Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow);
when code_rea => write_noParam(l); Data := reduced_and(Reg(Y)) ; Reg(X) := Data; Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow);
when code_reo => write_noParam(l); Data := reduced_or(Reg(Y)) ; Reg(X) := Data; Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow);
when code_rex => write_noParam(l); Data := reduced_xor(Reg(Y)) ; Reg(X) := Data; Set_Flags_Logic(Data,Zero,Carry,Negative,Overflow);

when code_in => write_noParam(l); read_io(IOInputFile, data, Zero, Negative, Overflow); Reg(X) := Data;
when code_out => write_noParam(l); data := Reg(X); write_io(IOOutputFile, data); 

when code_ldc => 
    write_Param(l,Memory(PC));
    Data := Memory(PC); 
    Reg(X) := Data; 
    Set_Flags_Load(Data,Zero,Negative,Overflow);
    PC := PC+1;
when
    code_ldd => 
    write_Param(l,Memory(Memory(PC)));
    Data := Memory(Memory(PC));
    Reg(X) := Data; 
    Set_Flags_Load(Data,Zero,Negative,Overflow);
    PC := PC+1;
when code_ldr => 
    write_noParam(l);
    Data :=Memory(Reg(Y)); 
    Reg(X) := Data;
    Set_Flags_Load(Data,Zero,Negative,Overflow);
when code_std => 
    write_Param(l,Memory(Memory(PC)));
    Memory(Memory(PC)):=Reg(X);
    PC := PC+1;
when code_str => 
    write_noParam(l);
    Memory(Reg(Y)):=Reg(X);
-- => 3.1.2.5. End
when code_jmp => write_Param(l,Memory(PC)); PC := Memory( PC );
when code_jz => write_Param(l,Memory(PC)); if zero then PC := Memory( PC ); else PC := PC + 1 ; end if;
when code_jnz => write_Param(l,Memory(PC)); if not zero then PC := Memory( PC ); else PC := PC + 1; end if;
when code_jc => write_Param(l,Memory(PC)); if carry then PC := Memory( PC ); else PC := PC + 1; end if;
when code_jnc => write_Param(l,Memory(PC)); if not carry then PC := Memory( PC ); else PC := PC + 1; end if;
when code_jn => write_Param(l,Memory(PC)); if negative then PC := Memory( PC ); else PC := PC + 1; end if;
when code_jnn => write_Param(l,Memory(PC)); if not negative then PC := Memory( PC ); else PC := PC + 1; end if;
when code_jo => write_Param(l,Memory(PC)); if overflow then PC := Memory( PC ); else PC := PC + 1; end if;
when code_jno => write_Param(l,Memory(PC)); if not overflow then PC := Memory( PC ); else PC := PC + 1; end if;




when code_sll	=>write_noParam(l);EXEC_SLL(Reg( Y ), Reg ( X ), Zero, Carry, Negative, Overflow);

when code_srl	=>write_noParam(l);EXEC_SRL(Reg( Y ), Reg ( X ), Zero, Carry, Negative, Overflow);

when code_sra	=>write_noParam(l);EXEC_SRA(Reg( Y ), Reg ( X ), Zero, Carry, Negative, Overflow);

when code_rol	=>write_noParam(l);EXEC_ROL(Reg( Y ), Reg ( X ), Zero, Carry, Negative, Overflow);

when code_rolc	=>write_noParam(l);EXEC_ROLC(Reg( Y ), Reg ( X ), Zero, Carry, Negative, Overflow);

when code_ror	=>write_noParam(l);EXEC_ROR(Reg( Y ), Reg ( X ), Zero, Carry, Negative, Overflow);

when code_rorc	=>write_noParam(l);EXEC_RORC(Reg( Y ), Reg ( X ), Zero, Carry, Negative, Overflow);


when others => 
assert FALSE -- => 3.3.5
report "Illegal Operation"
severity error;
end case;
write_regs (l, Reg, Zero, Carry, Negative, Overflow);
writeline( TraceFile, l );
end loop;
dump_memory( Memory, DumpFile );
end PROCESS;

end Functional;

-- Can End