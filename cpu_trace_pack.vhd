-- Authoren : Can SEYMAN
library WORK;
use work.cpu_defs_pack.ALL;
use std.textio.all;
package cpu_trace_pack is

--Can Begin
constant mnemonic_nop : string  := "NOPE";
constant mnemonic_stop: string := "STOP";
constant mnemonic_ldc: string := "LDC";
constant mnemonic_ldd: string := "LDD";
constant mnemonic_ldr: string := "LDR";
constant mnemonic_std: string := "STD";
constant mnemonic_str: string := "STR";
constant mnemonic_add: string := "ADDX";
constant mnemonic_addc: string := "ADDC";
constant mnemonic_sub: string := "SUBX";
constant mnemonic_subc: string := "SUBC";
constant mnemonic_not: string := "NOT";
constant mnemonic_and: string := "AND";
constant mnemonic_or: string := "OR";
constant mnemonic_rea: string := "REA";
constant mnemonic_reo: string := "REO";
constant mnemonic_rex: string := "REX";
constant mnemonic_xor: string := "XOR";
constant mnemonic_sll	: string := "SLL";
constant mnemonic_srl	: string := "SRL";
constant mnemonic_sra	: string := "SRA";
constant mnemonic_rol	: string := "ROLX";
constant mnemonic_rolc	: string := "ROLC";
constant mnemonic_ror	: string := "RORX";
constant mnemonic_rorc	: string := "RORC";
constant mnemonic_in	: string := "IN";
constant mnemonic_out	: string := "OUT";
constant mnemonic_jmp	: string := "JMP";
constant mnemonic_jz	: string := "JZ";
constant mnemonic_jc	: string := "JC";
constant mnemonic_jn	: string := "JNX";
constant mnemonic_jo	: string := "JO";
constant mnemonic_jnz	: string := "JNZ";
constant mnemonic_jnc	: string := "JNC";
constant mnemonic_jnn	: string := "JNN";
constant mnemonic_jno	: string := "JNO";

constant mnemonic_R0: string := "R0";
constant mnemonic_R1: string := "R1";
constant mnemonic_R2: string := "R2";
constant mnemonic_R3: string := "R3";

procedure print_tail( variable f : out text ); 

procedure print_header( variable f : out text ); 

function bool_character (b:Boolean)
return character;

procedure
 write_PC_CMD (variable l: inout line;
constant PC: in data_type;
constant OP: in opcode_type;
constant X,Y,Z: in reg_addr_type);   

function hex_image ( d : data_type)
return string;

function cmd_image (cmd : opcode_type)
return string;

procedure write_param (
    variable l: inout line;
    constant Data: in data_type);

procedure write_noparam(
    variable l: inout line);

procedure write_regs (variable l: inout line;
constant Reg: in reg_type;
constant Zero,Carry, Negative,Overflow : in BOOLEAN);   


procedure init_memory(
    variable f : in text;
    variable mem: out mem_type);

function opcode_mnemonic_to_integer (mnem : string)
return opcode_type;

function register_mnemonic_to_integer (mnem : string)
return reg_addr_type;

function mnemonic_to_integer (mnem : string)
return data_type;

procedure init_memory_mnemonic(
    variable f : in text;
    variable mem: out mem_type);

type Operation;
type Queue_Element_Ptr is access Operation;
type Operation is record
    Mnemonic_OP : String (1 to 6);
    Mnemonic_X : String (1 to 2);
    Mnemonic_Y : String (1 to 2);
    Mnemonic_Z : String (1 to 2);
    Integer_Param : data_type;
end record;

procedure dump_memory(
    variable mem: in mem_type;
    variable f : out text );


end cpu_trace_pack;
---------------------------------------------------------------------------------------------------------
package body cpu_trace_pack is

procedure
 print_tail( variable f : out text ) is
variable l:line;
begin
write( l , string'("-----------------------------------------------------------------------------")); 
writeline (f,l);
end print_tail;

procedure
 print_header( variable f : out text ) is
variable l:line;
begin
write( l , string'("PC  | CMD  | XYZ |  P  | R0   | R 1  | R 2  | R3   | ZCNO ")); 
writeline (f,l);
end print_header;

function bool_character (b:Boolean)
return character is 
    begin
        if b then return 'T';
        else return 'F';
end if;
end bool_character;



procedure
write_PC_CMD (variable l: inout line;
constant PC: in data_type;
constant OP: in opcode_type;
constant X,Y,Z: in reg_addr_type) is
begin
write( l , hex_image (PC), left, 3);
write( l , string'(" | ") );
write( l , cmd_image (OP), left, 4);
write( l , string'(" | ") );
write( l , X, left, 1);
write( l , Y, left, 1);
write( l , Z, left, 1);
write( l , string'(" | ") );
end
write_PC_CMD;

function hex_image ( d : data_type)
return string is
constant hex_table : string(1 to 16):= "0123456789ABCDEF";
variable result : string( 1 to 3 );
begin
result(3):=hex_table(d mod 16 + 1);
result(2):=hex_table((d / 16) mod 16 + 1);
result(1):=hex_table(d / 256 + 1);
return result;
end hex_image;

function cmd_image (cmd : opcode_type)
return string is
    begin
    case cmd is
    when code_nop => return mnemonic_nop;
    when code_ldc => return mnemonic_ldc;
    when code_ldd => return mnemonic_ldd;
    when code_ldr => return mnemonic_ldr;
    when code_std => return mnemonic_std;
    when code_str => return mnemonic_str;
    when code_stop => return mnemonic_stop;
    when code_add => return mnemonic_add;
    when code_addc => return mnemonic_addc;
    when code_sub => return mnemonic_sub;
    when code_subc => return mnemonic_subc;
    when code_jmp	=> return mnemonic_jmp;
    when code_jz	=> return mnemonic_jz;
    when code_jc	=> return mnemonic_jz;
    when code_jn	=> return mnemonic_jn;
    when code_jo	=> return mnemonic_jo;
    when code_jnz	=> return mnemonic_jnz;
    when code_jnc	=> return mnemonic_jnc;
    when code_jnn	=> return mnemonic_jnn;
    when code_jno	=> return mnemonic_jno;
    when code_not	=> return mnemonic_not;
    when code_and	=> return mnemonic_and;
    when code_or	=> return mnemonic_or;
    when code_xor	=> return mnemonic_xor;
    when code_rea	=> return mnemonic_rea;
    when code_reo	=> return mnemonic_reo;
    when code_rex	=> return mnemonic_rex;
    when code_in	=> return mnemonic_in;
    when code_out	=> return mnemonic_out;
    when code_sll	=> return mnemonic_sll;
    when code_srl	=> return mnemonic_srl;
    when code_sra	=> return mnemonic_sra;
    when code_rol	=> return mnemonic_rol;
    when code_rolc	=> return mnemonic_rolc;
    when code_ror	=> return mnemonic_ror;
    when code_rorc	=> return mnemonic_rorc;
    
    when others =>
        assert FALSE
        report "Illegal command in cmd_image"
        severity warning;
        return "";
end case;
end cmd_image;

procedure write_param (
    variable l: inout line;
    constant Data: in data_type) is
begin
    write( l , hex_image (data));
    write( l , string'(" | ") );
    end write_param;
    
    
    procedure write_noparam (
    variable l: inout line) is
begin
    write( l , string'("--- | ") );
  end write_noparam;

procedure write_regs (variable l: inout line;
constant Reg: in reg_type;
constant Zero,Carry, Negative,Overflow : in BOOLEAN) is
begin
write( l , reg(0) , left, 4);
write( l , string'(" | ") );
write( l , reg(1), left, 4);
write( l , string'(" | ") );
write( l , reg(2), left, 4);
write( l , string'(" | ") );
write( l , reg(3), left, 4);
write( l , string'(" | ") );
write( l , bool_character(Zero) );
write( l , bool_character(Carry) );
write( l , bool_character(Negative) );
write( l , bool_character(Overflow) );
end write_regs;

procedure init_memory(
    variable f : in text;
    variable mem: out mem_type) is

variable l : line;
variable success : boolean;
variable v : data_type;
variable i : addr_type := 0;
    begin
outest : loop 
exit when endfile (f);
readline (f, l);
success := TRUE;

while success loop
    read (l, v, success);
        if success then
            mem (i):=v;
        exit outest when i = 2**addr_width -1;
        i := i+1;
    end if;
end loop;
end loop;
end init_memory;
--
procedure init_memory_mnemonic(
    variable f : in text;
    variable mem: out mem_type) is

variable l : line;
variable success_b, success_c, success_d, success_v: boolean;
variable v,a,b,c,d : data_type :=0;
variable i : addr_type := 0;
variable k : string(1 to 4);
variable tbs : string(1 to 2);
    begin
outest : loop 
exit when endfile (f);
readline (f, l);
success_v := false;
a := 0;
b := 0;
c := 0;
d := 0;
v := 0;
read (l, k);
a := ( mnemonic_to_integer(k));
read (l, v, success_v);
    if success_v then
   -- i := i+1;
        wait for 0 ns;  wait for 0 ns;

    mem(i+1) := v;
    else
 read (l, tbs, success_b); 
if success_b then
b := mnemonic_to_integer(tbs);
else b := 0; end if;
 read (l, v, success_v);
    if success_v then
  --  i := i+1;
        wait for 0 ns;  wait for 0 ns;

    mem(i+1) := v;
    else
 read (l, tbs, success_c); 
    if success_c then
c := mnemonic_to_integer(tbs);
else c := 0; end if;
      read (l, v, success_v);
    if success_v then
     --i := i+1;
        wait for 0 ns;  wait for 0 ns;

    mem(i+1) := v;
    else
       read (l, tbs, success_d); 
if success_d then
d := mnemonic_to_integer(tbs);
else d := 0; end if;
       end if;
       end if;
 end if; 
 wait for 0 ns;  wait for 0 ns;
 
    
 
mem(i) :=  (64* a + 16 * b + 4* c + d);

--readline (f, l_i);
--while loo < 2 loop
--read (l, v, success_v);
  --  if success_v then
    -- i := i+1;
      --  wait for 0 ns;  wait for 0 ns;

    --mem(i) := v;
   --  loo :=4 ;
     --else 
    --loo := loo +1 ;
   -- end if;
 --end loop;

 exit outest when i = 2**addr_width -1;
 wait for 0 ns;  wait for 0 ns;
      if success_v then
          i := i + 2;
          else i := i + 1;
      end if;

end loop;
end init_memory_mnemonic;
--


function opcode_mnemonic_to_integer (mnem : string)
return opcode_type is
 begin
    case mnem is
    when mnemonic_nop  => return code_nop;
    when mnemonic_ldc => return code_ldc;
    when mnemonic_ldd => return code_ldd;
    when mnemonic_ldr => return code_ldr;
    when mnemonic_std => return code_std;
    when mnemonic_str => return code_str;
    when mnemonic_stop => return code_stop;
    when mnemonic_add => return code_add;
    when mnemonic_addc => return code_addc;
    when mnemonic_sub => return code_sub;
    when mnemonic_subc => return code_subc;
    
    when others =>
        assert FALSE
        report "Illegal command in opcode_mnemonic_to_integer"
        severity warning;
        return code_nop;
end case;
end opcode_mnemonic_to_integer;

function register_mnemonic_to_integer (mnem : string)
return reg_addr_type is
    begin
    case mnem is
        when mnemonic_R0  => return 0;
        when mnemonic_R1 => return 1;
        when mnemonic_R2 => return 2;
        when mnemonic_R3 => return 3;
    end case;
end register_mnemonic_to_integer;

function mnemonic_to_integer (mnem : string)
return data_type is
 begin
    case mnem is
    when mnemonic_nop  => return code_nop;
    when mnemonic_ldc => return code_ldc;
    when mnemonic_ldd => return code_ldd;
    when mnemonic_ldr => return code_ldr;
    when mnemonic_std => return code_std;
    when mnemonic_str => return code_str;
    when mnemonic_stop => return code_stop;
    when mnemonic_add => return code_add;
    when mnemonic_addc => return code_addc;
    when mnemonic_sub => return code_sub;
    when mnemonic_subc => return code_subc;
    when mnemonic_jmp	=> return code_jmp;
    when mnemonic_jz	=> return code_jz;
    when mnemonic_jc	=> return code_jc;
    when mnemonic_jn	=> return code_jn;
    when mnemonic_jo	=> return code_jo;
    when mnemonic_jnz	=> return code_jnz;
    when mnemonic_jnc	=> return code_jnc;
    when mnemonic_jnn	=> return code_jnn;
    when mnemonic_jno	=> return code_jno;
    when mnemonic_not	=> return code_not;
    when mnemonic_and	=> return code_and;
    when mnemonic_or	=> return code_or;
    when mnemonic_xor	=> return code_xor;
    when mnemonic_rea	=> return code_rea;
    when mnemonic_reo	=> return code_reo;
    when mnemonic_rex	=> return code_rex;
    when mnemonic_in	=> return code_in;
    when mnemonic_out	=> return code_out;
    when mnemonic_sll	=> return code_sll;
    when mnemonic_srl	=> return code_srl;
    when mnemonic_sra	=> return code_sra;
    when mnemonic_rol	=> return code_rol;
    when mnemonic_rolc	=> return code_rolc;
    when mnemonic_ror	=> return code_ror;
    when mnemonic_rorc	=> return code_rorc;
    when mnemonic_R0  => return 0;
    when mnemonic_R1 => return 1;
    when mnemonic_R2 => return 2;
    when mnemonic_R3 => return 3;
    
    when others =>
        return 0;
        assert FALSE
        report "Illegal command in mnemonic_to_integer"
        severity warning;
        
end case;
end mnemonic_to_integer;

procedure dump_memory(
    variable mem: in mem_type;
    variable f : out text ) is
    variable l : line;
        begin
        write (l, string'("ADDR"));
        write(l, string'(" | "));
        write (l, string'("MEM"));
        writeline(f, l);
        write( l , string'("------------------------------------------------------------------")); 
        writeline(f, l);
        
        for mm in 0 to (2**addr_width)-1 loop
         write(l, mm,left, 4);
         write(l, string'(" | "));
         write( l , mem(mm),left, 4);
         writeline(f, l);
         end loop;
  
end dump_memory;
    

--Can END
end cpu_trace_pack;