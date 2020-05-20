
Library work;
use WORK.bit_vector_natural_pack.all;
use std.textio.all;
package cpu_defs_pack is
constant bus_width 	: natural := 12; 
constant data_width	: natural := bus_width; 
constant addr_width : natural := bus_width;

constant reg_addr_width : natural := 2;
constant opcode_width	: natural := 6;

subtype data_type is
	natural range 0 to 2**data_width-1; --> 2.1.2.1
subtype addr_type is
	natural range 0 to 2**addr_width-1;
subtype reg_addr_type is
	natural range 0 to 2**reg_addr_width-1;
subtype opcode_type is
	natural range 0 to 2**opcode_width-1;

type reg_type is array( reg_addr_type ) of data_type;
type mem_type is array( addr_type ) of data_type;
constant code_nop : opcode_type:= 0;
constant code_stop: opcode_type:= 1;

constant code_ldc: opcode_type:= 32; -- => 3.3.1.21
constant code_ldd: opcode_type:= 33; -- => 3.3.1.22
constant code_ldr: opcode_type:= 34; -- => 3.3.1.23
constant code_std: opcode_type:= 35; -- => 3.3.1.24
constant code_str: opcode_type:= 36; -- => 3.3.1.25
constant code_add: opcode_type:= 2; -- => 3.3.1.3
constant code_addc: opcode_type:= 3; -- => 3.3.1.4
constant code_sub: opcode_type:= 4; -- => 3.3.1.5
constant code_subc: opcode_type:= 5; -- => 3.3.1.6
constant code_jmp	: opcode_type := 48 ;
constant code_jz	: opcode_type := 49;
constant code_jc	: opcode_type := 50;
constant code_jn	: opcode_type := 51;
constant code_jo	: opcode_type := 52;
constant code_jnz	: opcode_type := 53;
constant code_jnc	: opcode_type := 54;
constant code_jnn	: opcode_type := 55;
constant code_jno	: opcode_type := 56;
constant code_not	: opcode_type := 6 ;
constant code_and	: opcode_type := 7;
constant code_or	: opcode_type := 8;
constant code_xor	: opcode_type := 9;
constant code_rea	: opcode_type := 10;
constant code_reo	: opcode_type := 11;
constant code_rex	: opcode_type := 12;
constant code_in	: opcode_type := 37;
constant code_out	: opcode_type := 38;

procedure Set_Flags_Load(
    data : in data_type;
    Z,N,O : out boolean);
    
procedure Set_Flags_Logic(
    data : in data_type;
    Z,C,N,O : out boolean);

procedure EXEC_ADDC (
    constant A,B : in data_type;
    constant CI : in Boolean;
    variable R : out data_type;
    variable Z,CO,N,O : out Boolean );

procedure EXEC_SUBC (
    constant A,B : in data_type;
    constant CI : in Boolean;
    variable R : out data_type;
    variable Z,CO,N,O : out Boolean );

function "NOT" (constant A :data_type)
return data_type;
function "AND" (constant A, B :data_type)
return data_type;
function "OR" (constant A, B :data_type)
return data_type;
function "XOR" (constant A, B :data_type)
return data_type;

function reduced_and (constant A :data_type)
return data_type;
function reduced_or (constant A:data_type)
return data_type;
function reduced_xor (constant A :data_type)
return data_type;

procedure read_io (
    variable f : in text;
    variable data : out data_type;
    variable Z,N,O : out Boolean);
    
procedure write_io (
    variable f : out text;
    variable data : in data_type);

constant code_sll	: opcode_type:= 13;-- => 3.3.1.13
constant code_srl	: opcode_type:= 14;-- => 3.3.1.14
constant code_sra	: opcode_type:= 15;-- => 3.3.1.15
constant code_rol	: opcode_type:= 16;-- => 3.3.1.16
constant code_rolc	: opcode_type:= 17;-- => 3.3.1.17
constant code_ror	: opcode_type:= 18;-- => 3.3.1.18
constant code_rorc	: opcode_type:= 19;-- => 3.3.1.19


--SLL
procedure EXEC_SLL ( constant A : in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean);
--SRL 
procedure EXEC_SRL (

	constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   );
--SRA
procedure EXEC_SRA (constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   );
--ROL
procedure EXEC_ROL (constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   );
--ROLC
procedure EXEC_ROLC (constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   );
--ROR
procedure EXEC_ROR (constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   );
--RORC
procedure EXEC_RORC (constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   );

end cpu_defs_pack;	

package body cpu_defs_pack is

--Can Begin
procedure Set_Flags_Load(
    data : in data_type;
    Z,N,O : out boolean) is
   variable data_s : data_type;
    begin
    if data >= 2**(data_width-1) then
data_s := data - 2**(data_width);
else
data_s := data;
   end if;
    if data mod 2**data_width = 0 then 
        Z := TRUE;
    else
        Z := FALSE;
end if;
    if data_s < 0 then
        N := TRUE;
    else
        N := FALSE;
end if;
    O := FALSE;

end Set_Flags_Load;


procedure Set_Flags_Logic(
    data : in data_type;
    Z,C,N,O : out boolean) is
   variable data_s : data_type;
    begin
    if data >= 2**(data_width-1) then
data_s := data - 2**(data_width);
else
data_s := data;
   end if;
    if data mod 2**data_width = 0 then 
        Z := TRUE;
    else
        Z := FALSE;
end if;
    if data_s < 0 then
        N := TRUE;
    else
        N := FALSE;
end if;
    O := FALSE;
    C := FALSE;
end Set_Flags_Logic;



procedure EXEC_ADDC (
    constant A,B : in data_type;
    constant CI : in Boolean;
    variable R : out data_type;
    variable Z,CO,N,O : out Boolean ) is
    
variable T: integer := A+B+Boolean'Pos(CI);
variable A_s, B_s, T_s : integer;
    begin
        if A >= 2**(data_width-1) then
            A_s := A - 2**(data_width);
        else
            A_s := A;
    end if;
        if B >= 2**(data_width-1) then
            B_s := B - 2**(data_width);
        else
            B_s := B;
    end if;

    T_s := A_s+B_s+Boolean'Pos(CI);
        if T >= 2**data_width then
            R := T - 2**data_width;
            CO := TRUE;
        else
            R := T;
            CO := FALSE;
    end if;
        if T mod 2**data_width = 0 then
            Z := TRUE;
        else
            Z := FALSE;
    end if;
        
        if T_s < 0 then
            N := TRUE;
        else
            N := FALSE;
    end if;
            
        if (T_s < -2**(data_width-1)) or (T_s >= 2**(data_width-1)) then
            O := TRUE;
        else
            O := FALSE;
    end if ;
end EXEC_ADDC;

procedure EXEC_SUBC (
    constant A,B : in data_type;
    constant CI : in Boolean;
    variable R : out data_type;
    variable Z,CO,N,O : out Boolean ) IS
variable T: integer := A-B-Boolean'Pos(CI);
variable A_s, B_s, T_s : integer;
    begin
        if A >= 2**(data_width-1) then
            A_s := A - 2**(data_width);
        else
            A_s := A;
    end if;
        if B >= 2**(data_width-1) then
            B_s := B - 2**(data_width);
        else
            B_s := B;
    end if;

    T_s := A_s-B_s-Boolean'Pos(CI);
        if T >= 2**data_width then
            R := T - 2**data_width;
            CO := TRUE;
        else
            R := T;
            CO := FALSE;
    end if;
        if T mod 2**data_width = 0 then
            Z := TRUE;
        else
            Z := FALSE;
    end if;
        
        if T_s < 0 then
            N := TRUE;
        else
            N := FALSE;
    end if;
            
        if (T_s < -2**(data_width-1)) or (T_s >= 2**(data_width-1)) then
            O := TRUE;
        else
            O := FALSE;
    end if ;
end EXEC_SUBC;


function "NOT" (constant A : data_type)
return data_type is
begin
return bit_vector2natural( NOT natural2bit_vector ( A , data_width ) );
end "NOT";

function "AND" (constant A,B : data_type)
return data_type is
begin
return bit_vector2natural(natural2bit_vector( A , data_width ) AND natural2bit_vector( B , data_width ) );
end "AND";

function "OR" (constant A,B : data_type)
return data_type is
begin
return bit_vector2natural(natural2bit_vector( A , data_width ) OR natural2bit_vector( B , data_width ) );
end "OR";

function "XOR" (constant A,B : data_type)
return data_type is
begin
return bit_vector2natural(natural2bit_vector( A , data_width ) XOR natural2bit_vector( B , data_width ) );
end "XOR";

function reduced_and (constant A :data_type)
return data_type is
variable v : bit_vector(0 to data_width - 1);
variable t : bit_vector(0 to 0 );
variable data : data_type;
begin
v := natural2bit_vector ( A , data_width );
for i in v'range loop
t(0) := t(0) and v(i);
end loop;
 data :=  (bit_vector2natural(t));
            return data;
end reduced_and;

function reduced_or (constant A:data_type)
return data_type is
variable v : bit_vector(0 to data_width - 1);
variable t : bit_vector(0 to 0 );
variable data : data_type;
begin
v := natural2bit_vector ( A , data_width );
for i in v'range loop
t(0) := t(0) or v(i);
end loop;
 data :=  (bit_vector2natural(t));
            return data;
end reduced_or;

function reduced_xor (constant A :data_type)
return data_type is
variable v : bit_vector(0 to data_width - 1);
variable t : bit_vector(0 to 0 );
variable data : data_type;
begin
v := natural2bit_vector ( A , data_width );
for i in v'range loop
t(0) := t(0) xor v(i);
end loop;
 data :=  (bit_vector2natural(t));
            return data;
end reduced_xor;
   
procedure read_io (
    variable f : in text;
    variable data : out data_type;
    variable Z,N,O : out Boolean) is
    variable l : line;
    variable data_s : data_type;
        begin
        readline (f,l);
        read(l, data);
         if data >= 2**(data_width-1) then
data_s := data - 2**(data_width);
else
data_s := data;
   end if;
    if data mod 2**data_width = 0 then 
        Z := TRUE;
    else
        Z := FALSE;
end if;
    if data_s < 0 then
        N := TRUE;
    else
        N := FALSE;
end if;
    O := FALSE;
end read_io;
    
procedure write_io (
    variable f : out text;
    variable data : in data_type) is
    variable l : line;
        begin
   write(l, data);
   writeline(f,l);
   
end write_io;   



--SLL
procedure EXEC_SLL (

	constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   )
is
	variable s:  bit_vector (11 downto 0);
    variable L:  bit_vector (11 downto 0);
    
begin 
	
	s := natural2bit_vector( A, 12 );
	CO := s(s'left) = '1';
	O  := ( s(s'left) xor s(s'left-1) ) ='1';
	L  := s(s'left-1 downto 0) & '0';
	R  := bit_vector2natural(L);
	  if R mod 2**data_width = 0 then
            Z := TRUE;
        else
            Z := FALSE;
    end if;
        
        if R < 0 then
            N := TRUE;
        else
            N := FALSE;
    end if;
end EXEC_SLL;	
--
--SRL
procedure EXEC_SRL (

	constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   )
     is
	variable s:  bit_vector (11 downto 0);
    variable L:  bit_vector (11 downto 0);
    
begin 
    s  := natural2bit_vector( A, 12 );
	CO := s(s'right) = '1';
	O  := FALSE;
	L  := '0' & s(s'left downto 1);
	R  := bit_vector2natural(L);
	
	  if R mod 2**data_width = 0 then
            Z := TRUE;
        else
            Z := FALSE;
    end if;
        
        if R < 0 then
            N := TRUE;
        else
            N := FALSE;
    end if;
end EXEC_SRL;	


--SRA
procedure EXEC_SRA (


	constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   )
is
	
	variable s:  bit_vector (11 downto 0);
    variable L:  bit_vector (11 downto 0);
    
begin 
	
	s := natural2bit_vector( A, 12 );
	CO := s(s'right) = '1';
	O  := FALSE;
	L  := s(s'left) & s(s'left downto 1);
	R  := bit_vector2natural(L);
	 if R mod 2**data_width = 0 then
            Z := TRUE;
        else
            Z := FALSE;
    end if;
        
        if R < 0 then
            N := TRUE;
        else
            N := FALSE;
    end if;
end EXEC_SRA;

--ROL
procedure EXEC_ROL (constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   )
is

	variable s:  bit_vector (11 downto 0);
	variable L:  bit_vector (11 downto 0);
    variable b:  bit ; 
    
begin 
	
	s := natural2bit_vector( A, 12 );
	CO := FALSE;
	O  := FALSE;
	b := s(s'left);
	L  := s(s'left-1 downto 0) & b;
	R  := bit_vector2natural(L);
     if R mod 2**data_width = 0 then
            Z := TRUE;
        else
            Z := FALSE;
    end if;
        
        if R < 0 then
            N := TRUE;
        else
            N := FALSE;
    end if;
end EXEC_ROL;

--ROLC
procedure EXEC_ROLC (

	constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   )
is
	
	variable s:  bit_vector (11 downto 0);
	variable L:  bit_vector (11 downto 0);
	variable b:  bit ; 
	variable rest: bit_vector (10 downto 0); 
begin 
	
	s := natural2bit_vector( A, 12 );
	O  := FALSE;
	b := s(s'left);
	L  := s(s'left-1 downto 0) & b; 
	R  := bit_vector2natural(L);
	if L(L'left) = '1' then CO := TRUE;
	else CO := FALSE;
	end if;
     if R mod 2**data_width = 0 then
            Z := TRUE;
        else
            Z := FALSE;
    end if;
        
        if R < 0 then
            N := TRUE;
        else
            N := FALSE;
    end if;
end EXEC_ROLC;

--ROR
procedure EXEC_ROR (

	constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   )
is
	
	variable s:  bit_vector (11 downto 0);
	variable L:  bit_vector (11 downto 0);
	variable b:  bit ;
	variable rest: bit_vector (10 downto 0); 
begin 
	
	s := natural2bit_vector( A, 12 );
	CO := FALSE;
	O  := FALSE;
	b := s(s'right);
	L  := b & s(s'left downto 1); 
	R  := bit_vector2natural(L);
     if R mod 2**data_width = 0 then
            Z := TRUE;
        else
            Z := FALSE;
    end if;
        
        if R < 0 then
            N := TRUE;
        else
            N := FALSE;
    end if;
end EXEC_ROR;

--RORC
procedure EXEC_RORC (

	constant A : 	in data_type;
	variable R :	out data_type;
    variable Z,CO,N,O:	out boolean   )
is
	
	variable s:  bit_vector (11 downto 0);
	variable L:  bit_vector (11 downto 0);
	variable b:  bit ; 
	variable rest: bit_vector (10 downto 0); 
begin 
	
	s := natural2bit_vector( A, 12 );
	O := FALSE;
	b := s(s'right);
	L  := b & s(s'left downto 1); 
	R  := bit_vector2natural(L);
	if L(L'right) = '1' then CO := TRUE;
	else CO := FALSE;
	end if;
	if R mod 2**data_width = 0 then
            Z := TRUE;
        else
            Z := FALSE;
    end if;
        
        if R < 0 then
            N := TRUE;
        else
            N := FALSE;
    end if;
end EXEC_RORC;


end cpu_defs_pack;
