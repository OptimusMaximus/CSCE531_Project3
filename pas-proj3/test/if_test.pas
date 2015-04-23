(* Test file for level 80 of CSCE 531 Pascal compiler Project III *)

(* There are no errors in this file. *)


program T3L80_ok;

var
   a,b,c : Integer;
   x	 : Integer;
   y	 : Char;
   z	 : Single;
   w	 : Real;
   i,j,k : Integer;
   ca	 : array[1..8] of Char;
   ia	 : array[5..10] of Integer;
   ra	 : array[7..9] of Real;
   (* Arrays of arrays *)
   iaa   : array[0..10] of array[0..10] of Integer;
   iaaa  : array[0..2] of array[2..5] of array[5..9] of Integer;

procedure print_abc; External;
procedure print_x; External;
procedure print_z; External;
procedure print_w; External;
procedure print_ca; External;
procedure print_ia; External;
procedure print_ra; External;
procedure print_iaa; External;
procedure print_iaaa; External;
procedure print_oops; External;
procedure print_7; External;
procedure print_1; External;
procedure print_2; External;
procedure print_newline; External;
procedure print_is; External;
procedure print_grooviest; External;
procedure print_outer; External;
procedure print_inner; External;
procedure print_inner_inner; External;
procedure print_looping; External;
procedure print_looping_looping; External;
procedure print_looping_looping_looping; External;


begin
   a := -1;
   b := 0;
   c := 1;

   (*if a > b then
      print_oops
   else begin
      if b = c+a then begin
	 print_7;
	 print_1;
	 if true then print_2;
	 print_newline
      end else
	 print_oops;
      print_is
   end;*)

   print_grooviest;

end.