(* Test file for level 100 of CSCE 531 Pascal compiler Project III *)

(* There are no errors in this file *)

program T3L100ok;

var
   a,b : Integer;
   c   : Char;


procedure print_one; External;
procedure print_two; External;
procedure print_three; External;
procedure print_dunno; External;
procedure print_just; External;
procedure print_isnt; External;
procedure print_hello; External;
procedure print_oops; External;
procedure print_712; External;
procedure print_is_the; External;
procedure print_grooviest; External;
procedure print_newline; External;


begin

   a := 9;
   b := 3;

   case a of
     1 : print_one;
     2 : print_two;
     3 : print_three
   end; { case }


end.
