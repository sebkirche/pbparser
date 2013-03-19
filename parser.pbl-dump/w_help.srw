$PBExportHeader$w_help.srw
forward
global type w_help from window
end type
type cb_ok from commandbutton within w_help
end type
type mle_help from multilineedit within w_help
end type
end forward

global type w_help from window
integer width = 1797
integer height = 1756
boolean titlebar = true
string title = "Parser mini-help"
boolean controlmenu = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_ok cb_ok
mle_help mle_help
end type
global w_help w_help

event open;
mle_help.text = "Supported operators (by order of precedence) ~r~n&
	pow (^) ~r~n&
	- (unary) ~r~n&
	* / % (modulo)~r~n&
	+ - (binary) ~r~n&
	< <= > >= = <> ~r~n&
	not ~r~n&
	and or xor ~r~n&
~r~n&
Functions (can have any number of parameters) ~r~n&
	answer()		: 42 (dummy func) ~r~n&
	sum(i,j,k,...)	: returns the sum of arguments ~r~n&
	mul(i,j,k,...)	: returns the multiplication of arguments ~r~n&
	abs(x)		: return absolute value of x ~r~n&
	min(a,b,c,...)	: returns the minimum of arguments~r~n&
	max(a,b,c,...)	: returns the maximum of arguments~r~n&
	len(string)	: returns the length of the given string ~r~n&
"

end event

on w_help.create
this.cb_ok=create cb_ok
this.mle_help=create mle_help
this.Control[]={this.cb_ok,&
this.mle_help}
end on

on w_help.destroy
destroy(this.cb_ok)
destroy(this.mle_help)
end on

type cb_ok from commandbutton within w_help
integer x = 677
integer y = 1536
integer width = 402
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "OK"
boolean default = true
end type

event clicked;
close(parent)

end event

type mle_help from multilineedit within w_help
integer x = 41
integer y = 32
integer width = 1673
integer height = 1480
integer taborder = 10
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

