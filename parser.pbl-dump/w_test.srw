forward
global type w_test from window
end type
type cb_1 from commandbutton within w_test
end type
type mle_eval from multilineedit within w_test
end type
type cb_eval from commandbutton within w_test
end type
type cbx_reverse from checkbox within w_test
end type
type mle_polish from multilineedit within w_test
end type
type cb_parse from commandbutton within w_test
end type
type mle_tokens from multilineedit within w_test
end type
type cb_tokenize from commandbutton within w_test
end type
type mle_formula from multilineedit within w_test
end type
type s_test from structure within w_test
end type
end forward

type s_test from structure
	character		s1[20]
	long		l2
end type

global type w_test from window
integer width = 1979
integer height = 1336
boolean titlebar = true
string title = "Shunting-Yard"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_1 cb_1
mle_eval mle_eval
cb_eval cb_eval
cbx_reverse cbx_reverse
mle_polish mle_polish
cb_parse cb_parse
mle_tokens mle_tokens
cb_tokenize cb_tokenize
mle_formula mle_formula
end type
global w_test w_test

type prototypes
SUBROUTINE CopyMemory &
   ( REF blob b, s_test s, long l )  &
   LIBRARY "kernel32.dll"  &
   ALIAS FOR RtlMoveMemory
   
end prototypes

type variables

nv_parser i_parser

end variables

on w_test.create
this.cb_1=create cb_1
this.mle_eval=create mle_eval
this.cb_eval=create cb_eval
this.cbx_reverse=create cbx_reverse
this.mle_polish=create mle_polish
this.cb_parse=create cb_parse
this.mle_tokens=create mle_tokens
this.cb_tokenize=create cb_tokenize
this.mle_formula=create mle_formula
this.Control[]={this.cb_1,&
this.mle_eval,&
this.cb_eval,&
this.cbx_reverse,&
this.mle_polish,&
this.cb_parse,&
this.mle_tokens,&
this.cb_tokenize,&
this.mle_formula}
end on

on w_test.destroy
destroy(this.cb_1)
destroy(this.mle_eval)
destroy(this.cb_eval)
destroy(this.cbx_reverse)
destroy(this.mle_polish)
destroy(this.cb_parse)
destroy(this.mle_tokens)
destroy(this.cb_tokenize)
destroy(this.mle_formula)
end on

type cb_1 from commandbutton within w_test
integer x = 526
integer y = 288
integer width = 402
integer height = 72
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "none"
end type

event clicked;
s_test s
blob{512} b

s.s1 = "qwerty"
s.l2 = 65535
//b = blob(s)

long i

CopyMemory(b, s, 24)

mle_tokens.text = ""
for i = 1 to 32
	mle_tokens.text += string(asc(string(blobmid(b,i,1))), "00") +' ' 
next
end event

type mle_eval from multilineedit within w_test
integer x = 37
integer y = 1028
integer width = 1906
integer height = 196
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = fixed!
fontfamily fontfamily = modern!
string facename = "Courier New"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type cb_eval from commandbutton within w_test
integer x = 41
integer y = 928
integer width = 402
integer height = 72
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "eval"
end type

event clicked;
mle_eval.text = i_parser.eval()

end event

type cbx_reverse from checkbox within w_test
integer x = 471
integer y = 608
integer width = 608
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Postfix (RPN)"
boolean checked = true
end type

type mle_polish from multilineedit within w_test
integer x = 37
integer y = 704
integer width = 1906
integer height = 196
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = fixed!
fontfamily fontfamily = modern!
string facename = "Courier New"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type cb_parse from commandbutton within w_test
integer x = 37
integer y = 608
integer width = 402
integer height = 72
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "polish"
end type

event clicked;
long i
any la_tokens[]

i_parser.setreverse( cbx_reverse.checked )
if i_parser.polish() then
	i_parser.getparsed( la_tokens[] )
	mle_polish.text = ""
	for i = 1 to upperbound(la_tokens[])
		mle_polish.text += la_tokens[i] + ' '
	next
else
	mle_polish.text = i_parser.getlasterror()
end if


end event

type mle_tokens from multilineedit within w_test
integer x = 37
integer y = 388
integer width = 1906
integer height = 196
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = fixed!
fontfamily fontfamily = modern!
string facename = "Courier New"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type cb_tokenize from commandbutton within w_test
integer x = 37
integer y = 288
integer width = 402
integer height = 72
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "tokenize"
end type

event clicked;
long i
any la_tokens[]

if i_parser.tokenize( mle_formula.text ) then
	i_parser.gettokens( la_tokens[] )
	mle_tokens.text = ""
	for i = 1 to upperbound(la_tokens[])
		mle_tokens.text += la_tokens[i] + ' '
	next
else
	mle_tokens.text = i_parser.getlasterror()
end if


end event

type mle_formula from multilineedit within w_test
integer x = 37
integer y = 64
integer width = 1906
integer height = 196
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = fixed!
fontfamily fontfamily = modern!
string facename = "Courier New"
long textcolor = 33554432
string text = "33+42*2.5/(0.1-5)^2^3"
borderstyle borderstyle = stylelowered!
end type

