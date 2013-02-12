forward
global type w_test from window
end type
type rte_help from richtextedit within w_test
end type
type mle_eval from multilineedit within w_test
end type
type cb_eval from commandbutton within w_test
end type
type cbx_postfix from checkbox within w_test
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
integer width = 3296
integer height = 1392
boolean titlebar = true
string title = "Shunting-Yard evaluator - Seki 2011-2013"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
rte_help rte_help
mle_eval mle_eval
cb_eval cb_eval
cbx_postfix cbx_postfix
mle_polish mle_polish
cb_parse cb_parse
mle_tokens mle_tokens
cb_tokenize cb_tokenize
mle_formula mle_formula
end type
global w_test w_test

type prototypes

Function ULong GetLocaleInfo(ulong Locale, ulong LCType, ref string lpLCData, ulong cchData) Library "kernel32.dll" Alias for "GetLocaleInfoW" 
Function ULong GetSystemDefaultLCID() Library "kernel32.dll"
Function ULong GetUserDefaultLCID() Library "kernel32.dll"

end prototypes

type variables

nv_parser i_parser
constant ulong LOCALE_SDECIMAL = 14 // decimal separator

end variables

forward prototypes
public function string getlocaleinfo (unsignedlong al_localetype, boolean ab_userlocale)
public function string fixdecimal (string as_text)
end prototypes

public function string getlocaleinfo (unsignedlong al_localetype, boolean ab_userlocale);// returns the LOCALE setting of the given type
//
// al_localetype = the local we want to get
// ab_userlocale = TRUE - use the user locales /  FALSE - use the system locales


ulong ll_cid
ulong ll_len
string s_ret = "", s_tmp = ""

if ab_userlocale then
	ll_cid = getuserdefaultlcid( )
else
	ll_cid = getsystemdefaultlcid( )
end if

// first call to get the correct length for the data to return
ll_len = getlocaleinfo(ll_cid, al_localetype, s_tmp , len(s_tmp))

if ll_len > 0 then
	s_tmp = space(ll_len)
	
	//second call to get the data
	ll_len = getlocaleinfo(ll_cid, al_localetype, s_tmp , len(s_tmp))
	if ll_len > 0 then s_ret = left(s_tmp, ll_len -1) // the returned data includes the string terminator
end if

return s_ret

end function

public function string fixdecimal (string as_text);
string ls_sep, ls_rep
long p

ls_sep = getlocaleinfo(locale_sdecimal, true)

if ls_sep = ',' then 
	ls_rep = '.'
else
	ls_rep = ','
end if

p = pos(as_text, ls_rep)
do while p > 0 
	as_text = replace(as_text, p, 1, ls_sep)
	p = pos(as_text, ls_rep)
loop

return as_text

end function

on w_test.create
this.rte_help=create rte_help
this.mle_eval=create mle_eval
this.cb_eval=create cb_eval
this.cbx_postfix=create cbx_postfix
this.mle_polish=create mle_polish
this.cb_parse=create cb_parse
this.mle_tokens=create mle_tokens
this.cb_tokenize=create cb_tokenize
this.mle_formula=create mle_formula
this.Control[]={this.rte_help,&
this.mle_eval,&
this.cb_eval,&
this.cbx_postfix,&
this.mle_polish,&
this.cb_parse,&
this.mle_tokens,&
this.cb_tokenize,&
this.mle_formula}
end on

on w_test.destroy
destroy(this.rte_help)
destroy(this.mle_eval)
destroy(this.cb_eval)
destroy(this.cbx_postfix)
destroy(this.mle_polish)
destroy(this.cb_parse)
destroy(this.mle_tokens)
destroy(this.cb_tokenize)
destroy(this.mle_formula)
end on

event open;
//mle_formula.text = "33+42*2.5/(0.1-5)^2^3"
//mle_formula.text = "1*sum()"
//mle_formula.text = "2+3*4"
//mle_formula.text = "answer()*sum(1)"
mle_formula.text = "2^ (abs  (sum(2 ;-3 ; 4))-1)+1"
//mle_formula.text = "2^ abs(sum(2;-3;4))"

rte_help.InsertDocument("help.rtf", true)

end event

type rte_help from richtextedit within w_test
integer x = 1989
integer y = 64
integer width = 1271
integer height = 1160
integer taborder = 20
long init_backcolor = 67108864
string init_documentname = "help"
boolean init_displayonly = true
boolean border = false
borderstyle borderstyle = stylelowered!
end type

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

long i
st_tok lst_parsed[]

i_parser.getparsed( lst_parsed[] )
mle_eval.text = i_parser.eval(lst_parsed[])


end event

type cbx_postfix from checkbox within w_test
integer x = 471
integer y = 608
integer width = 1198
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "postfix (uncheck for prefix - won~'t evaluate)"
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
string text = "parse"
end type

event clicked;
long i
st_tok lst_tokens[], lst_parsed[]

i_parser.setreverse( cbx_postfix.checked )
mle_formula.text = fixdecimal(mle_formula.text)

if i_parser.tokenize( mle_formula.text ) then
	i_parser.gettokens( lst_tokens[] )
	if i_parser.parse(lst_tokens[]) then
		i_parser.getparsed( lst_parsed[] )
		mle_polish.text = ""
		for i = 1 to upperbound(lst_parsed[])
			mle_polish.text += i_parser.tokentostring(lst_parsed[i]) + ' '
		next
	else
		mle_polish.text = i_parser.getlasterror()
	end if
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
st_tok lst_tokens[]

mle_formula.text = fixdecimal(mle_formula.text)

if i_parser.tokenize( mle_formula.text ) then
	i_parser.gettokens( lst_tokens[] )
	mle_tokens.text = ""
	for i = 1 to upperbound(lst_tokens[])
		mle_tokens.text += i_parser.tokentostring(lst_tokens[i]) + ' '
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
borderstyle borderstyle = stylelowered!
end type

