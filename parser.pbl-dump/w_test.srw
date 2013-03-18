$PBExportHeader$w_test.srw
forward
global type w_test from window
end type
type cbx_dbgtokens from checkbox within w_test
end type
type mle_debug from multilineedit within w_test
end type
type mle_eval from multilineedit within w_test
end type
type cb_eval from commandbutton within w_test
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
integer width = 2203
integer height = 1536
boolean titlebar = true
string title = "Shunting-Yard evaluator"
string menuname = "m_main"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cbx_dbgtokens cbx_dbgtokens
mle_debug mle_debug
mle_eval mle_eval
cb_eval cb_eval
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
public subroutine showerror (string as_return)
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

public subroutine showerror (string as_return);

if match(as_return, "at [0-9]+$") then
	long offset
	string ls_tmp
	offset = long(mid(as_return,lastpos(as_return,' ')))
	ls_tmp = fill('-', offset - 1) + "^"
	mle_formula.text += "~r~n" + ls_tmp
end if


end subroutine

on w_test.create
if this.MenuName = "m_main" then this.MenuID = create m_main
this.cbx_dbgtokens=create cbx_dbgtokens
this.mle_debug=create mle_debug
this.mle_eval=create mle_eval
this.cb_eval=create cb_eval
this.mle_polish=create mle_polish
this.cb_parse=create cb_parse
this.mle_tokens=create mle_tokens
this.cb_tokenize=create cb_tokenize
this.mle_formula=create mle_formula
this.Control[]={this.cbx_dbgtokens,&
this.mle_debug,&
this.mle_eval,&
this.cb_eval,&
this.mle_polish,&
this.cb_parse,&
this.mle_tokens,&
this.cb_tokenize,&
this.mle_formula}
end on

on w_test.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cbx_dbgtokens)
destroy(this.mle_debug)
destroy(this.mle_eval)
destroy(this.cb_eval)
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
//mle_formula.text = "2^ (abs  (sum(2 ;-3 ; 4))-1)+1"
//mle_formula.text = "2^ abs(sum(2;-3;4))"
//mle_formula.text = "2 - 1 <= 1 + 1"
//mle_formula.text = "+1 + --+1 = 2 = not(true)"
//mle_formula.text = "+1 + --+1" // attendu 2
//mle_formula.text = "true and not false"
//mle_formula.text = "len('toto') = 4"
//mle_formula.text = "min(len('toto')+7,-len(~"machin~"),4.2)+42"
//mle_formula.text = "'aa'+'bb'"
mle_formula.text = "if(pi=3.14,msgbox('true'),msgbox('false'))"

end event

type cbx_dbgtokens from checkbox within w_test
integer x = 1810
integer y = 288
integer width = 343
integer height = 64
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 67108864
string text = "debug --->"
end type

event clicked;
if checked then
	parent.width = 3287
else
	parent.width = 2203
end if

end event

type mle_debug from multilineedit within w_test
integer x = 2199
integer y = 64
integer width = 1015
integer height = 1160
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = fixed!
fontfamily fontfamily = modern!
string facename = "Courier New"
long textcolor = 33554432
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type mle_eval from multilineedit within w_test
integer x = 37
integer y = 1028
integer width = 2117
integer height = 196
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = fixed!
fontfamily fontfamily = modern!
string facename = "Courier New"
long textcolor = 33554432
boolean hscrollbar = true
boolean autohscroll = true
boolean autovscroll = true
boolean displayonly = true
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
string facename = "Tahoma"
string text = "&eval"
end type

event clicked;

long i
string ls_res
nv_term lt_parsed[]

i_parser.getparsed( lt_parsed[] )

any vars[]
vars[1] = "toto"
vars[2] = "'titi'"
vars[3] = "vrai"
vars[4] = true
vars[5] = "faux"
vars[6] = false
vars[7] = "e"
vars[8] = 2.718

i_parser.setvariables( vars[] )

if not i_parser.eval(lt_parsed[1]) then
	showerror(lt_parsed[1].value)
end if

mle_eval.text = string(lt_parsed[1].value)
end event

type mle_polish from multilineedit within w_test
integer x = 37
integer y = 704
integer width = 2117
integer height = 196
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = fixed!
fontfamily fontfamily = modern!
string facename = "Courier New"
long textcolor = 33554432
boolean hscrollbar = true
boolean autohscroll = true
boolean autovscroll = true
boolean displayonly = true
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
string facename = "Tahoma"
string text = "&parse"
end type

event clicked;
long i
nv_term lt_tokens[], lt_parsed[]

if i_parser.tokenize( mle_formula.text ) then
	i_parser.gettokens( lt_tokens[] )
	if i_parser.parse(lt_tokens[]) then
		i_parser.getparsed( lt_parsed[] )
		mle_polish.text = ""
		for i = 1 to upperbound(lt_parsed[])
			mle_polish.text += lt_parsed[i].dump() + ' '
		next
	else
		mle_polish.text = i_parser.getlasterror()
	end if
end if

end event

type mle_tokens from multilineedit within w_test
integer x = 37
integer y = 388
integer width = 2117
integer height = 196
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = fixed!
fontfamily fontfamily = modern!
string facename = "Courier New"
long textcolor = 33554432
boolean hscrollbar = true
boolean autohscroll = true
boolean autovscroll = true
boolean displayonly = true
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
string facename = "Tahoma"
string text = "&tokenize"
end type

event clicked;
long i, p
nv_term lt_tokens[]
string ls_err, ls_form

ls_form = mle_formula.text
p = pos(ls_form, "~r~n")
if p > 0 then ls_form = left(ls_form, p - 1)

//ls_form = fixdecimal(ls_form)
mle_formula.text = ls_form

if i_parser.tokenize( ls_form ) then
	i_parser.gettokens( lt_tokens[] )
	mle_tokens.text = ""
	if cbx_dbgtokens.checked then mle_debug.text = ""
	for i = 1 to upperbound(lt_tokens[])
		mle_tokens.text += lt_tokens[i].dump() + ' '
		if cbx_dbgtokens.checked then 
			mle_debug.text += lt_tokens[i].dump() + &
									'[' + lt_tokens[i].typename() + ']' +&
									iif(lt_tokens[i].kind = nv_term.UNARYOP or lt_tokens[i].kind = nv_term.BINARYOP, "p="+string(i_parser.getprec(lt_tokens[i])), "") + &
									'~r~n' 
		end if 
	next
else
	ls_err = i_parser.getlasterror()
	showerror(ls_err)
	mle_tokens.text = ls_err
end if


end event

type mle_formula from multilineedit within w_test
integer x = 37
integer y = 64
integer width = 2117
integer height = 196
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = fixed!
fontfamily fontfamily = modern!
string facename = "Courier New"
long textcolor = 33554432
boolean autohscroll = true
boolean autovscroll = true
borderstyle borderstyle = stylelowered!
end type

