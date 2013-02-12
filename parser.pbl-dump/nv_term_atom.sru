forward
global type nv_term_atom from nv_term
end type
end forward

global type nv_term_atom from nv_term
end type
global nv_term_atom nv_term_atom

forward prototypes
public function boolean accept (ref string as_input, readonly long al_inplen, readonly long al_startoff, ref long al_endoff, ref nv_termqueue atq_terms, readonly string as_ops, readonly string as_funcs)
end prototypes

public function boolean accept (ref string as_input, readonly long al_inplen, readonly long al_startoff, ref long al_endoff, ref nv_termqueue atq_terms, readonly string as_ops, readonly string as_funcs);
//check if we accept something at the given point of input

boolean lb_ret = true, lb_isdec
long ll_tokens
nv_term_atom lnv_term
//any la_empty[]
char lc, lc_quote

is_lasterror = ""

lc = mid(as_input, al_startoff, 1)
choose case lc
	case '0' to '9',  DECSEP
		lb_isdec = false
		do while (al_endoff <= al_inplen) and match(mid(as_input, al_endoff, 1), is_numbercharpattern)
			if pos(mid(as_input, al_startoff, al_endoff - al_startoff + 1), DECSEP) > 0 then lb_isdec = true
			al_endoff++
		loop 
		lnv_term = create nv_term_atom
		lnv_term.value = dec(fixdecimal(trim(mid(as_input, al_startoff, al_endoff - al_startoff))))
		lnv_term.kind = iif(lb_isdec, nv_term.DECIM, nv_term.INTG)
/*	case '*', '/', '^', '%', '=', '<', '>'
		al_endoff++
		if lc = '<' or lc = '>' and al_endoff < al_inplen then //lookahead ;)
			if mid(as_input, al_endoff, 1) = '=' &
			or mid(as_input, al_endoff, 1) = '>' then
				//TODO warning, can have >>
				al_endoff++	
			end if
		end if
		lnv_term = create nv_term
		lnv_term.value = mid(as_input, al_startoff, al_endoff - al_startoff)
		lnv_term.kind = nv_term.BINARYOP
	case '+', '-'
		al_endoff++
		lnv_term = create nv_term
		lnv_term.value = mid(as_input, al_startoff, al_endoff - al_startoff)
		ll_tokens = atq_terms.size()
		if ll_tokens = 0 then
			lnv_term.kind = nv_term.UNARYOP
		elseif atq_terms.peek(-1).kind = nv_term.INTG &
			or atq_terms.peek(-1).kind = nv_term.DECIM &
			or atq_terms.peek(-1).kind = nv_term.STR &
			or atq_terms.peek(-1).kind = nv_term.IDENT &
			or atq_terms.peek(-1).kind = nv_term.TRPAR  then
			lnv_term.kind = nv_term.BINARYOP
		else
			lnv_term.kind = nv_term.UNARYOP
		end if*/
	case 'A' to 'Z', 'a' to 'z', '_'
		do while (al_endoff <= al_inplen) and isWordChar(mid(as_input, al_endoff, 1))
			al_endoff++
		loop
		lnv_term = create nv_term_atom
		lnv_term.value = trim(mid(as_input, al_startoff, al_endoff - al_startoff))
		if isbool(lnv_term.value) then
			lnv_term.value = iif(lower(lnv_term.value)="true", true, false)
			lnv_term.kind = nv_term.BOOL
		elseif isfunc(lnv_term.value, as_funcs) then
			lnv_term.kind = nv_term.FUNC
			lnv_term.count = -1
		/*elseif isop(lnv_term.value, as_ops) then
			lnv_term.kind = nv_term.BINARYOP 	//FIXME : avoir la liste des opérateurs unaires / binaires
			lnv_term.count = -1
		elseif lower(lnv_term.value) = "and" or lower(lnv_term.value) = "or" or lower(lnv_term.value) = "xor" then
			lnv_term.value = lower(lnv_term.value)
			lnv_term.kind = nv_term.BINARYOP
		elseif lower(lnv_term.value) = "not" then
			lnv_term.value = lower(lnv_term.value)
			lnv_term.kind = nv_term.UNARYOP*/
		else
			lnv_term.kind = nv_term.IDENT
		end if
	case "'", '"', '`'
		long ll_begin, ll_cur
		lc_quote = lc
		ll_begin = al_startoff
		ll_cur = ll_begin + 1
		do while (ll_cur < al_inplen) and (mid(as_input, ll_cur, 1) <> lc_quote)
			ll_cur++
		loop
		//if ll_cur < al_inplen and mid(as_input, ll_cur, 1) = lc_quote then ll_cur++
		if ll_cur >= al_inplen and (mid(as_input, ll_cur, 1) <> lc_quote) then
			is_lasterror = "unclosed string that begins at " + string(ll_begin)
			lb_ret = false
			goto exit_tokenize
		else
			al_endoff = ll_cur + 1
			lnv_term = create nv_term_atom
			lnv_term.value = mid(as_input, ll_begin + 1, ll_cur - ll_begin - 1)
			lnv_term.kind = nv_term.STR
		end if			
	case ARGSEP, LPAR, RPAR
		al_endoff++
		lnv_term = create nv_term_atom
		lnv_term.value = mid(as_input, al_startoff, al_endoff - al_startoff)
		choose case lc
			case ARGSEP; lnv_term.kind = TARGSEP
			case LPAR; lnv_term.kind = TLPAR
			case RPAR; lnv_term.kind = TRPAR
		end choose
	case else
		//is_lasterror = "possible unexpected char : `" + lc +"` at " + string(al_startoff)
		lb_ret = false
		goto exit_tokenize
end choose
lnv_term.position = al_startoff
atq_terms.push(lnv_term)
if al_endoff = al_startoff then 
	is_lasterror = "parsing error"
	lb_ret = false
end if

exit_tokenize:
return lb_ret

end function

on nv_term_atom.create
call super::create
end on

on nv_term_atom.destroy
call super::destroy
end on

