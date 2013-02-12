forward
global type nv_term_basicop from nv_term
end type
end forward

global type nv_term_basicop from nv_term
string is_ops = "+,-,*,/,^,%,=,<,<=,>,>=,<>"
end type
global nv_term_basicop nv_term_basicop

type variables

end variables

forward prototypes
public function boolean accept (ref string as_input, readonly long al_inplen, readonly long al_startoff, ref long al_endoff, ref nv_termqueue atq_terms, readonly string as_ops, readonly string as_funcs)
public function nv_term eval (nv_termstack ast_args)
end prototypes

public function boolean accept (ref string as_input, readonly long al_inplen, readonly long al_startoff, ref long al_endoff, ref nv_termqueue atq_terms, readonly string as_ops, readonly string as_funcs);

char lc

lc = mid(as_input, al_startoff, 1)

al_endoff++

nv_term_basicop lt_term
lt_term = create nv_term_basicop

if lc = '+' or lc = '-' then
	//choose between unary an binary ops
	if atq_terms.size() = 0 then
		lt_term.kind = UNARYOP
	elseif atq_terms.peek(-1).kind = INTG &
				or atq_terms.peek(-1).kind = DECIM &
				or atq_terms.peek(-1).kind = STR &
				or atq_terms.peek(-1).kind = IDENT &
				or atq_terms.peek(-1).kind = TRPAR  then
		lt_term.kind = BINARYOP
	else
		lt_term.kind = UNARYOP
	end if
else
	//tests for binary ops, may have a <= or >=
	if lc = '<' or lc = '>' and al_endoff < al_inplen then //lookahead ;)
		if mid(as_input, al_endoff, 1) = '=' &
		or mid(as_input, al_endoff, 1) = '>' then
			//TODO warning, can have >>
			al_endoff++	
		end if
	end if
	lt_term.kind = BINARYOP
end if
lt_term.value = mid(as_input, al_startoff, al_endoff - al_startoff)
lt_term.position = al_startoff
atq_terms.push(lt_term)

return true

end function

public function nv_term eval (nv_termstack ast_args);
// evaluate the value of an operator

nv_term lt_ret, st_op1, st_op2
dec ldc_op1, ldc_op2, ldc_res
string ls_op1, ls_op2
any la_op1, la_op2
boolean lb_val
long i,n

lt_ret = create nv_term_basicop

choose case kind
	case nv_term.UNARYOP
		choose case value
			case '-', '+'
				st_op1 = ast_args.pop()
				if st_op1.kind <> nv_term.INTG and st_op1.kind <> nv_term.DECIM then
					lt_ret.kind = nv_term.ERR
					lt_ret.value = "`" + value + "` cannot handle " + st_op1.typename() + " `" + string(st_op1.value) + "` at " + string(position)
					goto end_eval
				end if
				ldc_op1 = dec(st_op1.value)
				if value = '-' then
					ldc_res = - ldc_op1
				elseif value = '+' then
					ldc_res = ldc_op1
				end if
				lt_ret.kind = st_op1.kind
				lt_ret.value = ldc_res
			case 'not'
				st_op1 = ast_args.pop()
				//TODO pourra être traité par un nv_term.tobool
				if st_op1.kind <> nv_term.BOOL then
					lt_ret.kind = nv_term.ERR
					lt_ret.value = "`" + value + "` cannot handle " + st_op1.typename() + " `" + string(st_op1.value) + "` at " + string(position)
					goto end_eval
				end if
				lt_ret.kind = nv_term.BOOL
				lt_ret.value = not(st_op1.value)
		end choose
	case nv_term.BINARYOP
			if ast_args.size() >= 2 then
				//TODO, assume this is 2 numerical tokens
				//need to improve type checking / inference
				choose case value
					case '+', '-', '*', '/', '%', '^'
						if ast_args.peek(-1).kind = nv_term.INTG or ast_args.peek(-1).kind = nv_term.DECIM then
							if ast_args.top().kind <> nv_term.INTG and ast_args.top().kind <> nv_term.DECIM then
								lt_ret.kind = nv_term.ERR
								lt_ret.value = "`" + value + "` cannot handle `" + ast_args.peek(-1).typename() + "`, `" + ast_args.top().typename() + "` at " + string(position)
							else
								ldc_op2 = dec(ast_args.pop().value)
								ldc_op1 = dec(ast_args.pop().value)
								choose case value
									case '+'
										ldc_res = ldc_op1 + ldc_op2
									case '-'
										ldc_res = ldc_op1 - ldc_op2
									case '*'
										ldc_res = ldc_op1 * ldc_op2
									case '/'
										ldc_res = ldc_op1 / ldc_op2
									case '%'
										ldc_res = mod(ldc_op1, ldc_op2)
									case '^'
										ldc_res = ldc_op1 ^ ldc_op2
								end choose
								lt_ret.kind = nv_term.DECIM
								lt_ret.value = ldc_res
							end if
						elseif ast_args.peek(-1).kind = nv_term.STR then
							choose case value
								case '+'
									ls_op2 = string(ast_args.pop().value) //coercion to str
									ls_op1 = ast_args.pop().value
									lt_ret.kind = nv_term.STR
									lt_ret.value = ls_op1 + ls_op2
								/*case '*'
									n = long(ast_args.pop().value) //coercion to integer value
									ls_op1 = ast_args.pop().value
									lt_ret.kind = nv_term.STR
									lt_ret.value = fill(ls_op1,n)*/
								case else
									lt_ret.kind = nv_term.ERR
									lt_ret.value = "`" + value + "` cannot handle `" + ast_args.peek(-1).typename() + "`, `" + ast_args.top().typename() + "` at " + string(position)
							end choose
						end if
					case '<', '<=', '>', '>='
						la_op2 = ast_args.pop().value
						la_op1 = ast_args.pop().value
						choose case value
							case '<'
								lb_val = la_op1 < la_op2
							case '<=', '=<'
								lb_val = la_op1 <= la_op2
							case '>'
								lb_val = la_op1 > la_op2
							case '>=', '=>'
								lb_val = la_op1 >= la_op2
						end choose
						lt_ret.kind = nv_term.BOOL
						lt_ret.value = lb_val
					case '=', '<>', 'and', 'or', 'xor'
						st_op2 = ast_args.pop()
						st_op1 = ast_args.pop()
						if st_op1.kind <> st_op2.kind then
							lt_ret.kind = nv_term.ERR
							lt_ret.value = "broken expression : mismatch operands (" + st_op1.typename() + '/' + st_op2.typename() + ") for `" + value + "` at " + string(position)
							goto end_eval
						end if
						//TODO pourra être traité par un nv_term.tobool
						if (value = 'and' or value = 'or') and st_op1.kind <> nv_term.BOOL then
							lt_ret.kind = nv_term.ERR
							lt_ret.value = "`" + value + "` cannot handle `" + st_op1.typename() + "` at " + string(position)
							goto end_eval
						end if
						choose case value
							case '='
								lb_val = (st_op1.value = st_op2.value)
							case '<>'
								lb_val = (st_op1.value <> st_op2.value)
							case 'and'
								lb_val = (st_op1.value and st_op2.value)
							case 'or'
								lb_val = (st_op1.value or st_op2.value)
							case 'xor'
								lb_val = (st_op1.value and not st_op2.value) or (not st_op1.value and st_op2.value)
						end choose
						lt_ret.kind = nv_term.BOOL
						lt_ret.value = lb_val
					case else
						lt_ret.kind = nv_term.ERR
						lt_ret.value = "unknown op " + value
						goto end_eval
				end choose
			else
				lt_ret.kind = nv_term.ERR
				lt_ret.value = "broken expression : wrong number of operands for `" + value + "` at " + string(position)
				goto end_eval
			end if
	case else
		is_lasterror = "cannot evaluate op `" + value + "`"
		lt_ret.kind = nv_term.ERR
		lt_ret.value = is_lasterror
end choose

end_eval:
return lt_ret

end function

on nv_term_basicop.create
call super::create
end on

on nv_term_basicop.destroy
call super::destroy
end on

event constructor;call super::constructor;
is_inputstartmatches[] = {'\+', '-', '\*', '/', '^', '%', '=', '<', '>'}

end event

