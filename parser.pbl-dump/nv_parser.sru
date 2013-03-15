$PBExportHeader$nv_parser.sru
forward
global type nv_parser from nonvisualobject
end type
end forward

global type nv_parser from nonvisualobject autoinstantiate
end type

type prototypes

Function ULong GetLocaleInfo(ulong Locale, ulong LCType, ref string lpLCData, ulong cchData) Library "kernel32.dll" Alias for "GetLocaleInfoW" 
Function ULong GetSystemDefaultLCID() Library "kernel32.dll"
Function ULong GetUserDefaultLCID() Library "kernel32.dll"

end prototypes

type variables
/*
	Shunting-Yard evaluator v0.9 - (c) Sebastien KIRCHE 2011-2013

	PB implementation of Dijkstra's Shunting-Yard Algorithm
	for parsing infix math expressions and preparing a postfix 
	representation to ease the evaluation.
		
	The Shunting-Yard algorithm is described on Wikipedia:
	http://en.wikipedia.org/wiki/Shunting_yard_algorithm
	
	This implementation differs from the original Dijkstra by 
	allowing functions with variable argument number.
	It is based on the idea described by Robin in his blog:
	http://www.kallisti.net.nz/blog/2008/02/extension-to-the-shunting-yard-algorithm-to-allow-variable-numbers-of-arguments-to-functions/
	
	This code is free software.

	Usage:
	from a string containing an expression
	1) tokenize() -> "lexical" parse of the expression, 
	   identification of unary minus and terms
	2) parse() -> infix to postfix (or prefix, but the evaluation
	   only support postfixed tokens), parenthesis removal
	3) eval() -> process the expression and resolve function calls
	
	Addition of new functions:
	- Add the function name to the isFunc() method 
	- and implement the function in evalFunc() method
	
*/
public:
//associativities
constant char CC_LEFT = 'L'
constant char CC_RIGHT = 'R'

constant char DECSEP = '.'		//decimal separator
constant char ARGSEP = ','		//argument separator for funcalls
constant char LPAR = '('
constant char RPAR = ')'

private:
nv_term inv_terms[]	//array for tokenized input
nv_term ist_parsed[]	//array for parsed tokens
any ia_vars[]			//array for variables (pairs of ident-value)
string is_lasterror
string is_numbercharpattern

constant ulong LOCALE_SDECIMAL = 14 // decimal separator
string is_SYSDECSEP

boolean ib_postfix = true //for debug

end variables

forward prototypes
public subroutine setreverse (boolean ab_reverse)
public function string getlasterror ()
public function boolean tokenize (string as_input)
public function boolean iswordchar (character ac_char)
public function boolean gettokens (ref nv_term at_terms[])
public function boolean getparsed (ref nv_term at_terms[])
public function string eval (nv_term a_terms[])
public function boolean isop (nv_term a_term)
public function boolean parse (nv_term at_terms[])
public function integer getprec (nv_term a_term)
public function boolean isbool (any a_term)
public function character getassoc (nv_term ast_op)
public function nv_term evalident (nv_term ast_ident)
public function nv_term evalop (nv_term ast_op, ref nv_termstack atst_args)
public function boolean isfunc (string as_name)
public function string getlocaleinfo (unsignedlong al_localetype, boolean ab_userlocale)
public function string fixdecimal (string as_text)
public function boolean setvariables (any aa_vals[])
protected function nv_term evalfunc (nv_term ast_func, ref nv_termstack atst_args)
end prototypes

public subroutine setreverse (boolean ab_reverse);
ib_postfix = ab_reverse

end subroutine

public function string getlasterror ();
return is_lasterror

end function

public function boolean tokenize (string as_input);
// simple tokenizer
// the produced tokens will be processed by parse()

boolean lb_ret = true, lb_isdec
long ll_tk_start = 1, ll_tk_end, ll_inplen, ll_tokens
nv_term l_term
any la_empty[]
char lc, prec, lc_quote

is_lasterror = ""
inv_terms = la_empty[]
ll_inplen= len(as_input)

do while ll_tk_start <= ll_inplen
	ll_tk_end = ll_tk_start
	
	lc = mid(as_input, ll_tk_start, 1)
	choose case lc
		case '0' to '9', DECSEP
			lb_isdec = false
			do while (ll_tk_end <= ll_inplen) and match(mid(as_input, ll_tk_end, 1), is_numbercharpattern)
				if pos(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start + 1), DECSEP) > 0 then lb_isdec = true
				ll_tk_end++
			loop 
			l_term = create nv_term
			l_term.value = dec(fixdecimal(trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start))))
			l_term.kind = iif(lb_isdec, nv_term.DECIM, nv_term.INTG)
		case '*', '/', '^', '%', '=', '<', '>'
			ll_tk_end++
			if lc = '<' or lc = '>' and ll_tk_end < ll_inplen then //lookahead ;)
				if mid(as_input, ll_tk_end, 1) = '=' &
				or mid(as_input, ll_tk_end, 1) = '>' then
					//TODO warning, can have >>
					ll_tk_end++	
				end if
			end if
			l_term = create nv_term
			l_term.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			l_term.kind = nv_term.BINARYOP
		case '+', '-'
			ll_tk_end++
			l_term = create nv_term
			l_term.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			ll_tokens = upperbound(inv_terms[])
			if ll_tokens = 0 then
				l_term.kind = nv_term.UNARYOP
			elseif inv_terms[ll_tokens].kind = nv_term.INTG &
				or inv_terms[ll_tokens].kind = nv_term.DECIM &
				or inv_terms[ll_tokens].kind = nv_term.STR &
				or inv_terms[ll_tokens].kind = nv_term.IDENT &
				or inv_terms[ll_tokens].kind = nv_term.TRPAR  then
				l_term.kind = nv_term.BINARYOP
			else
				l_term.kind = nv_term.UNARYOP
			end if
		case 'A' to 'Z', 'a' to 'z', '_'
			do while (ll_tk_end <= ll_inplen) and isWordChar(mid(as_input, ll_tk_end, 1))
				ll_tk_end++
			loop
			l_term = create nv_term
			l_term.value = lower(trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)))
			if isbool(l_term.value) then
				l_term.value = iif(lower(l_term.value)="true", true, false)
				l_term.kind = nv_term.BOOL
			elseif isfunc(l_term.value) then
				l_term.kind = nv_term.FUNC
				l_term.count = -1
			elseif lower(l_term.value) = "and" or lower(l_term.value) = "or" or lower(l_term.value) = "xor" then
				l_term.value = lower(l_term.value)
				l_term.kind = nv_term.BINARYOP
			elseif lower(l_term.value) = "not" then
				l_term.value = lower(l_term.value)
				l_term.kind = nv_term.UNARYOP
			else
				l_term.kind = nv_term.IDENT
			end if
		case "'", '"', '`'
			long ll_begin, ll_cur
			lc_quote = lc
			ll_begin = ll_tk_start
			ll_cur = ll_begin + 1
			do while (ll_cur < ll_inplen) and (mid(as_input, ll_cur, 1) <> lc_quote)
				ll_cur++
			loop
			//if ll_cur < ll_inplen and mid(as_input, ll_cur, 1) = lc_quote then ll_cur++
			if ll_cur >= ll_inplen and (mid(as_input, ll_cur, 1) <> lc_quote) then
				is_lasterror = "unclosed string that begins at " + string(ll_begin)
				lb_ret = false
				goto exit_tokenize
			else
				ll_tk_end = ll_cur + 1
				l_term = create nv_term
				l_term.value = mid(as_input, ll_begin + 1, ll_cur - ll_begin - 1)
				l_term.kind = nv_term.STR
			end if			
		case ARGSEP, LPAR, RPAR
			ll_tk_end++
			l_term = create nv_term
			l_term.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			choose case lc
				case ARGSEP; l_term.kind = nv_term.TARGSEP
				case LPAR; l_term.kind = nv_term.TLPAR
				case RPAR; l_term.kind = nv_term.TRPAR
			end choose
		case ' ' 
			ll_tk_start++		//ignore blanks
			continue
		case else
			is_lasterror = "possible unexpected char : `" + lc +"` at " + string(ll_tk_start)
			lb_ret = false
			goto exit_tokenize
	end choose
	l_term.position = ll_tk_start
	inv_terms[upperbound(inv_terms[]) + 1] = l_term
	if ll_tk_end = ll_tk_start then 
		is_lasterror = "parsing error"
		lb_ret = false
		goto exit_tokenize
	end if
	prec = mid(as_input, ll_tk_end, 1)
	ll_tk_start = ll_tk_end
loop

exit_tokenize:
return lb_ret

end function

public function boolean iswordchar (character ac_char);
// tell if the given character if a word character

choose case ac_char
	case 'a' to 'z', 'A' to 'Z', '0' to '9', '_'
		return true

	case else
		return false
end choose

end function

public function boolean gettokens (ref nv_term at_terms[]);
// helper for the tokens read from the input

at_terms[] = inv_terms[]

return upperbound(inv_terms[]) > 0


end function

public function boolean getparsed (ref nv_term at_terms[]);
// helper for the parsed tokens

at_terms[] = ist_parsed[]

return true

end function

public function string eval (nv_term a_terms[]);
/*
	Evaluation of the token stream after parsing

	the evaluator reads the tokens array 
	and uses a stack for intermediate evaluations 
	including function calls
*/

nv_termstack ltst_eval
nv_term item, st_op1, st_op2
dec ldc_op1, ldc_op2, ldc_res
any la_op1, la_op2
boolean lb_res
string ls_ret = "", ls_val
long i, n

n = upperbound(a_terms[]) 
if n = 0 then return ""

for i = 1 to n
	ls_val = lower(string(a_terms[i].value))
	choose case a_terms[i].kind
		case nv_term.INTG, nv_term.DECIM, nv_term.BOOL, nv_term.STR
			ltst_eval.push(a_terms[i])
		case nv_term.IDENT
			item = evalident(a_terms[i])
			if item.kind <> nv_term.ERR then
				ltst_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
		case nv_term.UNARYOP, nv_term.BINARYOP
			item = evalop(a_terms[i], ltst_eval)
			if item.kind <> nv_term.ERR then
				ltst_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
		case nv_term.FUNC
			item = evalfunc(a_terms[i], ltst_eval)
			if item.kind <> nv_term.ERR then
				ltst_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
	end choose
next
if ltst_eval.size() = 1 then
	item = ltst_eval.pop()
	if item.kind = nv_term.INTG or item.kind = nv_term.DECIM then
		ls_ret = string(item.value)
	elseif item.kind = nv_term.BOOL then
		ls_ret = iif(item.value, 'True', 'False')
	elseif item.kind = nv_term.STR then
		ls_ret = '"' + item.value + '"'
	else
		ls_ret = "not sure how to process " + item.typename() + " `" + string(item.value) + "`"
	end if
else
	ls_ret = "evaluation failed"
end if

end_eval:
return ls_ret

end function

public function boolean isop (nv_term a_term);
// tell if the given text is an operator

/*
boolean lb_ret 

choose case a_term.value
	case '+', '-', '*', '/', '^'
		lb_ret = true
		
	case else
		lb_ret = false
end choose

return lb_ret
*/

return (a_term.kind = nv_term.BINARYOP) or (a_term.kind = nv_term.UNARYOP)

end function

public function boolean parse (nv_term at_terms[]);
// parse the infix input token stream and transform it into prefix or postfix

boolean lb_ret = true
nv_term l_term, l_func, empty_toks[]
nv_queue lq_out			//output queue
nv_termstack ltst_op	//intermediate operators/funcs stack
nv_stack lt_argcount	//arguments counter
nv_stack lt_args		//arguments presence
long ll_count

is_lasterror = ""

if upperbound(at_terms[]) < 1 then
	is_lasterror = "no input to parse"
	lb_ret = false
	goto end_of_shunt
end if

long t = 1
do while t <= upperbound(at_terms[])
	l_term = at_terms[t]

	if l_term.kind = nv_term.INTG or l_term.kind = nv_term.DECIM or l_term.kind = nv_term.BOOL or l_term.kind = nv_term.IDENT or l_term.kind = nv_term.STR then
		//if the token is an operand, pass it to the output queue
		lq_out.push(l_term)
		if lt_args.size() > 0 then lt_args.settop(true)
	elseif l_term.kind = nv_term.FUNC then
		//if it is a function, push it to the stack
		ltst_op.push(l_term)
		lt_argcount.push(0)
		if lt_args.size() > 0 then lt_args.settop(true)
		lt_args.push(false)
	elseif l_term.kind = nv_term.TARGSEP then
		//if it is a function argument separator
		//1) pop all pending operators until getting '('
		do while not ltst_op.isempty() 
			if ltst_op.top().value <> LPAR then
				lq_out.push(ltst_op.pop())
			else
				exit //while
			end if
		loop
		if ltst_op.isempty() or (ltst_op.top().value <> LPAR) then
			//if stack is empty or we did not get a '('
			//there is an expression error
			is_lasterror = "bad argument separator or parenthesis mismatch"
			lb_ret = false
			goto end_of_shunt
		end if
		//2) remember arg count
		//if lt_args.size() > 0 then
			if lt_args.top() = true then
				ll_count = lt_argcount.top()
				lt_argcount.settop(ll_count + 1)
			end if
		//end if
		lt_args.push(false)
	elseif isop(l_term) then
		//we have an operator, process operator precedence
		//if there are other pending operations
		do while not ltst_op.isempty() 
			if ((getassoc(l_term) = cc_left and getprec(l_term) <= getprec(ltst_op.top())) &
			or &
			(getassoc(l_term) = cc_right and getprec(l_term) < getprec(ltst_op.top()))) then
				lq_out.push(ltst_op.pop())
			else
				exit //while
			end if
		loop
		ltst_op.push(l_term)
	elseif l_term.value = LPAR then
		//push a '(' to the stack
		ltst_op.push(l_term)
	elseif l_term.value = RPAR then
		//process all pending operations up to '('
		do while not ltst_op.isempty() 
			if ltst_op.top().value <> LPAR then
				lq_out.push(ltst_op.pop())
			else
				exit //while
			end if
		loop
		//just pop the '('
		if ltst_op.top().value = LPAR then
			destroy ltst_op.top()
			ltst_op.pop()
		else
			//we might have a parenthesis mismatch
			is_lasterror = "mismatch parenthesis"
			lb_ret = false
			goto end_of_shunt
		end if
		if ltst_op.top().kind = nv_term.FUNC then 
			//if there is a function after the paren
			//add it to the queue with arguments count
			ll_count = lt_argcount.pop()
			if lt_args.pop() then ll_count ++
			l_func = ltst_op.pop()
			l_func.count = ll_count
			lq_out.push(l_func)
		end if
	//else what to do ?
	end if
	t++
loop
do while not ltst_op.isempty()
	//add the final pending operators to the queue
	if ltst_op.top().value = LPAR or ltst_op.top().value = RPAR then
		//if there are still parenthesis on the stack, we have a problem
		is_lasterror = "mismatch parenthesis"
		lb_ret = false
		goto end_of_shunt
	end if
	lq_out.push(ltst_op.pop())
loop

//put the queue into an array, in postfix or prefix order
ist_parsed[] = empty_toks[]
if ib_postfix then
	do while not lq_out.isempty()
		ist_parsed[upperbound(ist_parsed[]) + 1] = lq_out.pop()
	loop
else
	//reverse the reversed => prefixed stream
	do while not lq_out.isempty()
		ltst_op.push(lq_out.pop())
	loop
	do while not ltst_op.isempty()
		ist_parsed[upperbound(ist_parsed[]) + 1] = ltst_op.pop()
	loop
end if

end_of_shunt:
return lb_ret

end function

public function integer getprec (nv_term a_term);
// return the precedence for an operator

int li_ret

if a_term.kind <> nv_term.UNARYOP and a_term.kind <> nv_term.BINARYOP then return 0

choose case a_term.value
	case '^'; 					li_ret = 8
	case '-'
		//special minus case
		if a_term.kind = nv_term.UNARYOP then 
			li_ret = 7
		else
			li_ret = 5
		end if
	case '*', '/', '%';			li_ret = 6
	case '+' /*- above*/;		li_ret = 5
	case '<', '<=', '>', '>=';	li_ret = 4
	case '=', '<>';				li_ret = 3
	case 'not';						li_ret = 2
	case 'and', 'or', 'xor';	li_ret = 1
	case else;						li_ret = 0
end choose

return li_ret

end function

public function boolean isbool (any a_term);
//check if the given identifier is a boolean literal

boolean lb_ret = false

choose case lower(a_term)
	case 'true', "false"
		lb_ret = true
end choose

return lb_ret

end function

public function character getassoc (nv_term ast_op);
// return the associativity for an operator

char lc_ret

choose case ast_op.value
	case '^', 'not'
		lc_ret = CC_RIGHT
	case '*', '/', '%'
		lc_ret = CC_LEFT
	case '+', '-', 'and', 'or', 'xor'
		if ast_op.kind = nv_term.UNARYOP then
			lc_ret = CC_RIGHT
		else
			lc_ret = CC_LEFT
		end if
	case else
		lc_ret = CC_LEFT
end choose

return lc_ret

end function

public function nv_term evalident (nv_term ast_ident);
// evaluate the value of a function

nv_term lt_ret, item
dec ldc_ret = 0, ldc_val
long i, max

lt_ret = create nv_term

//get the type of the identifier, then push the value

choose case lower(ast_ident.value)
	case "pi"	//dummy function
		lt_ret.kind = nv_term.DECIM
		lt_ret.value = 3.14
	case else
		max = upperbound(ia_vars[]) - 1
		for i = 1 to max step 2
			if ast_ident.value = ia_vars[i] then
				lt_ret = ia_vars[i+1]
				goto eval_done
			end if
		next
		lt_ret.kind = nv_term.ERR
		is_lasterror = "cannot resolve `" + ast_ident.value + "`"
		lt_ret.value = is_lasterror
end choose

eval_done:
return lt_ret

end function

public function nv_term evalop (nv_term ast_op, ref nv_termstack atst_args);
// evaluate the value of an operator

nv_term lt_ret, st_op1, st_op2
dec ldc_op1, ldc_op2, ldc_res
any la_op1, la_op2, la_res
boolean lb_val
long i

lt_ret = create nv_term

choose case ast_op.kind
	case nv_term.UNARYOP
		choose case ast_op.value
			case '-', '+'
				st_op1 = atst_args.pop()
				if st_op1.kind <> nv_term.DECIM and st_op1.kind <> nv_term.INTG then
					lt_ret.kind = nv_term.ERR
					lt_ret.value = "`" + ast_op.value + "` cannot handle " + st_op1.typename() + " `" + string(st_op1.value) + "` at " + string(ast_op.position)
					goto end_eval
				end if
				ldc_op1 = dec(st_op1.value)
				if ast_op.value = '-' then
					ldc_res = - ldc_op1
				elseif ast_op.value = '+' then
					ldc_res = ldc_op1
				end if
				lt_ret.kind = nv_term.DECIM
				lt_ret.value = ldc_res
			case 'not'
				st_op1 = atst_args.pop()
				//TODO pourra être traité par un nv_term.tobool
				if st_op1.kind <> nv_term.BOOL then
					lt_ret.kind = nv_term.ERR
					lt_ret.value = "`" + ast_op.value + "` cannot handle " + st_op1.typename() + " `" + string(st_op1.value) + "` at " + string(ast_op.position)
					goto end_eval
				end if
				lt_ret.kind = nv_term.BOOL
				lt_ret.value = not(st_op1.value)
		end choose
	case nv_term.BINARYOP
			if atst_args.size() >= 2 then
				//TODO, assume this is 2 numerical tokens
				//need to improve type checking / inference
				choose case ast_op.value
					case '+', '-', '*', '/', '%', '^'
						if not atst_args.top().iscompatiblewith(atst_args.peek(1)) then
							//Coercion to first operand
							if atst_args.peek(1).kind = nv_term.STR then
								if ast_op.value <> '+' then
									lt_ret.kind = nv_term.ERR
									lt_ret.value = "eval failed: mismatch operands (" + atst_args.peek(1).typename() + '/' + atst_args.top().typename() + ") for `" + ast_op.value + "` at " + string(ast_op.position)
									goto end_eval
								else
									la_op2 = string(atst_args.pop().value)
									lt_ret.kind = nv_term.STR
								end if
							elseif atst_args.peek(1).kind = nv_term.DECIM or atst_args.peek(1).kind = nv_term.INTG then
								if match(atst_args.top().value, is_numbercharpattern) then
									la_op2 = dec(fixdecimal(atst_args.pop().value))
									lt_ret.kind = nv_term.DECIM
								else
									lt_ret.kind = nv_term.ERR
									lt_ret.value = "eval failed: cannot convert `" + atst_args.top().value + "` to numeric for `" + ast_op.value + "` at " + string(ast_op.position)
									goto end_eval
								end if
							end if
							la_op1 = atst_args.pop().value
						else
							//same operand types
							lt_ret.kind = atst_args.top().kind
							la_op2 = atst_args.pop().value
							la_op1 = atst_args.pop().value
						end if
						choose case ast_op.value
							case '+'
								la_res = la_op1 + la_op2
							case '-'
								la_res = la_op1 - la_op2
							case '*'
								la_res = la_op1 * la_op2
							case '/'
								la_res = la_op1 / la_op2
							case '%'
								la_res = mod(la_op1, la_op2)
							case '^'
								la_res = la_op1 ^ la_op2
						end choose
						lt_ret.value = la_res
					case '<', '<=', '>', '>='
						la_op2 = atst_args.pop().value
						la_op1 = atst_args.pop().value
						choose case ast_op.value
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
						st_op2 = atst_args.pop()
						st_op1 = atst_args.pop()
						if st_op1.kind <> st_op2.kind then
							lt_ret.kind = nv_term.ERR
							lt_ret.value = "eval failed: mismatch operands (" + st_op1.typename() + '/' + st_op2.typename() + ") for `" + ast_op.value + "` at " + string(ast_op.position)
							goto end_eval
						end if
						//TODO pourra être traité par un nv_term.tobool
						if (ast_op.value = 'and' or ast_op.value = 'or') and st_op1.kind <> nv_term.BOOL then
							lt_ret.kind = nv_term.ERR
							lt_ret.value = "`" + ast_op.value + "` cannot handle `" + st_op1.typename() + "` at " + string(ast_op.position)
							goto end_eval
						end if
						choose case ast_op.value
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
						lt_ret.value = "unknown op " + ast_op.value
						goto end_eval
				end choose
			else
				lt_ret.kind = nv_term.ERR
				lt_ret.value = "broken expression : wrong number of operands for `" + ast_op.value + "` at " + string(ast_op.position)
				goto end_eval
			end if
	case else
		is_lasterror = "cannot evaluate op `" + ast_op.value + "`"
		lt_ret.kind = nv_term.ERR
		lt_ret.value = is_lasterror
end choose

end_eval:
return lt_ret

end function

public function boolean isfunc (string as_name);
//check if the given identifier is a function

boolean lb_ret = false

choose case lower(as_name)
	case 'answer', "sum", "mul", "abs", "min", "max", "len", "msgbox" /*, "not"*/
		lb_ret = true
end choose

return lb_ret

end function

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
// adapt a decimal value to the format suitable for the system
// because dec() use the decimal separator locales setting to be able to get decimals
string ls_rep
long p

if is_SYSDECSEP = ',' then 
  ls_rep = '.'
else
  ls_rep = ','
end if

p = pos(as_text, ls_rep)
do while p > 0 
  as_text = replace(as_text, p, 1, is_SYSDECSEP)
  p = pos(as_text, ls_rep)
loop

return as_text

end function

public function boolean setvariables (any aa_vals[]);
//populate the variables
// aa_vals[] must be an even sized array of key-value pairs

boolean lb_ret = true
int i, max, n
nv_term l_term

max = upperbound(aa_vals[])
if mod(max, 2) <> 0 then return false


n = 1
for i = 1 to max step 2
	if not isbool(aa_vals[i]) &
		and not isfunc(aa_vals[i]) /*TODO : and not isOP*/ then

		l_term = create nv_term
		choose case classname(aa_vals[i+1])
			case "boolean"
				l_term.kind = nv_term.BOOL
				l_term.value = aa_vals[i+1] //iif(lower(string(aa_vals[i+1])) = "true", true, false)
			case "integer"
				l_term.kind = nv_term.INTG
				l_term.value = aa_vals[i+1]
			case "decimal"
				l_term.kind = nv_term.DECIM
				l_term.value = aa_vals[i+1]
			case "string"
				if left(aa_vals[i+1],1) = right(aa_vals[i+1],1) then
					l_term.kind = nv_term.STR
					l_term.value = mid(aa_vals[i+1], 2, len(string(aa_vals[i+1])) - 2)
				end if
		end choose
		ia_vars[n+1] = l_term
		ia_vars[n] = lower(aa_vals[i])
		n += 2
	end if
next

return true

end function

protected function nv_term evalfunc (nv_term ast_func, ref nv_termstack atst_args);
// evaluate the value of a function

nv_term lt_ret, item
dec ldc_ret = 0, ldc_val
long i

lt_ret = create nv_term
choose case ast_func.value
	case "answer"	//dummy function
		lt_ret.kind = nv_term.DECIM
		lt_ret.value = 42
	case "sum"
		for i = 1 to ast_func.count
			ldc_val = dec(atst_args.pop().value)
			ldc_ret += ldc_val
		next
		lt_ret.kind = nv_term.DECIM
		lt_ret.value = ldc_ret
	case "mul"
		if atst_args.size() > 0 then ldc_ret = dec(atst_args.pop().value)
		for i = 2 to ast_func.count
			ldc_val = dec(atst_args.pop().value)
			ldc_ret *= ldc_val
		next
		lt_ret.kind = nv_term.DECIM
		lt_ret.value = ldc_ret
	case "abs"
		if ast_func.count = 1 then 
			ldc_ret = abs(dec(atst_args.pop().value))
			lt_ret.kind = nv_term.DECIM
			lt_ret.value = ldc_ret
		else
			is_lasterror = "abs() needs 1 argument"
			lt_ret.kind = nv_term.ERR
			lt_ret.value = is_lasterror
		end if
	/*case "not"
		if ast_func.count = 1 then
			lt_ret.kind = BOOL
			lt_ret.value = not(atst_args.pop().value)
		else
			is_lasterror = "abs() needs 1 argument"
		end if*/
	case "min"
		if atst_args.size() > 0 then 
			ldc_ret = dec(atst_args.pop().value)
			for i = 2 to ast_func.count
				ldc_val = dec(atst_args.pop().value)
				if ldc_val < ldc_ret then ldc_ret = ldc_val
			next
			lt_ret.kind = nv_term.DECIM
			lt_ret.value = ldc_ret
		else
			is_lasterror = "missing argument"
			lt_ret.kind = nv_term.ERR
			lt_ret.value = is_lasterror
		end if
	case "max"
		if atst_args.size() > 0 then 
			ldc_ret = dec(atst_args.pop().value)
			for i = 2 to ast_func.count
				ldc_val = dec(atst_args.pop().value)
				if ldc_val > ldc_ret then ldc_ret = ldc_val
			next
			lt_ret.kind = nv_term.DECIM
			lt_ret.value = ldc_ret
		else
			is_lasterror = "missing argument"
			lt_ret.kind = nv_term.ERR
			lt_ret.value = is_lasterror
		end if
	case "len"
		if ast_func.count = 1 then 
			if atst_args.top().kind = nv_term.STR then
				ldc_ret = len(string(atst_args.pop().value))
				lt_ret.kind = nv_term.DECIM
				lt_ret.value = ldc_ret
			else
				is_lasterror = "len() can only take string argument, given " + atst_args.top().typename()
				lt_ret.kind = nv_term.ERR
				lt_ret.value = is_lasterror
			end if
		else
			is_lasterror = "len() needs 1 argument"
			lt_ret.kind = nv_term.ERR
			lt_ret.value = is_lasterror
		end if
	case "msgbox"
		if ast_func.count = 1 then
			if atst_args.top().kind = nv_term.STR then
				ldc_ret = MessageBox("Evaluator", string(atst_args.pop().value))
				lt_ret.kind = nv_term.INTG
				lt_ret.value = ldc_ret
			else
				is_lasterror = "msgbox() can only take string argument, given " + atst_args.top().typename()
				lt_ret.kind = nv_term.ERR
				lt_ret.value = is_lasterror
			end if
		else
			is_lasterror = "msgbox() needs 1 argument"
			lt_ret.kind = nv_term.ERR
			lt_ret.value = is_lasterror
		end if
	case else
		is_lasterror = "cannot evaluate function `" + ast_func.value + "`"
		lt_ret.kind = nv_term.ERR
		lt_ret.value = is_lasterror
end choose

if lt_ret.kind = nv_term.ERR then 
	string ls_tmp
	ls_tmp = lt_ret.value
	ls_tmp += " at " + string(ast_func.position)
	lt_ret.value = ls_tmp
end if

return lt_ret

end function

on nv_parser.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_parser.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;
is_SYSDECSEP = getlocaleinfo(locale_sdecimal, true)
is_numbercharpattern = "[0-9" + iif(DECSEP = '.', "\.", DECSEP) + "]"

end event

