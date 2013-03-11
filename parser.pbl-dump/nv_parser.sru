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

//constant char DECSEP = '.'			//decimal separator
constant char ARGSEP = ';'			//argument separator for funcalls
constant char LPAR = '('
constant char RPAR = ')'

private:
constant ulong LOCALE_SDECIMAL = 14 // decimal separator
string is_SYSDECSEP
string is_lasterror

nv_term it_termbase
any is_operators_matches[]
nv_term it_operators[]
string is_operators
string is_functions

//nv_term inv_tokens[]	//array for tokenized input
nv_termqueue inv_terms
nv_termqueue inv_parsed

//nv_term ist_parsed[]	//array for parsed tokens
boolean ib_postfix = true



end variables

forward prototypes
public subroutine setreverse (boolean ab_reverse)
public function string getlasterror ()
public function boolean tokenize (string as_input)
public function boolean iswordchar (character ac_char)
public function boolean isop (nv_term anv_term)
public function integer getprec (nv_term anv_term)
public function boolean isbool (any aa_term)
public function character getassoc (nv_term ast_op)
public function nv_term evalfunc (nv_term ast_func, ref nv_termstack ast_args)
public function nv_term evalident (nv_term ast_ident)
public function nv_term evalop (nv_term ast_op, ref nv_termstack ast_args)
public function boolean isfunc (string as_name)
public function string getlocaleinfo (unsignedlong al_localetype, boolean ab_userlocale)
public function string fixdecimal (string as_text)
public function nv_termqueue getterms ()
public function boolean registerterhandler (string as_class)
public function nv_termqueue getparsed ()
public function boolean parse (nv_termqueue anv_tokens)
public function string eval (nv_termqueue anv_terms)
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

boolean lb_ret = true//, lb_isdec
long ll_tk_start = 1, ll_tk_end, ll_inplen, ll_tokens
//nv_term lnv_term
//any la_empty[]
char lc//, prec, lc_quote
//string ls_numberpattern 
string ls_opmatches[]
long i, o

is_lasterror = ""

if not isvalid(inv_terms) then 
	inv_terms = create nv_termqueue
else
	inv_terms.reset()
end if
//inv_tokens = la_empty[]

ll_inplen= len(as_input)

do while ll_tk_start <= ll_inplen
	ll_tk_end = ll_tk_start
	
	lc = mid(as_input, ll_tk_start, 1)
	if lc = ' ' then
		ll_tk_start++
		continue
	end if
	
	if not it_termbase.accept(as_input, ll_inplen, ll_tk_start, ll_tk_end, inv_terms, is_operators, is_functions) then
		for o = 1 to upperbound(it_operators[])
			ls_opmatches[] = is_operators_matches[o]
			for i = 1 to upperbound(ls_opmatches[])
				if match(lc, ls_opmatches[i]) then
					if it_operators[o].accept(as_input, ll_inplen, ll_tk_start, ll_tk_end, inv_terms, is_operators, is_functions) then
						goto token_parsed
					end if
				end if
			next
		next
	end if
	
	token_parsed:
	if ll_tk_end = ll_tk_start then 
		is_lasterror = "parsing failed"
		lb_ret = false
		goto exit_tokenize
	end if
	//prec = mid(as_input, ll_tk_end, 1)
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

public function boolean isop (nv_term anv_term);
// tell if the given text is an operator

/*
boolean lb_ret 

choose case anv_term.value
	case '+', '-', '*', '/', '^'
		lb_ret = true
		
	case else
		lb_ret = false
end choose

return lb_ret
*/

return (anv_term.kind = nv_term.BINARYOP) or (anv_term.kind = nv_term.UNARYOP)

end function

public function integer getprec (nv_term anv_term);
// return the precedence for an operator

int li_ret

if anv_term.kind <> nv_term.UNARYOP and anv_term.kind <> nv_term.BINARYOP then return 0

choose case anv_term.value
	case '^'; 					li_ret = 8
	case '-'
		//special minus case
		if anv_term.kind = nv_term.UNARYOP then 
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

public function boolean isbool (any aa_term);
//check if the given identifier is a boolean literal

boolean lb_ret = false

choose case lower(aa_term)
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

public function nv_term evalfunc (nv_term ast_func, ref nv_termstack ast_args);
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
			ldc_val = dec(ast_args.pop().value)
			ldc_ret += ldc_val
		next
		lt_ret.kind = nv_term.DECIM
		lt_ret.value = ldc_ret
	case "mul"
		if ast_args.size() > 0 then ldc_ret = dec(ast_args.pop().value)
		for i = 2 to ast_func.count
			ldc_val = dec(ast_args.pop().value)
			ldc_ret *= ldc_val
		next
		lt_ret.kind = nv_term.DECIM
		lt_ret.value = ldc_ret
	case "abs"
		if ast_func.count = 1 then 
			ldc_ret = abs(dec(ast_args.pop().value))
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
			lt_ret.value = not(ast_args.pop().value)
		else
			is_lasterror = "abs() needs 1 argument"
		end if*/
	case "min"
		if ast_args.size() > 0 then 
			ldc_ret = dec(ast_args.pop().value)
			for i = 2 to ast_func.count
				ldc_val = dec(ast_args.pop().value)
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
		if ast_args.size() > 0 then 
			ldc_ret = dec(ast_args.pop().value)
			for i = 2 to ast_func.count
				ldc_val = dec(ast_args.pop().value)
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
			if ast_args.top().kind = nv_term.STR then
				ldc_ret = len(string(ast_args.pop().value))
				lt_ret.kind = nv_term.INTG
				lt_ret.value = ldc_ret
			else
				is_lasterror = "len() con only take string argument"
				lt_ret.kind = nv_term.ERR
				lt_ret.value = is_lasterror
			end if
		else
			is_lasterror = "len() needs 1 argument"
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

public function nv_term evalident (nv_term ast_ident);
// evaluate the value of a function

nv_term lt_ret, item
dec ldc_ret = 0, ldc_val
long i

lt_ret = create nv_term

//get the type of the identifier, then push the value

choose case lower(ast_ident.value)
	case "pi"	//dummy function
		lt_ret.kind = nv_term.DECIM
		lt_ret.value = 3.14
	case else
		lt_ret.kind = nv_term.ERR
		is_lasterror = "cannot resolve `" + ast_ident.value + "`"
		lt_ret.value = is_lasterror
end choose

return lt_ret

end function

public function nv_term evalop (nv_term ast_op, ref nv_termstack ast_args);
// evaluate the value of an operator

nv_term lt_ret, st_op1, st_op2
dec ldc_op1, ldc_op2, ldc_res
string ls_op1, ls_op2
any la_op1, la_op2
boolean lb_val
long i,n

lt_ret = create nv_term

choose case ast_op.kind
	case nv_term.UNARYOP
		choose case ast_op.value
			case '-', '+'
				st_op1 = ast_args.pop()
				if st_op1.kind <> nv_term.INTG and st_op1.kind <> nv_term.DECIM then
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
				lt_ret.kind = st_op1.kind
				lt_ret.value = ldc_res
			case 'not'
				st_op1 = ast_args.pop()
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
			if ast_args.size() >= 2 then
				//TODO, assume this is 2 numerical tokens
				//need to improve type checking / inference
				choose case ast_op.value
					case '+', '-', '*', '/', '%', '^'
						if ast_args.peek(-1).kind = nv_term.INTG or ast_args.peek(-1).kind = nv_term.DECIM then
							if ast_args.top().kind <> nv_term.INTG and ast_args.top().kind <> nv_term.INTG then
								lt_ret.kind = nv_term.ERR
								lt_ret.value = "`" + ast_op.value + "` cannot handle `" + ast_args.peek(-1).typename() + "`, `" + ast_args.top().typename() + "` at " + string(ast_op.position)
							else
								ldc_op2 = dec(ast_args.pop().value)
								ldc_op1 = dec(ast_args.pop().value)
								choose case ast_op.value
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
							choose case ast_op.value
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
									lt_ret.value = "`" + ast_op.value + "` cannot handle `" + ast_args.peek(-1).typename() + "`, `" + ast_args.top().typename() + "` at " + string(ast_op.position)
							end choose
						end if
					case '<', '<=', '>', '>='
						la_op2 = ast_args.pop().value
						la_op1 = ast_args.pop().value
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
						st_op2 = ast_args.pop()
						st_op1 = ast_args.pop()
						if st_op1.kind <> st_op2.kind then
							lt_ret.kind = nv_term.ERR
							lt_ret.value = "broken expression : mismatch operands (" + st_op1.typename() + '/' + st_op2.typename() + ") for `" + ast_op.value + "` at " + string(ast_op.position)
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
	case 'answer', "sum", "mul", "abs", "min", "max", "len"//, "not"
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

public function nv_termqueue getterms ();
if not isvalid(inv_terms) then inv_terms = create nv_termqueue

return inv_terms

end function

public function boolean registerterhandler (string as_class);
//register a new operator object
nv_term lt_obj
string ls_matches[], ls_ops, ls_funcs
integer li_max

lt_obj = create using as_class
ls_matches[] = lt_obj.is_inputstartmatches[]
ls_ops = lt_obj.is_ops
if ls_ops <> "" then is_operators += ',' + ls_ops
ls_funcs = lt_obj.is_funcs
if ls_funcs <> "" then is_functions += ',' + ls_funcs

li_max = upperbound(it_operators) + 1
it_operators[li_max] = lt_obj
is_operators_matches[li_max] = ls_matches[]

return true

end function

public function nv_termqueue getparsed ();
// helper for the parsed tokens

return inv_parsed

end function

public function boolean parse (nv_termqueue anv_tokens);
// parse the infix input token stream and transform it into prefix or postfix

boolean lb_ret = true
nv_term lnv_term, lt_func
nv_termqueue lq_out			//output queue
nv_termstack lt_op		//intermediate operators/funcs stack
nv_stack lt_argcount	//arguments counter
nv_stack lt_args		//arguments presence
long ll_count

is_lasterror = ""

if anv_tokens.size() < 1 then
	is_lasterror = "no input to parse"
	lb_ret = false
	goto exit_parsing
end if

if not isvalid(inv_parsed) then
	inv_parsed = create nv_termqueue
else
	inv_parsed.reset( )
end if

lq_out = create nv_termqueue

lt_op = create nv_termstack
do while anv_tokens.size() > 0
	lnv_term = anv_tokens.shift()

	if lnv_term.kind = nv_term.INTG or lnv_term.kind = nv_term.DECIM or lnv_term.kind = nv_term.BOOL or lnv_term.kind = nv_term.IDENT or lnv_term.kind = nv_term.STR then
		//if the token is an operand, pass it to the output queue
		lq_out.push(lnv_term)
		if lt_args.size() > 0 then lt_args.settop(true)
	elseif lnv_term.kind = nv_term.FUNC then
		//if it is a function, push it to the stack
		lt_op.push(lnv_term)
		lt_argcount.push(0)
		if lt_args.size() > 0 then lt_args.settop(true)
		lt_args.push(false)
	elseif lnv_term.kind = nv_term.TARGSEP then
		//if it is a function argument separator
		//1) pop all pending operators until getting '('
		do while not lt_op.isempty() and lt_op.top().value <> LPAR
			lq_out.push(lt_op.pop())
		loop
		if lt_op.isempty() or (lt_op.top().value <> LPAR) then
			//if stack is empty or we did not get a '('
			//there is an expression error
			is_lasterror = "bad argument separator or parenthesis mismatch"
			lb_ret = false
			goto exit_parsing
		end if
		//2) remember arg count
		//if lt_args.size() > 0 then
			if lt_args.top() = true then
				ll_count = lt_argcount.top()
				lt_argcount.settop(ll_count + 1)
			end if
		//end if
		lt_args.push(false)
	elseif isop(lnv_term) then
		//we have an operator, process operator precedence
		//if there are other pending operations
		do while not lt_op.isempty() &
			and ((getassoc(lnv_term) = cc_left and getprec(lnv_term) <= getprec(lt_op.top())) &
			or &
			(getassoc(lnv_term) = cc_right and getprec(lnv_term) < getprec(lt_op.top()))) 
			lq_out.push(lt_op.pop())
		loop
		lt_op.push(lnv_term)
	elseif lnv_term.value = LPAR then
		//push a '(' to the stack
		lt_op.push(lnv_term)
	elseif lnv_term.value = RPAR then
		//process all pending operations up to '('
		do while not lt_op.isempty() and lt_op.top().value <> LPAR
			lq_out.push(lt_op.pop())
		loop
		//just pop the '('
		if lt_op.top().value = LPAR then
			destroy lt_op.top()
			lt_op.pop()
		else
			//we might have a parenthesis mismatch
			is_lasterror = "mismatch parenthesis"
			lb_ret = false
			goto exit_parsing
		end if
		if lt_op.top().kind = nv_term.FUNC then 
			//if there is a function after the paren
			//add it to the queue with arguments count
			ll_count = lt_argcount.pop()
			if lt_args.pop() then ll_count ++
			lt_func = lt_op.pop()
			lt_func.count = ll_count
			lq_out.push(lt_func)
		end if
	//else what to do ?
	end if
loop
do while not lt_op.isempty()
	//add the final pending operators to the queue
	if lt_op.top().value = LPAR or lt_op.top().value = RPAR then
		//if there are still parenthesis on the stack, we have a problem
		is_lasterror = "mismatch parenthesis"
		lb_ret = false
		goto exit_parsing
	end if
	lq_out.push(lt_op.pop())
loop

//put the queue into an array, in postfix or prefix order
if ib_postfix then
	inv_parsed = lq_out
//	do while not lq_out.isempty()
//		//ist_parsed[upperbound(ist_parsed[]) + 1] = lq_out.pop()
//		inv_parsed.push(lq_out.pop())
//	loop
else
	//reverse the reversed => prefixed stream
	do while not lq_out.isempty()
//		lt_op.push(lq_out.pop())
		inv_parsed.push(lq_out.shift())
	loop
	if isvalid(lq_out) then destroy lq_out
//	do while not lt_op.isempty()
//		ist_parsed[upperbound(ist_parsed[]) + 1] = lt_op.pop()
//	loop
end if

exit_parsing:
if isvalid(lt_op) then destroy lt_op

return lb_ret

end function

public function string eval (nv_termqueue anv_terms);
/*
	Evaluation of the token stream after parsing

	the evaluator reads the tokens array 
	and uses a stack for intermediate evaluations 
	including function calls
*/

nv_termstack lt_eval
nv_term cur, item, st_op1, st_op2
dec ldc_op1, ldc_op2, ldc_res
any la_op1, la_op2
boolean lb_res
string ls_ret = ""
long i, n

n = anv_terms.size()
if n = 0 then return ""

lt_eval = create nv_termstack

//for i = 1 to n
do while anv_terms.size() > 0
	cur = anv_terms.shift()
	choose case cur.kind
		case nv_term.INTG, nv_term.DECIM, nv_term.BOOL, nv_term.STR
			lt_eval.push(cur)
		case nv_term.IDENT
			item = evalident(cur)
			if item.kind <> nv_term.ERR then
				lt_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
		case nv_term.UNARYOP, nv_term.BINARYOP
//			item = evalop(anv_terms[i], lt_eval)
			item = cur.eval(lt_eval)
			if item.kind <> nv_term.ERR then
				lt_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
		case nv_term.FUNC
			item = evalfunc(cur, lt_eval)
			if item.kind <> nv_term.ERR then
				lt_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
	end choose
loop
if lt_eval.size() = 1 then
	item = lt_eval.pop()
	if item.kind = nv_term.INTG or item.kind = nv_term.DECIM then
		ls_ret = string(item.value)
	elseif item.kind = nv_term.BOOL then
		ls_ret = iif(item.value, 'True', 'False')
	elseif item.kind = nv_term.STR then
		ls_ret = item.value
	else
		ls_ret = "not sure how to eval " + item.typename() + " `" + string(item.value) + "`"
	end if
else
	ls_ret = "evaluation failed"
end if

end_eval:
destroy lt_eval
return ls_ret		//TODO : return the last stack item

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

it_termbase = create nv_term_atom
registerterhandler("nv_term_basicop")

end event

event destructor;
if isvalid(it_termbase) then destroy it_termbase

end event

