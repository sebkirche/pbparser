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
nv_tok inv_tokens[]	//array for tokenized input
nv_tok ist_parsed[]	//array for parsed tokens
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
public function boolean gettokens (ref nv_tok anv_tokens[])
public function boolean getparsed (ref nv_tok ast_expr[])
public function string eval (nv_tok anv_toks[])
public function boolean isop (nv_tok anv_tok)
public function boolean parse (nv_tok anv_tokens[])
public function integer getprec (nv_tok anv_tok)
public function boolean isbool (any aa_tok)
public function character getassoc (nv_tok ast_op)
public function nv_tok evalident (nv_tok ast_ident)
public function nv_tok evalop (nv_tok ast_op, ref nv_tokstack ast_args)
public function boolean isfunc (string as_name)
public function string getlocaleinfo (unsignedlong al_localetype, boolean ab_userlocale)
public function string fixdecimal (string as_text)
public function boolean setvariables (any aa_vals[])
protected function nv_tok evalfunc (nv_tok ast_func, ref nv_tokstack ast_args)
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
nv_tok lnv_tok
any la_empty[]
char lc, prec, lc_quote

is_lasterror = ""
inv_tokens = la_empty[]
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
			lnv_tok = create nv_tok
			lnv_tok.value = dec(fixdecimal(trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start))))
			lnv_tok.kind = iif(lb_isdec, nv_tok.DECIM, nv_tok.INTG)
		case '*', '/', '^', '%', '=', '<', '>'
			ll_tk_end++
			if lc = '<' or lc = '>' and ll_tk_end < ll_inplen then //lookahead ;)
				if mid(as_input, ll_tk_end, 1) = '=' &
				or mid(as_input, ll_tk_end, 1) = '>' then
					//TODO warning, can have >>
					ll_tk_end++	
				end if
			end if
			lnv_tok = create nv_tok
			lnv_tok.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			lnv_tok.kind = nv_tok.BINARYOP
		case '+', '-'
			ll_tk_end++
			lnv_tok = create nv_tok
			lnv_tok.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			ll_tokens = upperbound(inv_tokens[])
			if ll_tokens = 0 then
				lnv_tok.kind = nv_tok.UNARYOP
			elseif inv_tokens[ll_tokens].kind = nv_tok.INTG &
				or inv_tokens[ll_tokens].kind = nv_tok.DECIM &
				or inv_tokens[ll_tokens].kind = nv_tok.STR &
				or inv_tokens[ll_tokens].kind = nv_tok.IDENT &
				or inv_tokens[ll_tokens].kind = nv_tok.TRPAR  then
				lnv_tok.kind = nv_tok.BINARYOP
			else
				lnv_tok.kind = nv_tok.UNARYOP
			end if
		case 'A' to 'Z', 'a' to 'z', '_'
			do while (ll_tk_end <= ll_inplen) and isWordChar(mid(as_input, ll_tk_end, 1))
				ll_tk_end++
			loop
			lnv_tok = create nv_tok
			lnv_tok.value = lower(trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)))
			if isbool(lnv_tok.value) then
				lnv_tok.value = iif(lower(lnv_tok.value)="true", true, false)
				lnv_tok.kind = nv_tok.BOOL
			elseif isfunc(lnv_tok.value) then
				lnv_tok.kind = nv_tok.FUNC
				lnv_tok.count = -1
			elseif lower(lnv_tok.value) = "and" or lower(lnv_tok.value) = "or" or lower(lnv_tok.value) = "xor" then
				lnv_tok.value = lower(lnv_tok.value)
				lnv_tok.kind = nv_tok.BINARYOP
			elseif lower(lnv_tok.value) = "not" then
				lnv_tok.value = lower(lnv_tok.value)
				lnv_tok.kind = nv_tok.UNARYOP
			else
				lnv_tok.kind = nv_tok.IDENT
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
				lnv_tok = create nv_tok
				lnv_tok.value = mid(as_input, ll_begin + 1, ll_cur - ll_begin - 1)
				lnv_tok.kind = nv_tok.STR
			end if			
		case ARGSEP, LPAR, RPAR
			ll_tk_end++
			lnv_tok = create nv_tok
			lnv_tok.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			choose case lc
				case ARGSEP; lnv_tok.kind = nv_tok.TARGSEP
				case LPAR; lnv_tok.kind = nv_tok.TLPAR
				case RPAR; lnv_tok.kind = nv_tok.TRPAR
			end choose
		case ' ' 
			ll_tk_start++		//ignore blanks
			continue
		case else
			is_lasterror = "possible unexpected char : `" + lc +"` at " + string(ll_tk_start)
			lb_ret = false
			goto exit_tokenize
	end choose
	lnv_tok.position = ll_tk_start
	inv_tokens[upperbound(inv_tokens[]) + 1] = lnv_tok
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

public function boolean gettokens (ref nv_tok anv_tokens[]);
// helper for the tokens read from the input

anv_tokens[] = inv_tokens[]

return upperbound(inv_tokens[]) > 0


end function

public function boolean getparsed (ref nv_tok ast_expr[]);
// helper for the parsed tokens

ast_expr[] = ist_parsed[]

return true

end function

public function string eval (nv_tok anv_toks[]);
/*
	Evaluation of the token stream after parsing

	the evaluator reads the tokens array 
	and uses a stack for intermediate evaluations 
	including function calls
*/

nv_tokstack lt_eval
nv_tok item, st_op1, st_op2
dec ldc_op1, ldc_op2, ldc_res
any la_op1, la_op2
boolean lb_res
string ls_ret = "", ls_val
long i, n

n = upperbound(anv_toks[]) 
if n = 0 then return ""

for i = 1 to n
	ls_val = lower(string(anv_toks[i].value))
	choose case anv_toks[i].kind
		case nv_tok.INTG, nv_tok.DECIM, nv_tok.BOOL, nv_tok.STR
			lt_eval.push(anv_toks[i])
		case nv_tok.IDENT
			item = evalident(anv_toks[i])
			if item.kind <> nv_tok.ERR then
				lt_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
		case nv_tok.UNARYOP, nv_tok.BINARYOP
			item = evalop(anv_toks[i], lt_eval)
			if item.kind <> nv_tok.ERR then
				lt_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
		case nv_tok.FUNC
			item = evalfunc(anv_toks[i], lt_eval)
			if item.kind <> nv_tok.ERR then
				lt_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
	end choose
next
if lt_eval.size() = 1 then
	item = lt_eval.pop()
	if item.kind = nv_tok.INTG or item.kind = nv_tok.DECIM then
		ls_ret = string(item.value)
	elseif item.kind = nv_tok.BOOL then
		ls_ret = iif(item.value, 'True', 'False')
	elseif item.kind = nv_tok.STR then
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

public function boolean isop (nv_tok anv_tok);
// tell if the given text is an operator

/*
boolean lb_ret 

choose case anv_tok.value
	case '+', '-', '*', '/', '^'
		lb_ret = true
		
	case else
		lb_ret = false
end choose

return lb_ret
*/

return (anv_tok.kind = nv_tok.BINARYOP) or (anv_tok.kind = nv_tok.UNARYOP)

end function

public function boolean parse (nv_tok anv_tokens[]);
// parse the infix input token stream and transform it into prefix or postfix

boolean lb_ret = true
nv_tok lnv_tok, lt_func, empty_toks[]
nv_queue lq_out			//output queue
nv_tokstack lt_op		//intermediate operators/funcs stack
nv_stack lt_argcount	//arguments counter
nv_stack lt_args		//arguments presence
long ll_count

is_lasterror = ""

if upperbound(anv_tokens[]) < 1 then
	is_lasterror = "no input to parse"
	lb_ret = false
	goto end_of_shunt
end if

long t = 1
do while t <= upperbound(anv_tokens[])
	lnv_tok = anv_tokens[t]

	if lnv_tok.kind = nv_tok.INTG or lnv_tok.kind = nv_tok.DECIM or lnv_tok.kind = nv_tok.BOOL or lnv_tok.kind = nv_tok.IDENT or lnv_tok.kind = nv_tok.STR then
		//if the token is an operand, pass it to the output queue
		lq_out.push(lnv_tok)
		if lt_args.size() > 0 then lt_args.settop(true)
	elseif lnv_tok.kind = nv_tok.FUNC then
		//if it is a function, push it to the stack
		lt_op.push(lnv_tok)
		lt_argcount.push(0)
		if lt_args.size() > 0 then lt_args.settop(true)
		lt_args.push(false)
	elseif lnv_tok.kind = nv_tok.TARGSEP then
		//if it is a function argument separator
		//1) pop all pending operators until getting '('
		do while not lt_op.isempty() 
			if lt_op.top().value <> LPAR then
				lq_out.push(lt_op.pop())
			else
				exit //while
			end if
		loop
		if lt_op.isempty() or (lt_op.top().value <> LPAR) then
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
	elseif isop(lnv_tok) then
		//we have an operator, process operator precedence
		//if there are other pending operations
		do while not lt_op.isempty() 
			if ((getassoc(lnv_tok) = cc_left and getprec(lnv_tok) <= getprec(lt_op.top())) &
			or &
			(getassoc(lnv_tok) = cc_right and getprec(lnv_tok) < getprec(lt_op.top()))) then
				lq_out.push(lt_op.pop())
			else
				exit //while
			end if
		loop
		lt_op.push(lnv_tok)
	elseif lnv_tok.value = LPAR then
		//push a '(' to the stack
		lt_op.push(lnv_tok)
	elseif lnv_tok.value = RPAR then
		//process all pending operations up to '('
		do while not lt_op.isempty() 
			if lt_op.top().value <> LPAR then
				lq_out.push(lt_op.pop())
			else
				exit //while
			end if
		loop
		//just pop the '('
		if lt_op.top().value = LPAR then
			destroy lt_op.top()
			lt_op.pop()
		else
			//we might have a parenthesis mismatch
			is_lasterror = "mismatch parenthesis"
			lb_ret = false
			goto end_of_shunt
		end if
		if lt_op.top().kind = nv_tok.FUNC then 
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
	t++
loop
do while not lt_op.isempty()
	//add the final pending operators to the queue
	if lt_op.top().value = LPAR or lt_op.top().value = RPAR then
		//if there are still parenthesis on the stack, we have a problem
		is_lasterror = "mismatch parenthesis"
		lb_ret = false
		goto end_of_shunt
	end if
	lq_out.push(lt_op.pop())
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
		lt_op.push(lq_out.pop())
	loop
	do while not lt_op.isempty()
		ist_parsed[upperbound(ist_parsed[]) + 1] = lt_op.pop()
	loop
end if

end_of_shunt:
return lb_ret

end function

public function integer getprec (nv_tok anv_tok);
// return the precedence for an operator

int li_ret

if anv_tok.kind <> nv_tok.UNARYOP and anv_tok.kind <> nv_tok.BINARYOP then return 0

choose case anv_tok.value
	case '^'; 					li_ret = 8
	case '-'
		//special minus case
		if anv_tok.kind = nv_tok.UNARYOP then 
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

public function boolean isbool (any aa_tok);
//check if the given identifier is a boolean literal

boolean lb_ret = false

choose case lower(aa_tok)
	case 'true', "false"
		lb_ret = true
end choose

return lb_ret

end function

public function character getassoc (nv_tok ast_op);
// return the associativity for an operator

char lc_ret

choose case ast_op.value
	case '^', 'not'
		lc_ret = CC_RIGHT
	case '*', '/', '%'
		lc_ret = CC_LEFT
	case '+', '-', 'and', 'or', 'xor'
		if ast_op.kind = nv_tok.UNARYOP then
			lc_ret = CC_RIGHT
		else
			lc_ret = CC_LEFT
		end if
	case else
		lc_ret = CC_LEFT
end choose

return lc_ret

end function

public function nv_tok evalident (nv_tok ast_ident);
// evaluate the value of a function

nv_tok lt_ret, item
dec ldc_ret = 0, ldc_val
long i, max

lt_ret = create nv_tok

//get the type of the identifier, then push the value

choose case lower(ast_ident.value)
	case "pi"	//dummy function
		lt_ret.kind = nv_tok.DECIM
		lt_ret.value = 3.14
	case else
		max = upperbound(ia_vars[]) - 1
		for i = 1 to max step 2
			if ast_ident.value = ia_vars[i] then
				lt_ret = ia_vars[i+1]
				goto eval_done
			end if
		next
		lt_ret.kind = nv_tok.ERR
		is_lasterror = "cannot resolve `" + ast_ident.value + "`"
		lt_ret.value = is_lasterror
end choose

eval_done:
return lt_ret

end function

public function nv_tok evalop (nv_tok ast_op, ref nv_tokstack ast_args);
// evaluate the value of an operator

nv_tok lt_ret, st_op1, st_op2
dec ldc_op1, ldc_op2, ldc_res
any la_op1, la_op2, la_res
boolean lb_val
long i

lt_ret = create nv_tok

choose case ast_op.kind
	case nv_tok.UNARYOP
		choose case ast_op.value
			case '-', '+'
				st_op1 = ast_args.pop()
				if st_op1.kind <> nv_tok.DECIM and st_op1.kind <> nv_tok.INTG then
					lt_ret.kind = nv_tok.ERR
					lt_ret.value = "`" + ast_op.value + "` cannot handle " + st_op1.typename() + " `" + string(st_op1.value) + "` at " + string(ast_op.position)
					goto end_eval
				end if
				ldc_op1 = dec(st_op1.value)
				if ast_op.value = '-' then
					ldc_res = - ldc_op1
				elseif ast_op.value = '+' then
					ldc_res = ldc_op1
				end if
				lt_ret.kind = nv_tok.DECIM
				lt_ret.value = ldc_res
			case 'not'
				st_op1 = ast_args.pop()
				//TODO pourra être traité par un nv_tok.tobool
				if st_op1.kind <> nv_tok.BOOL then
					lt_ret.kind = nv_tok.ERR
					lt_ret.value = "`" + ast_op.value + "` cannot handle " + st_op1.typename() + " `" + string(st_op1.value) + "` at " + string(ast_op.position)
					goto end_eval
				end if
				lt_ret.kind = nv_tok.BOOL
				lt_ret.value = not(st_op1.value)
		end choose
	case nv_tok.BINARYOP
			if ast_args.size() >= 2 then
				//TODO, assume this is 2 numerical tokens
				//need to improve type checking / inference
				choose case ast_op.value
					case '+', '-', '*', '/', '%', '^'
						if not ast_args.top().iscompatiblewith(ast_args.peek(1)) then
							//Coercion to first operand
							if ast_args.peek(1).kind = nv_tok.STR then
								if ast_op.value <> '+' then
									lt_ret.kind = nv_tok.ERR
									lt_ret.value = "eval failed: mismatch operands (" + ast_args.peek(1).typename() + '/' + ast_args.top().typename() + ") for `" + ast_op.value + "` at " + string(ast_op.position)
									goto end_eval
								else
									la_op2 = string(ast_args.pop().value)
									lt_ret.kind = nv_tok.STR
								end if
							elseif ast_args.peek(1).kind = nv_tok.DECIM or ast_args.peek(1).kind = nv_tok.INTG then
								if match(ast_args.top().value, is_numbercharpattern) then
									la_op2 = dec(fixdecimal(ast_args.pop().value))
									lt_ret.kind = nv_tok.DECIM
								else
									lt_ret.kind = nv_tok.ERR
									lt_ret.value = "eval failed: cannot convert `" + ast_args.top().value + "` to numeric for `" + ast_op.value + "` at " + string(ast_op.position)
									goto end_eval
								end if
							end if
							la_op1 = ast_args.pop().value
						else
							//same operand types
							lt_ret.kind = ast_args.top().kind
							la_op2 = ast_args.pop().value
							la_op1 = ast_args.pop().value
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
						lt_ret.kind = nv_tok.BOOL
						lt_ret.value = lb_val
					case '=', '<>', 'and', 'or', 'xor'
						st_op2 = ast_args.pop()
						st_op1 = ast_args.pop()
						if st_op1.kind <> st_op2.kind then
							lt_ret.kind = nv_tok.ERR
							lt_ret.value = "eval failed: mismatch operands (" + st_op1.typename() + '/' + st_op2.typename() + ") for `" + ast_op.value + "` at " + string(ast_op.position)
							goto end_eval
						end if
						//TODO pourra être traité par un nv_tok.tobool
						if (ast_op.value = 'and' or ast_op.value = 'or') and st_op1.kind <> nv_tok.BOOL then
							lt_ret.kind = nv_tok.ERR
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
						lt_ret.kind = nv_tok.BOOL
						lt_ret.value = lb_val
					case else
						lt_ret.kind = nv_tok.ERR
						lt_ret.value = "unknown op " + ast_op.value
						goto end_eval
				end choose
			else
				lt_ret.kind = nv_tok.ERR
				lt_ret.value = "broken expression : wrong number of operands for `" + ast_op.value + "` at " + string(ast_op.position)
				goto end_eval
			end if
	case else
		is_lasterror = "cannot evaluate op `" + ast_op.value + "`"
		lt_ret.kind = nv_tok.ERR
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
nv_tok lt_tok

max = upperbound(aa_vals[])
if mod(max, 2) <> 0 then return false

n = 1
for i = 1 to max step 2
	if not isbool(aa_vals[i]) &
		and not isfunc(aa_vals[i]) /*TODO : and not isOP*/ then

		lt_tok = create nv_tok
		choose case classname(aa_vals[i+1])
			case "boolean"
				lt_tok.kind = nv_tok.BOOL
				lt_tok.value = aa_vals[i+1] //iif(lower(string(aa_vals[i+1])) = "true", true, false)
			case "integer"
				lt_tok.kind = nv_tok.INTG
				lt_tok.value = aa_vals[i+1]
			case "decimal"
				lt_tok.kind = nv_tok.DECIM
				lt_tok.value = aa_vals[i+1]
			case "string"
				if left(aa_vals[i+1],1) = right(aa_vals[i+1],1) then
					lt_tok.kind = nv_tok.STR
					lt_tok.value = mid(aa_vals[i+1], 2, len(string(aa_vals[i+1])) - 2)
				end if
		end choose
		ia_vars[n+1] = lt_tok
		ia_vars[n] = lower(aa_vals[i])
		n += 2
	end if
next

return true

end function

protected function nv_tok evalfunc (nv_tok ast_func, ref nv_tokstack ast_args);
// evaluate the value of a function

nv_tok lt_ret, item
dec ldc_ret = 0, ldc_val
long i

lt_ret = create nv_tok
choose case ast_func.value
	case "answer"	//dummy function
		lt_ret.kind = nv_tok.DECIM
		lt_ret.value = 42
	case "sum"
		for i = 1 to ast_func.count
			ldc_val = dec(ast_args.pop().value)
			ldc_ret += ldc_val
		next
		lt_ret.kind = nv_tok.DECIM
		lt_ret.value = ldc_ret
	case "mul"
		if ast_args.size() > 0 then ldc_ret = dec(ast_args.pop().value)
		for i = 2 to ast_func.count
			ldc_val = dec(ast_args.pop().value)
			ldc_ret *= ldc_val
		next
		lt_ret.kind = nv_tok.DECIM
		lt_ret.value = ldc_ret
	case "abs"
		if ast_func.count = 1 then 
			ldc_ret = abs(dec(ast_args.pop().value))
			lt_ret.kind = nv_tok.DECIM
			lt_ret.value = ldc_ret
		else
			is_lasterror = "abs() needs 1 argument"
			lt_ret.kind = nv_tok.ERR
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
			lt_ret.kind = nv_tok.DECIM
			lt_ret.value = ldc_ret
		else
			is_lasterror = "missing argument"
			lt_ret.kind = nv_tok.ERR
			lt_ret.value = is_lasterror
		end if
	case "max"
		if ast_args.size() > 0 then 
			ldc_ret = dec(ast_args.pop().value)
			for i = 2 to ast_func.count
				ldc_val = dec(ast_args.pop().value)
				if ldc_val > ldc_ret then ldc_ret = ldc_val
			next
			lt_ret.kind = nv_tok.DECIM
			lt_ret.value = ldc_ret
		else
			is_lasterror = "missing argument"
			lt_ret.kind = nv_tok.ERR
			lt_ret.value = is_lasterror
		end if
	case "len"
		if ast_func.count = 1 then 
			if ast_args.top().kind = nv_tok.STR then
				ldc_ret = len(string(ast_args.pop().value))
				lt_ret.kind = nv_tok.DECIM
				lt_ret.value = ldc_ret
			else
				is_lasterror = "len() can only take string argument, given " + ast_args.top().typename()
				lt_ret.kind = nv_tok.ERR
				lt_ret.value = is_lasterror
			end if
		else
			is_lasterror = "len() needs 1 argument"
			lt_ret.kind = nv_tok.ERR
			lt_ret.value = is_lasterror
		end if
	case "msgbox"
		if ast_func.count = 1 then
			if ast_args.top().kind = nv_tok.STR then
				ldc_ret = MessageBox("Evaluator", string(ast_args.pop().value))
				lt_ret.kind = nv_tok.INTG
				lt_ret.value = ldc_ret
			else
				is_lasterror = "msgbox() can only take string argument, given " + ast_args.top().typename()
				lt_ret.kind = nv_tok.ERR
				lt_ret.value = is_lasterror
			end if
		else
			is_lasterror = "msgbox() needs 1 argument"
			lt_ret.kind = nv_tok.ERR
			lt_ret.value = is_lasterror
		end if
	case else
		is_lasterror = "cannot evaluate function `" + ast_func.value + "`"
		lt_ret.kind = nv_tok.ERR
		lt_ret.value = is_lasterror
end choose

if lt_ret.kind = nv_tok.ERR then 
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

