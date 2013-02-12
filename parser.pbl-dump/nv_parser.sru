forward
global type nv_parser from nonvisualobject
end type
end forward

global type nv_parser from nonvisualobject autoinstantiate
end type

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

private:

st_tok ist_tokens[]	//array for tokenized input
st_tok ist_parsed[]	//array for parsed tokens
boolean ib_postfix = true

string is_lasterror

//associativities
constant char CC_LEFT = 'L'
constant char CC_RIGHT = 'R'

//types of tokens
constant char ARGSEP = ';'			//argument separator for funcalls
constant string NUM = "Num"			//numeric type
constant string BOOL = "Bool"		//boolean type
constant string STR = "Str"			//string type
constant string UNARYOP = "UnOp"	//unary operator
constant string BINARYOP = "BinOp"	//binary operator 
constant string IDENT = "Id"		//identifier (variable)
constant string FUNC = "Func"		//function
constant string LPAR = "("
constant string RPAR = ")"


end variables
forward prototypes
public subroutine setreverse (boolean ab_reverse)
public function string getlasterror ()
public function boolean tokenize (string as_input)
public function boolean iswordchar (character ac_char)
public function boolean gettokens (ref st_tok ast_tokens[])
public function boolean getparsed (ref st_tok ast_expr[])
public function string eval (st_tok ast_toks[])
public function boolean isop (st_tok ast_tok)
public function boolean parse (st_tok ast_tokens[])
public function string tokentostring (st_tok ast_token)
public function integer getprec (st_tok ast_tok)
public function boolean isbool (any aa_tok)
public function character getassoc (st_tok ast_op)
public function st_tok evalfunc (st_tok ast_func, ref nv_tokstack ast_args)
public function boolean isfunc (st_tok ast_tok)
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

boolean lb_ret = true
long ll_tk_start = 1, ll_tk_end, ll_inplen, ll_tokens
st_tok lst_tok
any la_empty[]
char lc, prec

is_lasterror = ""
ist_tokens = la_empty[]
ll_inplen= len(as_input)

do while ll_tk_start <= ll_inplen
	lst_tok.value = ""
	lst_tok.kind = ""
	ll_tk_end = ll_tk_start
	
	lc = mid(as_input, ll_tk_start, 1)
	choose case lc
		case '0' to '9' 
			do while (ll_tk_end <= ll_inplen) and isNumber(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start + 1))
				ll_tk_end++
			loop 
			lst_tok.value = dec(trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)))
			lst_tok.kind = NUM
		case '*', '/', '^', '%', '=', '<', '>'
			ll_tk_end++
			if lc = '<' or lc = '>' and ll_tk_end < ll_inplen then //lookahead ;)
				if mid(as_input, ll_tk_end, 1) = '=' &
				or mid(as_input, ll_tk_end, 1) = '>' then
					//TODO warning, can have >>
					ll_tk_end++	
				end if
			end if
			lst_tok.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			lst_tok.kind = BINARYOP
		case '+', '-'
			ll_tk_end++
			lst_tok.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			ll_tokens = upperbound(ist_tokens[])
			if ll_tokens = 0 then
				lst_tok.kind = UNARYOP
			elseif ist_tokens[ll_tokens].kind = NUM &
				or ist_tokens[ll_tokens].kind = IDENT &
				or ist_tokens[ll_tokens].kind = RPAR  then
				lst_tok.kind = BINARYOP
			else
				lst_tok.kind = UNARYOP
			end if
		case 'A' to 'Z', 'a' to 'z', '_'
			do while (ll_tk_end <= ll_inplen) and isWordChar(mid(as_input, ll_tk_end, 1))
				ll_tk_end++
			loop 
			lst_tok.value = trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start))
			if isbool(lst_tok.value) then
				lst_tok.value = iif(lower(lst_tok.value)="true", true, false)
				lst_tok.kind = BOOL
			elseif lower(lst_tok.value) = "and" or lower(lst_tok.value) = "or" then
				lst_tok.value = lower(lst_tok.value)
				lst_tok.kind = BINARYOP
			elseif lower(lst_tok.value) = "not" then
				lst_tok.value = lower(lst_tok.value)
				lst_tok.kind = UNARYOP
			else
				lst_tok.kind = IDENT
			end if
		case ARGSEP, LPAR, RPAR
			ll_tk_end++
			lst_tok.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			lst_tok.kind = lst_tok.value
		case ' ' 
			ll_tk_start++		//ignore blanks
			continue
		case else
			is_lasterror = "possible unexpected char : " + lc
			lb_ret = false
			goto exit_tokenize
	end choose
	lst_tok.position = ll_tk_start
	ist_tokens[upperbound(ist_tokens[]) + 1] = lst_tok
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

public function boolean gettokens (ref st_tok ast_tokens[]);
// helper for the tokens read from the input

ast_tokens[] = ist_tokens[]

return upperbound(ist_tokens[]) > 0


end function

public function boolean getparsed (ref st_tok ast_expr[]);
// helper for the parsed tokens

ast_expr[] = ist_parsed[]

return true

end function

public function string eval (st_tok ast_toks[]);
/*
	Evaluation of the token stream after parsing

	the evaluator reads the tokens array 
	and uses a stack for intermediate evaluations 
	including function calls
*/

nv_tokstack lst_eval
st_tok item, st_op1, st_op2
dec ldc_op1, ldc_op2, ldc_res
any la_op1, la_op2
boolean lb_res
string ls_ret = "", ls_val
long i, n

n = upperbound(ast_toks[]) 
if n = 0 then return ""

for i = 1 to n
	ls_val = lower(string(ast_toks[i].value))
	choose case ast_toks[i].kind
		case NUM
			lst_eval.push(ast_toks[i])
		case BOOL
			lst_eval.push(ast_toks[i])
		case UNARYOP
			choose case ls_val
				case '-', '+'
					ldc_op1 = dec(lst_eval.pop().value)
					if ls_val = '-' then
						ldc_res = - ldc_op1
					elseif ls_val = '+' then
						ldc_res = ldc_op1
					end if
					item.kind = NUM
					item.value = ldc_res
				case 'not'
					item.kind = BOOL
					item.value = not(lst_eval.pop().value)
			end choose
			lst_eval.push(item)
		case BINARYOP
			if lst_eval.size() >= 2 then
				//TODO, assume this is 2 numerical tokens
				//need to improve type checking / inference
				choose case ls_val
					case '+', '-', '*', '/', '%', '^'
						ldc_op2 = dec(lst_eval.pop().value)
						ldc_op1 = dec(lst_eval.pop().value)
						choose case ls_val
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
						item.value = ldc_res
						item.kind = NUM
						lst_eval.push(item)
					case '<', '<=', '>', '>='
						la_op2 = lst_eval.pop().value
						la_op1 = lst_eval.pop().value
						choose case ls_val
							case '<'
								lb_res = la_op1 < la_op2
							case '<=', '=<'
								lb_res = la_op1 <= la_op2
							case '>'
								lb_res = la_op1 > la_op2
							case '>=', '=>'
								lb_res = la_op1 >= la_op2
						end choose
						item.value = lb_res
						item.kind = BOOL
						lst_eval.push(item)
					case '=', '<>', 'and', 'or'
						st_op2 = lst_eval.pop()
						st_op1 = lst_eval.pop()
						if st_op1.kind <> st_op2.kind then
							ls_ret = "broken expression : mismatch operands (" + st_op1.kind + '/' + st_op2.kind + ") for `" + ls_val + "` at " + string(ast_toks[i].position)
							goto end_eval
						end if
						choose case ls_val
							case '='
								lb_res = (st_op1.value = st_op2.value)
							case '<>'
								lb_res = (st_op1.value <> st_op2.value)
							case 'and'
								lb_res = (st_op1.value and st_op2.value)
							case 'or'
								lb_res = (st_op1.value or st_op2.value)
						end choose
						item.value = lb_res
						item.kind = BOOL
						lst_eval.push(item)
					case else
						ls_ret = "unknown op " + ls_val
						goto end_eval
				end choose
			else
				ls_ret = "broken expression : wrong number of operands for `" + ls_val + "` at " + string(ast_toks[i].position)
				goto end_eval
			end if

		case FUNC
			item = evalfunc(ast_toks[i], lst_eval)
			lst_eval.push(item)
	end choose
next
if lst_eval.size() = 1 then
	item = lst_eval.pop()
	if item.kind = NUM then
		ls_ret = string(item.value)
	elseif item.kind = BOOL then
		ls_ret = iif(item.value, 'True', 'False')
	elseif item.kind = STR then
		ls_ret = item.value
	else
		ls_ret = "not sure how to process " + item.kind + " `" + string(item.value) + "`"
	end if
else
	ls_ret = "evaluation failed"
end if

end_eval:
return ls_ret

end function

public function boolean isop (st_tok ast_tok);
// tell if the given text is an operator

/*
boolean lb_ret 

choose case ast_tok.value
	case '+', '-', '*', '/', '^'
		lb_ret = true
		
	case else
		lb_ret = false
end choose

return lb_ret
*/

return (ast_tok.kind = BINARYOP) or (ast_tok.kind = UNARYOP)

end function

public function boolean parse (st_tok ast_tokens[]);
// parse the infix input token stream and transform it into prefix or postfix

boolean lb_ret = true
st_tok lst_tok, lst_func, empty_toks[]
nv_queue lq_out			//output queue
nv_tokstack lst_op		//intermediate operators/funcs stack
nv_stack lst_argcount	//arguments counter
nv_stack lst_args		//arguments presence
long ll_count

is_lasterror = ""

if upperbound(ast_tokens[]) < 1 then
	is_lasterror = "no input to parse"
	lb_ret = false
	goto end_of_shunt
end if

long t = 1
do while t <= upperbound(ast_tokens[])
	lst_tok = ast_tokens[t]

	if lst_tok.kind = NUM or lst_tok.kind = BOOL then
		//if the token is an operand, pass it to the output queue
		lq_out.push(lst_tok)
		if lst_args.size() > 0 then lst_args.settop(true)
	elseif isfunc(lst_tok) then
		//if it is a function, push it to the stack
		lst_tok.kind = FUNC
		lst_op.push(lst_tok)
		lst_argcount.push(0)
		if lst_args.size() > 0 then lst_args.settop(true)
		lst_args.push(false)
	elseif lst_tok.value = ARGSEP then
		//if it is a function argument separator
		//1) pop all pending operators until getting '('
		do while not lst_op.isempty() and lst_op.top().value <> LPAR
			lq_out.push(lst_op.pop())
		loop
		if lst_op.isempty() or (lst_op.top().value <> LPAR) then
			//if stack is empty or we did not get a '('
			//there is an expression error
			is_lasterror = "bad argument separator or parenthesis mismatch"
			lb_ret = false
			goto end_of_shunt
		end if
		//2) remember arg count
		if lst_args.top() = true then
			ll_count = lst_argcount.top()
			lst_argcount.settop(ll_count + 1)
		end if
		lst_args.push(false)
	elseif isop(lst_tok) then
		//we have an operator, process operator precedence
		//if there are other pending operations
		do while not lst_op.isempty() &
			and ((getassoc(lst_tok) = cc_left and getprec(lst_tok) <= getprec(lst_op.top())) &
			or &
			(getassoc(lst_tok) = cc_right and getprec(lst_tok) < getprec(lst_op.top()))) 
			lq_out.push(lst_op.pop())
		loop
		lst_op.push(lst_tok)
	elseif lst_tok.value = LPAR then
		//push a '(' to the stack
		lst_op.push(lst_tok)
	elseif lst_tok.value = RPAR then
		//process all pending operations up to '('
		do while not lst_op.isempty() and lst_op.top().value <> LPAR
			lq_out.push(lst_op.pop())
		loop
		//just pop the '('
		if lst_op.top().value = LPAR then 
			lst_op.pop()
		else
			//we might have a parenthesis mismatch
			is_lasterror = "mismatch parenthesis"
			lb_ret = false
			goto end_of_shunt
		end if
		if isfunc(lst_op.top()) then 
			//if there is a function after the paren
			//add it to the queue with arguments count
			ll_count = lst_argcount.pop()
			if lst_args.pop() then ll_count ++
			lst_func = lst_op.pop()
			lst_func.count = ll_count
			lq_out.push(lst_func)
		end if
	//else what to do ?
	end if
	t++
loop
do while not lst_op.isempty()
	//add the final pending operators to the queue
	if lst_op.top().value = LPAR or lst_op.top().value = RPAR then
		//if there are still parenthesis on the stack, we have a problem
		is_lasterror = "mismatch parenthesis"
		lb_ret = false
		goto end_of_shunt
	end if
	lq_out.push(lst_op.pop())
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
		lst_op.push(lq_out.pop())
	loop
	do while not lst_op.isempty()
		ist_parsed[upperbound(ist_parsed[]) + 1] = lst_op.pop()
	loop
end if

end_of_shunt:
return lb_ret

end function

public function string tokentostring (st_tok ast_token);
//return a string representation of a token
string ls_ret

if ast_token.kind = FUNC then
	//ls_ret = ast_token.value + '(' + string(ast_token.count) + ')'
	ls_ret = ast_token.value + '.' + string(ast_token.count)
elseif ast_token.kind = UNARYOP then
	if ast_token.value = '-' then 
		ls_ret = '_'
	else
		ls_ret = ast_token.value
	end if
elseif ast_token.kind = BOOL or ast_token.kind = NUM then
	ls_ret = string(ast_token.value)
else
	ls_ret = ast_token.value
end if

return ls_ret

end function

public function integer getprec (st_tok ast_tok);
// return the precedence for an operator

int li_ret

choose case ast_tok.value
	case '^'; 					li_ret = 8
	case '-'
		//special minus case
		if ast_tok.kind = UNARYOP then 
			li_ret = 7
		else
			li_ret = 5
		end if
	case '*', '/', '%';			li_ret = 6
	case '+' /*- above*/;		li_ret = 5
	case '<', '<=', '>', '>=';	li_ret = 4
	case '=', '<>';				li_ret = 3
	case 'and', 'or';			li_ret = 2
	case 'not';					li_ret = 1
	case else;					li_ret = 0
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

public function character getassoc (st_tok ast_op);
// return the associativity for an operator

char lc_ret

choose case ast_op.value
	case '^', 'not'
		lc_ret = CC_RIGHT
	case '*', '/', '%'
		lc_ret = CC_LEFT
	case '+', '-', 'and', 'or'
		if ast_op.kind = UNARYOP then
			lc_ret = CC_RIGHT
		else
			lc_ret = CC_LEFT
		end if
	case else
		lc_ret = CC_LEFT
end choose

return lc_ret

end function

public function st_tok evalfunc (st_tok ast_func, ref nv_tokstack ast_args);
// evaluate the value of a function

st_tok lst_ret, item
dec ldc_ret = 0, ldc_val
long i

choose case ast_func.value
	case "answer"	//dummy function
		lst_ret.kind = NUM
		lst_ret.value = 42
	case "sum"
		for i = 1 to ast_func.count
			ldc_val = dec(ast_args.pop().value)
			ldc_ret += ldc_val
		next
		lst_ret.kind = NUM
		lst_ret.value = ldc_ret
	case "mul"
		if ast_args.size() > 0 then ldc_ret = dec(ast_args.pop().value)
		for i = 2 to ast_func.count
			ldc_val = dec(ast_args.pop().value)
			ldc_ret *= ldc_val
		next
		lst_ret.kind = NUM
		lst_ret.value = ldc_ret
	case "abs"
		if ast_func.count = 1 then 
			ldc_ret = abs(dec(ast_args.pop().value))
			lst_ret.kind = NUM
			lst_ret.value = ldc_ret
		else
			is_lasterror = "abs() needs 1 argument"
		end if
	/*case "not"
		if ast_func.count = 1 then
			lst_ret.kind = BOOL
			lst_ret.value = not(ast_args.pop().value)
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
			lst_ret.kind = NUM
			lst_ret.value = ldc_ret
		else
			is_lasterror = "missing argument"
		end if
	case "max"
		if ast_args.size() > 0 then 
			ldc_ret = dec(ast_args.pop().value)
			for i = 2 to ast_func.count
				ldc_val = dec(ast_args.pop().value)
				if ldc_val > ldc_ret then ldc_ret = ldc_val
			next
			lst_ret.kind = NUM
			lst_ret.value = ldc_ret
		else
			is_lasterror = "missing argument"
		end if

end choose

return lst_ret

end function

public function boolean isfunc (st_tok ast_tok);
//check if the given identifier is a function

boolean lb_ret = false

choose case lower(ast_tok.value)
	case 'answer', "sum", "mul", "abs", "min", "max"//, "not"
		lb_ret = true
end choose

return lb_ret

end function

on nv_parser.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_parser.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

