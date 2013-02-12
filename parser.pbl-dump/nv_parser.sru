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
public:
//associativities
constant char CC_LEFT = 'L'
constant char CC_RIGHT = 'R'

//types of tokens
constant integer UNDEF = 0
constant integer DECIM = 2			//numeric type
constant integer BOOL = 3			//boolean type
constant integer STR = 4			//string type
constant integer UNARYOP = 5		//unary operator
constant integer BINARYOP = 6		//binary operator 
constant integer IDENT = 7			//identifier (variable)
constant integer FUNC = 8			//function
constant integer TARGSEP = 9		//function
constant integer TLPAR = 10		//function
constant integer TRPAR = 11		//function
constant integer ERR = 11			//error

constant char ARGSEP = ';'			//argument separator for funcalls
constant char LPAR = '('
constant char RPAR = ')'

private:
st_tok ist_tokens[]	//array for tokenized input
st_tok ist_parsed[]	//array for parsed tokens
boolean ib_postfix = true

string is_lasterror


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
public function st_tok evalident (st_tok ast_ident)
public function string typename (integer ai_type)
public function st_tok evalop (st_tok ast_op, ref nv_tokstack ast_args)
public function boolean isfunc (string as_name)
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
char lc, prec, lc_quote

is_lasterror = ""
ist_tokens = la_empty[]
ll_inplen= len(as_input)

do while ll_tk_start <= ll_inplen
	lst_tok.value = ""
	lst_tok.kind = UNDEF
	ll_tk_end = ll_tk_start
	
	lc = mid(as_input, ll_tk_start, 1)
	choose case lc
		case '0' to '9' 
			do while (ll_tk_end <= ll_inplen) and isNumber(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start + 1))
				ll_tk_end++
			loop 
			lst_tok.value = dec(trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)))
			lst_tok.kind = DECIM
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
			elseif ist_tokens[ll_tokens].kind = DECIM &
				or ist_tokens[ll_tokens].kind = IDENT &
				or ist_tokens[ll_tokens].kind = TRPAR  then
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
			elseif isfunc(lst_tok.value) then
				lst_tok.kind = FUNC
				lst_tok.count = -1
			elseif lower(lst_tok.value) = "and" or lower(lst_tok.value) = "or" or lower(lst_tok.value) = "xor" then
				lst_tok.value = lower(lst_tok.value)
				lst_tok.kind = BINARYOP
			elseif lower(lst_tok.value) = "not" then
				lst_tok.value = lower(lst_tok.value)
				lst_tok.kind = UNARYOP
			else
				lst_tok.kind = IDENT
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
				lst_tok.value = mid(as_input, ll_begin + 1, ll_cur - ll_begin - 1)
				lst_tok.kind = STR
			end if			
		case ARGSEP, LPAR, RPAR
			ll_tk_end++
			lst_tok.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			choose case lc
				case ARGSEP; lst_tok.kind = TARGSEP
				case LPAR; lst_tok.kind = TLPAR
				case RPAR; lst_tok.kind = TRPAR
			end choose
		case ' ' 
			ll_tk_start++		//ignore blanks
			continue
		case else
			is_lasterror = "possible unexpected char : `" + lc +"` at " + string(ll_tk_start)
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
		case DECIM, BOOL, STR
			lst_eval.push(ast_toks[i])
		case IDENT
			item = evalident(ast_toks[i])
			if item.kind <> ERR then
				lst_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
		case UNARYOP, BINARYOP
			item = evalop(ast_toks[i], lst_eval)
			if item.kind <> ERR then
				lst_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
		case FUNC
			item = evalfunc(ast_toks[i], lst_eval)
			if item.kind <> ERR then
				lst_eval.push(item)
			else
				ls_ret = item.value
				goto end_eval
			end if
	end choose
next
if lst_eval.size() = 1 then
	item = lst_eval.pop()
	if item.kind = DECIM then
		ls_ret = string(item.value)
	elseif item.kind = BOOL then
		ls_ret = iif(item.value, 'True', 'False')
	elseif item.kind = STR then
		ls_ret = item.value
	else
		ls_ret = "not sure how to process " + typename(item.kind) + " `" + string(item.value) + "`"
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

	if lst_tok.kind = DECIM or lst_tok.kind = BOOL or lst_tok.kind = IDENT or lst_tok.kind = STR then
		//if the token is an operand, pass it to the output queue
		lq_out.push(lst_tok)
		if lst_args.size() > 0 then lst_args.settop(true)
	elseif lst_tok.kind = FUNC then
		//if it is a function, push it to the stack
		lst_op.push(lst_tok)
		lst_argcount.push(0)
		if lst_args.size() > 0 then lst_args.settop(true)
		lst_args.push(false)
	elseif lst_tok.kind = TARGSEP then
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
		//if lst_args.size() > 0 then
			if lst_args.top() = true then
				ll_count = lst_argcount.top()
				lst_argcount.settop(ll_count + 1)
			end if
		//end if
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
		if lst_op.top().kind = FUNC then 
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
	ls_ret = ast_token.value + '.' + iif(ast_token.count > -1, string(ast_token.count), '?')
elseif ast_token.kind = UNARYOP then
	if ast_token.value = '-' then 
		ls_ret = '_'
	else
		ls_ret = ast_token.value
	end if
elseif ast_token.kind = BOOL or ast_token.kind = DECIM then
	ls_ret = string(ast_token.value)
else
	ls_ret = ast_token.value
end if

return ls_ret

end function

public function integer getprec (st_tok ast_tok);
// return the precedence for an operator

int li_ret

if ast_tok.kind <> UNARYOP and ast_tok.kind <> BINARYOP then return 0

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

public function character getassoc (st_tok ast_op);
// return the associativity for an operator

char lc_ret

choose case ast_op.value
	case '^', 'not'
		lc_ret = CC_RIGHT
	case '*', '/', '%'
		lc_ret = CC_LEFT
	case '+', '-', 'and', 'or', 'xor'
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
		lst_ret.kind = DECIM
		lst_ret.value = 42
	case "sum"
		for i = 1 to ast_func.count
			ldc_val = dec(ast_args.pop().value)
			ldc_ret += ldc_val
		next
		lst_ret.kind = DECIM
		lst_ret.value = ldc_ret
	case "mul"
		if ast_args.size() > 0 then ldc_ret = dec(ast_args.pop().value)
		for i = 2 to ast_func.count
			ldc_val = dec(ast_args.pop().value)
			ldc_ret *= ldc_val
		next
		lst_ret.kind = DECIM
		lst_ret.value = ldc_ret
	case "abs"
		if ast_func.count = 1 then 
			ldc_ret = abs(dec(ast_args.pop().value))
			lst_ret.kind = DECIM
			lst_ret.value = ldc_ret
		else
			is_lasterror = "abs() needs 1 argument"
			lst_ret.kind = ERR
			lst_ret.value = is_lasterror
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
			lst_ret.kind = DECIM
			lst_ret.value = ldc_ret
		else
			is_lasterror = "missing argument"
			lst_ret.kind = ERR
			lst_ret.value = is_lasterror
		end if
	case "max"
		if ast_args.size() > 0 then 
			ldc_ret = dec(ast_args.pop().value)
			for i = 2 to ast_func.count
				ldc_val = dec(ast_args.pop().value)
				if ldc_val > ldc_ret then ldc_ret = ldc_val
			next
			lst_ret.kind = DECIM
			lst_ret.value = ldc_ret
		else
			is_lasterror = "missing argument"
			lst_ret.kind = ERR
			lst_ret.value = is_lasterror
		end if
	case "len"
		if ast_func.count = 1 then 
			if ast_args.top().kind = STR then
				ldc_ret = len(string(ast_args.pop().value))
				lst_ret.kind = DECIM
				lst_ret.value = ldc_ret
			else
				is_lasterror = "len() con only take string argument"
				lst_ret.kind = ERR
				lst_ret.value = is_lasterror
			end if
		else
			is_lasterror = "len() needs 1 argument"
			lst_ret.kind = ERR
			lst_ret.value = is_lasterror
		end if
	case else
		is_lasterror = "cannot evaluate function `" + ast_func.value + "`"
		lst_ret.kind = ERR
		lst_ret.value = is_lasterror
end choose

if lst_ret.kind = ERR then 
	string ls_tmp
	ls_tmp = lst_ret.value
	ls_tmp += " at " + string(ast_func.position)
	lst_ret.value = ls_tmp
end if

return lst_ret

end function

public function st_tok evalident (st_tok ast_ident);
// evaluate the value of a function

st_tok lst_ret, item
dec ldc_ret = 0, ldc_val
long i

//get the type of the identifier, then push the value

choose case lower(ast_ident.value)
	case "pi"	//dummy function
		lst_ret.kind = DECIM
		lst_ret.value = 3.14
	case else
		lst_ret.kind = ERR
		is_lasterror = "cannot resolve `" + ast_ident.value + "`"
		lst_ret.value = is_lasterror
end choose

return lst_ret

end function

public function string typename (integer ai_type);
//return the stringified type 
string ls_name

choose case ai_type

	case DECIM;		ls_name = "Num"			//numeric type
	case BOOL;		ls_name = "Bool"			//boolean type
	case STR;		ls_name = "String"			//string type
	case UNARYOP;	ls_name = "UnOp"		//unary operator
	case BINARYOP;	ls_name = "BinOp"	//binary operator 
	case IDENT;		ls_name = "Id"			//identifier (variable)
	case FUNC;		ls_name = "Func"			//function
	case TARGSEP, TLPAR, TRPAR;		ls_name = ""
	case ERR;		ls_name = "Err"			//error
end choose

return ls_name

end function

public function st_tok evalop (st_tok ast_op, ref nv_tokstack ast_args);
// evaluate the value of an operator

st_tok lst_ret, st_op1, st_op2
dec ldc_op1, ldc_op2, ldc_res
any la_op1, la_op2
boolean lb_val
long i

choose case ast_op.kind
	case UNARYOP
		choose case ast_op.value
			case '-', '+'
				st_op1 = ast_args.pop()
				if st_op1.kind <> DECIM then
					lst_ret.kind = ERR
					lst_ret.value = "`" + ast_op.value + "` cannot handle " + typename(st_op1.kind) + " `" + string(st_op1.value) + "` at " + string(ast_op.position)
					goto end_eval
				end if
				ldc_op1 = dec(st_op1.value)
				if ast_op.value = '-' then
					ldc_res = - ldc_op1
				elseif ast_op.value = '+' then
					ldc_res = ldc_op1
				end if
				lst_ret.kind = DECIM
				lst_ret.value = ldc_res
			case 'not'
				st_op1 = ast_args.pop()
				//TODO pourra être traité par un st_tok.tobool
				if st_op1.kind <> BOOL then
					lst_ret.kind = ERR
					lst_ret.value = "`" + ast_op.value + "` cannot handle " + typename(st_op1.kind) + " `" + string(st_op1.value) + "` at " + string(ast_op.position)
					goto end_eval
				end if
				lst_ret.kind = BOOL
				lst_ret.value = not(st_op1.value)
		end choose
	case BINARYOP
			if ast_args.size() >= 2 then
				//TODO, assume this is 2 numerical tokens
				//need to improve type checking / inference
				choose case ast_op.value
					case '+', '-', '*', '/', '%', '^'
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
						lst_ret.kind = DECIM
						lst_ret.value = ldc_res
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
						lst_ret.kind = BOOL
						lst_ret.value = lb_val
					case '=', '<>', 'and', 'or', 'xor'
						st_op2 = ast_args.pop()
						st_op1 = ast_args.pop()
						if st_op1.kind <> st_op2.kind then
							lst_ret.kind = ERR
							lst_ret.value = "broken expression : mismatch operands (" + typename(st_op1.kind) + '/' + typename(st_op2.kind) + ") for `" + ast_op.value + "` at " + string(ast_op.position)
							goto end_eval
						end if
						//TODO pourra être traité par un st_tok.tobool
						if (ast_op.value = 'and' or ast_op.value = 'or') and st_op1.kind <> BOOL then
							lst_ret.kind = ERR
							lst_ret.value = "`" + ast_op.value + "` cannot handle `" + typename(st_op1.kind) + "` at " + string(ast_op.position)
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
						lst_ret.kind = BOOL
						lst_ret.value = lb_val
					case else
						lst_ret.kind = ERR
						lst_ret.value = "unknown op " + ast_op.value
						goto end_eval
				end choose
			else
				lst_ret.kind = ERR
				lst_ret.value = "broken expression : wrong number of operands for `" + ast_op.value + "` at " + string(ast_op.position)
				goto end_eval
			end if
	case else
		is_lasterror = "cannot evaluate op `" + ast_op.value + "`"
		lst_ret.kind = ERR
		lst_ret.value = is_lasterror
end choose

end_eval:
return lst_ret

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

on nv_parser.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_parser.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

