forward
global type nv_parser from nonvisualobject
end type
end forward

global type nv_parser from nonvisualobject autoinstantiate
end type

type variables
/*
	Shunting-Yard evaluator - Sebastien KIRCHE 2011-2013

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
constant char ARGSEP = ';'				//argument separator for funcalls
constant string NUMERIC = "Num"		//numeric type
constant string STR = "Str"			//string type
constant string UNARYOP = "UnOp"		//unary operator
constant string BINARYOP = "BinOp"	//binary operator 
constant string IDENT = "Id"			//identifier (variable)
constant string FUNC = "Func"			//function
constant string LPAR = "("
constant string RPAR = ")"


end variables
forward prototypes
public subroutine setreverse (boolean ab_reverse)
public function string getlasterror ()
public function boolean tokenize (string as_input)
public function boolean isfunc (any aa_tok)
public function character getassoc (any aa_op)
public function boolean iswordchar (character ac_char)
public function boolean gettokens (ref st_tok ast_tokens[])
public function boolean getparsed (ref st_tok ast_expr[])
public function string eval (st_tok ast_toks[])
public function boolean isop (st_tok ast_tok)
public function boolean parse (st_tok ast_tokens[])
public function string tokentostring (st_tok ast_token)
public function integer getprec (st_tok ast_tok)
public function decimal evalfunc (st_tok ast_func, ref nv_tokstack ast_args)
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
long ll_tk_start = 1, ll_tk_end, ll_in_len, ll_tokens
st_tok lst_tok
any la_empty[]
char lc, prec

is_lasterror = ""
ist_tokens = la_empty[]
ll_in_len= len(as_input)

do while ll_tk_start <= ll_in_len
	lst_tok.value = ""
	lst_tok.kind = ""
	ll_tk_end = ll_tk_start
	
	lc = mid(as_input, ll_tk_start, 1)
	choose case lc
		case '0' to '9' 
			do while (ll_tk_end <= ll_in_len) and isNumber(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start + 1))
				ll_tk_end++
			loop 
			lst_tok.value = trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start))
			lst_tok.kind = NUMERIC
		case '+', '*', '/', '^'
			ll_tk_end++
			lst_tok.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			lst_tok.kind = BINARYOP
		case '-'
			ll_tk_end++
			lst_tok.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			ll_tokens = upperbound(ist_tokens[])
			if ll_tokens = 0 then
				lst_tok.kind = UNARYOP
			elseif ist_tokens[ll_tokens].kind = NUMERIC &
				or ist_tokens[ll_tokens].kind = IDENT &
				or ist_tokens[ll_tokens].kind = RPAR  then
				lst_tok.kind = BINARYOP
			else
				lst_tok.kind = UNARYOP
			end if
		case 'A' to 'Z', 'a' to 'z', '_'
			do while (ll_tk_end <= ll_in_len) and isWordChar(mid(as_input, ll_tk_end, 1))
				ll_tk_end++
			loop 
			lst_tok.value = trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start))
			lst_tok.kind = IDENT
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

public function boolean isfunc (any aa_tok);
//check if the given identifier is a function

boolean lb_ret = false

choose case lower(aa_tok)
	case 'answer', "sum", "mul", "abs"; lb_ret = true
end choose

return lb_ret

end function

public function character getassoc (any aa_op);
// return the associativity for an operator

char lc_ret

choose case string(aa_op, "[general]")
	case '^'
		lc_ret = CC_RIGHT
	case '*', '/'
		lc_ret = CC_LEFT
	case '+', '-'
		lc_ret = CC_LEFT
	case else
		lc_ret = CC_LEFT
end choose

return lc_ret

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
st_tok item
dec ldc_op1, ldc_op2, ldc_res
string ls_ret = "", ls_val
long i, n

n = upperbound(ast_toks[]) 
if n = 0 then return ""

for i = 1 to n
	ls_val = ast_toks[i].value
	choose case ast_toks[i].kind
		case NUMERIC
			lst_eval.push(ast_toks[i])
		case UNARYOP
			choose case ls_val
				case '-'
					ldc_op1 = dec(lst_eval.pop().value)
					ldc_res = - ldc_op1
			end choose
			item.value = ldc_res
			item.kind = NUMERIC
			lst_eval.push(item)
		case BINARYOP
			choose case ls_val
				case '+', '-', '*', '/', '^'
					if lst_eval.size() >= 2 then
						ldc_op2 = dec(lst_eval.pop().value)
						ldc_op1 = dec(lst_eval.pop().value)
						if ls_val = '+' then
							ldc_res = ldc_op1 + ldc_op2
						elseif ls_val = '-' then
							ldc_res = ldc_op1 - ldc_op2
						elseif ls_val = '*' then
							ldc_res = ldc_op1 * ldc_op2
						elseif ls_val = '/' then
							ldc_res = ldc_op1 / ldc_op2
						elseif ls_val = '^' then
							ldc_res = ldc_op1 ^ ldc_op2
						end if
						item.value = ldc_res
						item.kind = NUMERIC
						lst_eval.push(item)
					else
						ls_ret = "broken expression : not enough operands for " + ls_val + " at " + string(ast_toks[i].position)
						goto end_eval
					end if
				case else
					ls_ret = "unknown op " + ls_val
					goto end_eval
			end choose
		case FUNC
			item.kind = NUMERIC //TODO we can implement string funcs too
			item.value = evalfunc(ast_toks[i], lst_eval)
			lst_eval.push(item)
	end choose
next
if lst_eval.size() = 1 then
	item = lst_eval.pop()
	if item.kind = NUMERIC then
		ls_ret = string(item.value)
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

	if isnumber(lst_tok.value) then
		//if the token is a number, pass it to the output queue
		lq_out.push(lst_tok)
		if lst_args.size() > 0 then lst_args.settop(true)
	elseif isfunc(lst_tok.value) then
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
			and ((getassoc(lst_tok.value) = cc_left and getprec(lst_tok) <= getprec(lst_op.top())) &
			or &
			(getassoc(lst_tok.value) = cc_right and getprec(lst_tok) < getprec(lst_op.top()))) 
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
		if isfunc(lst_op.top().value) then 
			//if there is a function after the paren
			//add it to the queue with arguments count
			ll_count = lst_argcount.pop()
			if lst_args.pop() then ll_count ++
			lst_func = lst_op.pop()
			lst_func.count = ll_count
			lq_out.push(lst_func)
		end if
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
	ls_ret = ast_token.value + '(' + string(ast_token.count) + ')'
elseif ast_token.kind = UNARYOP and ast_token.value = '-' then
	ls_ret = '_'
else
	ls_ret = ast_token.value
end if

return ls_ret

end function

public function integer getprec (st_tok ast_tok);
// return the precedence for an operator

int li_ret

choose case ast_tok.value
	case '-'
		//special minus case
		if ast_tok.kind = UNARYOP then
			li_ret = 5
		else
			li_ret = 2
		end if
	case '^'; 		li_ret = 4
	case '*', '/';	li_ret = 3
	case '+';	li_ret = 2
	case else;		li_ret = 0
end choose

return li_ret

end function

public function decimal evalfunc (st_tok ast_func, ref nv_tokstack ast_args);
// evaluate the value of a function

dec ldc_ret = 0, ldc_val
long i

choose case ast_func.value
	case "answer"	//dummy function
		ldc_ret = 42
	case "sum"
		for i = 1 to ast_func.count
			ldc_val = dec(ast_args.pop().value)
			ldc_ret += ldc_val
		next
	case "mul"
		if ast_args.size() > 0 then ldc_ret = dec(ast_args.pop().value)
		for i = 2 to ast_func.count
			ldc_val = dec(ast_args.pop().value)
			ldc_ret *= ldc_val
		next
	case "abs"
		if ast_args.size() > 0 then ldc_ret = abs(dec(ast_args.pop().value))
	
end choose

return ldc_ret

end function

on nv_parser.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_parser.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

