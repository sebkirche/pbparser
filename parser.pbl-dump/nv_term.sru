forward
global type nv_term from nonvisualobject
end type
end forward

global type nv_term from nonvisualobject
end type
global nv_term nv_term

type prototypes
Function ULong GetLocaleInfo(ulong Locale, ulong LCType, ref string lpLCData, ulong cchData) Library "kernel32.dll" Alias for "GetLocaleInfoW" 
Function ULong GetSystemDefaultLCID() Library "kernel32.dll"
Function ULong GetUserDefaultLCID() Library "kernel32.dll"

end prototypes

type variables

string is_description = "basic terms"
string is_inputstartmatches[] //descriptor "pb_format"="Group=General~tLabel=Input start matches~tStyle=Grid~tDatatype=String~tLines=4"
string is_ops
string is_funcs

any value
integer kind
integer returntype


long count
long position
string is_lasterror

//types of tokens
constant integer UNDEF = -1
constant integer EMPTY = 0
constant integer INTG = 1			//integer numeric type
constant integer DECIM = 2			//decimal numeric type
constant integer BOOL = 3			//boolean type
constant integer STR = 4			//string type
constant integer UNARYOP = 5		//unary operator
constant integer BINARYOP = 6		//binary operator 
constant integer IDENT = 7			//identifier (variable)
constant integer FUNC = 8			//function
constant integer LIST = 9			//list of items
constant integer TARGSEP = 10		//argument saparator
constant integer TLPAR = 11			//left paren
constant integer TRPAR = 12			//right paren
constant integer ERR = 99			//error

constant char DECSEP = '.'			//decimal separator
constant char ARGSEP = ';'			//argument separator for funcalls
constant char LPAR = '('
constant char RPAR = ')'

string is_numbercharpattern
constant ulong LOCALE_SDECIMAL = 14 // decimal separator
string is_SYSDECSEP

end variables

forward prototypes
public function string tostring ()
public function string typename (integer ai_type)
public function string typename ()
public function boolean tobool ()
public function decimal todec ()
public function decimal tolong ()
public function string dump ()
public function boolean check ()
public function nv_term set (integer ai_kind)
public function nv_term set (integer ai_kind, any aa_val)
public function string fixdecimal (string as_text)
public function boolean iswordchar (character ac_char)
public function boolean isbool (any aa_term)
public function string getlocaleinfo (unsignedlong al_localetype, boolean ab_userlocale)
public function boolean isop (readonly string as_id, readonly string as_ops)
public function boolean isfunc (readonly string as_id, readonly string as_funcs)
public function nv_term eval (nv_termstack ast_args)
public function boolean accept (ref string as_input, readonly long al_inplen, readonly long al_startoff, ref long al_endoff, ref nv_termqueue atq_terms, readonly string as_ops, readonly string as_funcs)
end prototypes

public function string tostring ();
string ls_ret

if classname(value) = "string" then
	ls_ret = value
else
	//Coercion ?
end if

return ls_ret

end function

public function string typename (integer ai_type);
//return the stringified type 
string ls_name

choose case ai_type
	case UNDEF;		ls_name = "Undef"
	case EMPTY;		ls_name = "<empty>"
	case INTG;		ls_name = "Int"
	case DECIM;		ls_name = "Dec"			//numeric type
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

public function string typename ();
//return the stringified type 

return typename(kind)

end function

public function boolean tobool ();
boolean lb_ret

if classname(value) = "any" then
	lb_ret = value
else
	//Coercion ?
end if

return lb_ret

end function

public function decimal todec ();
dec ldc_ret

if classname(value) = "decimal" then
	ldc_ret = value
else
	//Coercion ?
end if

return ldc_ret

end function

public function decimal tolong ();
long ll_ret

if classname(value) = "long" then
	ll_ret = value
else
	//Coercion ?
end if

return ll_ret

end function

public function string dump ();
//return a string representation of a token
string ls_ret

if kind = FUNC then
	ls_ret = value + '.' + iif(count > -1, string(count), '?')
elseif kind = UNARYOP then
	if value = '-' then 
		ls_ret = '_'
	else
		ls_ret = value
	end if
elseif kind = BOOL or kind = DECIM or kind = INTG then
	ls_ret = string(value)
else
	ls_ret = value
end if

return ls_ret

end function

public function boolean check ();
//check the validity of the token

is_lasterror = ""

return true

end function

public function nv_term set (integer ai_kind);
kind = ai_kind
returntype = ai_kind

return this

end function

public function nv_term set (integer ai_kind, any aa_val);
kind = ai_kind
value = aa_val
returntype = ai_kind

return this

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

public function boolean iswordchar (character ac_char);
// tell if the given character if a word character

choose case ac_char
	case 'a' to 'z', 'A' to 'Z', '0' to '9', '_'
		return true

	case else
		return false
end choose

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

public function boolean isop (readonly string as_id, readonly string as_ops);
return pos(as_ops, ',' + as_id + ',') > 0

end function

public function boolean isfunc (readonly string as_id, readonly string as_funcs);
return pos(as_funcs, ',' + as_id + ',') > 0

end function

public function nv_term eval (nv_termstack ast_args);
//return the evalutaion of a term

//for single types, it is the term itself

return this

end function

public function boolean accept (ref string as_input, readonly long al_inplen, readonly long al_startoff, ref long al_endoff, ref nv_termqueue atq_terms, readonly string as_ops, readonly string as_funcs);
//check if we accept something at the given point of input

is_lasterror = ""
//by default, NO
return false

end function

on nv_term.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_term.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;
kind = UNDEF

is_SYSDECSEP = getlocaleinfo(locale_sdecimal, true)
is_numbercharpattern = "[0-9" + iif(DECSEP = '.', "\.", DECSEP) + "]"

end event

