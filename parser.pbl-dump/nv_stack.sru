forward
global type nv_stack from nonvisualobject
end type
end forward

global type nv_stack from nonvisualobject autoinstantiate
end type

type variables

private:

any ia_values[]
long il_pos = 0

end variables

forward prototypes
public function boolean isempty ()
public subroutine push (any aa_val)
public function any pop ()
public function any top ()
public subroutine reset ()
public function long size ()
public subroutine settop (any aa_val)
public function any peek (long al_pos)
end prototypes

public function boolean isempty ();
return il_pos = 0

end function

public subroutine push (any aa_val);
il_pos++
ia_values[il_pos] = aa_val

end subroutine

public function any pop ();
any la_ret

if il_pos = 0 then return la_ret	

la_ret = ia_values[il_pos]
setnull(ia_values[il_pos])
il_pos --

return la_ret

end function

public function any top ();
any la_ret

if il_pos = 0 then return la_ret	

la_ret = ia_values[il_pos]

return la_ret

end function

public subroutine reset ();
any empty[]

ia_values[] = empty[]
il_pos = 0

end subroutine

public function long size ();
return il_pos

end function

public subroutine settop (any aa_val);
if il_pos > 0 then
	ia_values[il_pos] = aa_val
end if

end subroutine

public function any peek (long al_pos);
// for debug : get a value

any la_val

if il_pos > 0 and al_pos <= il_pos then
	la_val = ia_values[al_pos]
end if

return la_val

end function

on nv_stack.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_stack.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

