forward
global type nv_queue from nonvisualobject
end type
end forward

global type nv_queue from nonvisualobject autoinstantiate
end type

type variables

private:

any ia_values[]
long il_start = 1, il_end = 0

end variables

forward prototypes
public function boolean isempty ()
public subroutine push (any aa_val)
public function any pop ()
public subroutine reset ()
end prototypes

public function boolean isempty ();
return il_end = 0

end function

public subroutine push (any aa_val);
if il_end = 0 then 
	il_end = il_start
else
	il_end ++
end if

ia_values[il_end] = aa_val

end subroutine

public function any pop ();
any la_ret

if il_end = 0 then return la_ret

la_ret = ia_values[il_start]
il_start ++

if il_start > il_end then il_end = 0

return la_ret

end function

public subroutine reset ();
any empty[]

ia_values[] = empty[]
il_start = 1
il_end = 0

end subroutine

on nv_queue.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_queue.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

