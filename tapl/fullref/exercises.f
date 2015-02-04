
Counter = {get: Unit -> Nat, inc: Unit -> Unit};
CounterRep = {x: Ref Nat};

counterClass =
    lambda r:CounterRep.
        {get = lambda _:Unit. !(r.x),
         inc = lambda _:Unit. r.x := succ(!(r.x))};

newCounter =
    lambda _:Unit. let r = {x=ref 1} in
                    counterClass r;

resetCounterClass =
    lambda r:CounterRep.
        let super = counterClass r in
            {get   = super.get,
             inc   = super.inc,
             reset = lambda _:Unit. r.x := 1};

newResetCounter =
    lambda _:Unit. let r = {x=ref 1} in resetCounterClass r;

/* Exercise 18.6.1 */
decCounterClass =
    lambda r:CounterRep.
        let super = resetCounterClass r in
            {get   = super.get,
             inc   = super.inc,
             reset = super.reset,
             dec   = lambda _:Unit. r.x := pred(!(r.x))};

newDecCounter =
    lambda _:Unit. let r = {x=ref 1} in decCounterClass r;

/*
cntr = newDecCounter unit;
cntr.inc unit; cntr.inc unit; cntr.inc unit;
cntr.get unit;
cntr.dec unit;
cntr.get unit;
*/

BackupCounter = {get:Unit->Nat, inc:Unit->Unit,
                 reset:Unit->Unit, backup:Unit->Unit};

BackupCounterRep = {x: Ref Nat, b: Ref Nat};

backupCounterClass =
    lambda r:BackupCounterRep.
        let super = resetCounterClass r in
            {get    = super.get,
             inc    = super.inc,
             reset  = lambda _:Unit. r.x := !(r.b),
             backup = lambda _:Unit. r.b := !(r.x)};

/* Exercise 18.7.1 */
BackupCounter2Rep = {x: Ref Nat, b: Ref Nat, b2: Ref Nat};

backupCounter2Class =
    lambda r:BackupCounter2Rep.
        let super = backupCounterClass r in
            {get     = super.get,
             inc     = super.inc,
             reset   = super.reset,
             backup  = super.backup,
             reset2  = lambda _:Unit. r.x := !(r.b2),
             backup2 = lambda _:Unit. r.b2 := !(r.x)};

newBackupCounter2 =
    lambda _:Unit. let r = {x=ref 1, b=ref 1, b2=ref 1} in backupCounter2Class r;

/*
bckup2 = newBackupCounter2 unit;
bckup2.inc unit;bckup2.inc unit;bckup2.inc unit;bckup2.backup2 unit;
bckup2.inc unit;bckup2.inc unit;bckup2.inc unit;bckup2.backup unit;
bckup2.get unit;
bckup2.reset2 unit;
bckup2.get unit;
bckup2.reset unit;
bckup2.get unit;
*/

/* Exercise 18.11.1 */
SetCounter = {get:Unit->Nat, inc:Unit->Unit, set:Nat->Unit};

setCounterClass =
    lambda r:CounterRep.
    lambda self:Unit->SetCounter.
        lambda _:Unit.
            {get = lambda _:Unit. !(r.x),
             set = lambda i:Nat. r.x := i,
             inc = lambda _:Unit. (self unit).set (succ((self unit).get unit))};

newSetCounter =
    lambda _:Unit.
        let r = {x=ref 1} in fix (setCounterClass r) unit;

InstrCounter = {get:Unit->Nat, set:Nat->Unit,
                inc:Unit->Unit, accesses:Unit->Nat};
InstrCounterRep = {x: Ref Nat, a: Ref Nat};

instrCounterClass =
    lambda r:InstrCounterRep.
    lambda self:Unit->InstrCounter.
        lambda _:Unit.
            let super = setCounterClass r self unit in
            {get = lambda _:Unit. (r.a := succ(!(r.a)); super.get unit),
             set = lambda i:Nat. (r.a := succ(!(r.a)); super.set i),
             inc = super.inc,
             accesses = lambda _:Unit. !(r.a)};

newInstrCounter =
    lambda _:Unit.
        let r = {x = ref 1, a = ref 0} in
            fix (instrCounterClass r) unit;

ResetInstrCounter = {get:Unit->Nat, set:Nat->Unit,
                     inc:Unit->Unit, accesses:Unit->Nat,
                     reset:Unit->Unit};

resetInstrCounterClass =
    lambda r:InstrCounterRep.
    lambda self:Unit -> ResetInstrCounter.
        lambda _:Unit.
            let super = instrCounterClass r self unit in
            {get = super.get,
             set = super.set,
             inc = super.inc,
             accesses = super.accesses,
             reset = lambda _:Unit. r.x := 1};

newResetInstrCounter =
    lambda _:Unit.
        let r = {x = ref 1, a = ref 0} in
            fix (resetInstrCounterClass r) unit;

BackupRestInstrCounter = {get:Unit->Nat, set:Nat->Unit,
                          inc:Unit->Unit, accesses:Unit->Nat,
                          reset:Unit->Unit, backup:Unit->Unit};

BackupResetInstrCounterRep = {x: Ref Nat, a: Ref Nat, b: Ref Nat};

backupResetInstrCounterClass =
    lambda r:BackupResetInstrCounterRep.
    lambda self:Unit -> BackupRestInstrCounter.
        lambda _:Unit.
            let super = resetInstrCounterClass r self unit in
            {get = super.get,
             set = super.set,
             inc = super.inc,
             accesses = super.accesses,
             reset = lambda _:Unit. r.x := !(r.b),
             backup = lambda _:Unit. r.b := !(r.x)};

newBackupResetInstrCounter =
    lambda _:Unit.
        let r = {x=ref 1, a=ref 0, b=ref 1} in
            fix (backupResetInstrCounterClass r) unit;

instr = newBackupResetInstrCounter unit;

/*
 * Exercise 18.13.1
 * How might the model of objects in this chapter be extended to support
 * object identity?
 */

Object = { id:Nat };
ObjectRep = {};

nextObjectId = newCounter unit;

objectClass =
    lambda _:ObjectRep.
    lambda self:Unit -> Object.
        lambda _:Unit.
            let id = nextObjectId.get unit in
                (nextObjectId.inc unit;
                 {id = id});

newObject =
    lambda _:Unit.
        let r = {} in
             fix (objectClass r) unit;

obj = newObject unit;
obj.id;

equals = fix (lambda eq:Nat->Nat->Bool.
              lambda x:Nat. lambda y:Nat.
                if iszero x
                then iszero y
                else if iszero y
                    then false
                    else eq (pred x) (pred y));

sameObject =
    lambda a:Object. lambda b:Object.
        equals (a.id) (b.id);

CounterObject = {get:Unit -> Nat,
                 inc:Unit -> Unit,
                 id:Nat};
CounterObjectRep = {x: Ref Nat};

counterObjectClass =
    lambda r:CounterObjectRep.
    lambda self:Unit -> CounterObject.
        lambda _:Unit.
            let super = objectClass r self unit in
                {get = lambda _:Unit. !(r.x),
                 inc = lambda _:Unit. r.x := (succ(!(r.x))),
                 id  = super.id};

newCounterObject =
    lambda _:Unit.
        let r = {x = ref 0} in
            fix (counterObjectClass r) unit;

a = newCounterObject unit;
b = newCounterObject unit;
sameObject a b;
sameObject b a;
sameObject a a;
sameObject b b;
