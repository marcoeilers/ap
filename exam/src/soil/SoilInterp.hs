-- | Interpreter for Soil programs. 
--   Runs Soil programs for a specified number of steps using
--   different process schedulers.

module SoilInterp ( runProgRR
                  , runFileRR
                  , runProgAll
                  , runFileAll
                  , ProgramState(..)
                  -- exported only for testing purposes
                  , SoilProgram(..)
                  , initialState
                  , Process(..)
                  , ProcessQueue(..)
                  , NameEnv(..)
                  , evalActOp
                  , evalExpr
                  , evalPrim
                  , nextProcessRR
                  , nextProcAll)
       where

import SoilAst
import SoilParser
import SimpleReadP
import Debug.Trace
import Data.Maybe
import Control.Monad
import qualified Data.Map as M 
import qualified Data.List as L
                   

--
-- Part 1: Define a name environment
--

-- | Data structure used to map names to lists of ident. 
data NameEnv = NameEnv (M.Map Name [Ident]) deriving (Eq, Show)

-- Functions for insert and lookup

-- | Gets the (list of) ident(s) mapped to a name
--   in the current environment. Throws a static error
--   if there is no such mapping.
lookupName :: Name -> SoilProgram (Maybe [Ident])
lookupName n = do 
    (NameEnv e) <- getNameEnv
    let mi = M.lookup n e 
    return mi

-- | Inserts a new binding (of a name to a (list of) ident(s)) into the current 
--   environment. Throws a static error if the name is already bound.
insertName :: (Name, [Ident]) -> SoilProgram ()
insertName (n,i) = do 
    (NameEnv e) <- getNameEnv 
    let mi = M.lookup n e
    case mi of
        (Just _) -> error "Static Error: Name is bound twice"
        Nothing  -> let newE = NameEnv $ M.insert n i e
                    in putNameEnv newE

-- | Empties the current name environment, i.e. deletes all bindings.
emptyNameEnv :: SoilProgram ()
emptyNameEnv = putNameEnv (NameEnv M.empty)

--
-- Part 2: Define a function environment
--

-- | Data structure used to map (function) identifiers to functions.
data FuncEnv = FuncEnv (M.Map Ident Func) deriving (Eq, Show)

-- Functions for insert and lookup

-- | Gets the function mapped to a function ID
--   in the current environment. Throws a static error
--   if there is no such mapping.
lookupFunc :: Ident -> SoilProgram (Maybe Func)
lookupFunc id = do 
    (FuncEnv fe) <- getFuncEnv
    let mf = M.lookup id fe
    return mf

-- | Inserts a new binding (of a function ID to a function) into the current 
--   environment. Throws a static error if the identifier is already bound.
insertFunc :: (Ident, Func) -> SoilProgram ()
insertFunc (i,f) = do 
    (FuncEnv fe) <- getFuncEnv
    let mf = M.lookup i fe
    case mf of
        (Just _) -> error "Static Error : FuncIdent is bound twice"
        Nothing  -> let newE = FuncEnv $ M.insert i f fe
                    in putFuncEnv newE

--
-- Part 3: Define a process and process queue
--
type Pid = Ident

type Message = [Ident]

-- | A process has a function it currently executes,
--   arguments for this function,
--   and a mailbox which may or may not contain messages.
data Process = Process { function  :: Ident
                       , arguments :: [[Ident]]
                       , mailbox   :: [Message]}
               deriving (Eq, Show)

-- | Maps process identifiers to processes, while at the same time
--   keeping track of the order of processes.
data ProcessQueue = Queue [(Pid, Process)] deriving (Eq, Show)

-- Functions for process modification

-- | Retrieves a process from the current environment.
--   Returns Nothing if there is no process mapped to the
--   spevified ID.
getProcess :: Pid -> SoilProgram (Maybe Process)
getProcess pid = do 
    (Queue q) <- getQueue
    let mp = lookup pid q
    return mp

-- | Adds a message to the mailbox of the specified process.
addMessage :: Pid -> Message -> SoilProgram ()
addMessage pid msg = do 
    mp <- getProcess pid
    case mp of 
        (Just p) -> do
            let newMB = mailbox p ++ [msg]
                newP = p {mailbox = newMB}
            updateProcess pid newP
        Nothing  ->
            putRuntimeError ("Sent message to unknown process " ++ pid)

-- | Updates the Process object in the environment for a given process ID.
updateProcess :: Pid -> Process -> SoilProgram ()
updateProcess pid p = do 
    (Queue q) <- getQueue
    let newq = map (replaceProc p pid) q
    putQueue (Queue newq)

replaceProc :: Process -> Pid -> (Pid, Process) -> (Pid, Process)
replaceProc newp pid (id, p) = 
    if pid == id
        then (id, newp)
        else (id, p)

-- | Returns the oldest message from the mailbox of the specified process.
--   Returns Nothing if the mailbox is empty. 
--   Throws an error if the provided process ID is invalid.
--   Since the function is only called by the scheduler, this should never happen.
getOldestMsg :: Pid -> SoilProgram (Maybe Message)
getOldestMsg pid = do 
    mp <- getProcess pid
    case mp of
        (Just p) -> do
            let mb = mailbox p
            case mb of 
                (x:xs) -> do updateProcess pid (p {mailbox = xs})
                             return (Just x)
                []     -> return Nothing
        Nothing -> 
            error ("Internal Error: Tried to get message for nonexisting process " ++ pid)

-- | For a given process, updates the information about the currently executed
--   function and its arguments.
--   Throws an error if the provided process ID is invalid.
--   Since the function is only called by the scheduler, this should never happen.
changeProcessFunc :: Pid -> Ident -> [[Ident]] -> SoilProgram ()
changeProcessFunc pid func args = do 
    mp <- getProcess pid
    case mp of
        (Just p) -> do
            let newp = p { function = func
                         , arguments = args}
            updateProcess pid newp
        Nothing ->
            error ("Internal Error: Tried to change function of nonexisting process "++ pid)

-- | Retrieves the currently executed function of a given process as well as its arguments.
--   Throws an error if the provided process ID is invalid.
--   Since the function is only called by the scheduler, this should never happen.
getProcessFunc :: Pid -> SoilProgram (Ident, [[Ident]])
getProcessFunc pid = do 
    mp <- getProcess pid
    case mp of
        (Just p) ->
            return (function p, arguments p)
        Nothing ->
            error ("Internal Error: Tried to access function of nonexisting process "++pid)

-- | Adds a new process with the given Id to the current process queue.
addProcess :: Pid -> Process -> SoilProgram ()
addProcess pid p = do 
    (Queue q) <- getQueue
    let newq = q ++ [(pid, p)]
    putQueue $ Queue newq


--
-- Part 4: Define and implement a process step
--

-- | Executes one step in a given process, i.e.
--   executes the process function once for the oldest message
--   in the mailbox
processStep :: Pid -> SoilProgram ()
processStep pid = case pid of 
    -- Special cases for the two predefined processes 
    "println" -> do
        mmsg <- getOldestMsg pid
        case mmsg of
            (Just msg) -> do
                out <- getStdout
                let str = L.intercalate ":" msg
                    newOut = str : out
                putStdout newOut
            Nothing    ->
                return ()
    "errorlog" -> do
        mmsg <- getOldestMsg pid
        case mmsg of
            (Just msg) -> do
                err <- getErrlog
                let str = L.intercalate ":" msg
                    newOut = str : err
                putErrlog newOut
            Nothing    ->
                return ()
    _ -> do
        (fid, args) <- getProcessFunc pid
        mf <- lookupFunc fid
        mp <- getProcess pid
        case (mf, mp) of
            (Just f, Just p) -> do
                mmsg <- getOldestMsg pid
                case mmsg of
                    (Just msg) -> do 
                        -- discard all bindings from the previous function call
                        emptyNameEnv
                        -- and bind the new arguments
                        bind (params f) (arguments p)
                        -- and the message
                        bind [receive f] (wrap msg)
                        evalExpr (body f)
                    Nothing    -> 
                        return ()
            (Nothing, Just _) ->
                putRuntimeError "Internal error: Process has reference to nonexisting function"
            (_, _) ->
                putRuntimeError "Internal error: processStep got nonexisting Pid"

--
-- Part 5: Define and implement the round-robin algorithm
--

-- | Returns the Pid of the first process in the process queue (ignoring if it has any
--   messages or not) and puts said process at the end of the queue.
nextProcessRR :: SoilProgram Pid
nextProcessRR = do 
    (Queue q) <- getQueue
    case q of 
        []     -> 
            error "Internal Error: This can never happen. There are always at least two processes in the queue."
        (p:ps) -> do 
            let (pid, _) = p
            putQueue (Queue (ps++ [p]))
            return pid

--
-- Part 6: Implement the round-robin evaluator
--

-- | The initial state of every program run.
initialState = ProgramState {stdout = [],
                             errorlog = [],
                             queue = Queue [],
                             funcs = FuncEnv M.empty,
                             names = NameEnv M.empty,
                             process = Nothing }

-- | Runs a given program for a given amount of steps.
--   If no static errors occur, returns a pair of string lists.
--   The first one contains everything the program has printed to stdout.
--   The second one contains all the runtime errors the program has produced.
runProgRR :: Int                   -- ^ The number of steps that should be executed
          -> Program               -- ^ A soil program (in the form of its AST)
          -> ([String], [String])  -- ^ A pair consisting of stdout and the errorlog.
runProgRR rounds p = (stdout result, errorlog result)
    where prog = startProgRR rounds p
          ((), result) = runSP prog initialState

emptyProcess :: Process
emptyProcess = Process { function = ""
                       , arguments = []
                       , mailbox = [] }

-- | Initializes a program, i.e. stores all function definitions in the function
--   environment, executes the top-level ActOps, then runs the Round-Robin scheduler
--   if there are any processes (apart from the two standard ones) in the process queue.
startProgRR :: Int -> Program -> SoilProgram ()
startProgRR rounds (funcs, actops) = do 
    initProg funcs actops
    (Queue q) <- getQueue
    -- if those are the only two processes (i.e. no others have been created)
    -- then we can stop right here
    unless (length q <= 2) $ runProgRRrec rounds

initProg :: [Func] -> [ActOp] -> SoilProgram ()
initProg funcs actops = do
    let funcpairs = map getFuncPair funcs
    -- insert all defined functions into environment
    mapM_ insertFunc funcpairs
    -- add preexisting processes to queue
    addProcess "println" emptyProcess
    addProcess "errorlog" emptyProcess
    -- evaluate all top-level ActOps
    mapM_ evalActOp actops
                                 

-- | Gets the next process from the RR scheduler,
--   executes one step of it. Repeats n times.
runProgRRrec :: Int -> SoilProgram ()
runProgRRrec 0 = return ()
runProgRRrec n = do 
    pid <- nextProcessRR
    putCurrentProcess (Just pid)
    processStep pid
    runProgRRrec (n-1)

-- | Reads a Soil program from a given file and executes it for a given number of
--   steps using the Round Robin scheduler.
runFileRR :: Int -> FilePath -> IO ([String], [String])
runFileRR n f = do 
    s <- readFile f
    let res = parseEof program s
        (r:rs) = res
        (p,_) = r
        val = runProgRR n p
    return val

--
-- Part 7: Implement a find all possible executions evaluator
--

-- | Returns a list of the process IDs of all processes which have at least one
--   message in their mailbox. Does NOT change the order of processes in the queue.
nextProcAll :: SoilProgram [Pid]
nextProcAll = do 
    (Queue q) <- getQueue
    let procs = allExecutableProcesses q
    return procs

-- | Filters all processes with messages from the message queue
allExecutableProcesses :: [(Pid, Process)] -> [Pid]
allExecutableProcesses [] = []
allExecutableProcesses (p:ps) = 
    let (id, proc) = p
    in if null (mailbox proc)
        then allExecutableProcesses ps
        else id : allExecutableProcesses ps

-- | Runs a given program for a given number of rounds using nextProcAll 
--   as the scheduler. This means ALL possible combinations of evaluation 
--   orders are tried.
runProgAll :: Int -> Program -> [([String], [String])]
runProgAll rounds p = map getOutput endstate
    where prog = startProgAll p
          (cont, result) = runSP prog initialState
          endstate = if cont 
                     then runAll rounds result
                     else [result]

-- | Initializes a program, checks if there are processes in the queue
--   (other than the usual two ones). Returns true if this is the case,
--   otherwise false.
startProgAll ::  Program -> SoilProgram Bool
startProgAll (funcs, actops) = do 
    initProg funcs actops
    (Queue q) <- getQueue
    let continue = length q > 2
    return continue 

-- | Runs all processes which can run in the
--   current state, resulting in a list of possible states;
--   then repeats this procedure for each one of the results 
--   n times.
runAll :: Int -> ProgramState -> [ProgramState]
runAll 0 s = [s]
runAll n s = 
    let (pids, s') = runSP nextProcAll s 
    in case pids of
        [] -> [s]
        _  -> let a = map runProcess pids -- [Pid] -> [SC ()]
                  b = map runSP a          -- [SC ()] -> (PS -> ((), PS) )
                  c = pam b s       -- (PS -> ((), PS) )  -> ((), PS)
                  d = map snd c            -- -> [PS] 
              in flatten $ map (runAll (n-1)) d

-- | For a given process, puts its function data into the current environment,
--   then executes the next step.
runProcess :: Pid -> SoilProgram ()
runProcess pid = do 
    putCurrentProcess (Just pid)
    processStep pid

-- | Reads a Soil program from a given file and executes it for a given number of 
--   steps using nextProcAll. Returns ALL results for all possible evaluation orders.
runFileAll :: Int -> FilePath -> IO [([String], [String])]
runFileAll n f = do 
    s <- readFile f
    let res = parseEof program s
        (r:rs) = res
        (p,_) = r
        val = runProgAll n p
    return val

    
--
-- Implementation of the SoilProgram monad,
-- data structures for program state, 
-- associated functions
--

-- | The monad used to keep track of the program state.
--   Essentially an implementation of the state monad.
newtype SoilProgram a = SP {runSP :: ProgramState -> (a, ProgramState)}

instance Monad SoilProgram where
    return a     = SP $ \s    -> (a, s)
    (SP h) >>= f = SP $ \s -> let (x,s') = h s
                                  (SP g) = f x
                              in g s'

-- | The data structure which stores the program state.
--   Contains output, runtime errors, process queue, the current
--   process ID, and the name and function environments.
data ProgramState = ProgramState { stdout   :: [String]
                                 , errorlog :: [String]
                                 , queue    :: ProcessQueue
                                 , funcs    :: FuncEnv
                                 , names    :: NameEnv 
                                 , process  :: Maybe Ident} deriving (Eq, Show)


-- Some utility functions for manipulating the current state
-- Should be self-explanatory
getStdout :: SoilProgram [String]
getStdout = SP $ \st@(ProgramState {stdout=res}) -> (res, st)

getErrlog :: SoilProgram [String]
getErrlog = SP $ \st@(ProgramState {errorlog=res}) -> (res, st)

getQueue :: SoilProgram ProcessQueue
getQueue = SP $ \st@(ProgramState {queue=res}) -> (res, st)

getFuncEnv :: SoilProgram FuncEnv
getFuncEnv = SP $ \st@(ProgramState {funcs=res}) -> (res, st)

getNameEnv :: SoilProgram NameEnv
getNameEnv = SP $ \st@(ProgramState {names=res}) -> (res, st)

getCurrentProcess :: SoilProgram (Maybe Ident)
getCurrentProcess = SP $ \st@(ProgramState {process=res}) -> (res, st)

putCurrentProcess :: Maybe Ident -> SoilProgram ()
putCurrentProcess v = SP $ \st -> ((), st {process = v})

putStdout :: [String] -> SoilProgram ()
putStdout v = SP $ \st -> ((), st {stdout = v})

putErrlog :: [String] -> SoilProgram ()
putErrlog v = SP $ \st -> ((), st {errorlog = v})

putQueue :: ProcessQueue -> SoilProgram ()
putQueue v = SP $ \st -> ((), st {queue = v})

putFuncEnv :: FuncEnv -> SoilProgram ()
putFuncEnv v = SP $ \st -> ((), st {funcs = v})

putNameEnv :: NameEnv -> SoilProgram ()
putNameEnv v = SP $ \st -> ((), st {names = v})

putRuntimeError :: String -> SoilProgram ()
putRuntimeError err = addMessage "errorlog" [err]

--
-- Interpreter functions
--

-- | Binds parameters (names) to their values (ident lists)
bind :: [Name] -> [[Ident]] -> SoilProgram ()
bind [] [] = return ()
bind [] (i:is) = putRuntimeError "Number of params and args does not match"
bind (n:ns) [] = putRuntimeError "Number of params and args does not match"
bind (n:ns) (i:is) = do 
    insertName (n,i) 
    bind ns is

-- | Evaluates primitives to lists of identifiers.
--   Always returns [id] instead of [id] for single-valued prims
--   Returns Nothing if the prim contains an illegal concat operation.
--   Self may throw an error if called outside a process context.
evalPrim :: Prim -> SoilProgram (Maybe [Ident])
-- Idents evaluate to themselves
evalPrim (Id ident) = return $ Just [ident]
-- Name evaluate to the ident(s) mapped to them
evalPrim (Par name) = do 
    res <- lookupName name
    case res of 
        (Just r) ->
            return $ Just r
        Nothing ->
            error "Static error: Undefined parameter"
-- Self evaluates to the ID of the current process.
-- If called at top-level, throws a static error.
evalPrim Self       = do 
    cp <- getCurrentProcess
    case cp of
        (Just i) -> return $ Just [i]
        Nothing  -> error "Static error: Self and become can only be used inside functions"
-- Concat f s evaluates to f ++ s
-- Returns nothing if one of the prims to be concatenated contains more than one ident
-- because concat does not make sense in that context 
evalPrim (Concat first second) = do 
    mpf <- evalPrim first
    mps <- evalPrim second
    case (mpf, mps) of
        (Just [f], Just [s]) ->
            return $ Just  [f ++ s]
        (Just _, Just _) -> 
            return Nothing
        (_, _) -> return Nothing

-- | Matches a message to a case in a case statement.
--   If a match is found, binds all parts of the message to the corresponding names.
--   Returns Nothing if no match is found.
match :: Message -> [([Name], Expr)] -> SoilProgram (Maybe Expr)
match _ [] = return Nothing
match [] _ = return Nothing
match msg (c:cs) = do 
    let (names, expr) = c
    if length msg == length names
        then do bind names (map wrap msg)
                return (Just expr) 
        else match msg cs

-- | Evaluates case-expressions, if-then-expressions and blocks of ActOps
evalExpr :: Expr -> SoilProgram ()
evalExpr (CaseOf pmsg cases fallthrough) = do 
        mmsg <- evalPrim pmsg
        ensureJust mmsg ( do
            -- Note: in case-expressions, a name may actually be mapped to a list of idents
            me <- match (fromJust mmsg) cases
            case me of
                (Just e) -> evalExpr e
                Nothing  -> evalExpr fallthrough )    
evalExpr (IfEq first second thenExp elseExp) = do 
        mf <- evalPrim first 
        ms <- evalPrim second
        ensureJusts [mf, ms] ( do
            let f = fromJust mf
                s = fromJust ms
            -- Take else branch if any argument contains more than a single ident
            if length f == 1 && length s == 1 && f == s
                then evalExpr thenExp
                else evalExpr elseExp )
-- Empty ActOp blocks are allowed, and simply return the current state
evalExpr (Acts []) = return ()
evalExpr (Acts (a:as)) = do 
        evalActOp a 
        evalExpr (Acts as)

-- | Executes ActOps, i.e.
--   sends messages to processes,
--   creates new processes
--   or changes the function (and arguments) of the current one.
evalActOp :: ActOp -> SoilProgram ()
evalActOp (SendTo args receiver) = do
    -- args has to contain either one name, evaluating to any number of idents
    -- or any number of prims, all of which evaluate to exactly one ident
    mmsg <- mapM evalPrim args 
    mpid <- evalPrim receiver
    ensureJusts (mpid:mmsg) ( do
        let pid = fromJust mpid
            msg = map fromJust mmsg
        ensureSingleIdent pid (
            -- flatten the message to get a list of idents
            addMessage (unwrap pid) (flatten msg)))
-- Create is only allowed in a process context (static error otherwise)
evalActOp (Create ppid pfid pargs) = do
    mppid <- evalPrim ppid
    mpfid <- evalPrim pfid
    margs <- mapM evalPrim pargs
    ensureJusts (mppid:mpfid:margs) ( do
        let ppid = fromJust mppid
            pfid = fromJust mpfid
            args = map fromJust margs
        ensureSingleIdents [pfid, ppid] ( do
            let fid = unwrap pfid
                pid = unwrap ppid
            fun <- lookupFunc fid
            p <- getProcess pid
            case p of
                Nothing -> 
                    case fun of
                        (Just f) ->
                            if length (params f) == length args   
                            then let newP = Process {function = fid,   
                                                     arguments = args,
                                                     mailbox = []}
                                 in addProcess pid newP
                            else putRuntimeError "Create statement had wrong number of args for function."
                        Nothing -> 
                            putRuntimeError "Create used unknown function id."
                (Just _) ->
                    putRuntimeError "Create operation uses existing process ID."))
-- Become is only allowed in a process context (static error otherwise)
evalActOp (Become pfid pargs) = do
    mffid <- evalPrim pfid
    margs <- mapM evalPrim pargs 
    mthis <- evalPrim Self
    ensureJusts (mffid:mthis:margs) (do
        let ffid = fromJust mffid
            args = map fromJust margs
            this = fromJust mthis
        ensureSingleIdents [ffid, this] ( do
            let fid = unwrap ffid
            fun <- lookupFunc fid
            case fun of               
                (Just f) ->            
                    if length (params f) == length args
                    then changeProcessFunc (unwrap this) fid args
                    else putRuntimeError ("Become statement had wrong number of args for function "++ fid)
                Nothing -> putRuntimeError ("Become used unknown function id "++ fid)))
    

--
-- Utility functions
--

flatten :: [[a]] -> [a]
flatten = L.intercalate []

-- may only be used if it is certain that the list contains exactly one element
unwrap :: [a] -> a
unwrap [a] = a 
unwrap _ = error "Internal Error: Unwrap may only be used on lists that have been checked with ensureSingleIdent"

getFuncPair :: Func -> (Ident, Func)
getFuncPair f = (funcname f, f)

wrap :: a -> [a]
wrap a = [a]

pam :: [a -> b] -> a -> [b]
pam f x = map g f
    where g h = h x

-- | Makes sure that all lists of idents in the first argument only contain
--   one ident (because most functions expect that to be the case).
--   Executes the given action if this is the case, throws a runtime
--   error otherwise.
ensureSingleIdents :: [[Ident]] -> SoilProgram () -> SoilProgram ()
ensureSingleIdents [] action = action
ensureSingleIdents (i:is) action = case i of
    [ident] -> ensureSingleIdents is action
    _       -> putRuntimeError ("Function or process id bound to more or less than a single ident: "++ show i)

ensureSingleIdent :: [Ident] -> SoilProgram () -> SoilProgram ()
ensureSingleIdent ident = ensureSingleIdents [ident]

-- | Makes sure all elements of the first argument list actually contain 
--   Just _ instead of Nothing. If this is the case, executes the provided action,
--   otherwise throws a runtime error.
ensureJusts :: [Maybe [Ident]] -> SoilProgram () -> SoilProgram ()
ensureJusts [] action = action
ensureJusts (m:ms) action = case m of
    (Just ids) ->
        ensureJusts ms action
    Nothing ->
        putConcatError

ensureJust :: Maybe [Ident] -> SoilProgram () -> SoilProgram ()
ensureJust mi = ensureJusts [mi]

putConcatError :: SoilProgram ()
putConcatError = putRuntimeError "An argument of 'concat' was more or less than a single Ident."

getOutput :: ProgramState -> ([String], [String])
getOutput s = (stdout s, errorlog s)
