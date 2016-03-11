import System.IO
import System.Process

main = do
    (_, Just hout, Just herr, jHandle) <-
        -- Replace with some other command on Windows
        createProcess (proc "/bin/date" args)
           { cwd = Just "."
           , std_out = CreatePipe
           , std_err = CreatePipe 
           }

    

    where args = []
