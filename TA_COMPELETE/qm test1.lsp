(setq FILEPATH (getvar "DWGPREFIX"))
(setq FILENAME (getvar "DWGNAME"))
(setq sum (strcat FILEPATH FILENAME))
(command "_SAVEAS" 
        "2010"
        sum
        "Y"
)
(command "close")

