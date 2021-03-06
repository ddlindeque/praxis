# The syntax of *praxis*

* A single file only.
* I'm thinking to add the semantic analysis to this file in future. This will likely add commands, like 'tables', 'graph-parser', etc.
* Contains all the information required to generate a lexical analyser.
* Example:  
    ```
    # Read a line comment
    analyser line_comment ["This is my comment\n"]
    {
        '\n'      -> line_comment, return;
        '[^\n]+'  -> line_comment;
    }

    # Read a block comment
    analyser block_comment ["This is a block\ncomment*/"]
    {
        '\*/'             -> block_comment, return;
        '\*+[^\*]+'       -> block_comment;
        '[^\*]+'          -> block_comment;
    }

    # Read a string
    analyser string ["hello world\""]
    {
        '"'               -> string_end, return;
        '\\[^\n\r\f\v]'   -> string_escaped;
        '[^\\"\n\r\f\v]+' -> string_content;
        '[\\"\n\r\f\v]+'  -> error;
    }

    # Read a document
    analyser document ["abc", "123", "if", "in", "then", "//", "/*"]
    {
        '//'                                      -> line_comment, goto line_comment;
        '/*'                                      -> block_comment, goto block_comment;
        '"'                                       -> string_begin, goto string;
        '\'[^\\\']|(\\.)\''                       -> char;
        'if'                                      -> if;
        'in'                                      -> in;
        'then'                                    -> then;
        '[a-zA-Z_][a-zA-Z0-9_]*'                  -> identifier;
        '(0(\.[0-9]+)?)|[1-9][0-9]*(\.[0-9]+)?'   -> real_number;
        '0|([1-9][0-9]*)'                         -> int_number;
        '\s+'                                     -> whitespace;
        '.'                                       -> error;
    }

    # Standalone string analyser
    analyser str ["\""]
    {
        '"'     -> string_begin, goto string;
        '.'     -> error;
    }

    # standalone string scanner
    scanner scan_str processing str ignoring [string_begin, string_end]
    {}

    # Scanner for colourisers
    scanner colouriser processing document
    {
        line_comment+   -> line_comment;
        block_comment+  -> block_comment;
    }

    # Scanner for parser
    scanner @scanner processing document ignoring [whitespace, line_comment, block_comment]
    {
        string_begin (string_content | string_escaped)* string_end?   -> string;
    }

    ```