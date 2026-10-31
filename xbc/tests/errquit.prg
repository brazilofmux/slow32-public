* An error under the default handler with the terminal's input closed:
* its Alert gets no key, ever, and the program must still end (Quit, the
* report, ERRORLEVEL 1) rather than ask again forever.
x := "a"
? x + 1
