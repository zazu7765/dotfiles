(setenv "LIBRARY_PATH"
	(string-join
	 '("/opt/homebrew/Cellar/gcc/14.2.0"
	   "/opt/homebrew/opt/libgccjit/lib/gcc/current"
	   "opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin23/14")
	 ":"))
