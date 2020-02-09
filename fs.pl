%% Yupiel for FS.

:- module(fs,[]).
:- use_module(yupiel).

file{path:Path} :< This =>
	not(file{content:_}:<This),
	catch((read_file_to_string(Path,String,[]),
	This $> file{content:String}),_,This $> file{content:""}).

file{path:Path,content:Content} => 
	open(Path,write,Stream),
	write(Stream,Content),
	close(Stream).
	
directory{path:Path} :< This =>
	not(_{files:_}:<This),
	directory_files(Path, Files),
	This$>directory{files:Files}.