An Introduction to Releases with atm

====================================================================

1. Release Management refers to consolidating an application and its dependencies into a single package which can be easily started and stopped as a whole. Releases can be “installed” into an Erlang runtime system. Releases can also be upgraded in real-time, so when a new version of the application is created, it can be seamlessly installed into the system without bringing it down.

2. Release Directory Structure

	1) In general, all application releases follow a certain directory structure. This structure is not required but it is a common practice which increases in readability of application files:

	Application-Vsn/ebin          -> contains all the compiled *.beam files and application specification file
	               /include       -> contains all the *.hrl files
	               /priv          -> contains any executables that are used by the application, such as port applications and Erlang drivers
	               /src           -> contains all the *.erl files

	2) Another important thing to remember is that when compiling Erlang sources, the resulting beam file is usually placed in the same directory as the erl file. To make it so that our sources are compiled into the ebin/ directory, compilation should be done with the following command, which you may easily turn into a make command or whatever you want:

	erlc -o ../ebin eb_app.erl eb_sup.erl eb_event_manager.erl eb_atm.erl eb_withdrawal_handler.erl eb_server.erl

3.	Release Specification File
	
	The one thing you need to create a release is a release specification file. This file is used by the systools module to figure out how to package the Erlang application. 
	The release specification for the atm release is:

	{release, {"eb_rel", "1"},
	          {erts, "5.6.3"},
	          [{kernel, "2.15.1"},
	           {stdlib, "1.18.1"},
	           {sasl, "2.2.1"},
	           {atm, "1.0"}]
	}.
	
	The contents of the file are saved as eb_rel-1.rel. eb_rel is the name of the release with “1″ being the version number. The tuple {erts, “5.6.3″} specifies the version of the Erlang runtime system that the release is meant for. This can be obtained by opening the Erlang shell and seeing what version you have. Following these tuples, there is a list of more tuples which specify the applications for this release. By convention they are listed in order of dependency but systools is smart enough to figure this out on its own when you create the boot script later.

	The versions of dependencies can be retrieved by using the application:loaded_applications/0 method.

	One thing you may notice that was added was the dependency on sasl, which stands for “System Architecture Support Libraries.” The sasl application is required for the release handler module which is used to install releases into a system. 

4.	Creating a Boot Script

	The purpose of the release specification file is create a boot script that we will use to boot up the entire release quickly and easily. To create the boot script, run the following commands:
	atm$ erl -pa ./ebin
	1> systools:make_script("eb_rel-1", [local]).
	
	Once the shell is open, the systools:make_script/2 method is used to create the boot script. The make_script method accepts as its first argument the name of the release spec file without the extension, and it is expected to be in the current working directory. The second argument is a list of options. The option specified here, local, uses the complete path of where the dependency applications are found instead of using variables such as $ROOT. This is useful for testing in a development environment. When you’re ready to package it for true release, omit this option.

	If the result of calling the function is ok, then you should find a eb_rel-1.boot file in the current working directory. This file is used to start the Erlang system. 

5.	Starting the System with the Boot File
	
	typing the following into the shell:

	erl -boot eb_rel-1
	
	The Erlang system will locate the boot file specified and will follow it to startup the system. You can test that it worked by using the atm server to create accounts, make deposits, etc.

	To shut down the system, use the normal q() command in the shell. All processes will be gracefully stopped.

6.	Packaging the Release
	
	The final step is to make the system a nice single package that can be carried around different systems to ease in setup and installation. This can all be done in a single step now that we already have the correct directory structure and a compiled boot script.

	4> systools:make_tar("eb_rel-1").
	
	In the working directory, there should now be an eb_rel-1.tar.gz. That’s all there is to it!
	
7.	Installing a Packaged Release
	To install the packaged release, we first need to copy the tar.gz package we created earlier into the $ROOT/releases directory. $ROOT can be determined by running code:root_dir().. Next, start up and Erlang shell and make sure the sasl application is started so we can use the release_handler module. Also, make sure the shell is running with valid permissions to make changes to the $ROOT directory. This may require running erl with sudo.

	3> release_handler:unpack_release("eb_rel-1").
	{ok,"1"} 

	The unpack_release method does just what it says. It unpacks the package file, copying the libs to the code:lib_dir() and created a proper release directory. Since this is the initial release, there is no need to “install” it, as its already done.
	
	Now restart the Erlang shell, start sasl, and you should now be able to start erlybank with application:start(erlybank) anywhere! The system has been installed!

	Alternatively you can start the ErlyBank application with a single command:	erl -boot $ROOT/releases/1/start

	
