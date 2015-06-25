%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%%%-------------------------------------------------------------------
%%% File    : miniblog.erl
%%% Author  : Simon Thompson
%%% Description : MiniBlog example
%%% Created : Dec '08 - Jan '09 by Simon Thompson <s.j.thompson@kent.ac.uk>
%%%-------------------------------------------------------------------

%% Modelled on xrc.erl as included in the wxErlang distribution, and
%% the example in chapter 2 of the wxWidgets book.

-module(miniblog).
-compile(export_all).

-include_lib("wx/include/wx.hrl").

-define(ABOUT,?wxID_ABOUT).
-define(EXIT,?wxID_EXIT).
-define(APPEND, 131).
-define(UNDO,132).
-define(OPEN,133).
-define(SAVE,134).
-define(NEW,135).


%% Top-level function: create the wx-server, the graphical objects
%% (both frame and text area), show the application, process and 
%% clean up on termination.

start() ->
     WX = wx:new(),
     Frame = wxFrame:new(wx:null(), ?wxID_ANY, "MiniBlog"),
     Text = wxTextCtrl:new(Frame, ?wxID_ANY,
			  [{value,"MiniBlog"},
			   {style,?wxTE_MULTILINE}]),
     setup(WX,Frame,Text),
     wxFrame:show(Frame),
     loop(Frame,Text),
     wx:destroy().

%% Top-level frame: create a menu bar, three menus, menu items (both
%% standard and application-specific), and a status bar.
%% Connect the frame to handle events and set properties of the text area.


setup(WX,Frame,Text) ->
     MenuBar = wxMenuBar:new(),
     File = wxMenu:new(),
     Help = wxMenu:new(),
     Edit = wxMenu:new(),

     wxMenu:append(Help,?ABOUT,"About MiniBlog"),

     wxMenu:append(File,?EXIT,"Quit"),
     wxMenu:append(File,?NEW,"New\tCtrl-N"),
     wxMenu:append(File,?OPEN,"Open saved\tCtrl-O"),
     wxMenu:appendSeparator(File),    
     wxMenu:append(File,?SAVE,"Save\tCtrl-S"),

     wxMenu:append(Edit,?APPEND,"Add en&try\tCtrl-T"),
     wxMenu:append(Edit,?UNDO,"Undo latest\tCtrl-U"),

     wxMenuBar:append(MenuBar,File,"&File"),
     wxMenuBar:append(MenuBar,Edit,"&Edit"),
     wxMenuBar:append(MenuBar,Help,"&Help"),

     wxFrame:setMenuBar(Frame,MenuBar),

     wxTextCtrl:setEditable(Text,false),

     wxFrame:createStatusBar(Frame),
     wxFrame:setStatusText(Frame,"Welcome to wxErlang"),
     wxFrame:connect(Frame, command_menu_selected),
     wxFrame:connect(Frame, close_window).

%% Main processing loop: will process menu selection of the following
%% "about"  display about dialogue 
%% "exit"   exit the appliction
%% "append" append an item to the blog window 
%% "undo"   undo item addition (recursively)
%% "open"   open the saved BLOG file, losing existing file
%% "save"   save the current file as BLOG
%% "new"    create a new blog

loop(Frame,Text) ->
     receive
 	#wx{id=?ABOUT, event=#wxCommand{}} ->
 	    Str = "MiniBlog is a minimal WxErlang example.",
 	    MD = wxMessageDialog:new(Frame,Str,
 				     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
 				      {caption, "About MiniBlog"}]),
 	    wxDialog:showModal(MD),
 	    wxDialog:destroy(MD),
 	    loop(Frame,Text);

  	#wx{id=?EXIT, event=#wxCommand{type=command_menu_selected}} ->
  	    wxWindow:close(Frame,[]);

 	#wx{id=?APPEND, event=#wxCommand{type=command_menu_selected}} ->
 	    Prompt = "Please enter text here.",
 	    MD = wxTextEntryDialog:new(Frame,Prompt,
 				     [{caption, "New blog entry"}]),
  	    case wxTextEntryDialog:showModal(MD) of 
  		?wxID_OK -> 
  		    Str = wxTextEntryDialog:getValue(MD),
  		    wxTextCtrl:appendText(Text,[10]++dateNow()++Str);
 		_ -> ok
	    end,
	    wxDialog:destroy(MD),
	    loop(Frame,Text);

	 #wx{id=?UNDO, event=#wxCommand{type=command_menu_selected}} ->
	    {StartPos,EndPos} = lastLineRange(Text),
	    wxTextCtrl:remove(Text,StartPos-2,EndPos+1),
	    loop(Frame,Text);

	  #wx{id=?OPEN, event=#wxCommand{type=command_menu_selected}} ->
	     wxTextCtrl:loadFile(Text,"BLOG"),
	     loop(Frame,Text);

	  #wx{id=?SAVE, event=#wxCommand{type=command_menu_selected}} ->
	     wxTextCtrl:saveFile(Text,[{file,"BLOG"}]),
	     loop(Frame,Text);

	  #wx{id=?NEW, event=#wxCommand{type=command_menu_selected}} ->
	     {_,EndPos} = lastLineRange(Text),
	     StartPos = wxTextCtrl:xYToPosition(Text,0,0),
	     wxTextCtrl:replace(Text,StartPos,EndPos,"MiniBlog"),
	     loop(Frame,Text)

    end.
 
lastLineRange(Text) ->   
	    NLines = wxTextCtrl:getNumberOfLines(Text),
%	    io:format("Number of lines ~p~n",[NLines]),
	    LineLen =  wxTextCtrl:getLineLength(Text,NLines-1),
%	    io:format("length of line ~p is ~p ~n",[NLines, LineLen]),
	    StartPos = wxTextCtrl:xYToPosition(Text,0,NLines-1),
%	    io:format("Start position is ~p ~n",[StartPos]),
	    EndPos = wxTextCtrl:xYToPosition(Text,LineLen-1,NLines-1),
%	    io:format("End position is ~p ~n",[EndPos]),
            {StartPos,EndPos}.
    
dateNow() ->
    {_,M,D}=erlang:date(),
    Mon = httpd_util:month(M),
    Day = integer_to_list(D),
    string:left(Mon++" "++Day++" ",8,$ ).

    
    
    
