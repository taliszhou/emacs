(defun code_get_path ()
"
     2   修改开发环境
     当你启动程序时，一般都是把所有模块和文件都放在同一个目录当中，并且在这个目录启动Erlang。如果这样做当然没问题。不过如果你的应用变得更加复杂时，你可能需要把它们分成便于管理的块，把代码放入不同的目录。而且你从其他项目导入代码时，扩展代码也有其自己的目录结构。

     2.1   设置装载代码的搜索路径
     Erlang运行时系统包含了代码自动重新载入的功能。想要让这个功能正常工作，你必须设置一些搜索路径以便找到正确版本的代码。

     代码装载功能是内置在Erlang中的，我们将在E.4节详细讨论。代码的装载仅在需要时才执行。

     当系统尝试调用一个模块中的函数，而这个模块还没有装载时，就会发生异常，系统就会尝试这个模块的代码文件。如果需要的模块叫做 myMissingModule ，那么代码装载器将会在当前目录的所有目录中搜索一个叫做 myMissingModule.beam 的文件。寻找到的第一个匹配会停止搜索，然后就会把这个文件中的对象代码载入系统。

     你可以在Erlang shell中看看当前装载路径，使用如下命令 code:get_path() 。如下是例子:

     code:get_path()
     “/usr/local/lib/erlang/lib/kernel-2.11.3/ebin“,
     “/usr/local/lib/erlang/lib/stdlib-1.14.3/ebin“,
     “/usr/local/lib/erlang/lib/xmerl-1.1/ebin“,
     “/usr/local/lib/erlang/lib/webtool-0.8.3/ebin“,
     “/usr/local/lib/erlang/lib/typer-0.1.0/ebin“,
     “/usr/local/lib/erlang/lib/tv-2.1.3/ebin“,
     “/usr/local/lib/erlang/lib/tools-2.5.3/ebin“,
     “/usr/local/lib/erlang/lib/toolbar-1.3/ebin“,
     “/usr/local/lib/erlang/lib/syntax_tools-1.5.2/ebin“,
     ...]

"
());


(defun code_add_patha () 
"
     管理装载路径两个最常用的函数如下：

     @spec code:add_patha(Dir) => true | {error,bad_directory}

     添加新目录Dir到装载路径的开头

     @spec code:add_pathz(Dir) => true | {error,bad_directory}

     添加新目录Dir到装载路径的末尾

     通常并不需要自己来关注。只要注意两个函数产生的结果是不同的。如果你怀疑装载了错误的模块，你可以调用 code:all_loaded() (返回所有已经装载的模块列表)，或者 code:clash() 来帮助你研究错误所在。

     code 模块中还有其他一些程序可以用于管理路径，但是你可能根本没机会用到它们，除非你正在做一些系统编程。

     按照惯例，经常把这些命令放入一个叫做 .erlang 的文件到你的HOME目录。另外，也可以在启动Erlang时的命令行中指定:

     > erl -pa Dir1 -pa Dir2 ... -pz DirK1 -pz DirK2
     其中 -pa 标志将目录加到搜索路径的开头，而 -pz 则把目录加到搜索路径的末尾。

"
());


(defun init_get_argument()
"
     2.2   在系统启动时执行一系列命令
     我们刚才把载入路径放到了HOME目录的 .erlang 文件中。事实上，你可以把任意的Erlang代码放入这个文件，在你启动Erlang时，它就会读取和求值这个文件中的所有命令。

     假设我的 .erlang 文件如下:

     io:format(“Running Erlang~n“).
     code.add_patha(“.“)
     code.add_pathz(“/home/joe/2005/erl/lib/supported“).
     code.add_pathz(“/home/joe/bin“).
     当我启动系统时，我就可以看到如下输出:

     $ erl
     Erlang (BEAM) emulator version 5.5.1 [source] [async-threads:0] [hipe]

     Running Erlang
     Eshell V5.5.1  (abort with ^G)
     1>
     如果当前目录也有个 .erlang 文件，则会在优先于HOME目录的。这样就可以在不同启动位置定制Erlang的行为，这对特定应用非常有用。在这种情况下，推荐加入一些打印语句到启动文件；否则你可能忘记了本地的启动文件，这可能会很混乱。

     某些系统很难确定HOME目录的位置，或者根本就不是你以为的位置。可以看看Erlang认为的HOME目录的位置，通过如下的:

     1> init:get_argument(home).
     {ok,[[“/home/joe“]]}
     通过这里，我们可以推断出Erlang认为的HOME目录就是 /home/joe 。
"
());



(defun escript () 
"     3   运行程序的其他方式
     Erlang程序存储在模块中。一旦写好了程序，运行前需要先编译。不过，也可以以脚本的方式直接运行程序，叫做 escript 。

     下一节会展示如何用多种方式编译和运行一对程序。这两个程序很不同，启动和停止的方式也不同。

     第一个程序 hello.erl 只是打印 “Hello world” ，这不是启动和停止系统的可靠方式，而且他也不需要存取任何命令行参数。与之对比的第二个程序则需要存取命令行参数。

     这里是一个简单的程序。它输出 “Hello world” 然后输出换行。 “~n” 在Erlang的io和io_lib模块中解释为换行。

     -module(hello).
     -export([start/0]).

     start() ->
         io:format(“Hello world~n“).
     让我们以3种方式编译和运行它。

     3.1   在Erlang shell中编译和运行
     $ erl
     ...
     1> c(hello).
     {ok,hello}
     2> hello:start().
     Hello world
     ok
     3.2   在命令行编译和运行
     $ erlc hello.erl
     $ erl -noshell -s hello start -s init stop
     Hello World
     $
     Note

     快速脚本：

     有时我们需要在命令行执行一个函数。可以使用 -eval 参数来快速方便的实现。这里是例子:

     erl -eval 'io:format(“Memory: ~p~n“, [erlang:memory(total)]).'
         -noshell -s init stop
     Windows用户：想要让如上工作，你需要把Erlang可执行文件目录加入到环境变量中。否则就要以引号中的全路径来启动，如下:

     “C:Program Fileserl5.5.3binerlc.exe“ hello.erl
     第一行 erlc hello.erl 会编译文件 hello.erl ，生成叫做 hello.beam 的代码文件。第一个命令拥有三个选项：

     -noshell ：启动Erlang而没有交互式shell，此时不会得到Erlang的启动信息来提示欢迎

     -s hello start ：运行函数 hello:start() ，注意使用 -s Mod … 选项时，相关的模块Mod必须已经编译完成了。

     -s init stop ：当我们调用 apply(hello,start,[]) 结束时，系统就会对函数 init:stop() 求值。

     命令 erl -noshell … 可以放入shell脚本，所以我们可以写一个shell脚本负责设置路径(使用-pa参数)和启动程序。

     在我们的例子中，我们使用了两个 -s 选项，我们可以在一行拥有多个函数。每个 -s 都会使用 apply 语句来求职，而且，在一个执行完成后才会执行下一个。

     如下是启动hello.erl的例子:

     #! /bin/sh
     erl -noshell -pa /home/joe/2006/book/JAERANG/Book/code
         -s hello start -s init stop
     Note

     这个脚本需要使用绝对路径指向包含 hello.beam 。不过这个脚本是运行在我的电脑上，你使用时应该修改。

     运行这个shell脚本，我们需要改变文件属性(chmod)，然后运行脚本:

     $ chmod u+x hello.sh
     $ ./hello.sh
     Hello world
     $
     Note

     在Windows上， #! 不会起效。在Windows环境下，可以创建.bat批处理文件，并且使用全路径的Erlang来启动(假如尚未设置PATH环境变量)。

     一个典型的Windows批处理文件如下:

     “C:Program Fileserl5.5.3binerl.exe“ -noshell -s hello start -s init stop
     3.3   以Escript运行
     使用escript，你可以直接运行你的程序，而不需要先编译。

     Warning

     escript包含在Erlang R11B-4或以后的版本，如果你的Erlang实在太老了，你需要升级到最新版本。

     想要以escript方式运行hello，我们需要创建如下文件:

     #! /usr/bin/env escript

     main(_) ->
         io:format(“Hello worldn“).
     Note

     开发阶段的导出函数

     如果你正在开发代码，可能会非常痛苦于需要不断在导出函数列表增删函数。

     一个声明 -compile(export_all) ，告知编译器导出所有函数。使用这个可以让你的开发工作简化一点。

     当你完成开发工作时，你需要抓实掉这一行，并添加适当的导出列表。首先，重要的函数需要导出，而其他的都要隐藏起来。隐藏方式可以按照个人喜好，提供接口也是一样的效果。第二，编译器会对需要导出的函数生成更好的代码。

     在Unix系统中，我们可以立即按照如下方式运行:

     $ chmod u+x hello
     $ ./hello
     Hello world
     $
     Note

     这里的文件模式在Unix系统中表示可执行，这个通过chmod修改文件属性的步骤只需要执行一次，而不是每次运行程序时。

     在Windows系统中可以如下方式运行:

     C:> escript hello
     Hello world
     C:>
     Note

     在以escript方式运行时，执行速度会明显的比编译方式慢上一个数量级。

     3.4   程序的命令行参数
     “Hello world”没有参数。让我们重新来做一个计算阶乘的程序，可以接受一个参数。

     首先是代码:

     -module(fac).
     -export([fac/1]).

     fac(0) -> 1;
     fac(N) -> N*fac(N-1).
     我们可以编译 fac.erl 并且在Erlang中运行:

     $ erl
     1> c(fac).
     {ok,fac}
     2> fac:fac(25).
     15511210043330985984000000
     如果我们想要在命令行运行这个程序，我们需要修改一下他的命令行参数:

     -module(fac1).
     -export([main/1]).

     main([A]) ->
         I=list_to_integer(atom_to_list(A)).
         F=fac(I),
         io:format(“factorial ~w= ~w~n“,[I,F]),
         init:stop().

     fac(0) -> 1;
     fac(N) -> N*fac(N-1).
     让我们编译和运行:

     $ erlc fac1.erl
     $ erl -noshell -s fac1 main 25
     factorial 25 = 15511210043330985984000000
     Note

     事实上这里的main()函数并没有特殊意义，你可以把它改成任何名字。重要的一点是命令行参数中要使用函数名。

     最终，我们可以把它直接作为escript来运行:

     #! /usr/bin/env escript

     main([A]) ->
         I=list_to_integer(A),
         F=fac(I),
         io:format(“factorial ~w = ~w~n“,[I,F]).

     fac(0) -> 1;
     fac(N) -> N*fac(N-1).
     运行时无需编译，可以直接运行:

     $ ./factorial 25
     factorial 25 = 15511210043330985984000000
     $
"
());


(defun makefile ()
    "
    4   通过makefile自动编译
     当编写一个大型程序时，我希望只在需要的时候自动编译。这里有两个原因。首先，节省那些一遍遍相同的打字。第二，因为经常需要同时进行多个项目，当再次回到这个项目时，我已经忘了该如何编译这些代码。而 make 可以帮助我们解决这个问题。

     make 是一个自动工具，用以编译和发布Erlang代码。大多数我的makefile都非常简单，并且我有个模板可以解决大多数问题。

     我不想在这里解释makefile的意义。而我会展示如何将makefile用于Erlang程序。推荐看看本书附带的makefile，然后你就会明白他们并且构建你自己的makefile了。

     4.1   一个makefile模板
     如下的模板是很常用的，很多时候可以基于他们来做实际的事情:

     #让这一行就这么放着
     .SUFFIXES: .erl .beam .yrl

     .erl.beam:
         erlc -W $<

     .yrl.erl:
         erlc -W $<

     ERL=erl -boot start_clean

     #如下是需要编译的模块列表
     #如果一行写不下可以用反斜线  来折行

     #修改如下行
     MODS=module1 module2 
          module3 ....

     #makefile中的第一个目标是缺省目标
     #也就是键入make时默认的目标
     all: compile

     compile: ${MODS:%=%.beam} subdirs

     #指定编译需求，并且添加到这里
     special1.beam: special1.erl
         ${ERL} -Dflag1 -W0 special1.erl

     #从makefile运行一个应用
     application1: compile
         ${ERL} -pa Dir1 -s application1 start Arg1 Arg2

     #在子目录编译子目录的目标
     subdirs:
         cd dir1; make
         cd dir2; make
         ...

     #清除所有编译后的文件
     clean:
         rm -rf *.beam erl_crash.dump
         cd dir1; make clean
         cd dir2; make clean
     makefile开始于一些编译Erlang模块的规则，包括扩展名为 .yrl 的文件(是Erlang词法解析器的定义文件)(Erlang的词法解析器生成器为yecc，是yacc的Erlang版本，参考 http://www.erlang.org/contrib/parser_tutorial-1.0.tgz )。

     重要的部分开始于这一行:

     MODS=module1 module2
     这是我们需要编译的Erlang模块的列表。

     任何在MODS列表中的模块都会被Erlang命令 erlc Mod.erl 来编译。一些模块可能会需要特殊的待遇，比如模板中的special1。所以会有单独的规则用来处理这些。

     makefile中有一些目标。一个目标是一个字符串，随后是冒号”:”。在makefile模板中，all，compile和special1.beam都是目标。想要运行makefile，你可以在shell中执行:

     $ make [Target]
     参数Target是可选的。如果Target忽略掉了，则假设被第一个目标。在前面的例子中，目标all就是缺省的。

     如果我希望构建软件并运行，我们就会使用 make application1 。如果我希望这个作为缺省行为，也就是在我每次键入 make 时都会执行，我就可以把application1目标作为第一个目标。

     目标clean会删除所有编译过的Erlang目标代码和 erl_crash.dump 。crashdump包含了帮助调试应用程序的信息。查看6.10了解更多。

     4.2   实际修改makefile模板
     我并不热衷于让自己的程序变得混乱，所以我一般以一个不包含无用行的模板开始。所以得到的makefile会更短，而且也更易于阅读。另外，你也可以使用到处都是变量的makefile，以方便定制。

     一旦我遍历整个流程，就会得到一个很简单的makefile，有如如下:

     .SUFFIXES: .erl .beam

     .erl.beam:
         erlc -W $<

     ERL = erl -boot start_clean

     MODS=module1 module2 module3

     all: compile
         ${ERL} -pa '/home/joe/.../this/dir' -s module1 start

     compile: ${MODS:%=%.beam}

     clean:
         rm -rf *.beam erl_crash.dump
    "
    ());

(defun emacs_shell ()
    "
    %% 5   Erlang shell中的命令编辑
     Erlang shell包含了内置的行编辑器。它可以执行emacs的一部分行编辑命令，前一行可以以多种方式来调用。可用命令如下，注意 “^Key” 是指按下 “Ctrl+Key” 。

     ==================================================
          命令       描述
     --------------------------------------------------
          ^A               开始一行
          ^E               最后一行
          ^F或右箭头       向前一个字符
          ^B或左箭头       向后一个字符
          ^P或上箭头       前一行
          ^N或下箭头       下一行
          ^T               调换最后两个字符的顺序
          Tab              尝试补全模块名或函数名
     ==================================================
    "
    ());


(defun detached () 
"   
     6   解决错误
     Erlang有时会发生一些问题而退出。下面是可能的错误原因：

     shell没有响应
     Ctrl+C处理器被禁用了
     Erlang以 -detached 标志启动，这时你甚至感觉不到他在运行
     Erlang以 -heart Cmd 标志启动。这会让OS监控器进程看管Erlang的OS进程。如果Erlang的OS进程死掉，那么就会求值 Cmd 。一般来说 Cmd 只是用于简单的重启Erlang系统。这个是用于生产可容错系统的一个重要技巧，用于结点-如果erlang自己死掉了(基本不可能发生)，就会自己重启。这个技巧对Unix类操作系统使用 ps 命令来监控，对Windows使用任务管理器。进行心跳信息检测并且尝试kill掉erlang进程。
     有且确实很无奈的错误，留下一个erlang僵尸进程。
"
());


(defun features () 
    "
     虽然ERLANG本身一种很有吸引力的编语言，但当你把它与虚拟机（VM），OTP中间件和类库相结合的时候，其真正的实力才能完全体现出来。其中的每一点都要使ERLANG软件开发变得如此特别。
     1）高级构造
        ERLANG是一种声明性的语言。声明性语言工作的原则是去描述应该计算什么，而不是去解释这个值是如何计算而来的。一个函数定义就像一组行等式，尤其是当使用模式匹配从不同的情况中去选择和从复杂的数据结构中抽取组件的时候。如：
        area({square, Side}) -> Side * Side;
        area({circle, Radius}) -> math:pl() * Radius * Radius.

        这个函数定义包含一个形状参数（这里是一个方形或者园形），依靠它收到的形状类型，系统匹配正确的函数定义，并返回面积计算结果。
        ERLANG中，不仅可以对高层数据进行模式匹配，而且同样可以对二进制序列数据进行匹配。另一个特性是，函数（或者闭包）属于第一类数据。可以把它们绑定到一个变量上，也可以像其他数据一样处理它们：在一个列表中存储，以一个函数方式返回，或在不同的进程中传递它们。
      
        ERLANG中的列表解析（list comprehension）借鉴了函数式编程范例，它结合了列表生成器（list generator）和过滤器(filter)，返回在使用过滤器后由一个列表生成产生的部分元素的列表。 
       下面例子只用几行代码就实现了快速排序算法。
       qsort([]) -> [];
       qsort([X|Xs]) -> 
            qsort([Y || Y <- Xs, Y =< X]) ++ [X] ++ qsort([Y || Y <- Xs, Y > X]).

     2）并发进程和消息传递
        并发是ERLANG成功的根本。ERLANG不提供共享内存的线程，而是每个ERLANG进程都在它自己的内存空间里执行，并拥有它自己的堆和栈。进程之间不能随意相互干扰，而这在线程模型中很容易发生。从而极易导致死锁和其他可怕的事情。

        进程之间通过消息进行相互交流，而这个消息可以是ERLANG中任意数据。消息传递是异步的，因此一旦消息发送，这一进程就能够马上继续执行，消息是有选择属于 取自进程信箱，因此没有必要按照消息的到达顺序来处理它们。特别是当进程处理发生在不同的计算机上，并且消息收取的顺序依赖于周围的网络环境的时候，这使并发更为健壮。


     3）可扩展，安全和高效的并发
        ERLANG的并发具有快速和可扩展的特性。它的进程是轻量级的，ERLANG虚拟机不会为每一个已经生成的进程创建一个操作系统线程。ERLANG进程在虚拟机中生成，调度和处理，而与底层的操作系统无关。因此，进程的生成时间是以微秒为单位的，并且独立于现存的进程的数量。
        ERLANG进程相互之间的通信是通过消息传递进行的，不管在你的系统中有多少并发进程，系统的消息交换仅需要几微秒。消息传递的流程是，将数据从一个进程的内存空间复制到另一个内存空间这些都发生在同一个虚拟机中，这点不同于JAVA和C#中常使用的共享内存，信号量和操作系统进程。即便如此，性能测试结果显示ERLANG同样超越了其他语言，就像在进程生成时的优势一样。

     4）软实时性
        尽管ERLANG是一种高级语言，但你同样可以利用它的软实时性。ERLANG中的存储管理是自动的，垃圾收集的实现是以每个进程为基础。即使存在需要垃圾收集的内存，系统的响应时间也能以毫秒计算。正因为如此，即使在持续高峰的时候，ERLANG也能不降低吐吞量而高负荷运行。

     5）健壮性
        如何创建一个健壮的系统呢，虽然ERLANG未必能解决你所有的问题，但和其他语言相比它在很大程度上给你的工作提供了便利。ERLANG有一整套简单但有效的错误处理机制和异常监控机制，并且已经内置了大量通用库模块，其内核加入了健壮性的设计。通过针对正确分支进行编程和由类库来处理错误，程序变得简短易懂，而且错误更少。

     5.1）ERLANG进程可以链接在一起，如果一个进程崩溃，就会通知其他进程，然后（该进程）可以处理这个崩溃，或者选择结束自身。 
    
     5.2）OTP提供了一些通用行为包，如服务器，有限状态机和事件句柄。这些工作进程具有内在的健壮性，它们处理所有这些模式的通用（因此很难）并发部分；而用户所需要做的只是对特定服务器的具体行为进程编程，这种编程显然比通用行为包更为直接。
 
     5.3）这些通用行为包与监控行为包链接在一起，而监控行为包唯一的任务是监控和处理进程终止。OTP把链接的构想引入框架的设计中，当一个进程监控其他工作进程和监控进程的同时，可能它自身也被其他进程所监控，所有这些都在一个结构中。

     5.4）利用这种监控和链接方法，ERLANG程序员可以专注于针对正确的情况编程 ，而在其他任何情况下可以使这一进程出错。避免防错性编程便程序员的工作容易了很多，也能更加直接了解程序的行为。

     6）分布式计算
        ERLANG已经把分布式计算纳入了语言的语法和语义中，它允许创建与位置无关的系统。默认的分布模式是基于TCP/IP协议的，它允许一个异构网络上的节点（或ERLANG运行时系统）链接到任意一个运行任何操作系统的其他节点上。只要这些节点通过TCP/IP网络连接并且正确配置了防火墙，这样就形成了一个所有节点都能相互通信的全互联网络。由于当初ERLANG是设计在防火墙后面运行的，ERLANG的安全性是基于COOKIE而很限制访问权限。你可以使用网关来创建更多不同的ERLANG分布式网络节点，如果有必要，可以让它们使用安全的因特网通信协议（如SSL）来通信。

        ERLANG程序由通过消息传递进通信的进程所组成。当你开始用ERLANG编写程序的时候，这些都在一个节点上，由于在节点内发送消息的语法和发送到远程节点上的语法是一样的，因此你可以轻松地在一组计算机上分布你的进程。由于分布式成为语言的一部分，因此操作（例如集群，负载平衡，硬件和节点的增加），通信和可靠性都只需要非常少的开销和相应非常少的代码就可以实现。

     7）集成与开放
        为不同的工作你应该使用正确的工具。ERLANG作为一门开放的语言，允许你保留遗留代码，或放入比ERALNG更适合这项工作的其他编程语言的新代码。ERLANG有专门的机制来和其他语言进行交互。
"
());


(defun erlang_integer ()
"
     Integers在ERLANG中用来表示整数，可以是正整数或者负整数，也可表示基数不为10的整数。最大数值的概念在ERALNG中不存在。当一个整数大到一个WORD不能容纳的时候，ERLANG内部会自动把它转换成用多个WORD表示的bignums类型。虽然用BIGNUMS类型能精确完成任意大整数的计算，但是相对于固定大小的整数类型它们的效率相对 比较低。一个整数究竟能有多大，在ERLANG中它唯一的限制是实际使用的机器可用内存。

     1）常规整数
        一些整数的例子：―234      0      10     100000000000000

     2）Base#Value符号
        用于表示基数不是10的整数。BASE是一个介于2~16的整数，VALUE就是基于BASE的具体数值。

        2#1010    表示整数10的二进制形式。
        -16#EA    表示整数―234的十六进制形式。因为在16进制中字母A―F被用来表示10至15。 

     3）$CHARACTER
        表示字符的ASCII值。

        $a      97
        $A      65 
        $\n     10 
"
());

(defun erlang_float ()
"
     在ERLANG中，浮点数用来代表实数。
     	17.68
     	―56.432
     	1.234E-10       E-10是一种常规的浮点数表示符号.用来表示十进制小数点必须左移动10个位置.

     ERLANG中的浮点数的精确度是由IEEE754-1985标准中的64位表示法来保证的. 
"
());

(defun erlang_expression ()
"
 =============================================================================
     类型              描述                          数据类型
 -----------------------------------------------------------------------------
     +                 一元操作符+                   整数  |  浮点数
     -                 一元操作符-                   整数  |  浮点数
     *                 乘法                          整数  |  浮点数
     /                 浮点除法                      整数  |  浮点数
     div               整数除法                      整数
     rem               整数取余                      整数  
     +                 加法                          整数  |  浮点数 
     -                 减法                          整数  |  浮点数 
 ============================================================================= 
  
     所有的数学运算都是左结合的.
     把一个整数和一个浮点数相加是可以的，在进行加法之前，先强制把整数转换成了浮点数来进行.
     如果要改变默认的优先级，那就需要使用到圆括号. 
 
 可以出现在保护式中的项式比较运算符如下：
 ============================================================
     运算符 	          描述 	        类型
 ------------------------------------------------------------
     X > Y 	          X大于Y 	coerce
     X < Y 	          X小于Y 	coerce
     X =< Y 	          X小于或等于Y 	coerce
     X >= Y 	          X大于或等于Y 	coerce
     X == Y 	          X等于Y 	coerce
     X /= Y 	          X不等于Y 	coerce
     X =:= Y 	          X等于Y 	exact
     X =/= Y 	          X不等于Y 	exact
 ============================================================


 比较运算符工作机制如下：首先对运算符两边求值（如，在表达式两边存在算术表达式或包含BIF保护式函数时）；然后再进行比较。

 为了进行比较，定义如下的偏序关系：

 number < atom < reference < port < pid < tuple < list

 元组首先按大小排序，然后再按元素排序。列表的比较顺序是先头部，后尾部。

 如果比较运算符的两个参数都是数值类型且运算符为coerce型，则如果一个参数是integer另一个是float，那么integer将被转换为float再进行比较。

 exact类型的运算符则不做这样的转换。

 因此5.0 == 1 + 4为真，而5.0 =:= 4 + 1为假。

 保护函数子句示例：

 foo(X, Y, Z) when integer(X), integer(Y), integer(Z), X == Y + Z ->
 foo(X, Y, Z) when list(X), hd(X) == {Y, length(Z)}  ->
 foo(X, Y, Z) when {X, Y, size(Z)} == {a, 12, X} ->
 foo(X) when list(X), hd(X) == c1, hd(tl(X)) == c2 ->

 注意在保护式中不可引	入新的变量。

 算术表达式由以下运算符构成：
 =====================================================================================
          运算符 	描述            	类型 	操作数类型 	优先级
 -------------------------------------------------------------------------------------
           + X        + X 	                单目 	混合        	1
           - X        - X 	                单目   	混合        	1
           X * Y 	X * Y                	双目 	混合     	2
           X / Y 	X / Y（浮点除法） 	双目 	混合 	        2
           X div Y 	X整除Y          	双目 	整数 	        2
           X rem Y 	X除以Y的余数          	双目 	整数 	        2
           X band Y 	X与Y的位与       	双目 	整数 	        2
           X + Y 	X + Y           	双目 	混合 	        3
           X - Y 	X - Y 	                双目 	混合 	        3
           X bor Y 	X与Y位或 	        双目 	整数 	        3
           X bxor Y 	X与Y的位算数异或 	双目 	整数 	        3
           X bsl N 	X算数左移N位 	        双目 	整数 	        3
           X bsr N 	X右移N位        	双目 	整数 	        3
 =====================================================================================
 单目运算符有一个参数，双目运算符有两个参数。混合意味着参数即可以是integer 也可以是float。单目运算符的返回值与其参数类型相同。

 双目混合运算符（即*、-、+）在参数都是integer时返回类型为integer的对象，在参数至少包含一个float时返回一个float。浮点除法运算符/总是返回一个float。

 双目整数运算符（即band、div、rem、bor、bxor、bsl、bsr）的参数必须是整数，其返回值也是整数。

 求值顺序取决于运算符的优先级：首先计算第1优先级的运算符，然后是第2优先级，以此类推。括号内的表达式优先求值。

 优先级相同的运算符从左到右进行求值。比如：

 A - B - C - D

 其求值顺序与下面的表达式一致：

 (((A - B) - C) - D)

"
());


(defun atom ()
"
    在ERLANG中用基元（ATOM）来表示文字常量.这和其他语言中的枚举类型的作用是一样的.
   
     唯一可用于基元的操作是比较.而且这在ERLANG中是以一种非常高效的方法实现的.

     基元由小写字母开始或是由单引号界定.当基元用小写字母开始的时候，字母，数据，@符号，英文句点和下划线（_）都是有效的字符.如果一个基元通过单引号封装起来，则可以使用任意字符.

     基元按照字典顺序来排序.

 原子式以小写字母（a..z）开头，以非字母数字字符结尾——否则就必须用引号括起来。

 通过将原子式以引号括起来，原子式中便可以出现任意字符。原子式总是以可被 Erlang 读取程序读入的格式输出。原子式引号内的字符遵循如下规范：
 字符 	含义
 ＼b 	退格符
 ＼d 	删除符
 ＼e 	转义符（ESC）
 ＼f 	换页符
 ＼n 	换行符
 ＼r 	回车符
 ＼t 	制表符
 ＼v 	垂直制表符
 ＼＼ 	反斜线
 ＼^A .. ＼^Z 	control A到control Z（即0 .. 26）
 ＼' 	单引号
 ＼“ 	双引号
 ＼OOO 	使用八进制格式OOO表示的字符

 在引号括起来的原子式中如果包含字符序列＼C，其中C的ASCII值小于32，则表示＼C的这部分源码被忽略（这样我们在编程时就可以使用一个反斜线加换行符来将长原子式分隔为几行）。

"
());


(defun erlang_boolean () 
"
     ERLANG中没有单独表示布尔类型的布尔值或者字符.基元true和 false与布尔操作符一起使用，而不是布尔类型.它们用来表示测试的布尔返回值，尤其是用于比较操作. 
     内置函数is_boolean用来测试一个ERLANG值是否是布尔类型.

 ===================================================================================================================
     运算           描述
 -------------------------------------------------------------------------------------------------------------------
     and            如果两个参数都是真，那么就返回真
     andalso        and的快捷计算形式：如果第一个参数是假，那么就返回假，而不需要计算第二个参数
     or             如果两个参数的任何一个是真，那么就返回真 
     orelse         or的快捷计算形式，如果第一个参数是真，那么就返回真，而不需要计算第二个参数
     xor            “异或“：如果两个参数的任何一个是真，并且另一个是假，那么就返回真
     not            一元否定运算符：如果参数是假，那么就返回真，反之亦然  
 ===================================================================================================================
     例子：
     1> not((1<3) and (2=2)).
     false
     2> not((1<3) or (2==2)).
     false
     3> not((1<3) xor (2==2)).
     true

"
());

(defun is_boolean ()
"
   内置函数is_boolean用来测试一个ERLANG值是否是布尔类型.
"
());


(defun erlang_tuple ()
"
     元组（TUPLE）是用来保存一组数据元素的复合数据类型，其中数据元素要求是ERLANG数据类型，但并不一定要是相同的类型.元组使用封闭的花括号（{}）来定义，而其中的元素由逗号隔开.

     空元组{}不包含任何元素.元组可以只包含一个元素，但由于你可以直接使用“非元组“的元素自身，因此在代码中这样使用通常不是一个好主意.

     当一个元组的第一个元素是一个基元时，称它为标记（TAG）.这一ERLANG惯例用来表示不同类型的数据.同时通常在使用它的程序中有着特殊的意义，例如：
     {person, 'JOE','Armstrong'},基元person是标记（tag），这有可能也就表明第二个元素是这个人的名，第三个是姓.

     使用第一个位置的标记是为了区分代码中使用不同的元组的不同目的.当错误元组当做一个参数被错误的传递，或者是作为一个函数调用的结果错误的返回的时候，这有助于找到错误的原因.这被认为是ERALNG的最佳实践之一. 

     元组的元素索引是从1开始的.

     ===== 返回多个值 =====

     我们经常想让一个函数返回多个值，使用元组来实现这一目的是十分方便的。

     例如，函数parse_int(List)从一个由ASCII字符构成的列表List中提取最开始的数字，如果存在，就返回一个由被提取出来的数字和列表剩下的部分组成的元组，如果列表中没有数字的话，就返回原子式eoString。

     parse_int(List) ->
         parse_int(skip_to_int(List), 0).

     parse_int([H|T], N) when H >= $0, H =< $9 ->
         parse_int(T, 10 * N + H - $0);
     parse_int([], 0) ->
         eoString;
     parse_int(L, N) ->
         {N,L}.

     skip_to_int(L)返回L中第一个以ASCII字符0到9中的任意一个开始的子列表。

     skip_to_int([]) ->
         [];
     skip_to_int([H|T]) when H >= $0, H =< $9 ->
         [H|T];
     skip_to_int([H|T]) ->
         skip_to_int(T).

     如果我们使用字符串“abcd123def“（“abcd123def“的列表形式是[97,98,99,49,50,51,100,101,102]）来测试parse_int：

     > tuples:parse_int(“abc123def“).
     {123,[100,101,102]}}

     在parse_int的基础上，可以实现一个提取所有嵌入在字符串里面的数字的解释器。

     parse_ints([]) ->
         [];
     parse_ints(L) ->
         case parse_int(L) of
             eoString ->
                 [];
             {H,Rest} ->
                 [H|parse_ints(Rest)]
         end.

     因此：

     > tuples:parse_ints(“abc,123,def,456,xx“).
     [123,456]

     ===== 密码加密 =====

     几乎每天笔者们都不得不记住许多不同的密码——信用卡的密码，门禁密码等等。这些密码可以用一种方法记录下来，并且不会被犯罪分子利用吗？

     假设我们有一张密码为3451的LISA信用卡，它的密码可以像这样被编码：

     a b c d e f g h i j k l m n o p q r s t u v w x y z
     1 0 5 3 4 3 2 7 2 5 4 1 9 4 9 6 3 4 1 4 1 2 7 8 5 0   lisa

     这样密码就可以写在一张纸上，即使这张纸落在他人手上，密码也是安全的。

     我们如何解码信息呢？用来加密密码的密钥是公开的——因此我们可以很容易地读出密码（3451）–试试看！

     我们很容易的就可以构造一个用来执行加密的函数encode(Pin, Password)[1]：

     encode(Pin, Password) ->
         Code = {nil,nil,nil,nil,nil,nil,nil,nil,nil,
                 nil,nil,nil,nil,nil,nil,nil,nil,nil,
                 nil,nil,nil,nil,nil,nil,nil,nil},
         encode(Pin, Password, Code).

     encode([], _, Code) ->
         Code;
     encode(Pin, [], Code) ->
         io:format(“Out of Letters~n“,[]);

     encode([H|T], [Letter|T1], Code) ->
         Arg = index(Letter) + 1,
         case element(Arg, Code) of
             nil ->
                 encode(T, T1, setelement(Arg, Code, index(H)));
             _ ->
                 encode([H|T], T1, Code)
         end.

     index(X) when X >= $0, X =< $9 ->
         X - $0;

     index(X) when X >= $A, X =< $Z ->
         X - $A.

     我们看一下以下的例子：

     > pin:encode(“3451“,“DECLARATIVE“).
     {nil,nil,5,3,4,nil,nil,nil,nil,nil,nil,1,nil,nil,nil,
      nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil}

     我们现在使用随机数来替换没有被填充的nil元素：

     print_code([], Seed) ->
         Seed;

     print_code([nil|T], Seed) ->
         NewSeed = ran(Seed),
         Digit = NewSeed rem 10,
         io:format(“~w “,[Digit]),
         print_code(T, NewSeed);

     print_code([H|T],Seed) ->
         io:format(“~w “,[H]),
     print_code(T, Seed).

     ran(Seed) ->
         (125 * Seed + 1) rem 4096.

     然后我们需要一些小函数将所有东西连接在一起：

     test() ->
         title(),
         Password = “DECLARATIVE“,
         entries([{“3451“,Password,lisa},
                  {“1234“,Password,carwash},
                  {“4321“,Password,bigbank},
                  {“7568“,Password,doorcode1},
                  {“8832“,Password,doorcode2},
                  {“4278“,Password,cashcard},
                  {“4278“,Password,chequecard}]).

     title() ->
         io:format(“a b c d e f g h i j k l m ＼
                    n o p q r s t u v w x y z~n“,[]).

     entries(List) ->
         {_,_,Seed} = time(),
         entries(List, Seed).

     entries([], _) -> true;

     entries([{Pin,Password,Title}|T], Seed) ->
         Code = encode(Pin, Password),
         NewSeed = print_code(tuple_to_list(Code), Seed),
         io:format(“ ~w~n“,[Title]),
         entries(T, NewSeed).

     最后我们可以运行这个程序了：

     1> pin:test().
     1 0 5 3 4 3 2 7 2 5 4 1 9 4 9 6 3 4 1 4 1 2 7 8 5 0 lisa
     9 0 3 1 2 5 8 3 6 7 0 4 5 2 3 4 7 6 9 4 9 2 7 4 9 2 carwash
     7 2 2 4 3 1 2 1 8 3 0 1 5 4 1 0 5 6 5 4 3 0 3 8 5 8 bigbank
     1 0 6 7 5 7 6 9 4 5 4 8 3 2 1 0 7 6 1 4 9 6 5 8 3 4 doorcode1
     1 4 3 8 8 3 2 5 6 1 4 2 7 2 9 4 5 2 3 6 9 4 3 2 5 8 doorcode2
     7 4 7 4 2 5 6 5 8 5 8 8 9 4 7 6 5 0 1 2 9 0 9 6 3 8 cashcard
     7 4 7 4 2 7 8 7 4 3 8 8 9 6 3 8 5 2 1 4 1 2 1 4 3 4 chequecard
     true

     之后这些信息可以用很小的字体打印出来，粘在一张邮票的背后，藏在你的领带里面[2]。
 "
());
