let color_config = {
    # UI Elements
    separator: "dark_gray"
    leading_trailing_space_bg: "#ffffff"
    header: "green"
    date: "magenta"
    filesize: "blue"
    row_index: "cyan"
    hints: "dark_gray"

    # Data Types
    string: "white"
    primitive: "white"
    int: "green"
    float: "green"
    bool: "cyan"
    nothing: "red"
    binary: "magenta"
    cellpath: "cyan"
    duration: "yellow"
    range: "yellow"

    # Highlighting and Errors
    search_result: "bright-yellow"
    shape_garbage: "bright-red"
    shape_bool: "bright-cyan"
    shape_int: "bright-green"
    shape_float: "bright-green"
    shape_range: "bright-yellow"
    shape_string: "bright-blue"
    shape_string_interpolation: "bright-cyan"
}

$env.config.color_config = $color_config
# maybe use a system akin to this eventually?
# $env.COLOR_SCHEME = {
#   background: '#353446'
#   foreground: '#C9C6DC'
#   blue: '#7FBBB3'
#   cyan: '#83C092'
#   green: '#A7C080'
#   purple: '#D699B6'
#   red: '#E67E80'
#   yellow: '#DBBC7F'
#   white: '#D3C6AA'
# }

$env.STARSHIP_SHELL = "nu"

$env.LS_COLORS = "*~=0;38;2;67;76;94:bd=1;38;2;235;203;139;48;2;67;76;94:ca=0:cd=1;38;2;235;203;139;48;2;67;76;94:di=1;38;2;143;188;187:do=1;38;2;180;142;173;48;2;67;76;94:ex=1;38;2;208;135;112:fi=0;38;2;76;86;106:ln=1;38;2;163;190;140:mh=0:mi=1;38;2;236;239;244;48;2;191;97;106:no=0;38;2;76;86;106:or=1;38;2;236;239;244;48;2;191;97;106:ow=0:pi=1;38;2;235;203;139;48;2;67;76;94:rs=0;38;2;76;86;106:sg=0:so=1;38;2;180;142;173;48;2;67;76;94:st=0:su=0:tw=0:*.1=0;38;2;180;142;173:*.a=0;38;2;208;135;112:*.c=0;38;2;163;190;140:*.d=0;38;2;163;190;140:*.h=0;38;2;163;190;140:*.m=0;38;2;163;190;140:*.o=0;38;2;67;76;94:*.p=0;38;2;163;190;140:*.r=0;38;2;163;190;140:*.t=0;38;2;163;190;140:*.v=0;38;2;163;190;140:*.z=1;38;2;129;161;193:*.7z=1;38;2;129;161;193:*.ai=0;38;2;180;142;173:*.as=0;38;2;163;190;140:*.bc=0;38;2;67;76;94:*.bz=1;38;2;129;161;193:*.cc=0;38;2;163;190;140:*.cp=0;38;2;163;190;140:*.cr=0;38;2;163;190;140:*.cs=0;38;2;163;190;140:*.db=1;38;2;129;161;193:*.di=0;38;2;163;190;140:*.el=0;38;2;163;190;140:*.ex=0;38;2;163;190;140:*.fs=0;38;2;163;190;140:*.go=0;38;2;163;190;140:*.gv=0;38;2;163;190;140:*.gz=1;38;2;129;161;193:*.ha=0;38;2;163;190;140:*.hh=0;38;2;163;190;140:*.hi=0;38;2;67;76;94:*.hs=0;38;2;163;190;140:*.jl=0;38;2;163;190;140:*.js=0;38;2;163;190;140:*.ko=0;38;2;208;135;112:*.kt=0;38;2;163;190;140:*.la=0;38;2;67;76;94:*.ll=0;38;2;163;190;140:*.lo=0;38;2;67;76;94:*.ma=0;38;2;180;142;173:*.mb=0;38;2;180;142;173:*.md=0;38;2;180;142;173:*.mk=0;38;2;163;190;140:*.ml=0;38;2;163;190;140:*.mn=0;38;2;163;190;140:*.nb=0;38;2;163;190;140:*.nu=0;38;2;163;190;140:*.pl=0;38;2;163;190;140:*.pm=0;38;2;163;190;140:*.pp=0;38;2;163;190;140:*.ps=0;38;2;180;142;173:*.py=0;38;2;163;190;140:*.rb=0;38;2;163;190;140:*.rm=0;38;2;180;142;173:*.rs=0;38;2;163;190;140:*.sh=0;38;2;163;190;140:*.so=0;38;2;208;135;112:*.td=0;38;2;163;190;140:*.ts=0;38;2;163;190;140:*.ui=0;38;2;180;142;173:*.vb=0;38;2;163;190;140:*.wv=0;38;2;180;142;173:*.xz=1;38;2;129;161;193:*FAQ=0;38;2;180;142;173:*.3ds=0;38;2;180;142;173:*.3fr=0;38;2;180;142;173:*.3mf=0;38;2;180;142;173:*.adb=0;38;2;163;190;140:*.ads=0;38;2;163;190;140:*.aif=0;38;2;180;142;173:*.amf=0;38;2;180;142;173:*.ape=0;38;2;180;142;173:*.apk=1;38;2;129;161;193:*.ari=0;38;2;180;142;173:*.arj=1;38;2;129;161;193:*.arw=0;38;2;180;142;173:*.asa=0;38;2;163;190;140:*.asm=0;38;2;163;190;140:*.aux=0;38;2;67;76;94:*.avi=0;38;2;180;142;173:*.awk=0;38;2;163;190;140:*.bag=1;38;2;129;161;193:*.bak=0;38;2;67;76;94:*.bat=0;38;2;208;135;112:*.bay=0;38;2;180;142;173:*.bbl=0;38;2;67;76;94:*.bcf=0;38;2;67;76;94:*.bib=0;38;2;180;142;173:*.bin=1;38;2;129;161;193:*.blg=0;38;2;67;76;94:*.bmp=0;38;2;180;142;173:*.bsh=0;38;2;163;190;140:*.bst=0;38;2;180;142;173:*.bz2=1;38;2;129;161;193:*.c++=0;38;2;163;190;140:*.cap=0;38;2;180;142;173:*.cfg=0;38;2;180;142;173:*.cgi=0;38;2;163;190;140:*.clj=0;38;2;163;190;140:*.com=0;38;2;208;135;112:*.cpp=0;38;2;163;190;140:*.cr2=0;38;2;180;142;173:*.cr3=0;38;2;180;142;173:*.crw=0;38;2;180;142;173:*.css=0;38;2;163;190;140:*.csv=0;38;2;180;142;173:*.csx=0;38;2;163;190;140:*.cxx=0;38;2;163;190;140:*.dae=0;38;2;180;142;173:*.dcr=0;38;2;180;142;173:*.dcs=0;38;2;180;142;173:*.deb=1;38;2;129;161;193:*.def=0;38;2;163;190;140:*.dll=0;38;2;208;135;112:*.dmg=1;38;2;129;161;193:*.dng=0;38;2;180;142;173:*.doc=0;38;2;180;142;173:*.dot=0;38;2;163;190;140:*.dox=0;38;2;163;190;140:*.dpr=0;38;2;163;190;140:*.drf=0;38;2;180;142;173:*.dxf=0;38;2;180;142;173:*.eip=0;38;2;180;142;173:*.elc=0;38;2;163;190;140:*.elm=0;38;2;163;190;140:*.epp=0;38;2;163;190;140:*.eps=0;38;2;180;142;173:*.erf=0;38;2;180;142;173:*.erl=0;38;2;163;190;140:*.exe=0;38;2;208;135;112:*.exr=0;38;2;180;142;173:*.exs=0;38;2;163;190;140:*.fbx=0;38;2;180;142;173:*.fff=0;38;2;180;142;173:*.fls=0;38;2;67;76;94:*.flv=0;38;2;180;142;173:*.fnt=0;38;2;180;142;173:*.fon=0;38;2;180;142;173:*.fsi=0;38;2;163;190;140:*.fsx=0;38;2;163;190;140:*.gif=0;38;2;180;142;173:*.git=0;38;2;67;76;94:*.gpr=0;38;2;180;142;173:*.gvy=0;38;2;163;190;140:*.h++=0;38;2;163;190;140:*.hda=0;38;2;180;142;173:*.hip=0;38;2;180;142;173:*.hpp=0;38;2;163;190;140:*.htc=0;38;2;163;190;140:*.htm=0;38;2;180;142;173:*.hxx=0;38;2;163;190;140:*.ico=0;38;2;180;142;173:*.ics=0;38;2;180;142;173:*.idx=0;38;2;67;76;94:*.igs=0;38;2;180;142;173:*.iiq=0;38;2;180;142;173:*.ilg=0;38;2;67;76;94:*.img=1;38;2;129;161;193:*.inc=0;38;2;163;190;140:*.ind=0;38;2;67;76;94:*.ini=0;38;2;180;142;173:*.inl=0;38;2;163;190;140:*.ino=0;38;2;163;190;140:*.ipp=0;38;2;163;190;140:*.iso=1;38;2;129;161;193:*.jar=1;38;2;129;161;193:*.jpg=0;38;2;180;142;173:*.jsx=0;38;2;163;190;140:*.jxl=0;38;2;180;142;173:*.k25=0;38;2;180;142;173:*.kdc=0;38;2;180;142;173:*.kex=0;38;2;180;142;173:*.kra=0;38;2;180;142;173:*.kts=0;38;2;163;190;140:*.log=0;38;2;67;76;94:*.ltx=0;38;2;163;190;140:*.lua=0;38;2;163;190;140:*.m3u=0;38;2;180;142;173:*.m4a=0;38;2;180;142;173:*.m4v=0;38;2;180;142;173:*.mdc=0;38;2;180;142;173:*.mef=0;38;2;180;142;173:*.mid=0;38;2;180;142;173:*.mir=0;38;2;163;190;140:*.mkv=0;38;2;180;142;173:*.mli=0;38;2;163;190;140:*.mos=0;38;2;180;142;173:*.mov=0;38;2;180;142;173:*.mp3=0;38;2;180;142;173:*.mp4=0;38;2;180;142;173:*.mpg=0;38;2;180;142;173:*.mrw=0;38;2;180;142;173:*.msi=1;38;2;129;161;193:*.mtl=0;38;2;180;142;173:*.nef=0;38;2;180;142;173:*.nim=0;38;2;163;190;140:*.nix=0;38;2;180;142;173:*.nrw=0;38;2;180;142;173:*.obj=0;38;2;180;142;173:*.obm=0;38;2;180;142;173:*.odp=0;38;2;180;142;173:*.ods=0;38;2;180;142;173:*.odt=0;38;2;180;142;173:*.ogg=0;38;2;180;142;173:*.ogv=0;38;2;180;142;173:*.orf=0;38;2;180;142;173:*.org=0;38;2;180;142;173:*.otf=0;38;2;180;142;173:*.otl=0;38;2;180;142;173:*.out=0;38;2;67;76;94:*.pas=0;38;2;163;190;140:*.pbm=0;38;2;180;142;173:*.pcx=0;38;2;180;142;173:*.pdf=0;38;2;180;142;173:*.pef=0;38;2;180;142;173:*.pgm=0;38;2;180;142;173:*.php=0;38;2;163;190;140:*.pid=0;38;2;67;76;94:*.pkg=1;38;2;129;161;193:*.png=0;38;2;180;142;173:*.pod=0;38;2;163;190;140:*.ppm=0;38;2;180;142;173:*.pps=0;38;2;180;142;173:*.ppt=0;38;2;180;142;173:*.pro=0;38;2;163;190;140:*.ps1=0;38;2;163;190;140:*.psd=0;38;2;180;142;173:*.ptx=0;38;2;180;142;173:*.pxn=0;38;2;180;142;173:*.pyc=0;38;2;67;76;94:*.pyd=0;38;2;67;76;94:*.pyo=0;38;2;67;76;94:*.qoi=0;38;2;180;142;173:*.r3d=0;38;2;180;142;173:*.raf=0;38;2;180;142;173:*.rar=1;38;2;129;161;193:*.raw=0;38;2;180;142;173:*.rpm=1;38;2;129;161;193:*.rst=0;38;2;180;142;173:*.rtf=0;38;2;180;142;173:*.rw2=0;38;2;180;142;173:*.rwl=0;38;2;180;142;173:*.rwz=0;38;2;180;142;173:*.sbt=0;38;2;163;190;140:*.sql=0;38;2;163;190;140:*.sr2=0;38;2;180;142;173:*.srf=0;38;2;180;142;173:*.srw=0;38;2;180;142;173:*.stl=0;38;2;180;142;173:*.stp=0;38;2;180;142;173:*.sty=0;38;2;67;76;94:*.svg=0;38;2;180;142;173:*.swf=0;38;2;180;142;173:*.swp=0;38;2;67;76;94:*.sxi=0;38;2;180;142;173:*.sxw=0;38;2;180;142;173:*.tar=1;38;2;129;161;193:*.tbz=1;38;2;129;161;193:*.tcl=0;38;2;163;190;140:*.tex=0;38;2;163;190;140:*.tga=0;38;2;180;142;173:*.tgz=1;38;2;129;161;193:*.tif=0;38;2;180;142;173:*.tml=0;38;2;180;142;173:*.tmp=0;38;2;67;76;94:*.toc=0;38;2;67;76;94:*.tsx=0;38;2;163;190;140:*.ttf=0;38;2;180;142;173:*.txt=0;38;2;180;142;173:*.typ=0;38;2;180;142;173:*.usd=0;38;2;180;142;173:*.vcd=1;38;2;129;161;193:*.vim=0;38;2;163;190;140:*.vob=0;38;2;180;142;173:*.vsh=0;38;2;163;190;140:*.wav=0;38;2;180;142;173:*.wma=0;38;2;180;142;173:*.wmv=0;38;2;180;142;173:*.wrl=0;38;2;180;142;173:*.x3d=0;38;2;180;142;173:*.x3f=0;38;2;180;142;173:*.xlr=0;38;2;180;142;173:*.xls=0;38;2;180;142;173:*.xml=0;38;2;180;142;173:*.xmp=0;38;2;180;142;173:*.xpm=0;38;2;180;142;173:*.xvf=0;38;2;180;142;173:*.yml=0;38;2;180;142;173:*.zig=0;38;2;163;190;140:*.zip=1;38;2;129;161;193:*.zsh=0;38;2;163;190;140:*.zst=1;38;2;129;161;193:*TODO=1;38;2;180;142;173:*hgrc=0;38;2;163;190;140:*.avif=0;38;2;180;142;173:*.bash=0;38;2;163;190;140:*.braw=0;38;2;180;142;173:*.conf=0;38;2;180;142;173:*.dart=0;38;2;163;190;140:*.data=0;38;2;180;142;173:*.diff=0;38;2;163;190;140:*.docx=0;38;2;180;142;173:*.epub=0;38;2;180;142;173:*.fish=0;38;2;163;190;140:*.flac=0;38;2;180;142;173:*.h264=0;38;2;180;142;173:*.hack=0;38;2;163;190;140:*.heif=0;38;2;180;142;173:*.hgrc=0;38;2;163;190;140:*.html=0;38;2;180;142;173:*.iges=0;38;2;180;142;173:*.info=0;38;2;180;142;173:*.java=0;38;2;163;190;140:*.jpeg=0;38;2;180;142;173:*.json=0;38;2;180;142;173:*.less=0;38;2;163;190;140:*.lisp=0;38;2;163;190;140:*.lock=0;38;2;67;76;94:*.make=0;38;2;163;190;140:*.mojo=0;38;2;163;190;140:*.mpeg=0;38;2;180;142;173:*.nims=0;38;2;163;190;140:*.opus=0;38;2;180;142;173:*.orig=0;38;2;67;76;94:*.pptx=0;38;2;180;142;173:*.prql=0;38;2;163;190;140:*.psd1=0;38;2;163;190;140:*.psm1=0;38;2;163;190;140:*.purs=0;38;2;163;190;140:*.raku=0;38;2;163;190;140:*.rlib=0;38;2;67;76;94:*.sass=0;38;2;163;190;140:*.scad=0;38;2;163;190;140:*.scss=0;38;2;163;190;140:*.step=0;38;2;180;142;173:*.tbz2=1;38;2;129;161;193:*.tiff=0;38;2;180;142;173:*.toml=0;38;2;180;142;173:*.usda=0;38;2;180;142;173:*.usdc=0;38;2;180;142;173:*.usdz=0;38;2;180;142;173:*.webm=0;38;2;180;142;173:*.webp=0;38;2;180;142;173:*.woff=0;38;2;180;142;173:*.xbps=1;38;2;129;161;193:*.xlsx=0;38;2;180;142;173:*.yaml=0;38;2;180;142;173:*stdin=0;38;2;67;76;94:*v.mod=0;38;2;163;190;140:*.blend=0;38;2;180;142;173:*.cabal=0;38;2;163;190;140:*.cache=0;38;2;67;76;94:*.class=0;38;2;67;76;94:*.cmake=0;38;2;163;190;140:*.ctags=0;38;2;67;76;94:*.dylib=0;38;2;208;135;112:*.dyn_o=0;38;2;67;76;94:*.gcode=0;38;2;163;190;140:*.ipynb=0;38;2;163;190;140:*.mdown=0;38;2;180;142;173:*.patch=0;38;2;163;190;140:*.rmeta=0;38;2;67;76;94:*.scala=0;38;2;163;190;140:*.shtml=0;38;2;180;142;173:*.swift=0;38;2;163;190;140:*.toast=1;38;2;129;161;193:*.woff2=0;38;2;180;142;173:*.xhtml=0;38;2;180;142;173:*Icon\r=0;38;2;67;76;94:*LEGACY=0;38;2;180;142;173:*NOTICE=0;38;2;180;142;173:*README=0;38;2;180;142;173:*go.mod=0;38;2;163;190;140:*go.sum=0;38;2;67;76;94:*passwd=0;38;2;180;142;173:*shadow=0;38;2;180;142;173:*stderr=0;38;2;67;76;94:*stdout=0;38;2;67;76;94:*.bashrc=0;38;2;163;190;140:*.config=0;38;2;180;142;173:*.dyn_hi=0;38;2;67;76;94:*.flake8=0;38;2;163;190;140:*.gradle=0;38;2;163;190;140:*.groovy=0;38;2;163;190;140:*.ignore=0;38;2;163;190;140:*.matlab=0;38;2;163;190;140:*.nimble=0;38;2;163;190;140:*COPYING=0;38;2;180;142;173:*INSTALL=0;38;2;180;142;173:*LICENCE=0;38;2;180;142;173:*LICENSE=0;38;2;180;142;173:*TODO.md=1;38;2;180;142;173:*VERSION=0;38;2;180;142;173:*.alembic=0;38;2;180;142;173:*.desktop=0;38;2;180;142;173:*.gemspec=0;38;2;163;190;140:*.mailmap=0;38;2;163;190;140:*Doxyfile=0;38;2;163;190;140:*Makefile=0;38;2;163;190;140:*TODO.txt=1;38;2;180;142;173:*setup.py=0;38;2;163;190;140:*.DS_Store=0;38;2;67;76;94:*.cmake.in=0;38;2;163;190;140:*.fdignore=0;38;2;163;190;140:*.kdevelop=0;38;2;163;190;140:*.markdown=0;38;2;180;142;173:*.rgignore=0;38;2;163;190;140:*.tfignore=0;38;2;163;190;140:*CHANGELOG=0;38;2;180;142;173:*COPYRIGHT=0;38;2;180;142;173:*README.md=0;38;2;180;142;173:*bun.lockb=0;38;2;67;76;94:*configure=0;38;2;163;190;140:*.gitconfig=0;38;2;163;190;140:*.gitignore=0;38;2;163;190;140:*.localized=0;38;2;67;76;94:*.scons_opt=0;38;2;67;76;94:*.timestamp=0;38;2;67;76;94:*CODEOWNERS=0;38;2;163;190;140:*Dockerfile=0;38;2;180;142;173:*INSTALL.md=0;38;2;180;142;173:*README.txt=0;38;2;180;142;173:*SConscript=0;38;2;163;190;140:*SConstruct=0;38;2;163;190;140:*.cirrus.yml=0;38;2;163;190;140:*.gitmodules=0;38;2;163;190;140:*.synctex.gz=0;38;2;67;76;94:*.travis.yml=0;38;2;163;190;140:*INSTALL.txt=0;38;2;180;142;173:*LICENSE-MIT=0;38;2;180;142;173:*MANIFEST.in=0;38;2;163;190;140:*Makefile.am=0;38;2;163;190;140:*Makefile.in=0;38;2;67;76;94:*.applescript=0;38;2;163;190;140:*.fdb_latexmk=0;38;2;67;76;94:*.webmanifest=0;38;2;180;142;173:*CHANGELOG.md=0;38;2;180;142;173:*CONTRIBUTING=0;38;2;180;142;173:*CONTRIBUTORS=0;38;2;180;142;173:*appveyor.yml=0;38;2;163;190;140:*configure.ac=0;38;2;163;190;140:*.bash_profile=0;38;2;163;190;140:*.clang-format=0;38;2;163;190;140:*.editorconfig=0;38;2;163;190;140:*CHANGELOG.txt=0;38;2;180;142;173:*.gitattributes=0;38;2;163;190;140:*.gitlab-ci.yml=0;38;2;163;190;140:*CMakeCache.txt=0;38;2;67;76;94:*CMakeLists.txt=0;38;2;163;190;140:*LICENSE-APACHE=0;38;2;180;142;173:*pyproject.toml=0;38;2;163;190;140:*CODE_OF_CONDUCT=0;38;2;180;142;173:*CONTRIBUTING.md=0;38;2;180;142;173:*CONTRIBUTORS.md=0;38;2;180;142;173:*.sconsign.dblite=0;38;2;67;76;94:*CONTRIBUTING.txt=0;38;2;180;142;173:*CONTRIBUTORS.txt=0;38;2;180;142;173:*requirements.txt=0;38;2;163;190;140:*package-lock.json=0;38;2;67;76;94:*CODE_OF_CONDUCT.md=0;38;2;180;142;173:*.CFUserTextEncoding=0;38;2;67;76;94:*CODE_OF_CONDUCT.txt=0;38;2;180;142;173:*azure-pipelines.yml=0;38;2;163;190;140";
# https://github.com/trapd00r/LS_COLORS
# LS_COLORS using standard ANSI codes to match the terminal theme
# $env.LS_COLORS = {
#     di: "38;5;30"                                    # Directory: Bold Blue
#     ln: "1;36"                                       # Symbolic Link: Bold Cyan
#     ex: "1;32"                                       # Executable: Bold Green
#     fi: "0"                                          # Regular File: Default color
#     pi: "33"                                         # Pipe: Yellow
#     so: "1;35"                                       # Socket: Bold Magenta
#     or: "1;31"                                       # Orphaned Symlink: Bold Red

#     BLK:                   "38;5;68"                   # core
#     CAPABILITY:            "38;5;17"                   # core
#     CHR:                   "38;5;113;1"                # core
#     DIR:                   "38;5;30"                   # core
#     DOOR:                  "38;5;127"                  # core
#     EXEC:                  "38;5;208;1"                # core
#     FIFO:                  "38;5;126"                  # core
#     FILE:                  "0"                         # core
#     LINK:                  "target"                    # core
#     MULTIHARDLINK:         "38;5;222;1"                # core
#     NORMAL:                "0"                         # core
#     ORPHAN:                "48;5;196;38;5;232;1"       # core
#     OTHER_WRITABLE:        "38;5;220;1"                # core
#     SETGID:                "48;5;3;38;5;0"             # core
#     SETUID:                "38;5;220;1;3;100;1"        # core
#     SOCK:                  "38;5;197"                  # core
#     STICKY:                "38;5;86;48;5;234"          # core
#     STICKY_OTHER_WRITABLE: "48;5;235;38;5;139;3"       # core
#     *LS_COLORS:            "48;5;89;38;5;197;1;3;4;7"  # :""-)
#     .txt:                  "38;5;253"                  # Plain-text
#     *README:               "38;5;220;1"                # Documentation
#     *README.rst:           "38;5;220;1"                # Documentation
#     *README.md:            "38;5;220;1"                # Documentation
#     *LICENSE:              "38;5;220;1"                # Documentation
#     *LICENSE.md:           "38;5;220;1"                # Documentation
#     *COPYING:              "38;5;220;1"                # Documentation
#     *INSTALL:              "38;5;220;1"                # Documentation
#     *COPYRIGHT:            "38;5;220;1"                # Documentation
#     *AUTHORS:              "38;5;220;1"                # Documentation
#     *HISTORY:              "38;5;220;1"                # Documentation
#     *CONTRIBUTORS:         "38;5;220;1"                # Documentation
#     *CONTRIBUTING:         "38;5;220;1"                # Documentation
#     *CONTRIBUTING.md:      "38;5;220;1"                # Documentation
#     *CHANGELOG:            "38;5;220;1"                # Documentation
#     *CHANGELOG.md:         "38;5;220;1"                # Documentation
#     *CODEOWNERS:           "38;5;220;1"                # Documentation
#     *PATENTS:              "38;5;220;1"                # Documentation
#     *VERSION:              "38;5;220;1"                # Documentation
#     *NOTICE:               "38;5;220;1"                # Documentation
#     *CHANGES:              "38;5;220;1"                # Documentation
#     .log:                  "38;5;190"                  # Logs
#     .adoc:                 "38;5;184"                  # Markup
#     .asciidoc:             "38;5;184"                  # Markup
#     .etx:                  "38;5;184"                  # Markup
#     .info:                 "38;5;184"                  # Markup
#     .markdown:             "38;5;184"                  # Markup
#     .md:                   "38;5;184"                  # Markup
#     .mkd:                  "38;5;184"                  # Markup
#     .mdx:                  "38;5;184"                  # Markup
#     .nfo:                  "38;5;184"                  # Markup
#     .org:                  "38;5;184"                  # Markup
#     .norg:                 "38;5;184"                  # Markup
#     .pod:                  "38;5;184"                  # Markup
#     .rst:                  "38;5;184"                  # Markup
#     .tex:                  "38;5;184"                  # Markup
#     .textile:              "38;5;184"                  # Markup
#     .bib:                  "38;5;178"                  # Data store                                      # non-relational
#     .json:                 "38;5;178"                  # Data store                                      # non-relational
#     .jsonc:                "38;5;178"                  # Data store                                      # non-relational
#     .json5:                "38;5;178"                  # Data store                                      # non-relational
#     .hjson:                "38;5;178"                  # Data store                                      # non-relational
#     .jsonl:                "38;5;178"                  # Data store                                      # non-relational
#     .jsonnet:              "38;5;178"                  # Data store                                      # non-relational
#     .libsonnet:            "38;5;142"                  # Data store                                      # non-relational
#     .ndjson:               "38;5;178"                  # Data store                                      # non-relational
#     .msg:                  "38;5;178"                  # Data store                                      # non-relational
#     .pgn:                  "38;5;178"                  # Data store                                      # non-relational
#     .rss:                  "38;5;178"                  # Data store                                      # non-relational
#     .xml:                  "38;5;178"                  # Data store                                      # non-relational
#     .fxml:                 "38;5;178"                  # Data store                                      # non-relational
#     .toml:                 "38;5;178"                  # Data store                                      # non-relational
#     .yaml:                 "38;5;178"                  # Data store                                      # non-relational
#     .yml:                  "38;5;178"                  # Data store                                      # non-relational
#     .RData:                "38;5;178"                  # Data store                                      # non-relational
#     .rdata:                "38;5;178"                  # Data store                                      # non-relational
#     .xsd:                  "38;5;178"                  # Data store                                      # non-relational
#     .dtd:                  "38;5;178"                  # Data store                                      # non-relational
#     .sgml:                 "38;5;178"                  # Data store                                      # non-relational
#     .rng:                  "38;5;178"                  # Data store                                      # non-relational
#     .rnc:                  "38;5;178"                  # Data store                                      # non-relational
#     .sexp:                 "38;5;178"                  # Data store                                      # non-relational
#     .accdb:                "38;5;60"                   # Data store                                      # MS Access
#     .accde:                "38;5;60"                   # Data store                                      # MS Access
#     .accdr:                "38;5;60"                   # Data store                                      # MS Access
#     .accdt:                "38;5;60"                   # Data store                                      # MS Access
#     .db:                   "38;5;60"                   # Data store
#     .fmp12:                "38;5;60"                   # Data store
#     .fp7:                  "38;5;60"                   # Data store
#     .localstorage:         "38;5;60"                   # Data store
#     .mdb:                  "38;5;60"                   # Data store
#     .mde:                  "38;5;60"                   # Data store
#     .sqlite:               "38;5;60"                   # Data store                                      # SQLite
#     .typelib:              "38;5;60"                   # Data store
#     .nc:                   "38;5;60"                   # Data store                                      # NetCDF
#     .azw:                  "38;5;141"                  # Documents                                       # binary
#     .azw3:                 "38;5;141"                  # Documents                                       # binary
#     .cbr:                  "38;5;141"                  # Documents                                       # binary
#     .cbz:                  "38;5;141"                  # Documents                                       # binary
#     .chm:                  "38;5;141"                  # Documents                                       # binary
#     .djvu:                 "38;5;141"                  # Documents                                       # binary
#     .fb2:                  "38;5;141"                  # Documents                                       # binary
#     .pdf:                  "38;5;141"                  # Documents                                       # binary
#     .PDF:                  "38;5;141"                  # Documents                                       # binary
#     .mobi:                 "38;5;141"                  # Documents                                       # binary
#     .epub:                 "38;5;141"                  # Documents                                       # binary
#     .docm:                 "38;5;111;4"                # Documents                                       # with macros
#     .doc:                  "38;5;111"                  # Documents
#     .docx:                 "38;5;111"                  # Documents
#     .odb:                  "38;5;111"                  # Documents
#     .odt:                  "38;5;111"                  # Documents
#     .rtf:                  "38;5;111"                  # Documents
#     .pages:                "38;5;111"                  # Documents
#     .odp:                  "38;5;166"                  # Presentation
#     .pps:                  "38;5;166"                  # Presentation
#     .ppt:                  "38;5;166"                  # Presentation
#     .pptx:                 "38;5;166"                  # Presentation
#     .ppts:                 "38;5;166"                  # Presentation
#     .pptxm:                "38;5;166;4"                # Presentation (with macros)
#     .pptsm:                "38;5;166;4"                # Presentation (with macros)
#     .prisma:               "38;5;222"                  # Prisma Schema/Config
#     .csv:                  "38;5;78"                   # Spreadsheet (plain text)
#     .tsv:                  "38;5;78"                   # Spreadsheet (plain text)
#     .numbers:              "38;5;112"                  # Spreadsheet
#     .ods:                  "38;5;112"                  # Spreadsheet
#     .xla:                  "38;5;76"                   # Spreadsheet
#     .xls:                  "38;5;112"                  # Spreadsheet
#     .xlsx:                 "38;5;112"                  # Spreadsheet
#     .xlsxm:                "38;5;112;4"                # Spreadsheet (with macros)
#     .xltm:                 "38;5;73;4"                 # Spreadsheet (with macros)
#     .xltx:                 "38;5;73"                   # Spreadsheet (with macros)
#     .key:                  "38;5;166"                  # License keys
#     *config:               "1"                         # Configs
#     *cfg:                  "1"                         # Configs
#     *conf:                 "1"                         # Configs
#     *rc:                   "1"                         # Configs
#     *authorized_keys:      "1"                         # Configs
#     *known_hosts:          "1"                         # Configs
#     .ini:                  "1"                         # Configs
#     .plist:                "1"                         # Configs
#     .profile:              "1"                         # Configs
#     .bash_profile:         "1"                         # Configs
#     .bash_login:           "1"                         # Configs
#     .bash_logout:          "1"                         # Configs
#     .zshenv:               "1"                         # Configs
#     .zprofile:             "1"                         # Configs
#     .zlogin:               "1"                         # Configs
#     .zlogout:              "1"                         # Configs
#     .viminfo:              "1"                         # Configs
#     .pcf:                  "1"                         # Configs
#     .psf:                  "1"                         # Configs
#     .hidden-color-scheme:  "1"                         # Configs
#     .hidden-tmTheme:       "1"                         # Configs
#     .last-run:             "1"                         # Configs
#     .merged-ca-bundle:     "1"                         # Configs
#     .sublime-build:        "1"                         # Configs
#     .sublime-commands:     "1"                         # Configs
#     .sublime-keymap:       "1"                         # Configs
#     .sublime-settings:     "1"                         # Configs
#     .sublime-snippet:      "1"                         # Configs
#     .sublime-project:      "1"                         # Configs
#     .sublime-workspace:    "1"                         # Configs
#     .tmTheme:              "1"                         # Configs
#     .user-ca-bundle:       "1"                         # Configs
#     .rstheme:              "1"                         # Configs
#     .epf:                  "1"                         # Configs
#     .git:                  "38;5;197"                  # Version control
#     .github:               "38;5;197"                  # Version control
#     .gitignore:            "38;5;240"                  # Version control
#     .gitattributes:        "38;5;240"                  # Version control
#     .gitmodules:           "38;5;240"                  # Version control
#     .awk:                  "38;5;172"                  # Code (shell)
#     .bash:                 "38;5;172"                  # Code (shell)
#     .bat:                  "38;5;172"                  # Code (shell)
#     .BAT:                  "38;5;172"                  # Code (shell)
#     .sed:                  "38;5;172"                  # Code (shell)
#     .sh:                   "38;5;172"                  # Code (shell)
#     .zsh:                  "38;5;172"                  # Code (shell)
#     .fish:                 "38;5;172"                  # Code (shell)
#     .vim:                  "38;5;172"                  # Code (shell)
#     .kak:                  "38;5;172"                  # Code (shell)
#     .nu:                   "38;5;172"                  # Code (shell)
#     .ahk:                  "38;5;41"                   # Code (interpreted) (AutoHotKey)
#     .py:                   "38;5;41"                   # Code (interpreted) (Python)
#     .ipynb:                "38;5;41"                   # Code (interpreted) (Python)
#     .xsh:                  "38;5;41"                   # Code (interpreted) (xonsh)
#     .rb:                   "38;5;41"                   # Code (interpreted) (ruby)
#     .gemspec:              "38;5;41"                   # Code (interpreted) (ruby)
#     .pl:                   "38;5;208"                  # Code (interpreted) (Perl)
#     .PL:                   "38;5;160"                  # Code (interpreted) (Perl)
#     .pm:                   "38;5;203"                  # Code (interpreted) (Perl)
#     .t:                    "38;5;114"                  # Code (interpreted) (Perl)
#     .msql:                 "38;5;222"                  # Code (SQL)
#     .mysql:                "38;5;222"                  # Code (SQL)
#     .prql:                 "38;5;222"                  # Code (SQL)
#     .pgsql:                "38;5;222"                  # Code (SQL)
#     .sql:                  "38;5;222"                  # Code (SQL)
#     .tcl:                  "38;5;64;1"                 # Code (interpreted) (Tool Command Language)
#     .r:                    "38;5;49"                   # Code (interpreted) (R)
#     .R:                    "38;5;49"                   # Code (interpreted) (R)
#     .gs:                   "38;5;81"                   # GrADS
#     .clj:                  "38;5;41"                   # Clojure
#     .cljs:                 "38;5;41"                   # Clojure
#     .cljc:                 "38;5;41"                   # Clojure
#     .cljw:                 "38;5;41"                   # Clojure Gorilla notebook
#     .scala:                "38;5;41"                   # Scala
#     .sc:                   "38;5;41"                   # Scala
#     .dart:                 "38;5;51"                   # Dart
#     .asm:                  "38;5;81"                   # ASM
#     .cl:                   "38;5;81"                   # LISP
#     .lisp:                 "38;5;81"                   # LISP
#     .rkt:                  "38;5;81"                   # LISP
#     .el:                   "38;5;81"                   # LISP (Emacs)
#     .elc:                  "38;5;241"                  # LISP (Emacs)
#     .eln:                  "38;5;241"                  # LISP (Emacs)
#     .lua:                  "38;5;81"                   # Code (interpreted) (Lua)
#     .moon:                 "38;5;81"                   # Code (interpreted) (Moonscript)
#     .c:                    "38;5;81"                   # C
#     .C:                    "38;5;81"                   # C
#     .h:                    "38;5;110"                  # C headers
#     .H:                    "38;5;110"                  # C headers
#     .tcc:                  "38;5;110"                  # C headers
#     .c++:                  "38;5;81"                   # C++
#     .h++:                  "38;5;110"                  # C++ headers
#     .hpp:                  "38;5;110"                  # C++ headers
#     .hxx:                  "38;5;110"                  # C++ headers
#     .ii:                   "38;5;110"                  # C++ headers
#     .M:                    "38;5;110"                  # Objective C method file
#     .m:                    "38;5;110"                  # Objective C method file
#     .cc:                   "38;5;81"                   # Csharp
#     .cs:                   "38;5;81"                   # Csharp
#     .cp:                   "38;5;81"                   # Csharp
#     .cpp:                  "38;5;81"                   # Csharp
#     .cxx:                  "38;5;81"                   # Csharp
#     .cr:                   "38;5;81"                   # Crystal
#     .go:                   "38;5;81"                   # Golang
#     .f:                    "38;5;81"                   # Fortran
#     .F:                    "38;5;81"                   # Fortran
#     .for:                  "38;5;81"                   # Fortran
#     .ftn:                  "38;5;81"                   # Fortran
#     .f90:                  "38;5;81"                   # Fortran
#     .F90:                  "38;5;81"                   # Fortran
#     .f95:                  "38;5;81"                   # Fortran
#     .F95:                  "38;5;81"                   # Fortran
#     .f03:                  "38;5;81"                   # Fortran
#     .F03:                  "38;5;81"                   # Fortran
#     .f08:                  "38;5;81"                   # Fortran
#     .F08:                  "38;5;81"                   # Fortran
#     .nim:                  "38;5;81"                   # Nim
#     .nimble:               "38;5;81"                   # Nim
#     .s:                    "38;5;110"                  # Pascal
#     .S:                    "38;5;110"                  # Pascal
#     .rs:                   "38;5;81"                   # Code (compiled) (Rust)
#     .scpt:                 "38;5;219"                  # AppleScript
#     .swift:                "38;5;219"                  # Swift
#     .sx:                   "38;5;81"                   # SimplexNumerica
#     .vala:                 "38;5;81"                   # Vala
#     .vapi:                 "38;5;81"                   # Vala
#     .hi:                   "38;5;110"                  # Haskell
#     .hs:                   "38;5;81"                   # Haskell
#     .lhs:                  "38;5;81"                   # Haskell
#     .ml:                   "38;5;81"                   # OCaml
#     .mli:                  "38;5;110"                  # OCaml
#     .mll:                  "38;5;81"                   # OCaml
#     .mly:                  "38;5;81"                   # OCaml
#     .agda:                 "38;5;81"                   # Agda
#     .lagda:                "38;5;81"                   # Agda
#     .lagda.tex:            "38;5;81"                   # Agda
#     .lagda.rst:            "38;5;81"                   # Agda
#     .lagda.md:             "38;5;81"                   # Agda
#     .agdai:                "38;5;110"                  # Agda
#     .zig:                  "38;5;81"                   # Zig
#     .v:                    "38;5;81"                   # V
#     .rego:                 "38;5;178"                  # Code (Rego)
#     .pyc:                  "38;5;240"                  # Code (Python)
#     .tf:                   "38;5;168"                  # Code (Terraform)
#     .tfstate:              "38;5;168"                  # Code (Terraform)
#     .tfvars:               "38;5;168"                  # Code (Terraform)
#     .http:                 "38;5;90;1"                 # Request (Web)
#     .eml:                  "38;5;90;1"                 # Email (Web)
#     .css:                  "38;5;105;1"                # Styling (Web)
#     .less:                 "38;5;105;1"                # Styling (Web)
#     .sass:                 "38;5;105;1"                # Styling (Web)
#     .scss:                 "38;5;105;1"                # Styling (Web)
#     .htm:                  "38;5;125;1"                # Styling (Web)
#     .html:                 "38;5;125;1"                # Markup (Web)
#     .jhtm:                 "38;5;125;1"                # Markup (Web)
#     .mht:                  "38;5;125;1"                # Markup (Web)
#     .mustache:             "38;5;135;1"                # Templating (Web)
#     .ejs:                  "38;5;135;1"                # Templating (Web)
#     .pug:                  "38;5;135;1"                # Templating (Web)
#     .svelte:               "38;5;135;1"                # Templating (Web)
#     .vue:                  "38;5;135;1"                # Templating (Web)
#     .astro:                "38;5;135;1"                # Templating (Web)
#     .js:                   "38;5;074;1"                # Javascript
#     .jsx:                  "38;5;074;1"                # Javascript eXtended
#     .ts:                   "38;5;074;1"                # Typescript
#     .tsx:                  "38;5;074;1"                # Typescript eXtended
#     .mjs:                  "38;5;074;1"                # ECMAScript
#     .cjs:                  "38;5;074;1"                # CommonJS
#     .coffee:               "38;5;079;1"                # Java
#     .java:                 "38;5;079;1"                # Java
#     .jsm:                  "38;5;079;1"                # Java
#     .jsp:                  "38;5;079;1"                # Java
#     .php:                  "38;5;81"                   # php
#     .ctp:                  "38;5;81"                   # php (CakePHP)
#     .twig:                 "38;5;81"                   # php (Twig)
#     .vb:                   "38;5;81"                   # VBA
#     .vba:                  "38;5;81"                   # VBA
#     .vbs:                  "38;5;81"                   # VBA
#     *Containerfile:        "38;5;155"                  # Build process                                   # Containers
#     .containerignore:      "38;5;240"                  # Build process                                   # Containers
#     *Dockerfile:           "38;5;155"                  # Build process (Docker)
#     .dockerignore:         "38;5;240"                  # Build process (Docker)
#     *dune:                 "38;5;155"                  # Build process (OCaml Dune)
#     *dune-project:         "38;5;243"                  # Build process (OCaml Dune)
#     .opam:                 "38;5;240"                  # Build process (OCaml Opam)
#     *Makefile:             "38;5;155"                  # Build process (Make)
#     *MANIFEST:             "38;5;243"                  # Build process (Make)
#     *pm_to_blib:           "38;5;240"                  # Build process (Perl)
#     .nix:                  "38;5;155"                  # Functional configuration
#     .dhall:                "38;5;178"                  # Functional configuration
#     .rake:                 "38;5;155"                  # Build process (Ruby)
#     .am:                   "38;5;242"                  # Build process (Automake)
#     .in:                   "38;5;242"                  # Build process (Automake)
#     .hin:                  "38;5;242"                  # Build process (Automake)
#     .scan:                 "38;5;242"                  # Build process (Automake)
#     .m4:                   "38;5;242"                  # Build process (Automake)
#     .old:                  "38;5;242"                  # Build process (Automake)
#     .out:                  "38;5;242"                  # Build process (Automake)
#     .SKIP:                 "38;5;244"                  # Build process (Automake)
#     .gradle:               "38;5;155"                  # Build process (Gradle)
#     *WORKSPACE:            "38;5;155"                  # Build process (Bazel)
#     *BUILD:                "38;5;155"                  # Build process (Bazel)
#     .bazel:                "38;5;155"                  # Build process (Bazel)
#     .bazelrc:              "38;5;155"                  # Build process (Bazel)
#     .bazelversion:         "38;5;155"                  # Build process (Bazel)
#     .bzl:                  "38;5;155"                  # Build process (Bazel)
#     *MODULE.bazel.lock:    "38;5;240"                  # Build process (Bazel)
#     *Cargo.toml:           "38;5;155"                  # Build process (Cargo)
#     *Cargo.lock:           "38;5;240"                  # Build process (Cargo)
#     .diff:                 "48;5;197;38;5;232"         # Patch files
#     .patch:                "48;5;197;38;5;232;1"       # Patch files
#     .bmp:                  "38;5;97"                   # Graphics
#     .dicom:                "38;5;97"                   # Graphics
#     .tiff:                 "38;5;97"                   # Graphics
#     .tif:                  "38;5;97"                   # Graphics
#     .TIFF:                 "38;5;97"                   # Graphics
#     .cdr:                  "38;5;97"                   # Graphics
#     .flif:                 "38;5;97"                   # Graphics
#     .gif:                  "38;5;97"                   # Graphics
#     .icns:                 "38;5;97"                   # Graphics
#     .ico:                  "38;5;97"                   # Graphics
#     .jpeg:                 "38;5;97"                   # Graphics
#     .JPG:                  "38;5;97"                   # Graphics
#     .jpg:                  "38;5;97"                   # Graphics
#     .jxl:                  "38;5;97"                   # Graphics
#     .nth:                  "38;5;97"                   # Graphics
#     .png:                  "38;5;97"                   # Graphics
#     .psd:                  "38;5;97"                   # Graphics
#     .pxd:                  "38;5;97"                   # Graphics
#     .pxm:                  "38;5;97"                   # Graphics
#     .xpm:                  "38;5;97"                   # Graphics
#     .webp:                 "38;5;97"                   # Graphics
#     .ai:                   "38;5;99"                   # Graphics (Vector)
#     .eps:                  "38;5;99"                   # Graphics (Vector)
#     .epsf:                 "38;5;99"                   # Graphics (Vector)
#     .drw:                  "38;5;99"                   # Graphics (Vector)
#     .ps:                   "38;5;99"                   # Graphics (Vector)
#     .svg:                  "38;5;99"                   # Graphics (Vector)
#     .avi:                  "38;5;114"                  # Video
#     .divx:                 "38;5;114"                  # Video
#     .IFO:                  "38;5;114"                  # Video
#     .m2v:                  "38;5;114"                  # Video
#     .m4v:                  "38;5;114"                  # Video
#     .mkv:                  "38;5;114"                  # Video
#     .MOV:                  "38;5;114"                  # Video
#     .mov:                  "38;5;114"                  # Video
#     .mp4:                  "38;5;114"                  # Video
#     .mpeg:                 "38;5;114"                  # Video
#     .mpg:                  "38;5;114"                  # Video
#     .ogm:                  "38;5;114"                  # Video
#     .rmvb:                 "38;5;114"                  # Video
#     .sample:               "38;5;114"                  # Video
#     .wmv:                  "38;5;114"                  # Video
#     .3g2:                  "38;5;115"                  # Video (mobile/streaming)
#     .3gp:                  "38;5;115"                  # Video (mobile/streaming)
#     .gp3:                  "38;5;115"                  # Video (mobile/streaming)
#     .webm:                 "38;5;115"                  # Video (mobile/streaming)
#     .gp4:                  "38;5;115"                  # Video (mobile/streaming)
#     .asf:                  "38;5;115"                  # Video (mobile/streaming)
#     .flv:                  "38;5;115"                  # Video (mobile/streaming)
#     .ogv:                  "38;5;115"                  # Video (mobile/streaming)
#     .f4v:                  "38;5;115"                  # Video (mobile/streaming)
#     .VOB:                  "38;5;115;1"                # Video (lossless)
#     .vob:                  "38;5;115;1"                # Video (lossless)
#     .ass:                  "38;5;117"                  # Subtitles
#     .srt:                  "38;5;117"                  # Subtitles
#     .ssa:                  "38;5;117"                  # Subtitles
#     .sub:                  "38;5;117"                  # Subtitles
#     .sup:                  "38;5;117"                  # Subtitles
#     .vtt:                  "38;5;117"                  # Subtitles
#     .3ga:                  "38;5;137;1"                # Audio (lossy)
#     .S3M:                  "38;5;137;1"                # Audio (lossy)
#     .aac:                  "38;5;137;1"                # Audio (lossy)
#     .amr:                  "38;5;137;1"                # Audio (lossy)
#     .au:                   "38;5;137;1"                # Audio (lossy)
#     .caf:                  "38;5;137;1"                # Audio (lossy)
#     .dat:                  "38;5;137;1"                # Audio (lossy)
#     .dts:                  "38;5;137;1"                # Audio (lossy)
#     .fcm:                  "38;5;137;1"                # Audio (lossy)
#     .m4a:                  "38;5;137;1"                # Audio (lossy)
#     .mod:                  "38;5;137;1"                # Audio (lossy)
#     .mp3:                  "38;5;137;1"                # Audio (lossy)
#     .mp4a:                 "38;5;137;1"                # Audio (lossy)
#     .oga:                  "38;5;137;1"                # Audio (lossy)
#     .ogg:                  "38;5;137;1"                # Audio (lossy)
#     .opus:                 "38;5;137;1"                # Audio (lossy)
#     .s3m:                  "38;5;137;1"                # Audio (lossy)
#     .sid:                  "38;5;137;1"                # Audio (lossy)
#     .wma:                  "38;5;137;1"                # Audio (lossy)
#     .ape:                  "38;5;136;1"                # Audio (lossless)
#     .aiff:                 "38;5;136;1"                # Audio (lossless)
#     .cda:                  "38;5;136;1"                # Audio (lossless)
#     .flac:                 "38;5;136;1"                # Audio (lossless)
#     .alac:                 "38;5;136;1"                # Audio (lossless)
#     .mid:                  "38;5;136;1"                # Audio (lossless)
#     .midi:                 "38;5;136;1"                # Audio (lossless)
#     .pcm:                  "38;5;136;1"                # Audio (lossless)
#     .wav:                  "38;5;136;1"                # Audio (lossless)
#     .wv:                   "38;5;136;1"                # Audio (lossless)
#     .wvc:                  "38;5;136;1"                # Audio (lossless)
#     .afm:                  "38;5;66"                   # Fonts
#     .fon:                  "38;5;66"                   # Fonts
#     .fnt:                  "38;5;66"                   # Fonts
#     .pfb:                  "38;5;66"                   # Fonts
#     .pfm:                  "38;5;66"                   # Fonts
#     .ttf:                  "38;5;66"                   # Fonts
#     .otf:                  "38;5;66"                   # Fonts
#     .woff:                 "38;5;66"                   # Fonts
#     .woff2:                "38;5;66"                   # Fonts
#     .PFA:                  "38;5;66"                   # Fonts
#     .pfa:                  "38;5;66"                   # Fonts
#     *.7z:                  "38;5;40"                   # Archives
#     *.a:                   "38;5;40"                   # Archives
#     *.arj:                 "38;5;40"                   # Archives
#     *.br:                  "38;5;40"                   # Archives
#     *.bz2:                 "38;5;40"                   # Archives
#     *.cpio:                "38;5;40"                   # Archives
#     *.gz:                  "38;5;40"                   # Archives
#     *.lrz:                 "38;5;40"                   # Archives
#     *.lz:                  "38;5;40"                   # Archives
#     *.lzma:                "38;5;40"                   # Archives
#     *.lzo:                 "38;5;40"                   # Archives
#     *.rar:                 "38;5;40"                   # Archives
#     *.s7z:                 "38;5;40"                   # Archives
#     *.sz:                  "38;5;40"                   # Archives
#     *.tar:                 "38;5;40"                   # Archives
#     *.tbz:                 "38;5;40"                   # Archives
#     *.tgz:                 "38;5;40"                   # Archives
#     *.warc:                "38;5;40"                   # Archives
#     *.WARC:                "38;5;40"                   # Archives
#     *.xz:                  "38;5;40"                   # Archives
#     *.z:                   "38;5;40"                   # Archives
#     *.zip:                 "38;5;40"                   # Archives
#     *.zipx:                "38;5;40"                   # Archives
#     *.zoo:                 "38;5;40"                   # Archives
#     *.zpaq:                "38;5;40"                   # Archives
#     *.zst:                 "38;5;40"                   # Archives
#     *.zstd:                "38;5;40"                   # Archives
#     *.zz:                  "38;5;40"                   # Archives
#     .apk:                  "38;5;215"                  # Packaged apps
#     .ipa:                  "38;5;215"                  # Packaged apps
#     .deb:                  "38;5;215"                  # Packaged apps
#     .rpm:                  "38;5;215"                  # Packaged apps
#     .jad:                  "38;5;215"                  # Packaged apps
#     .jar:                  "38;5;215"                  # Packaged apps
#     .ear:                  "38;5;215"                  # Packaged apps
#     .war:                  "38;5;215"                  # Packaged apps
#     .cab:                  "38;5;215"                  # Packaged apps
#     .pak:                  "38;5;215"                  # Packaged apps
#     .pk3:                  "38;5;215"                  # Packaged apps
#     .vdf:                  "38;5;215"                  # Packaged apps
#     .vpk:                  "38;5;215"                  # Packaged apps
#     .bsp:                  "38;5;215"                  # Packaged apps
#     .dmg:                  "38;5;215"                  # Packaged apps
#     .crx:                  "38;5;215"                  # Packaged apps                                   # Google Chrome extension
#     .xpi:                  "38;5;215"                  # Packaged apps                                   # Mozilla Firefox extension
#     .iso:                  "38;5;124"                  # Partition images
#     .img:                  "38;5;124"                  # Partition images
#     .bin:                  "38;5;124"                  # Partition images
#     .nrg:                  "38;5;124"                  # Partition images
#     .qcow:                 "38;5;124"                  # Partition images
#     .fvd:                  "38;5;124"                  # Partition images
#     .sparseimage:          "38;5;124"                  # Partition images
#     .toast:                "38;5;124"                  # Partition images
#     .vcd:                  "38;5;124"                  # Partition images
#     .vdi:                  "38;5;124"                  # Partition images
#     .vhd:                  "38;5;124"                  # Partition images
#     .vhdx:                 "38;5;124"                  # Partition images
#     .vfd:                  "38;5;124"                  # Partition images
#     .vmdk:                 "38;5;124"                  # Partition images
#     .swp:                  "38;5;244"                  # Temporary files
#     .swo:                  "38;5;244"                  # Temporary files
#     .tmp:                  "38;5;244"                  # Temporary files
#     .sassc:                "38;5;244"                  # Temporary files
#     .pacnew:               "38;5;33"                   # Undo files
#     .un~:                  "38;5;241"                  # Undo files
#     .orig:                 "38;5;241"                  # Undo files
#     .BUP:                  "38;5;241"                  # Backups
#     .bak:                  "38;5;241"                  # Backups
#     .o:                    "38;5;241"                  # *nix Object file (shared libraries, core dumps etc)
#     *core:                 "38;5;241"                  # Linux user core dump file (from /proc/sys/kernel/core_pattern)
#     .mdump:                "38;5;241"                  # Mini DuMP crash report
#     .rlib:                 "38;5;241"                  # Static rust library
#     .dll:                  "38;5;241"                  # dynamic linked library
#     .aria2:                "38;5;241"                  # state dumps
#     .dump:                 "38;5;241"                  # state dumps
#     .stackdump:            "38;5;241"                  # state dumps
#     .zcompdump:            "38;5;241"                  # state dumps
#     .zwc:                  "38;5;241"                  # state dumps
#     .part:                 "38;5;239"                  # Partial files
#     .r[0-9]{0,2}:          "38;5;239"                  # Archive segments
#     .zx[0-9]{0,2}:         "38;5;239"                  # Archive segments
#     .z[0-9]{0,2}:          "38;5;239"                  # Archive segments
#     .pid:                  "38;5;248"                  # state files
#     .state:                "38;5;248"                  # state files
#     *lockfile:             "38;5;248"                  # state files
#     *lock:                 "38;5;248"                  # state files
#     .err:                  "38;5;160;1"                # error logs
#     .error:                "38;5;160;1"                # error logs
#     .stderr:               "38;5;160;1"                # error logs
#     .pcap:                 "38;5;29"                   # tcpdump / network traffic capture
#     .cap:                  "38;5;29"                   # tcpdump / network traffic capture
#     .dmp:                  "38;5;29"                   # tcpdump / network traffic capture
#     .allow:                "38;5;112"                  # /etc/hosts
#     .deny:                 "38;5;196"                  # /etc/hosts
#     .service:              "38;5;45"                   # systemd
#     *@.service:            "38;5;45"                   # systemd
#     .socket:               "38;5;45"                   # systemd
#     .swap:                 "38;5;45"                   # systemd
#     .device:               "38;5;45"                   # systemd
#     .mount:                "38;5;45"                   # systemd
#     .automount:            "38;5;45"                   # systemd
#     .target:               "38;5;45"                   # systemd
#     .path:                 "38;5;45"                   # systemd
#     .timer:                "38;5;45"                   # systemd
#     .snapshot:             "38;5;45"                   # systemd
#     .lnk:                  "38;5;39"                   # windows symlink
#     .application:          "38;5;116"                  # metadata
#     .cue:                  "38;5;116"                  # metadata
#     .description:          "38;5;116"                  # metadata
#     .directory:            "38;5;116"                  # metadata
#     .m3u:                  "38;5;116"                  # metadata
#     .m3u8:                 "38;5;116"                  # metadata
#     .md5:                  "38;5;116"                  # metadata
#     .properties:           "38;5;116"                  # metadata
#     .sfv:                  "38;5;116"                  # metadata
#     .theme:                "38;5;116"                  # metadata
#     .torrent:              "38;5;116"                  # metadata
#     .urlview:              "38;5;116"                  # metadata
#     .webloc:               "38;5;116"                  # metadata
#     .asc:                  "38;5;192;3"                # Encrypted data
#     .bfe:                  "38;5;192;3"                # Encrypted data
#     .enc:                  "38;5;192;3"                # Encrypted data
#     .gpg:                  "38;5;192;3"                # Encrypted data
#     .signature:            "38;5;192;3"                # Encrypted data
#     .sig:                  "38;5;192;3"                # Encrypted data
#     .p12:                  "38;5;192;3"                # Encrypted data
#     .pem:                  "38;5;192;3"                # Encrypted data
#     .pgp:                  "38;5;192;3"                # Encrypted data
#     .p7s:                  "38;5;192;3"                # Encrypted data
#     *id_dsa:               "38;5;192;3"                # Encrypted data
#     *id_rsa:               "38;5;192;3"                # Encrypted data
#     *id_ecdsa:             "38;5;192;3"                # Encrypted data
#     *id_ed25519:           "38;5;192;3"                # Encrypted data
#     .32x:                  "38;5;213"                  # Game console emulation
#     .cdi:                  "38;5;213"                  # Game console emulation
#     .fm2:                  "38;5;213"                  # Game console emulation
#     .rom:                  "38;5;213"                  # Game console emulation
#     .sav:                  "38;5;213"                  # Game console emulation
#     .st:                   "38;5;213"                  # Game console emulation
#     .a00:                  "38;5;213"                  # Game console emulation                          # Atari
#     .a52:                  "38;5;213"                  # Game console emulation                          # Atari
#     .A64:                  "38;5;213"                  # Game console emulation                          # Atari
#     .a64:                  "38;5;213"                  # Game console emulation                          # Atari
#     .a78:                  "38;5;213"                  # Game console emulation                          # Atari
#     .adf:                  "38;5;213"                  # Game console emulation                          # Atari
#     .atr:                  "38;5;213"                  # Game console emulation                          # Atari
#     .gb:                   "38;5;213"                  # Game console emulation                          # Nintendo
#     .gba:                  "38;5;213"                  # Game console emulation                          # Nintendo
#     .gbc:                  "38;5;213"                  # Game console emulation                          # Nintendo
#     .gel:                  "38;5;213"                  # Game console emulation                          # Nintendo
#     .gg:                   "38;5;213"                  # Game console emulation                          # Nintendo
#     .ggl:                  "38;5;213"                  # Game console emulation                          # Nintendo
#     .ipk:                  "38;5;213"                  # Game console emulation                          # Nintendo DS Packed Images
#     .j64:                  "38;5;213"                  # Game console emulation                          # Nintendo
#     .nds:                  "38;5;213"                  # Game console emulation                          # Nintendo
#     .nes:                  "38;5;213"                  # Game console emulation                          # Nintendo NES
#     .sms:                  "38;5;213"                  # Sega
#     .8xp:                  "38;5;121"                  # Texas Instruments Calculator files
#     .8eu:                  "38;5;121"                  # Texas Instruments Calculator files
#     .82p:                  "38;5;121"                  # Texas Instruments Calculator files
#     .83p:                  "38;5;121"                  # Texas Instruments Calculator files
#     .8xe:                  "38;5;121"                  # Texas Instruments Calculator files
#     .stl:                  "38;5;216"                  # 3D printing
#     .dwg:                  "38;5;216"                  # 3D printing
#     .ply:                  "38;5;216"                  # 3D printing
#     .wrl:                  "38;5;216"                  # 3D printing
#     .vert:                 "38;5;136"                  # SPIR-V shaders
#     .comp:                 "38;5;136"                  # SPIR-V shaders
#     .frag:                 "38;5;136"                  # SPIR-V shaders
#     .spv:                  "38;5;217"                  # SPIR-V compiled shaders
#     .wgsl:                 "38;5;97"                   # WGSL shaders
#     .xib:                  "38;5;208"
#     .iml:                  "38;5;166"                  # AppCode files
#     .DS_Store:             "38;5;239"                  # macOS
#     .localized:            "38;5;239"                  # macOS
#     .CFUserTextEncoding:   "38;5;239"                  # macOS
#     *CodeResources:        "38;5;239"                  # macOS code signing apps
#     *PkgInfo:              "38;5;239"                  # macOS app bundle id
#     .nib:                  "38;5;57"                   # macOS UI
#     .car:                  "38;5;57"                   # macOS asset catalog
#     .dylib:                "38;5;241"                  # macOS shared lib
#     .entitlements:         "1"                         # Xcode files
#     .pbxproj:              "1"                         # Xcode files
#     .strings:              "1"                         # Xcode files
#     .storyboard:           "38;5;196"                  # Xcode files
#     .xcconfig:             "1"                         # Xcode files
#     .xcsettings:           "1"                         # Xcode files
#     .xcuserstate:          "1"                         # Xcode files
#     .xcworkspacedata:      "1"                         # Xcode files
#     .pot:                  "38;5;7"
#     .pcb:                  "38;5;7"
#     .mm:                   "38;5;7"
#     .gbr:                  "38;5;7"
#     .scm:                  "38;5;7"
#     .xcf:                  "38;5;7"
#     .spl:                  "38;5;7"
#     .Rproj:                "38;5;11"
#     .sis:                  "38;5;7"
#     .1p:                   "38;5;7"
#     .3p:                   "38;5;7"
#     .cnc:                  "38;5;7"
#     .def:                  "38;5;7"
#     .ex:                   "38;5;7"
#     .example:              "38;5;7"
#     .feature:              "38;5;7"
#     .ger:                  "38;5;7"
#     .ics:                  "38;5;7"                    # calendar information
#     .map:                  "38;5;7"
#     .mf:                   "38;5;7"
#     .mfasl:                "38;5;7"
#     .mi:                   "38;5;7"
#     .mtx:                  "38;5;7"
#     .pc:                   "38;5;7"
#     .pi:                   "38;5;7"
#     .plt:                  "38;5;7"
#     .rdf:                  "38;5;7"
#     .ru:                   "38;5;7"
#     .sch:                  "38;5;7"
#     .sty:                  "38;5;7"
#     .sug:                  "38;5;7"
#     .tdy:                  "38;5;7"
#     .tfm:                  "38;5;7"
#     .tfnt:                 "38;5;7"
#     .tg:                   "38;5;7"
#     .vcard:                "38;5;7"                    # contact information
#     .vcf:                  "38;5;7"                    # contact information
#     .xln:                  "38;5;7"
# } | to nuon
