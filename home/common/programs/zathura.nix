{ config, pkgs, ... }:
{
  xdg.mimeApps.defaultApplications = {
    "application/pdf" = [ "org.pwmt.zathura.desktop" ];
  };
  home.file = {
    ".local/share/applications/org.pwmt.zathura.desktop".source =
      pkgs.writeText /org.pwmt.zathura.desktop
        # ini
        ''
          [Desktop Entry]
          Version=1.0
          Type=Application
          Name[ar]=Zathura
          Name[ca]=Zathura
          Name[cs]=Zathura
          Name[de]=Zathura
          Name[el]=Zathura
          Name[eo]=Zathura
          Name[es]=Zathura
          Name[es_CL]=Zathura
          Name[fr]=Zathura
          Name[he]=Zathura
          Name[id_ID]=Zathura
          Name[it]=Zathura
          Name[lt]=Zathura
          Name[nl]=Zathura
          Name[no]=Zathura
          Name[pl]=Zathura
          Name[pt_BR]=Zathura
          Name[ru]=Zathura
          Name[sv]=Zathura
          Name[ta_IN]=Zathura
          Name[tr]=Zathura
          Name[uk_UA]=Zathura
          Name=Zathura
          Comment[ar]=عارض وثائق الحد الأدنى
          Comment[ca]=Un visualitzador de documents minimalista
          Comment[cs]=Jednoduchý prohlížeč dokumentů
          Comment[de]=Ein minimalistischer Dokumenten-Betrachter
          Comment[el]=Ένας ελαφρύς προβολέας κειμένων
          Comment[eo]=Malpeza dokumento spektanto
          Comment[es]=Un visor de documentos minimalista
          Comment[es_CL]=Un visor de documentos minimalista
          Comment[fr]=Un visionneur de document minimaliste
          Comment[he]=מציג מסמכים מינימליסטי
          Comment[id_ID]=Pembaca dokumen minimalis
          Comment[it]=Un visualizzatore di documenti minimalista
          Comment[lt]=Paprasta dokumentų skaitytuvė
          Comment[nl]=Een minimalistisch documentweergaveprogramma
          Comment[no]=En minimalistisk dokumentleser
          Comment[pl]=Minimalistyczny czytnik dokumentów
          Comment[pt_BR]=Um visualizador de documentos minimalista
          Comment[ru]=Минималистичный просмотрщик документов
          Comment[sv]=En minimalistisk dokumentvisare
          Comment[tr]=Minimalist bir belge görüntüleyicisi
          Comment[uk_UA]=Простий переглядач документів
          Comment=A minimalistic document viewer
          Exec=zathura %U
          Icon=/home/${config.home.username}/Flake/home/common/icons/zathura.png
          Terminal=false
          Categories=Office;Viewer;
          Keywords[cs]=PDF;PS;PostScript;DjVU;dokument;představení;prohlížeč;
          Keywords[de]=PDF;Ps;PostScript;DjVU;Dokumente;Presentation;Betrachter;
          Keywords[es]=PDF;PS;PostScript;DjVU;documento;presentación;visor;
          Keywords[fr]=PDF;PS;PostScript;DjVU;document;présentation;visionneur;
          Keywords[it]=PDF;PS;PostScript;DjVU;documenti;presentazioni;lettore;
          Keywords[nl]=PDF;PS;PostScript;DjVU;document;presentatie;weergave;
          Keywords[pl]=PDF;PS;PostScript;DjVU;dokument;czytnik;
          Keywords[ru]=PDF,PS,PostScript,DjVU,документ,презентация,просмотрщик;
          Keywords[sv]=PDF;PS;PostScript;DjVU;dokument;presentation;visare;
          Keywords[tr]=PDF;PS;PostScript;DjVU;dosya;sunum;görüntüleyici
          Keywords[uk_UA]=PDF;PS;PostScript;DjVU;документ;презентація;переглядач;
          Keywords=PDF;PS;PostScript;DjVU;document;presentation;viewer;

        '';
  };

  programs.zathura.enable = true;
}
