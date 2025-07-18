def --env qb [...args: string] {
  if ($args | length) < 1 or $args.0 == "help" {
    print "Valid Options:
      m or moonlander -> cd to kb/moonlander/km/xendak 
      ap or annepro   -> cd to kb/annepro2/km/xendak
      c or compile    -> specify keyboard to compile
      f or flash      -> specify keyboard to flash
      cf or fc        -> specify keyboard to compile and flash"
  } else {
    match $args.0 {
      "m" | "moonlander" => {
        cd ($env.HOME | path join "Programming" "qmk_userspace" "keyboards" "zsa" "moonlander" "keymaps" "xendak")
      }
      "ap" | "annepro" => {
        cd ($env.HOME | path join "Programming" "qmk_userspace" "keyboards" "annepro2" "keymaps" "xendak")
      }
      "compile" | "c" => {
        if ($args | length) < 2 {
          print "Valid Options:
            m or moonlander -> compiles kb/moonlander/km/xendak 
            ap or annepro   -> compiles kb/annepro2/km/xendak"
        } else {
          match $args.1 {
            "m" | "moonlander" => {
              qmk compile -kb moonlander -km xendak
            }
            "ap" | "annepro" => {
              qmk compile -kb annepro2 -km xendak
            }
            _ => {
              print "Valid Options:
                m or moonlander -> compiles kb/moonlander/km/xendak 
                ap or annepro   -> compiles kb/annepro2/km/xendak"
            }
          }
        }
      }
      "flash" | "f" => {
        if ($args | length) < 2 {
          print "Valid Options:
            m or moonlander -> flashes kb/moonlander/km/xendak 
            ap or annepro   -> flashes kb/annepro2/km/xendak"
        } else {
          match $args.1 {
            "m" | "moonlander" => {
              sudo wally-cli ($env.HOME | path join "Programming" "qmk_userspace" "zsa_moonlander_xendak.bin")
            }
            "ap" | "annepro" => {
              print "gotta remember to specify this later"
            }
            _ => {
              print "Valid Options:
                m or moonlander -> flashes kb/moonlander/km/xendak 
                ap or annepro   -> flashes kb/annepro2/km/xendak"
            }
          }
        }
      }
      "cf" | "fc" => {
        if ($args | length) < 2 {
          print "Valid Options:
            m or moonlander -> compiles and flashes kb/moonlander/km/xendak 
            ap or annepro   -> compiles and flashes kb/annepro2/km/xendak"
        } else {
          match $args.1 {
            "m" | "moonlander" => {
              qmk compile -kb moonlander -km xendak
              if $env.LAST_EXIT_CODE == 0 {
                sudo wally-cli ($env.HOME | path join "Programming" "qmk_userspace" "zsa_moonlander_xendak.bin")
              }
            }
            "ap" | "annepro" => {
              print "gotta remember to specify this later"
            }
            _ => {
              print "Valid Options:
                m or moonlander -> compiles and flashes kb/moonlander/km/xendak 
                ap or annepro   -> compiles and flashes kb/annepro2/km/xendak"
            }
          }
        }
      }
      _ => {
        print "Unknown option. Use 'qb help' for usage information."
      }
    }
  }
}
