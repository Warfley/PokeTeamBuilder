// Strengths table
let strengths:
    [PokeType: [PokeType]] = [.normal: [],
                              .fighting: [.normal, .rock, .ice, .steel, .dark],
                              .flying: [.fighting, .bug, .grass],
                              .poison: [.grass, .fairy],
                              .ground:[.poison, .rock, .fire, .electric, .steel],
                              .rock: [.flying, .bug, .fire, .ice],
                              .bug: [.grass, .psychic, .dark],
                              .ghost: [.ghost, .psychic],
                              .steel: [.rock, .ice, .fairy],
                              .fire: [.bug, .steel, .grass, .ice],
                              .water: [.ground, .rock, .fire],
                              .grass: [.ground, .rock, .water],
                              .electric: [.flying, .water],
                              .psychic: [.fighting, .poison],
                              .ice: [.flying, .ground, .grass, .dragon],
                              .dragon: [.dragon],
                              .dark: [.ghost, .psychic],
                              .fairy: [.fighting, .dragon, .dark]
]

// list of all types
let types = { () -> [PokeType] in
    var res = [PokeType]()
    for t in iterateEnum(PokeType.self) {
        res.append(t)
    }
    return res
}()

// Weakness table
let weaknesses = { () -> [PokeType:[PokeType]] in
    var result = [PokeType:[PokeType]]()
    for t in iterateEnum(PokeType.self) {
        result[t] = [PokeType]()
        strengths.forEach( { (tt, w) in
            if w.contains(t) {
                result[t]!.append(tt)
            }
        } )
    }
    return result
}()

// lists all strength elements of a team
func findStrengths(team: [Pokemon]) -> [PokeType] {
    var res = [PokeType]()
    
    for p in team {
        for t in p.types {
            for tt in strengths[t]! {
                if !res.contains(tt) {
                    res.append(tt)
                }
            }
        }
    }
    return res
}

// removes all strengths from a list
func removeStrengths(team: [Pokemon], types: [PokeType]) -> [PokeType] {
    var res = types
    let s = findStrengths(team: team)
    res = res.filter( {!s.contains($0)} )
    return res
}

// finds all weaknesses (which are not countered by strengths)
func findWeakness(team: [Pokemon]) -> [PokeType] {
    var w = [PokeType]()
    
    for p in team {
        for t in p.types {
            for tt in weaknesses[t]! {
                if !w.contains(tt) {
                    w.append(tt)
                }
            }
        }
    }
    
    w = removeStrengths(team: team, types: w)
    
    return w
}

// find neutral elements
func findNeutral(team: [Pokemon]) -> [PokeType] {
    var res = [PokeType]()
    let s = findStrengths(team: team)
    let w = findWeakness(team: team)
    
    for t in iterateEnum(PokeType.self) {
        if (!s.contains(t) && !w.contains(t)) {
            res.append(t)
        }
    }
    
    return res
}

// generate a good team
func genTeam(_ basePokemon: [Pokemon], _ pokePool: [Pokemon],
             _ requiredTypes: [PokeType], _ count: Int) -> [Pokemon] {
    if basePokemon.count == count {
        return basePokemon
    }
    
    var team = basePokemon
    let pokePool = pokePool.shuffled()
    
    var tp = types
    
    tp = removeStrengths(team: team, types: tp)
    var minV = tp.count
    var minP: Pokemon? = nil
    
    let pool = requiredTypes.isEmpty ? pokePool :
        pokePool.filter( { (p) in
            return !p.types.filter( { requiredTypes.contains($0) } ).isEmpty
        } )
    
    for p in pool {
        let t = removeStrengths(team: [p], types: tp).count
        if t + min(basePokemon.count - (count / 2), 0) < minV {
            minV = t
            minP = p
        }
    }
    
    team.append(minP ?? pool.first!)
    
    return genTeam(team, pokePool.filter( { $0.name != team.last?.name } ),
                   requiredTypes.filter( { !team.last!.types.contains($0) } ),
                   count)
}

// converts a String to a PokeType
func getType(rawValue: String) -> PokeType? {
    for t in iterateEnum(PokeType.self) {
        if t.rawValue.lowercased() == rawValue.lowercased() { return t }
    }
    return nil
}



func main() {
    var c = -1
    var teamSize = -1
    var reqTeam = [Pokemon]()
    var pokePool = [Pokemon]()
    var reqTypes = [PokeType]()
    
    if CommandLine.argc > 3 {
        switch CommandLine.arguments[1] {
        case "r":
            pokePool = redPokemon
        case "g":
            pokePool = greenPokemon
        case "b":
            pokePool = bluePokemon
        case "y":
            pokePool = yellowPokemon
        case "go":
            pokePool = goldPokemon
        case "si":
            pokePool = silverPokemon
        case "cr":
            pokePool = crystalPokemon
        default:
            print("unkown edition")
        }
        c = Int(CommandLine.arguments[2])!
        teamSize = Int(CommandLine.arguments[3])!
        var mode = 0
        for i in 4..<CommandLine.argc {
            let s = CommandLine.arguments[Int(i)].lowercased()
            if s == "-p" {
                mode = 1
            } else if s == "-e" {
                mode = 2
            } else if s == "-t" {
                mode = 3
            } else if mode == 1 {
                for p in pokemons {
                    if p.name.lowercased() == s {
                        reqTeam.append(p)
                        break
                    }
                }
            } else if mode == 2 {
                pokePool = pokePool.filter( { $0.name.lowercased() != s } )
            } else if mode == 3 {
                if let t = getType(rawValue: s) {
                    reqTypes.append(t)
                }
            }
        }
    } else if CommandLine.argc == 1 {
        var s: String;
        while pokePool.isEmpty {
            print("Enter edition shortcut (r, g, b, y, go, si, cr)")
            s = readLine()!
            switch s {
            case "r":
                pokePool = redPokemon
            case "g":
                pokePool = greenPokemon
            case "b":
                pokePool = bluePokemon
            case "y":
                pokePool = yellowPokemon
            case "go":
                pokePool = goldPokemon
            case "si":
                pokePool = silverPokemon
            case "cr":
                pokePool = crystalPokemon
            default:
                print("unkown edition")
            }
        }
        
        repeat {
            print("Enter amount of teams to generate")
            if let inp = Int(readLine()!) {
                c = inp
            } else {
                print("invalid input")
            }
        } while c < 0
        
        repeat {
            print("Enter team size")
            if let inp = Int(readLine()!) {
                teamSize = inp
            } else {
                print("invalid input")
            }
        } while teamSize < 0
        
        repeat {
            print("Enter pokemon to be in the team (empty if none)")
            s = (readLine() ?? "").lowercased()
            for p in pokemons {
                if p.name.lowercased() == s {
                    reqTeam.append(p)
                    break
                }
            }
        } while !s.isEmpty
        
        repeat {
            print("Enter pokemon to not be in the team (empty if none)")
            s = (readLine() ?? "").lowercased()
            pokePool = pokePool.filter( { $0.name.lowercased() != s} )
        } while !s.isEmpty
        
        repeat {
            print("Enter required type (empty if none)")
            s = (readLine() ?? "").lowercased()
            if let t = getType(rawValue: s) {
                reqTypes.append(t)
            }
        } while !s.isEmpty
        
    } else {
        print("Usage: PokeTeamBuilder E M N [-p P1 P2 ...] [-e E1 E2 ...] [-t T1 T2 ...]")
        print("E: Edition (r, g, b, y, go, si, cr)")
        print("M: Amount of teams to generate")
        print("N: Teamsize")
        print("PX: default pokemon name, e.g. raichu")
        print("EX: pokemon names to be excluded, e.g. raichu")
        print("TX: Types required in the team, e.g. flying")
        return
    }
    
    while true {
        var pool = pokePool
        
        let start = reqTeam
        
        let requiredTypes = reqTypes.filter( { (t) in
            return start.filter( { $0.types.contains(t) } ).isEmpty
        } )
        
        for s in start {
            pool = pool.filter({$0.name != s.name})
        }
        
        let t = genTeam(start, pool, requiredTypes, teamSize)
        
        
        var s = ""
        for p in t { s = s + p.name + ", " }
        
        s = s.substring(to: s.index(s.endIndex, offsetBy: -2))
        
        let w = findWeakness(team: t).filter( { types.contains($0) } )
        
        let n = findNeutral(team: t).filter( { types.contains($0) } )
        print(s)
        print("weak against")
        print(w.map({ (p) -> String in
            return p.rawValue
        }))
        print("neutral against")
        print(n.map({ (p) -> String in
            return p.rawValue
        }))
        print("-------------------------------------------------------------")
        c -= 1
        guard c > 0 else {
            break
        }
    }
}


// start programm
main()
